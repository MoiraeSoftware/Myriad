# Q010-prefix-stratified-generation / Movement 3 — Execute + write up

**Status:** DONE. Rounds 1–3 run to completion, 2026-07-15. Round 4 (the optional
`TransparentCompiler` snapshot-with-lazy-thunk stretch) not attempted — out of scope for the
verdict per the design, and Rounds 1–3 already answered the load-bearing questions.
**Environment:** scratch console projects under `artifacts/`, `net9.0`, `FSharp.Compiler.Service`
`43.9.101` (matches `paket.lock`), Windows, .NET SDK 9.0.310. Artifacts:
`artifacts/round1-reentrancy`, `artifacts/round2-cross-generator`, `artifacts/round3-cost`.

## Headline

Both the mechanism claim and the capability claim came back **positive**, with one load-bearing
correction to the design's assumed trigger, one correction to the design's `LensesGenerator`
stand-in sketch, and one generator bug found and fixed along the way. The reentrant
`DocumentSource.Custom` callback works: it returned correct, complete typed results for the prefix,
with zero diagnostics on the generated file, no hang, no exception, no stack overflow — under both
`TransparentCompiler` and `BackgroundCompiler`. A third generator (a JSON serializer) then reused a
second generator's (Lenses) already-generated, still-virtual output by verified typed symbol
resolution within one in-flight compilation.

## Correction to the design's trigger (load-bearing — read before the round results)

The design's Round 1 sketch says to fire the reentrant callback via
`checker.ParseAndCheckFileInProject("Stratified.fs", 0, <placeholder text>, options)`, expecting
"the real text comes from the callback." **That is not how it behaves at `43.9.101`.** When you
pass explicit source text as the `source` argument of `ParseAndCheckFileInProject`, FCS uses that
text for that file and does **not** consult `DocumentSource.Custom` for it — so the placeholder is
what gets typechecked and the reentrant callback for `Stratified.fs` never fires at all. Measured:
callback-fired = `false`, outer check returned in 733 ms typechecking the placeholder.

The callback only fires when FCS is made to pull `Stratified.fs`'s source from the document source
itself. Because `Stratified.fs` is the last file, the natural trigger is
`checker.ParseAndCheckProject(options)`, which reads every file's text through
`DocumentSource.Custom` (the hypothesis explicitly allowed "`ParseAndCheckProject`" as the outer
call, so this is within the pre-registration, but the specific `ParseAndCheckFileInProject`
+placeholder form in the design is wrong and is reported here rather than silently patched). Every
positive result below uses `ParseAndCheckProject` as the outer, in-flight check; the reentrant
nested calls inside the callback remain `ParseAndCheckFileInProject` on earlier files exactly as the
hypothesis describes.

## Round 1 — reentrancy baseline (the cheapest falsifier)

Two virtual files, `A.fs` (`module Domain` / `type Id = int` / `type Person = { Id: Id; Name: string }`)
and `Stratified.fs`, one reentrant `DocumentSource.Custom` callback. When `Stratified.fs`'s text is
requested, the callback first calls `checker.ParseAndCheckFileInProject("A.fs", …)` on the same
in-flight checker/opts, resolves `Person.Id`'s type from the typed tree, and synthesizes
`Stratified.fs`'s text from the alias-**stripped** resolved type. Each outer check was raced against
a 30 s `Task.Delay` timeout so a genuine deadlock would be a clean, reportable failure.

| outer check | config | callback fired | result | time |
|---|---|---|---|---|
| `ParseAndCheckFileInProject` + placeholder (design's literal form) | TransparentCompiler | **no** | typechecked the placeholder, callback never invoked | 733 ms |
| `ParseAndCheckProject` | TransparentCompiler | **yes** | correct, 0 errors on `Stratified.fs` | 359 ms |
| `ParseAndCheckProject` | BackgroundCompiler | **yes** | correct, 0 errors on `Stratified.fs` | 358 ms |

Alias-resolution proof (the "typed, not syntactic" check), from the reentrant callback's own read of
`A.fs`'s typed tree:

```
Person.Id via typed tree, alias-STRIPPED : System.Int32
Person.Id via typed tree, alias-preserving: Domain.Id
```

The generated `Stratified.fs` (never written to disk) contains the string `"System.Int32"` — the
alias-stripped form — which a syntax-only reader could never have produced, since syntax only ever
sees the token `Id`. This reuses Q001 Round 1's exact `StripAbbreviations().Format(ctx)` technique;
as in Q001, plain `.Format(ctx)` does **not** strip the alias (it prints `Domain.Id`), the strip is
explicit.

**Round 1 result: the reentrant mechanism works, and no hang/exception/stack-overflow occurred.**
Two findings beyond the pass/fail:

1. **`BackgroundCompiler` also tolerated the reentrancy.** The design flagged
   `BackgroundCompiler`'s queue/mailbox architecture as a distinct deadlock risk that
   `TransparentCompiler`'s snapshot design might not share. In this scenario it did **not**
   deadlock — the reentrant `ParseAndCheckFileInProject("A.fs")` from inside the callback returned
   correct results under both compilers, at effectively identical cost (358 vs 359 ms). This is a
   genuinely new finding either way; it does not prove `BackgroundCompiler` is safe under all
   reentrancy shapes, only that it survived this one.
2. The SHIP threshold names `TransparentCompiler` specifically, and that path passed cleanly, so the
   threshold is met without leaning on the `BackgroundCompiler` observation.

## Round 2 — cross-generator composition (the capability claim)

Three virtual files: `A.fs` (the `Person` record), `B.fs` (a hand-written `LensesGenerator`
stand-in), `Stratified.fs` (a JSON serializer computed on demand). Outer check =
`ParseAndCheckProject`; the reentrant callback for `Stratified.fs` checks `A.fs` then `B.fs`,
typed-inspects `Domain.PersonLenses`, and emits a serializer that reuses the discovered lens
accessors.

### Correction to the design's `B.fs` sketch (checked against `LensesGenerator.fs`, not guessed)

`01-design.md`'s `B.fs` sketch is **not** faithful to what `src/Myriad.Plugins/LensesGenerator.fs`
actually emits. Two concrete divergences, corrected in the artifact:

- **Binding names.** The design sketch writes `let name = …`, `let age = …` (lower-cased). The real
  generator (`CreateLenses.createLensForRecordField`, `letPat = SynPat.CreateNamed fieldName`) emits
  the binding under the **field's own name verbatim** — `let Name = …`, `let Age = …`. So the
  serializer must reference `Domain.PersonLenses.Name`, not `.name`.
- **Setter parameter order.** The design sketch writes the setter as
  `(fun (value: string) (x: Domain.Person) -> …)` (value-first, the Aether/piped order). The real
  generator's **default** (`usePipedSetter = false`, i.e. `aetherStyle = false`) emits the opposite
  order, `(fun (x: Person) (value: string) -> …)` (record-first). The value-first order is only
  produced when `pipedsetter = true`.

The faithful `B.fs` used (matching the real tuple-of-two-parenthesised-lambdas shape, the
`<Record>Lenses` nested module, the `open` of the parent namespace, and the recursive-namespace
wrapper `SynModuleOrNamespace.CreateNamespace(isRecursive = true)`):

```fsharp
namespace rec Domain

module PersonLenses =
    open Domain
    let Name = ((fun (x: Person) -> x.Name), (fun (x: Person) (value: string) -> { x with Name = value }))
    let Age = ((fun (x: Person) -> x.Age), (fun (x: Person) (value: int) -> { x with Age = value }))
```

### What the reentrant callback saw and produced

The callback's typed inspection of the still-virtual `B.fs` (from inside the in-flight
`ParseAndCheckProject`) discovered both lens bindings with their exact resolved tuple types:

```
[callback] typed-inspected Domain.PersonLenses, found 2 lens binding(s):
    Name : (Domain.Person -> Microsoft.FSharp.Core.string) * (Domain.Person -> Microsoft.FSharp.Core.string -> Domain.Person)   (getter returns Microsoft.FSharp.Core.string)
    Age  : (Domain.Person -> Microsoft.FSharp.Core.int) * (Domain.Person -> Microsoft.FSharp.Core.int -> Domain.Person)          (getter returns Microsoft.FSharp.Core.int)
```

The binding names, and the `%s`-vs-`%d` format specifiers, were both derived from that typed data
(not hardcoded). Generated `Stratified.fs` (never written to disk, 0 diagnostics):

```fsharp
module Domain.PersonJson

let serialize (p: Domain.Person) =
    let getName, _ = Domain.PersonLenses.Name
    let getAge, _ = Domain.PersonLenses.Age
    sprintf "{\"name\":\"%s\",\"age\":%d}" (getName p) (getAge p)
```

### Typed-resolution proof (the part that matters as much as "it compiles")

Two independent checks, both confirming the generated reference resolves to a real typed symbol
declared in `B.fs`, not a textual coincidence:

- `GetAllUsesOfAllSymbolsInFile()` on the re-checked `Stratified.fs`: **2** uses resolved to
  `PersonLenses` members, both with `DeclarationLocation.FileName = C:\virt\B.fs`:
  ```
  ref 'Name' at (4,21--4,45) -> symbol declared at C:\virt\B.fs (in B.fs: true)
  ref 'Age'  at (5,20--5,43) -> symbol declared at C:\virt\B.fs (in B.fs: true)
  ```
- `GetSymbolUseAtLocation(line, endCol, lineText, ["Domain";"PersonLenses";"Name"])` on the exact
  generated `Domain.PersonLenses.Name` reference:
  ```
  resolved to symbol 'Name', declared at file=C:\virt\B.fs range=Some (5,8--5,12)
  CONFIRMED: 'PersonLenses.Name' round-trips to B.fs's actual binding.
  ```
  Range `(5,8--5,12)` is exactly the `Name` binding's declaration site in `B.fs`.

**Round 2 result: capability claim PASS.** A third generator reused a second generator's
already-generated, still-virtual output by verified typed resolution, within one in-flight
compilation — the composition current Myriad's per-file-blind model structurally cannot do.

### Bug found and fixed while running (reported, not hidden)

Two API/behaviour corrections surfaced during Round 2, neither anticipated by the design:

1. **`FSharpMemberOrFunctionOrValue` has no `EnclosingEntity`** at `43.9.101`; the member is
   `DeclaringEntity : FSharpEntity option`. Used that instead.
2. **Getter return types format fully-qualified.** `FSharpType.Format` renders the getter's return
   as `Microsoft.FSharp.Core.string`, not `string`. My first synthesizer compared `ret = "string"`,
   which failed, so it emitted `%d` for the string field and produced a real type error
   (`The type 'string' is not compatible with … printf-style format string`). Fixed by matching on
   the suffix (`ret.EndsWith "string"`). Worth stating plainly: the typed data was correct and
   complete; the bug was in my formatting assumption, not in FCS or the mechanism. After the fix,
   0 diagnostics.

### A false alarm worth recording so Movement 4 doesn't misread it

An earlier run showed `PersonLenses` present but with **zero** member values (`sub = 0`,
empty `MembersFunctionsAndValues`) and an empty generated serializer. This looked like a
reentrancy-induced "incomplete/stale typed results" failure — one of the pre-registered REVISE
conditions. It was **not.** Root cause: my `B.fs` string literal used F# `\`-line-continuation
incorrectly, producing literal backslashes at line starts and a malformed module body. A separate
probe (four `B.fs` shapes, plus a middle-file-in-3-file-project variant, plus a variant with a
genuinely reentrant third-file callback) confirmed that a well-formed `B.fs` yields the full two-value
impl contents in every case, including under reentrancy — so the reentrant mechanism was never the
cause. Once `B.fs` was written with explicit `\n` (as above), the values appeared and the round
passed. Reported here because the transient symptom looks exactly like a mechanism failure and
should not be mistaken for one.

## Round 3 — cost sanity check (single sample, honestly labelled)

Timed the reentrant path (one `ParseAndCheckProject` whose callback reentrantly checks the A+B
prefix and synthesizes `Stratified.fs`) against the staged path (precompute A+B's typed results in a
separate prior checker/opts, derive `Stratified.fs`'s text, then one ordinary check of a static
3-file project). Each path run twice: run #1 with a cold process, run #2 warm.

| run | reentrant | staged |
|---|---|---|
| #1 (cold process) | 975 ms | 423 ms |
| #2 (warm) | **126 ms** | **309 ms** |

Both strategies produced **identical** `Stratified.fs` text.

Reading these honestly: run #1's reentrant number is confounded — it is the first FCS work in the
process and absorbs one-time JIT/assembly-load cold-start that run #1's staged number (running
second, into an already-warmed process) does not. Run #2, with both paths warm, is the fair
comparison, and it inverts the naive expectation: the reentrant path is **cheaper** (126 vs 309 ms),
because the staged path constructs **two** separate cold `FSharpChecker` instances (two independent
project/snapshot setups) whereas the reentrant path reuses one checker whose caches the nested prefix
checks hit warm. So reentrancy is not a cost penalty here; if anything the two-checker staging is the
more expensive shape once cold-start is factored out.

Single timed sample per path per run, no repeated-trial distribution — the same convention every
prior quartet in this repo uses, stated so Movement 4 doesn't read it as a benchmark distribution.
For scale context, Q001's cold single-file check was ~937 ms; these cold numbers are the same order.

## What was tested vs. what was intended (summary of deviations)

- **Trigger:** used `ParseAndCheckProject` as the outer in-flight check, not the design's
  `ParseAndCheckFileInProject`+placeholder form — because the latter does not fire the callback at
  `43.9.101` (explicit `source` short-circuits `DocumentSource`). Within the hypothesis's stated
  allowance ("`ParseAndCheckProject`"), but a real correction to the design's specific sketch.
- **`B.fs` shape:** corrected to the real `LensesGenerator` output (field-name-cased bindings,
  record-first default setter order), not the design's lower-cased / value-first sketch.
- **API names:** `DeclaringEntity` (not `EnclosingEntity`); `FSharpDiagnostic`/
  `FSharpDiagnosticSeverity` live in `FSharp.Compiler.Diagnostics`; getter return types format
  fully-qualified. All reported above.
- **Round 4:** skipped (optional stretch, explicitly excluded from the verdict).
- **Not tested:** any real IDE host (only `FSharpChecker` as a library, same scoping caveat Q001/Q006
  carry); scaling past three files / two generators; reentrancy shapes other than "check earlier
  files of the same project from inside the callback"; whether `BackgroundCompiler` survives deeper
  or concurrent reentrancy. `TransparentCompiler` remains labelled experimental at `43.9.101`, the
  same inherited caveat every Thread-1 quartet carries.
