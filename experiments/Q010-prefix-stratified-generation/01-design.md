# Q010-prefix-stratified-generation / Movement 2 — Design

**Status:** NOT YET EXECUTED.
**Location:** scratch console project, `dotnet new console -lang F#`, package
`FSharp.Compiler.Service` pinned to `43.9.101` (matches `Myriad/paket.lock`), same convention as
Q001-Q005. Not part of any committed repo; artifacts saved under `artifacts/` in this quartet folder
once built.

This design deliberately reuses `FSharpChecker.Create(..., documentSource = DocumentSource.Custom
callback, keepAssemblyContents = true)` exactly as Q001 Round 1 established it. The only new element
under test is making that callback **reentrant**: the branch of the callback that supplies the
generated file's text is allowed to call back into the same `checker`/`options` to check earlier
files, before returning.

## Round 1 — reentrancy baseline (the cheapest falsifier)

Two virtual files initially, `A.fs` and `Stratified.fs`, both backed by one `DocumentSource.Custom`
callback function.

`A.fs`:
```fsharp
module Domain
type Id = int
type Person = { Id: Id; Name: string }
```

Callback behavior:
- Requested `"A.fs"` → return the static text above immediately, no recursion.
- Requested `"Stratified.fs"` → **before returning**, call
  `checker.ParseAndCheckFileInProject("A.fs", 0, SourceText.ofString <A.fs text>, options)`
  synchronously (or `Async.RunSynchronously` with an explicit timeout, see below) on the *same*
  `checker` instance and the *same* `options` that the outer, in-flight check is already using.
  From the returned `FSharpCheckFileResults`, resolve `Person`'s fields via the typed tree
  (`ImplementationFile.Value.Declarations`, `FSharpEntity.FSharpFields`, `FSharpField.FieldType`,
  `FSharpType.StripAbbreviations().Format(ctx)` — exactly Q001 Round 1's alias-resolution technique,
  reused deliberately so this quartet's "typed, not syntactic" claim rests on the same already-
  validated mechanism, not a new one). Synthesize `Stratified.fs`'s text as a small module whose
  content depends on the *resolved* (alias-stripped) type, e.g. a function whose doc comment or a
  `let`-bound string literal names the resolved type of `Id` — chosen so the generated text
  demonstrably could not have been produced by a syntax-only reader (which would only ever see the
  literal token `Id`, never `int`).

Then perform the actual top-level check that exercises the callback:
`checker.ParseAndCheckFileInProject("Stratified.fs", 0, <placeholder text, irrelevant — the real
text comes from the callback>, options)`, wrapped in an explicit timeout (`Async.RunSynchronously`
with a `?timeout` argument, or `Task.WhenAny` against `Task.Delay(30_000)`) so a genuine deadlock
produces a clean, reportable failure instead of hanging the whole spike run.

**What Round 1 must record:**
- Did the outer check return at all, and within the timeout?
- Zero diagnostics on the spliced `Stratified.fs`?
- Confirmation the generated text used the alias-stripped (`int`), not alias-preserving (`Id`), form
  — printed side by side, same as Q001 Round 1's explicit comparison.
- Run this once under `useTransparentCompiler = true` (Q001's recommended default) and, if time
  allows, once under the default `BackgroundCompiler` (`useTransparentCompiler = false`) — these are
  architecturally different (queue/mailbox-based vs. snapshot-based), and a reentrant call landing
  back on a checker that's mid-processing-a-queued-request is a real, distinct risk under
  `BackgroundCompiler` specifically that `TransparentCompiler`'s design may not share. Not required
  for the base SHIP threshold (only `TransparentCompiler` is), but worth recording if it doesn't cost
  much extra time, since a difference here would be a genuinely new finding either way.

If Round 1 hangs or throws: stop, do not build Round 2, record it as-is, and evaluate the staged
fallback described in the hypothesis's REVISE threshold (precompute the prefix check as a fully
separate step *before* `DocumentSource.Custom` is ever installed for the project containing
`Stratified.fs`, i.e. two sequential `FSharpChecker` project setups instead of one self-referential
one) as a distinct, still-worth-recording result.

## Round 2 — cross-generator composition (the capability claim)

Three virtual files. Read `src/Myriad.Plugins/LensesGenerator.fs` first (as Q001 Round 4 read
`FieldsGenerator.fs`) to keep the stand-in structurally faithful to what that generator actually
emits — a `let <fieldName> = (getter, setter)` tuple-valued binding per field, not an invented shape.

`A.fs`:
```fsharp
module Domain
type Person = { Name: string; Age: int }
```

`B.fs` — a hand-written stand-in for `LensesGenerator`'s real output on `Person`, matching its actual
tuple-of-lambdas shape (confirm exact module/binding naming convention from the generator source
itself before writing this file, don't guess):
```fsharp
module Domain.PersonLenses
let name = (fun (x: Domain.Person) -> x.Name), (fun (value: string) (x: Domain.Person) -> { x with Name = value })
let age = (fun (x: Domain.Person) -> x.Age), (fun (value: int) (x: Domain.Person) -> { x with Age = value })
```

`Stratified.fs` — content computed on demand, standing in for a third generator (a JSON serializer)
that wants to reuse the *already-generated* lens accessors instead of re-deriving field access
itself:
- Callback for `"B.fs"` → static text, no recursion.
- Callback for `"Stratified.fs"` → reentrantly check `A.fs` then `B.fs` against the same
  `checker`/`options` (as in Round 1), typed-inspect `Domain.PersonLenses` via
  `FSharpCheckFileResults` to confirm `name` and `age` exist and resolve to the expected tuple-of-
  function types, then synthesize:
  ```fsharp
  module Domain.PersonJson
  let serialize (p: Domain.Person) =
      let getName, _ = Domain.PersonLenses.name
      let getAge, _ = Domain.PersonLenses.age
      sprintf "{\"name\":\"%s\",\"age\":%d}" (getName p) (getAge p)
  ```

**What Round 2 must record:**
- Zero diagnostics on the spliced `Stratified.fs`.
- Explicit proof the reference to `Domain.PersonLenses.name` resolved to a *real typed symbol* from
  `B.fs`, not a textual coincidence — use `FSharpCheckFileResults.GetSymbolUseAtLocation` (or
  equivalent) on the generated reference and confirm it round-trips to `B.fs`'s actual binding
  (matching range/declaration location), the same rigor Q002 used to distinguish typed resolution
  from syntax-level guessing.
- This is the concrete real-world scenario named in this thread's own prior discussion: one generator
  (a JSON serializer) reusing another generator's (Lenses) already-generated output within the same
  build, which Myriad's current per-file-blind model cannot do at all.

## Round 3 — cost sanity check (single sample, honestly labeled as such)

Time the full Round 2 sequence (outer check start to outer check completion, including the nested
reentrant calls) with `System.Diagnostics.Stopwatch`, and separately time an equivalent
non-reentrant baseline built the "staged" way (precompute `A.fs`+`B.fs`'s typed results in an
entirely separate, prior `FSharpChecker`/`options` pair, derive `Stratified.fs`'s text from that, then
run one ordinary check of a three-file static project). Report both numbers as single samples, not a
distribution — consistent with every prior quartet in this repo, and the review must say so rather
than imply otherwise. The question this answers: does routing the prefix-check through the live
callback (vs. a separate staging pass) cost meaningfully more, and is either cost small relative to
Q001's own cold-start baseline.

## Round 4 — stretch, optional, not part of the base claim

Only if time remains after Rounds 1-3: attempt the `TransparentCompiler` snapshot-with-lazy-source-
thunk route named as option (b) in `BACKLOG.md` item 11, to see whether it's a viable *alternative* to
the `DocumentSource.Custom`-reentrancy approach tested above. Explicitly out of scope for this
quartet's SHIP/REVISE/KILL verdict — report only as a side note if attempted, don't let a failure or
success here affect the main verdict, which rests entirely on Rounds 1-2.

## Reproduction

```
dotnet new console -lang F# -o q010-spike
cd q010-spike
dotnet add package FSharp.Compiler.Service --version 43.9.101
# Program.fs per round, see 02-results.md for the actual output of each
dotnet run
```

No MSBuild, no `Myriad.Sdk`, no real `.fsproj` beyond the throwaway host's own — same deliberate
scoping as Q001: the question under test is what the compiler-hosting layer can do, not the MSBuild
integration layer.

## Explicit instruction to whoever executes this (do not silently deviate)

- Do not edit this file or `00-hypothesis.md` once execution starts. If reality diverges from the
  design (an API doesn't behave as described, a signature is different, `LensesGenerator.fs`'s real
  output shape differs from the sketch above), report the correction honestly in `02-results.md` —
  do not quietly patch the design to match what was built.
- If Round 1 fails outright (hang/crash), still attempt the staged-fallback variant described above
  and report it, rather than stopping at a bare failure with nothing else recorded.
- Report the Round 1 `BackgroundCompiler`-vs-`TransparentCompiler` comparison if time allows; skip it
  cleanly and say so if it doesn't, don't let it block Rounds 2-3.
