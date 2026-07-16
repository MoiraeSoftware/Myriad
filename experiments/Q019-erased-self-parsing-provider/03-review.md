# Q019-erased-self-parsing-provider / Movement 4 — Adversarial review

## Reproduction status — read this first, it bounds everything below

I executed this quartet independently. The tooling outage that blocked Q016's reviewer was not in
effect: every `dotnet build`/`dotnet run` I issued ran. So the claims below are from observations I
made, not inherited from saved logs. I went past reproduction on four axes the executor did not:
removing `assemblyReplacementMap` as a controlled single-variable test, a nested/non-primitive field
shape, three failure-mode shapes (missing record name, non-record type, malformed source), and a
direct check of the `0.8.6.0` version-leak claim.

What I verified by executing:

- **All three rounds reproduce cell-for-cell.** Round 1: `0 diagnostics`, 1203ms cold (vs 1202ms
  logged). Round 2: `0 diagnostics`, 40ms after the live `email` edit (vs 39ms logged), `Person.fs`
  restored to two fields afterward. Round 3: a clean `dotnet build` of the independent
  `RuntimeConsumer`, then `dotnet run` printing `name="Ada" age=42` via the provider matching direct
  field access, `agreement: name=true age=true`, exit 0. The numbers in `02-results.md` are honest.
- **`Myriad.Core.dll` was already built** at `src/Myriad.Core/bin/Release/net9.0/`, so I did not have
  to build it — and `01-design.md`'s reproduction block *does* include `dotnet build src/Myriad.Core
  -c Release` as its first step, so there is no missing-step reproducibility gap of the kind the review
  brief asked me to watch for.
- **The precondition holds as claimed.** The harness's `mkOptions` reference list greps clean for
  `SampleLib`/`Person` (`suspicious refs: [||]`, reproduced); the only references are the net9.0 ref
  pack, `FSharp.Core`, and `MyriadPreview.Runtime.dll` (the bare `TypeProviderAssembly`-marker
  assembly, which carries no record type of its own). The target `SampleNs.Person` genuinely is absent
  from every reference path when Round 1 resolves the provider's members. That is the real result and
  it stands.

## The load-bearing "found by running" correction does not reproduce

`02-results.md`'s correction 3 is the write-up's single biggest self-reported discovery: that
`assemblyReplacementMap` is "load-bearing, not optional" even for an *erased* provider, and that
omitting it produces `"The type 'Fields,...' is required here and is unavailable. You must add a
reference to assembly 'MyriadPreview.DesignTime, Version=0.8.6.0...'"`. The provider's source carries a
14-line comment building the same causal story, and the result is elevated to "a new, general finding
for this lineage."

**I removed exactly that one argument** (`assemblyReplacementMap = [ ("MyriadPreview.DesignTime",
"MyriadPreview.Runtime") ]`), changing nothing else — `addDefaultProbingLocation = true` left in place —
rebuilt `MyriadPreview.DesignTime`, and reran the full quartet. Result: **all three rounds pass
identically.** Round 1's `ParseAndCheckFileInProject` reports `0 diagnostics` (1195ms). A *clean*
`dotnet build` of `RuntimeConsumer` (obj/bin deleted first, so the provider genuinely re-ran)
**succeeds with 0 errors**, and `dotnet run` prints the same `agreement: name=true age=true`. The FS0074
"must add a reference to assembly 'MyriadPreview.DesignTime'" error the correction pins on the map's
absence **never appears in any path this quartet exercises** — not the parse-and-check path, not the
full compile-and-emit path, not runtime. I then restored the argument and rebuilt.

So the map is present and harmless, but it is **not load-bearing** here, and the correction's causal
story ("Without it, FCS rejects every provided type") is not reproducible under a controlled
single-variable test. The most likely explanation is post-hoc misattribution: the executor changed more
than one thing between the failing and passing attempts, or hit a stale-build state, and credited the
recovery to the map. FS0074 of that exact shape is real for erased providers when the design-time and
runtime assemblies are genuinely distinct *and the erased members' signatures reference a design-time-only
type the runtime assembly doesn't re-export* — but every member here erases to `obj` and a
`ProvidedConstructor` returning its own boxed argument, so nothing crosses the assembly boundary that
would need remapping, which is exactly why removing the map is inert. This does not sink the result:
the three rounds still pass *with* the map. But the write-up's most confidently stated "general finding
for this lineage" is wrong as written and should be struck or downgraded to "included defensively,
mirroring Q016; not shown to be necessary for an all-`obj` erased provider." A review whose remit is to
catch this repo's overclaiming has to name it plainly.

## Generalization: every edge case degrades gracefully, but only because everything is `obj`

The executor tested one flat 2-to-3-field primitive record. I built four shapes it never tried, against
the checked-in provider (unmodified), each through a real `dotnet build` of a consumer referencing
`MyriadPreview.Runtime.dll`:

- **Nested / non-primitive field.** `Company = { title: string; addr: Address }` where `Address` is
  itself a record. `p.addr` round-trips: `(addrV :?> Address) = c.addr` is `true`. But this "generalizes"
  for a trivial reason — the member is typed `obj`, so `GetProperty("addr").GetValue(o)` boxes the
  `Address` and the caller casts it back. A non-primitive field buys nothing and costs nothing precisely
  because the provider discards the field's type. This is the flip side of the `obj` simplification, not
  evidence the mechanism scales to typed members.
- **Record name matching nothing** (`TestNs.DoesNotExist`): surfaces as a clean `error FS3033: The type
  provider ... reported an error: MyriadPreview: record 'TestNs.DoesNotExist' not found in '...'`. The
  `failwithf` path becomes a real compiler diagnostic naming the record and the file, not a crash.
- **Non-record type** (`type Marker = class end`): also hits the "not found" branch, because
  `Ast.extractRecords` only ever returns records, so a non-record never reaches the `fieldNames.IsEmpty`
  guard. That "parsed with zero fields" branch is therefore effectively dead code — not a defect, but
  worth knowing it can't fire via this path.
- **Malformed, non-parseable source**: `Ast.fromFilename` throws a `ParseException` carrying the full
  parse-diagnostic list, which FCS surfaces as `error FS3033` with every parse error and range. The
  design-time host **does not hang or crash** — the build completes and fails cleanly. The message is a
  raw exception dump (noisy) rather than a curated one line, but it is comprehensible and names the
  offending positions.

Net: the provider is more robust than `02-results.md` bothered to claim — all four failure and
generalization shapes behave sanely. But the robustness is cheap because the type system is doing none
of the work; the members are `obj` and the reflection is untyped.

## Round 3 is a mechanism result, and in isolation proves only that reflection works

The runtime capability threshold — "a provided member reads the correct field value via dynamic
reflection, matching direct field access" — is met, but the provider contributes nothing to it that a
one-line reflection helper wouldn't. The erased `getterCode` is literally
`o.GetType().GetProperty(fieldName).GetValue(o)`. Round 3's `"Ada"`/`42` agreement is a trivial echo:
it would be byte-identical whether the value came from the provider, from a hand-written
`getField o "name"`, or from `person.name` directly. The runtime comparison cannot discriminate the
provider's presence — the same NULL-adjacent trap Q001 named for structural-echo generators and Q016's
own review flagged for its "two independent checks agree" claim. The genuine, non-trivial part of this
quartet lives entirely in **Rounds 1-2**: design-time member *names* appearing, and updating live on a
disk edit, with the target record type provably absent from every reference path. `02-results.md` mostly
keeps the three claims apart honestly, but "its members are genuinely functional at runtime" reads as if
Round 3 adds capability weight it does not — it adds plumbing-works weight. Cite Rounds 1-2 as the
result; Round 3 is the confirmation that the erasure target isn't decorative, nothing more.

## Contradiction gate: genuinely new ground, but a narrower slice than "closes Q006's gap"

This does **not** contradict Q006. Q006's wall binds a *generative* provider that must correspond to a
real backing type the compiler can resolve; Q019 sidesteps it exactly as the hypothesis claims — the
provider never asks FCS to resolve `SampleNs.Person`, it self-parses the file and erases its own
invented members to `obj`. It is genuinely new ground in this lineage: the **first** erased provider in
either thread, the **first** to call Myriad's own `Myriad.Core.Ast` from inside a provider (rather than
reimplementing AST-walking as Q002 did, or forwarding into Myriad's *compiled* output as Q016-18 did),
and the first to get design-time member visibility for a type that is provably compiled nowhere. It also
does not hit Q016-18's dead end: there is no `Assembly.LoadFrom`, no satellite DLL, no file lock, so the
Q016/Q018 Round-2 lock that killed live re-exposure for the generative satellite route simply has no
analogue here — the live edit works (Round 2, 40ms, reproduced). That is a real and clean distinction
from the Q016-18 conclusion, and it should be stated positively.

But, like Q015 relative to Q014, it sounds like it closes more of the gap than it does, and the review
must draw the line precisely:

1. **Member names, not member types.** Every provided member is `obj`. In a real IDE this surfaces as
   `p.name : obj`, `p.age : obj` — you get completion on the *names*, but no type, no `string`/`int`,
   no downstream typed IntelliSense. That is materially weaker than what Myriad's actual generated code
   gives, and it is the single biggest narrowing. The hypothesis discloses it honestly as a named
   simplification; the review's job is to insist it is load-bearing to how far the result travels, not a
   footnote.
2. **It is not Myriad's usage shape.** Myriad's dominant pattern is `[<Lenses>]`/`[<Fields>]` *on the
   record*, producing typed members the user sees on their own type. Q019's authoring model is a
   *parallel* `Fields<"path/to/file.fs", "RecordName">` provided type the user points at a file. It does
   not put anything on the user's own record, and it requires hard-coding a source path as a static
   literal (Round 3's own comment records that `__SOURCE_DIRECTORY__ + "..."` is rejected in that
   position — only an absolute literal works). So this is a new *preview* mechanism sitting next to the
   record, not Myriad's own generated output made live in the IDE. It sidesteps Q006's wall by not being
   the thing behind the wall.
3. **`FSharpChecker`-as-library, never a real IDE host.** The standing caveat since Q006, unchanged: no
   Ionide/FSAC session was involved, so "would show live in the IDE" remains inferred from the checker's
   behavior, not observed in an editor.
4. **The erased-sample-file mechanism is well-trodden outside this repo.** An erased provider that reads
   an external sample file and invents members from it is the FSharp.Data / `JsonProvider` /
   `CsvProvider` pattern, roughly fifteen years old. What is new here is *this repo's* first use of it,
   the composition with `Myriad.Core.Ast`, and the explicit design-time/runtime decoupling proof — not
   the provider machinery itself. The pre-registered NULL threshold anticipated exactly this and judged
   it unlikely to be the whole verdict; I agree it is not NULL, but the result's weight is "a known
   ecosystem pattern applies here and composes with Myriad's parser," not "new provider machinery was
   discovered."

## Smaller points, checked

- **The `0.8.6.0` version tangent checks out and masks nothing.** The root `Directory.Build.props` sets
  `<VersionPrefix ...>0.8.6</VersionPrefix>`, and the built `MyriadPreview.DesignTime.dll` reports
  assembly version `0.8.6.0` (confirmed via `AssemblyName.GetAssemblyName`). MSBuild's directory-scoped
  auto-import walks from `experiments/.../artifacts/` up to the repo root and applies it. The
  explanation in correction 3 is correct and harmless. The same auto-import also injects
  `<TargetFrameworks>net9.0</TargetFrameworks>` and a `<None Include="..\..\README.md">` into every
  experiment project; both are inert here. Nothing is being masked.
- **Memoization is correct-by-reading, not correct-by-observation.** `successCache` keyed on
  `(sourcePath, recordName)` and the watcher clearing it before `Invalidate()` are the right shape
  (inherited from Q011), but `02-results.md` admits call counts were never logged, so whether
  `DefineStaticParameters` actually fired more than once in this run is unconfirmed. This is honestly
  disclosed and does not change the verdict.
- **Round 3's independence is real.** `RuntimeConsumer` defines its own `Person`, references only
  `MyriadPreview.Runtime.dll`, shares no assembly with the harness, and the design-time file path it
  points at is the on-disk `SampleLib/Person.fs`. The decoupling the design demanded is genuine — the
  Round 1 "compiled nowhere" precondition cannot be doing Round 3's work.
- **The dropped setter is correctly scoped out, not a silent cut.** Standard F# records are immutable;
  a `PropertyInfo.SetValue` against a get-only compiled property would throw `ArgumentException` and
  demonstrate nothing. The hypothesis flagged this as a named simplification up front. Fair.

## Verdict

**SHIP, scoped — and strike correction 3.**

Reading the four pre-registered thresholds literally: the cheapest falsifier passed (Round 1,
reproduced), so KILL is out — `Myriad.Core.Ast` loaded and ran fine inside the design-time host, and
erased members built from syntactic-only information were accepted by FCS with the target type absent.
The live edit was picked up via `FileSystemWatcher` + `Invalidate()` with no rebuild (Round 2,
reproduced), and the runtime capability held (Round 3, reproduced). All three SHIP conjuncts are met and
independently reproduced. REVISE's registered trigger ("the design-time half works but the runtime half
has a real gap not anticipated") did not fire — the runtime half worked, and the `obj`-typed narrowing
was pre-registered as accepted scope, not a surprise. NULL is out: this is more than a bare
sample-file-provider re-tread because of the `Myriad.Core.Ast` composition and the explicit
decoupling proof, and the pre-registration reasoned this out in advance.

So SHIP by the letter of the pre-registration. But it is a *scoped* SHIP, in the Q010/Q015 sense, and
several things travel with it that matter more than the label:

- **Correction 3 does not reproduce.** Removing `assemblyReplacementMap` as a controlled single-variable
  change leaves all three rounds passing, including a clean full `dotnet build` and runtime. The claim
  that it is "load-bearing for an erased provider too" and a "general finding for this lineage" is a
  post-hoc misattribution and should be removed from `02-results.md` and the provider comment. This is a
  write-up-accuracy defect, not a result failure, but it is exactly the kind of overclaim this discipline
  exists to catch.
- **The win is design-time member *names*, not typed IntelliSense.** Everything is `obj`. A real editor
  shows `p.name : obj`. That ceiling is inherent to the erasure choice made here, not a tuning gap.
- **It is a parallel `Fields<file, name>` preview mechanism, not Myriad's `[<Lenses>]`-on-the-record
  usage shape restored to the IDE.** It sidesteps Q006's wall by not being the thing behind it. Do not
  cite this as "closes Myriad's IDE-invisibility gap" — it opens a different, adjacent door.
- **Round 3 proves reflection works, not that the provider adds runtime capability.** The capability is
  entirely in Rounds 1-2.
- **`FSharpChecker`-as-library, toy flat record, well-trodden erased-sample-file pattern.** Standing
  caveats, honestly disclosed, and they bound how far the result generalizes.

The clean, unqualified win worth stating: this is the first thing in either thread to get live,
build-free, design-time member visibility for a record type that is compiled nowhere reachable, by
calling Myriad's own parser from inside an erased provider — and it does so without ever touching the
`Assembly.LoadFrom` file-lock that dead-ended Q016-18. That is real, new, and reproduced. Its reach is
just much narrower than "the IDE-invisibility gap is solved."

## Follow-ups, prioritized

1. **Type the members, then re-test.** The whole practical value hinges on whether provided members can
   carry the field's real declared type (`string`/`int`/a nested record type) instead of `obj`. For a
   primitive that is resolvable from the BCL without touching the in-progress compilation; for a
   field whose type is itself defined in the same uncompiled file, this walks straight back toward
   Q006's wall and is the genuinely interesting open question. Until this is done, the result is
   name-completion only.
2. **Strike or downgrade correction 3** in `02-results.md` and the provider comment, per the controlled
   test above.
3. **A real-IDE (Ionide/FSAC) host test** — the standing Q006 caveat. Whether an actual editor shows the
   provided names live on the disk edit, and how `obj`-typed members read in completion, is unobserved.
4. **A real Myriad generator shape** — Lenses' copy-and-update getter/setter, or `[<Fields>]`'s actual
   output — rather than a flat read-only record, to see whether the self-parsing/erasure model can
   express anything Myriad actually ships, or only field enumeration.
