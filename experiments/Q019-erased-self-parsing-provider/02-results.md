# Q019-erased-self-parsing-provider / Movement 3 — Results

**Status:** DONE. All three rounds ran and passed against `00-hypothesis.md`'s pre-registered
thresholds. See `03-review.md` for the adversarial pass.

## Summary

Built `MyriadPreview.DesignTime`/`MyriadPreview.Runtime` (an erased type provider,
`MyriadPreview.Provided.Fields<SourceFilePath, RecordName>`) whose instantiation function calls
`Myriad.Core.Ast.fromFilename` and `Ast.extractRecords` — Myriad's own real, unmodified parsing code,
referenced as a `<Reference>` to `src/Myriad.Core/bin/Release/net9.0/Myriad.Core.dll` built from this
repo, not reimplemented — to discover a record's field names purely syntactically, and exposes one
erased `obj`-typed property per field, backed at runtime by plain `System.Reflection` (`GetProperty`/
`GetValue`), not a compiled backing type. All three pre-registered rounds passed. Full source under
this quartet's own subfolders; raw run output in `run-logs/`.

## Round 1 — design-time member resolution, target type never compiled anywhere

**PASS**, first attempt after three corrections (below). `SampleLib/Person.fs` (`namespace SampleNs` /
`type Person = { name: string; age: int }`) exists only as text on disk — nothing in the harness, the
provider, or the reference set compiles or references it. `Harness/Program.fs`'s `mkOptions` builds a
real, hand-built (non-script) `FSharpProjectOptions` — `--noframework` plus the net9.0 ref pack,
`FSharp.Core`, and `MyriadPreview.Runtime.dll` only — and a precondition check greps `OtherOptions` for
anything mentioning `SampleLib`/`Person` before the check runs (confirmed empty, per `run-logs/
harness-round1-round2.txt`). `checker.ParseAndCheckFileInProject` on a consumer using
`P().name`/`P().age`: **0 diagnostics**, 1202ms cold. This is the core novel claim: the provider's
members resolved cleanly with the named record type provably absent from every reference path the
checking project could see — Q006's wall never had anything to bind to, because nothing here ever asked
FCS to resolve `SampleNs.Person`.

### Corrections found by running, not anticipated by `01-design.md`

1. **`Ast.fromFilename` returns `Async<(ParsedInput * string list) array>`, not a single tuple.**
   `01-design.md` assumed a bare `Async<ParsedInput * ...>`. Fixed with `|> Array.head`. A real API
   shape not previously exercised by any quartet in this repo (every prior quartet either parsed via
   raw `Fantomas.Core.CodeFormatter` calls directly or never called `Ast.fromFilename` at all).
2. **A `SynTypeDefn`'s own `SynComponentInfo` carries only the type's bare name (`"Person"`), never the
   enclosing namespace.** The first Round 1 attempt matched nothing (`RecordName "SampleNs.Person"`
   against a bare `"Person"`) because this was assumed rather than checked. `Ast.extractRecords`
   already returns `(LongIdent * SynTypeDefn list) list` — namespace and type kept separate precisely
   because they don't come bundled — so the fix reassembles the full name from both halves
   (`AstHelpers.getFullTypeName`) instead of reading it off the `SynTypeDefn` alone.
3. **`assemblyReplacementMap` is load-bearing for an erased provider too, not just Q016's generative
   one.** Omitting it (on the theory that an erased type, having no independent backing type, wouldn't
   need source/target assembly identity mapping) produced a real, previously-unseen-in-this-repo FCS
   error: `"The type 'Fields,SourceFilePath=...' is required here and is unavailable. You must add a
   reference to assembly 'MyriadPreview.DesignTime, Version=0.8.6.0...'"` — read literally, the fix
   the message names. Adding `assemblyReplacementMap = [("MyriadPreview.DesignTime",
   "MyriadPreview.Runtime")]` to the `TypeProviderForNamespaces` base call (mirroring Q016's own
   provider, whose necessity for *that* quartet's generative satellite-forwarding case was assumed to
   be specific to that shape) resolved it immediately. This is a new, general finding for this
   lineage: **`assemblyReplacementMap` is apparently required for any provider construction pattern in
   this SDK where the design-time and runtime assemblies are separate files**, isErased notwithstanding
   — not verified against every possible provider shape, but real for both quartets that have now hit
   it. The `0.8.6.0` in the error message is this repo's own `Directory.Build.props` `VersionPrefix`,
   picked up by MSBuild's directory-scoped auto-import from `experiments/Q019.../artifacts/` up to the
   repo root — harmless here, but worth knowing about for any future quartet's project under
   `experiments/`.

## Round 2 — live-edit re-parse via `FileSystemWatcher` + `Invalidate()`, no rebuild

**PASS**, same `FSharpChecker` instance as Round 1 (`Harness -- all`, not two separate processes).
`Person.fs` edited on disk to add a third field (`email: string`), a `FileSystemWatcher` registered by
the provider on that exact path (`ensureWatcher`) fires and calls `this.Invalidate()`; after a 1.5s
wait, a *new* consumer file referencing `P().email` was checked on the same checker instance: **0
diagnostics**, 39ms (vs. Round 1's 1202ms cold — the same order-of-magnitude live-recheck speedup every
prior quartet's live-edit test has shown, Q006's 47ms and Q008/Q09's 15-57ms band included). No
`dotnet build` ran anywhere between the edit and the re-check. `Person.fs` was restored to its original
two-field content afterward (confirmed in the log and by direct `cat` after the run), so the repo's
working state is unchanged by running this quartet's own reproduction steps.

## Round 3 — runtime capability, decoupled from Round 1's precondition

**PASS**, with one pre-registered scope change (see below). `RuntimeConsumer/Program.fs` is a genuinely
independent, separately-compiled console project — no reference to anything Myriad-specific, no shared
assembly with the provider's own test harness — defining its own `type Person = { name: string; age:
int }` and constructing `{ name = "Ada"; age = 42 }`. Built with a real `dotnet build` (not just
`FSharpChecker`) and actually run (`dotnet run`), matching Q006 Round 2 and Q016 Round 1's own
methodology. The provider's `P(box person)` erases to the boxed instance itself (its `ProvidedConstructor`'s
`invokeCode` is `fun args -> args.[0]`); `p.name`/`p.age`, each backed by `ty.GetProperty(fieldName).
GetValue(o)`, returned `"Ada"`/`42`, matching `person.name`/`person.age` read directly, byte-for-byte
(`run-logs/round3-runtime.txt`). Exit code 0.

**Scope change from `01-design.md`, reported honestly rather than silently matched to the design:** the
design's Round 3 described both a getter *and* a setter (`FSharpValue`-style reflection write). This
was dropped before writing any setter code: standard F# records are immutable — their compiled fields
have no `set` accessor for reflection to target at all (`Person.fs` was never marked `[<CLIMutable>]`,
and wasn't going to be, since that would make the test's "ordinary F# record with no relationship to
the provider" precondition false). A `PropertyInfo.SetValue` call against a genuinely get-only compiled
property would just throw `ArgumentException`, not demonstrate anything about the provider. The
hypothesis's own wording already flagged this as a real, named simplification ("field write via
`FSharpValue`-style reflection, not an immutable-copy update") rather than assuming it away — this is
that flag resolving to "not applicable to an ordinary immutable record," not a silent scope cut.

## What this does and doesn't show

- **Shown:** an erased provider whose members come from Myriad's own real parsing code, applied to a
  file whose named type is not compiled anywhere reachable, resolves at design time with zero
  diagnostics, stays live across a real on-disk edit with no rebuild, and its members are genuinely
  functional at runtime once a real object of matching shape exists elsewhere — three claims kept
  separate and each independently verified, not assumed to transfer from one to the next.
## Erratum (added after `03-review.md`, not editing the record above)

**Correction 3 above does not reproduce and should not be trusted.** The independent review removed
`assemblyReplacementMap` as a controlled single-variable change (`addDefaultProbingLocation` left in
place), rebuilt, and reran all three rounds: they passed identically, including a clean full `dotnet
build`/`dotnet run` of `RuntimeConsumer`. The FS0074 "must add a reference to assembly
'MyriadPreview.DesignTime'" error correction 3 blames on the map's absence never appeared in any path
the review exercised. The map is present in the checked-in source and harmless, but is not load-bearing
for an all-`obj` erased provider, and "a general finding for this lineage" was a post-hoc
misattribution — most likely more than one thing changed between a failing and a passing attempt during
the original build, and the recovery was credited to the wrong change. See `03-review.md`'s "The
load-bearing 'found by running' correction does not reproduce" section for the full account. The
provider source comment at `MyriadPreview.DesignTime/MyriadPreviewProvider.fs` has been corrected to
match.

- **Not shown:** Lenses' actual getter/setter-with-copy-and-update semantics (only plain field read);
  design-time member *types* reflecting the field's real declared type (every member here is `obj`,
  matching `00-hypothesis.md`'s stated simplification); anything about a real Ionide/FSAC session
  (`FSharpChecker`-as-library only, the same caveat every quartet in this lineage carries per
  `FINDINGS.md`'s cross-cutting limitations section); generalization past a two/three-field flat
  record (no nested record, no DU, no non-primitive field type attempted); whether
  `DefineStaticParameters` actually fired more than once in this run (the `successCache` memoization
  guard was exercised with no crash or type-identity mismatch, which is consistent with Q011's finding
  applying here too, but call counts were not explicitly logged to confirm multiple firing occurred —
  a cheap, named follow-up, not a gap that changes this quartet's own verdict).
