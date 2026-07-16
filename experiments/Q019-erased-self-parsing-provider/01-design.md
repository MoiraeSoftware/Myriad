# Q019-erased-self-parsing-provider / Movement 2 — Design

**Status:** IN PROGRESS.
**Location:** `experiments/Q019-erased-self-parsing-provider/artifacts/`, checked in per this repo's
own `artifacts/`-is-required rule (`README.md`, tightened after Q006/Q008/Q009's missing-artifacts
gap).
**Pins:** `FSharp.Compiler.Service` `43.9.101` (Harness only, matches every prior quartet and this
repo's `paket.lock`); `FSharp.TypeProviders.SDK`'s `ProvidedTypes.fs`/`.fsi` vendored from the same
commit every prior TP quartet used (`0a95768a2247daba80b24a2604f77f89fc88ff1f`, copied from
`Q016-satellite-dll-type-provider/artifacts/` rather than re-fetched, since it's already known-good);
`Fantomas.Core` `7.0.5` (this repo's own `paket.lock` pin, brings `Fantomas.FCS` `7.0.5` transitively)
for the design-time project, since it must be able to call `Myriad.Core.Ast.fromFilename` which itself
calls `Fantomas.Core.CodeFormatter.ParseAsync` — a version mismatch here would be a real, reportable
risk, not a formality, so it's pinned to exactly what `Myriad.Core.dll` itself was compiled against.

**Named deviation from Q016/Q017/Q018's own project-shape convention, stated upfront rather than
discovered mid-build:** those quartets' design-time projects multi-targeted `netstandard2.0;net8.0`,
the SDK's own documented convention for maximum host compatibility. This quartet's design-time project
targets `net9.0` only, because it references `Myriad.Core.dll` directly (`src/Myriad.Core/bin/Release/
net9.0/Myriad.Core.dll`, built from this repo unmodified), and `Myriad.Core.fsproj` itself targets
`net9.0` only (checked directly, not assumed). This means Q019's provider is *less* portable across host
TFMs than Q016-18's, and that caveat should attach to any future generalization of this quartet's
result, not be silently dropped in the write-up.

## Round 1 — falsifier: design-time-only member resolution with the target type provably absent

Tests the core novel claim before any reflection/runtime plumbing is built: does an erased provider,
built entirely by having its own instantiation function call `Myriad.Core.Ast.fromFilename` and
`Ast.extractRecords` against a source file, produce members the compiler accepts with zero
diagnostics — **when the record type named in that file has never been compiled or referenced by
anything the checking project can see?**

1. `MyriadPreview.Runtime` (trivial, carries the `TypeProviderAssembly` attribute pointing at
   `.DesignTime`, same split as every prior TP quartet) and `MyriadPreview.DesignTime` (references
   vendored `ProvidedTypes.fs`/`.fsi` plus `Myriad.Core.dll` directly as a `<Reference>`, plus
   `PackageReference Fantomas.Core 7.0.5`). `dotnet build` once.
2. `SampleLib/Person.fs`: a plain record file, **not compiled by anything in this spike** —
   `namespace SampleNs` / `type Person = { name: string; age: int }`, no `Myriad.Plugins` attribute
   needed since nothing here invokes Myriad's own CLI. Its only job is to exist as text on disk for
   the provider to parse.
3. `MyriadPreviewProvider.fs`: erased container type `MyriadPreview.Provided.Fields`, static parameters
   `(SourceFilePath: string, RecordName: string)`, instantiation function calls
   `Ast.fromFilename sourceFilePath |> Async.RunSynchronously`, `Ast.extractRecords`, finds the
   `SynTypeDefn` whose component-info name matches `RecordName`, extracts its `SynField` list, and
   for each field builds an erased `ProvidedProperty(fieldName, typeof<obj>, isStatic = false,
   getterCode = ..., setterCode = ...)` — invoke-code bodies deferred to Round 3 (Round 1 only needs
   the *members to exist and typecheck*, not to run correctly; a placeholder `<@@ () @@>`-shaped body
   is enough here, swapped for the real reflection body before Round 3).
4. Harness hosts `FSharpChecker` (real files on disk, matching Q006/Q016's own methodology) with
   `FSharpProjectOptions` referencing `MyriadPreview.Runtime.dll` — **and explicitly NOT referencing
   any assembly that defines `SampleNs.Person`**, checked as a precondition, not assumed: grep the
   constructed `FSharpProjectOptions.OtherOptions` for any `-r:` path and confirm none of them can
   possibly define that type (only `FSharp.Core`, `MyriadPreview.Runtime.dll`, and the BCL reference
   pack). A consumer file:
   ```fsharp
   type P = MyriadPreview.Provided.Fields<"...\Person.fs", "SampleNs.Person">
   let x : obj = P().name
   ```
5. `checker.ParseAndCheckFileInProject`. Assert zero diagnostics and that `name`/`age` resolve as real
   provided members. This is the falsifier: if this fails, Round 2/3 test a mechanism that doesn't clear
   the bar this quartet exists to check.

Kill condition, pre-registered: if step 5 produces diagnostics naming a missing type, a resolution
failure, or an exception thrown out of the provider's instantiation function (e.g. `Fantomas.Core`
failing to load inside the design-time host process, a distinct failure mode from a normal parse
error), stop — this is the KILL outcome named in `00-hypothesis.md`.

## Round 2 — falsifier: live-edit re-parse with no rebuild

Using the *same* `FSharpChecker` instance from Round 1 (so caching behaves as it would across real
keystrokes, matching every prior live-edit test in this repo):

1. Wire `FileSystemWatcher` on the static parameter's `SourceFilePath` inside the provider, calling
   `this.Invalidate()` on change (the mechanism named but never built before Q016 first built it;
   reused here unchanged).
2. Edit `Person.fs` on disk to add a third field, `email: string`.
3. Re-run `checker.ParseAndCheckFileInProject` on a *new* consumer file referencing `P().email` (a new
   file is used deliberately, not a re-check of the same file, to sidestep the specific
   `DocumentSource.Custom`-with-explicit-source-text footgun Q010 found — irrelevant to this provider's
   mechanism, but worth avoiding rather than accidentally re-triggering a known, unrelated bug).
4. Assert zero diagnostics and that `email` now resolves, with no `dotnet build` run anywhere between
   steps 2 and 3.

## Round 3 — capability falsifier: runtime correctness, decoupled from design-time resolution

Tests the second claim explicitly, on a *separate* compiled program so the "type never compiled
anywhere" precondition from Round 1 cannot leak into this round and quietly do the work instead:

1. `RuntimeConsumer.fsproj`: an independent console project, referencing nothing from this quartet's
   design-time machinery, defining its own `type Person = { name: string; age: int; email: string }`
   (matching Round 2's final shape) and constructing a real instance.
2. `MyriadPreviewProvider.fs`'s placeholder invoke-code bodies from Round 1 are replaced with real ones:
   `getterCode = fun args -> <@@ (%%args.[0] : obj) |> fun o -> o.GetType().GetProperty(fieldName).GetValue(o) @@>`
   (exact quotation form resolved concretely while building — reflection calls inside a quotation
   sometimes need `Expr.Call`/`Var` construction instead of a clean `<@@ @@>` splice; report which one
   actually worked in `02-results.md`, don't assume from the sketch here).
3. A small program uses the provider's type against the *runtime* `Person` instance from step 1 (passed
   in as `obj`), reads `name`/`age`/`email` through the provided members, and independently reads the
   same fields via direct field access. Assert the two agree, matching every prior quartet's
   "two independent checks agree" correctness bar (Q008, Q009, Q016's own Round 1).
4. This must be built and actually run (`dotnet run`), not just typechecked — runtime behavior can't be
   observed through `FSharpChecker` alone, same reasoning Q006 Round 2 and Q016 Round 1 used.

## Reproduction

```
dotnet build src/Myriad.Core -c Release          # from repo root, if not already built
cd experiments/Q019-erased-self-parsing-provider/artifacts
dotnet build MyriadPreview.Runtime MyriadPreview.DesignTime
dotnet run --project Harness -- round1
dotnet run --project Harness -- round2
dotnet build RuntimeConsumer && dotnet run --project RuntimeConsumer
```

No MSBuild integration with Myriad's own `Myriad.Sdk` targets — deliberate, matching every prior
quartet's same choice: the question under test is whether the erased-self-parsing mechanism itself
works, not whether Myriad's existing pipeline can be wired to it (a follow-on quartet if this ships).
