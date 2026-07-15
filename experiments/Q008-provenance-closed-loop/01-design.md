# Q008-provenance-closed-loop / Movement 2 — Design

**Status:** IN PROGRESS.
**Location:** scratch projects under a temp working directory, not part of any committed repo —
same precedent as Q001–Q003 and Q006 (no `artifacts/` subfolder checked in; this file plus
`02-results.md` are the durable record).
**Pins:** `FSharp.Compiler.Service` `43.9.101` (matches `Myriad/paket.lock`); `ProvidedTypes.fs`/
`.fsi` vendored from `FSharp.TypeProviders.SDK` commit `0a95768a2247daba80b24a2604f77f89fc88ff1f`
(same as Q006, for directly comparable timing numbers); `FSharp.Core` `4.7.2` for provider projects
(SDK's own pin).

## Round 1 — falsifier: do generative custom attributes survive into independently-reflectable IL?

Single provider, `ProvenanceTP.Provided.Tagged<Note>`, generative (`isErased = false`), one provided
type stamped via `AddCustomAttribute` with a custom `SchemaVersionAttribute(version: string)` — a
real attribute type (not one of the SDK's built-in helpers like `AddObsoleteAttribute`), defined in
the runtime assembly and referenced from the design-time assembly the way `assemblyReplacementMap`
requires for any type appearing in emitted IL. Build once (`dotnet build`). Then, from a **separate**
consumer program that never referenced `ProvidedTypes.fs` or the design-time assembly at all — only
the built runtime DLL — call `Assembly.LoadFrom` and `typeof<...>.GetCustomAttributesData()` (or
`GetCustomAttribute<SchemaVersionAttribute>()`), and assert the version string round-trips correctly.

Kill condition for the whole quartet, pre-registered: if the attribute doesn't appear, or appears
with the wrong constructor argument, or throws on load — stop. Round 2 and 3 would be building a
two-provider chain on a foundation that doesn't hold.

## Round 2 — the two-provider gate, both directions

Two providers in separate design-time/runtime assembly pairs (genuinely separate — not two provided
types from one provider, which would understate the test):

- **`SchemaTP.Provided.Schema<Version>`**: static parameter is a version string (e.g. `"v2"`).
  Generates a type stamped with `SchemaVersionAttribute(Version)` via the mechanism proven in Round
  1, plus a couple of trivial provided members so it's not a bare marker type.
- **`ClientTP.Provided.Client<SchemaAssemblyPath, SchemaTypeName, ExpectedVersion>`**: three string
  static parameters (an assembly path is required since `TypeProviderConfig.ReferencedAssemblies`
  scoping across two independently-built scratch projects needs to be resolved concretely during the
  build — record whatever the actual working mechanism turns out to be, this is exactly the kind of
  design-vs-reality detail to report honestly, not paper over). In its `DefineStaticParameters`
  instantiation function: load the schema assembly, find the named type, read its
  `SchemaVersionAttribute` via `GetCustomAttributesData()`, compare its version string to
  `ExpectedVersion`. On match: generate real provided members (a couple of simple properties,
  enough to prove generation genuinely happened, not just that no exception was thrown). On
  mismatch: raise a descriptive exception from within the instantiation function, naming both
  versions.

Test both directions through Q006's harness (`FSharpChecker.ParseAndCheckFileInProject` against a
real on-disk consumer, gating strictly on the diagnostics list per this quartet's validity
preconditions):

1. `SchemaTP.Schema<"v2">` + `ClientTP.Client<..., "v2">` — expect **zero diagnostics**, provided
   members resolve with correct types.
2. `SchemaTP.Schema<"v2">` + `ClientTP.Client<..., "v3">` — expect a **real diagnostic**, and the
   diagnostic message should be inspected to confirm it actually surfaces the exception text raised
   in `ApplyStaticArguments` (not a generic "type provider instantiation failed" swallowing the real
   reason) — record whatever FCS actually does with the exception message verbatim, don't assume.

## Round 3 — live-edit violation detection, extending Q006's Round 1 methodology exactly

Using the **same** `FSharpChecker` instance across all steps, on a real on-disk consumer file, timed
the same way Q006's Round 1 table was:

1. Cold check: `SchemaTP.Schema<"v2">`, `ClientTP.Client<..., "v2">` — zero diagnostics. Record time.
2. Live edit, same checker: change only `ClientTP`'s `ExpectedVersion` argument to `"v3"` in the
   consumer file's text, bump version the way `DocumentSource.Custom`/a real editor buffer would,
   re-check. Expect the mismatch diagnostic to appear with **no rebuild of anything**. Record time —
   this is the number to compare against Q006's 47ms live-edit baseline, since this recheck does
   genuinely more work (loading and reflecting a second assembly's attributes).
3. Live edit back to `"v2"`, re-check, confirm diagnostics clear again. Record time.

This is the central claim under test: a provenance conflict between two independently-versioned
providers gets caught the moment you edit the source, in the same re-check cycle Q006 proved is
build-free — not just "eventually, at the next full build."

## Reproduction

```
mkdir tp-provenance-spike && cd tp-provenance-spike
# vendor ProvidedTypes.fs/.fsi from FSharp.TypeProviders.SDK@0a95768 (reuse Q006's copy if present)
dotnet new classlib -lang F# -o SchemaTP.Runtime
dotnet new classlib -lang F# -o SchemaTP.DesignTime
dotnet new classlib -lang F# -o ClientTP.Runtime
dotnet new classlib -lang F# -o ClientTP.DesignTime
dotnet new console -lang F# -o AttrCheck     # Round 1's independent reflection check
dotnet new console -lang F# -o Harness       # FSharp.Compiler.Service 43.9.101, Rounds 2 + 3
dotnet build SchemaTP.Runtime SchemaTP.DesignTime ClientTP.Runtime ClientTP.DesignTime
dotnet run --project AttrCheck
dotnet run --project Harness
```

No MSBuild integration with Myriad's own `Myriad.Sdk` targets — this idea is explicitly independent
of Myriad per the user's own framing when it was proposed; nothing here should get wired into the
Myriad repo's build.
