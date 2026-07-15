# Q009-field-level-provenance / Movement 2 — Design

**Status:** IN PROGRESS.
**Location:** scratch projects under a temp working directory, not part of any committed repo —
same precedent as Q001–Q003, Q006, Q008 (no `artifacts/` subfolder checked in; this file plus
`02-results.md` are the durable record).
**Pins:** `FSharp.Compiler.Service` `43.9.101`; `ProvidedTypes.fs`/`.fsi` from the same
`FSharp.TypeProviders.SDK` commit Q006/Q008 used (`0a95768a2247daba80b24a2604f77f89fc88ff1f`), for
directly comparable timing numbers. `FSharp.Core` `4.7.2` for provider projects.

## Round 1 — falsifier: do member-level custom attributes survive into independently-reflectable IL?

Single provider, `FieldProvTP.Provided.Tagged`, generative, one provided type with **one** provided
property stamped via `AddCustomAttribute` (called on the `ProvidedProperty`, not the
`ProvidedTypeDefinition`) with a real `FieldProvenanceAttribute(fieldName: string, version: string)`
(a genuine attribute type in the runtime assembly, same pattern as Q008's `SchemaVersionAttribute`).
Build once, then from a separate consumer program that never references `ProvidedTypes.fs` or any
design-time assembly, `Assembly.LoadFrom` the built DLL and read the property's attribute back via
`PropertyInfo.GetCustomAttributesData()`. Assert both constructor arguments round-trip.

Kill condition for the whole quartet, pre-registered exactly as in Q006/Q008: if this fails, stop —
Rounds 2 and 3 would build selective-enforcement logic on a foundation that doesn't hold.

## Round 2 — the selective-enforcement gate, both directions

`SchemaTP.Provided.Schema<FieldSpec>` — one string static parameter encoding multiple fields and
their individual versions, e.g. `"Name:v1;Age:v2;Email:v3"`. Generates one provided type with three
provided properties (`Name: string`, `Age: int`, `Email: string`), **each individually stamped**
with its own `FieldProvenanceAttribute(fieldName, version)` via the mechanism proven in Round 1.

`ClientTP.Provided.Client<SchemaAssemblyPath, SchemaTypeName, DependsOn>` — `DependsOn` is a string
encoding only the fields *this* client claims to use and the version it expects for each, e.g.
`"Name:v1;Age:v2"` — deliberately omitting `Email`. Instantiation function: load the schema assembly,
find the named type, and for **only** the fields listed in `DependsOn`, read that field's individual
`FieldProvenanceAttribute` and compare its version to what `DependsOn` expects. Fields not listed are
never inspected. On full match: generate real provided members for the declared fields. On any
declared-field mismatch: raise, naming the specific field and both versions (not a whole-schema
failure).

Test through the same `FSharpChecker`-based harness pattern as Q006/Q008 (real on-disk consumer,
gating strictly on the diagnostics list), four scenarios:

1. Baseline: schema `"Name:v1;Age:v2;Email:v3"`, client depends on `"Name:v1;Age:v2"` — expect
   **zero diagnostics**, real generated members for `Name`/`Age`.
2. **Irrelevant-field change**: schema bumps to `"Name:v1;Age:v2;Email:v4"` (only `Email` changed;
   client still depends on `"Name:v1;Age:v2"`, never mentions `Email`) — expect **zero diagnostics**,
   unchanged. This is the actual precision claim under test, not a restatement of scenario 1.
3. **Relevant-field change**: schema bumps to `"Name:v1;Age:v3;Email:v3"` (`Age` changed; client
   still expects `Age:v2`) — expect a **specific diagnostic naming `Age`**, its declared version
   (`v3`), and the client's expected version (`v2`) — not a generic "schema changed" message.
4. Multiple simultaneous relevant changes (`Name` and `Age` both bump, client depends on both) —
   confirm the diagnostic identifies *both* mismatched fields, not just the first one found, since a
   real client depending on several changed fields should get complete information in one check.

## Round 3 — live-edit precision, same `FSharpChecker`, no rebuild, growing field count

Using the same checker instance throughout, on one on-disk consumer file, mirroring Q008's Round 3
table exactly:

1. Cold check: baseline (scenario 1 above) — record time.
2. Live edit, same checker: bump only `Email`'s version in the schema's static argument (irrelevant
   to this client) — re-check, expect **continued zero diagnostics**, record time. This is the
   sharpest version of the precision claim: an edit happened, the checker re-ran, and the client
   correctly stayed clean because the change was irrelevant to its declared dependencies.
3. Live edit, same checker: bump `Age`'s version (relevant) — re-check, expect the field-specific
   diagnostic from Round 2 scenario 3 to appear, record time.
4. Live edit back to matching — re-check, confirm diagnostics clear, record time.

Repeat the same four-step sequence with a **wider schema** (a double-digit field count, e.g. 12
fields, client depending on 4 of them) to test whether re-check cost scales linearly, badly, or not
at all with field count — this is the direct test of the REVISE threshold and partially subsumes
Q008's own still-open follow-up 2 (stress-test against a realistic schema size), scoped here
specifically to re-check cost rather than absolute schema size.

Compare every timing number explicitly against Q008's baseline (1161ms cold / 19-32ms live re-check).

## Reproduction

```
mkdir tp-field-provenance-spike && cd tp-field-provenance-spike
# reuse ProvidedTypes.fs/.fsi vendored copy from Q006/Q008's scratch dirs if present
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

No MSBuild integration with Myriad's own `Myriad.Sdk` targets — same as Q006/Q008, this idea is
independent of Myriad by design.
