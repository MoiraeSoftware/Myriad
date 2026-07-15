# Q009-field-level-provenance / Movement 3 — Execute + write up

**Status:** DONE. All three rounds built and run to completion, 2026-07-15. Kill condition **not**
triggered (Round 1 passed). Final SHIP/REVISE/NULL verdict is deferred to `03-review.md`; this file
records what actually ran.

**Environment:** .NET SDK 9.0.310 host; provider TFMs `netstandard2.0;net8.0`; consumer/harness
`net8.0`. `FSharp.Compiler.Service` `43.9.101` (pinned). `ProvidedTypes.fs`/`.fsi` reused by file
copy from Q008's `tp-provenance-spike` (vendored `FSharp.TypeProviders.SDK` @
`0a95768a2247daba80b24a2604f77f89fc88ff1f`). Provider projects pin `FSharp.Core` `4.7.2`; FCS
resolved `FSharp.Core` `10.1.301` for the harness (NU1608 warning, benign — identical to Q006/Q008;
the providers load with their own FSharp.Core).

**Two genuinely separate provider pairs, newly built for this quartet** (not reused from Q008):
`SchemaTP.Runtime`/`SchemaTP.DesignTime` (emits per-field provenance) and
`ClientTP.Runtime`/`ClientTP.DesignTime` (reads and *selectively* enforces it). Plus `SchemaAsm`
(a plain classlib that references only `SchemaTP.Runtime` and bakes the stamped generative types into
its own IL), `AttrCheck` (Round 1's independent-reflection console), and `Harness` (FCS host, Rounds
2+3). The `IsFSharpDesignTimeProvider=true` project reference colocated each `*.DesignTime.dll`
alongside its `*.Runtime.dll`, so FCS's default probing found both providers with no
`typeproviders/fsharpNN` subfolder needed — same as Q006/Q008.

The attribute under test is the member-level analogue of Q008's, defined in `SchemaTP.Runtime`, now
carrying **two** constructor arguments (field name and version):

```fsharp
[<AttributeUsage(AttributeTargets.All, AllowMultiple = false)>]
type FieldProvenanceAttribute(fieldName: string, version: string) =
    inherit Attribute()
    member _.FieldName = fieldName
    member _.Version = version
```

`SchemaTP` parses a single string static parameter (`"Name:v1;Age:v2;Email:v3"`) into fields and
stamps `FieldProvenanceAttribute(fieldName, version)` on **each `ProvidedProperty` individually**
(`prop.AddCustomAttribute(...)`), never on the `ProvidedTypeDefinition`. This exercises the code path
Q008 never touched: the SDK emits property-level attributes at `ProvidedTypes.fs:16081`
(`defineCustomAttrs pb.SetCustomAttribute (pinfo.GetCustomAttributesData())`), which is the *same*
`defineCustomAttrs` routine — and therefore the same `assemblyReplacementMap` remap — that
`:15933` uses for type-level attributes. Verified from source before building; confirmed empirically
in Round 1.

---

## Round 1 — falsifier: do MEMBER-LEVEL custom attributes survive into independently-reflectable IL?

`SchemaAsm.dll` was built containing (among others) `type Tagged = SchemaTP.Provided.Schema<"Name:v1">`
— one generative type with exactly one provided property (`Name`) carrying one
`FieldProvenanceAttribute("Name", "v1")`. Building `SchemaAsm` ran the generative provider and baked
the stamped property into `SchemaAsm.dll`'s real IL. `AttrCheck` — a separate console that references
**only** `SchemaTP.Runtime.dll` and never touches `ProvidedTypes.fs` or any design-time assembly —
then did `Assembly.LoadFrom` and read the attribute back via plain
`System.Reflection.PropertyInfo.GetCustomAttributesData()`:

```
Loading (plain reflection, no SDK): ...\tp-field-provenance-spike\SchemaAsm\bin\Release\net8.0\SchemaAsm.dll
type 'SchemaAsm.Schemas+Tagged'
  property 'Name'
    attribute: SchemaTP.Runtime.FieldProvenanceAttribute
    attribute assembly: SchemaTP.Runtime
    ctor arg [0] (FieldName) = "Name"
    ctor arg [1] (Version)   = "v1"

FieldProvenanceAttribute instances found (member-level): 1
ROUND 1 (independent member-level reflection) verdict: PASS
```

Four things are load-bearing and all held: (1) the attribute survives into on-disk IL **on a
property**, readable with zero SDK involvement; (2) **both** constructor arguments round-trip exactly
(`"Name"`, `"v1"`); (3) the attribute's declaring assembly in the emitted metadata is
`SchemaTP.Runtime`, i.e. the `assemblyReplacementMap` remap fired for the member-level path exactly as
it did for the type-level path in Q008; (4) `PropertyInfo.GetCustomAttributesData()` — not the
type-level `Type.GetCustomAttributesData()` Q008 used — is the reader.

**Round 1: PASS. Kill condition not triggered.** The novelty-gate's open question ("whether
member-level custom attributes survive into independently-reflectable IL the same way type-level ones
did") is answered: yes, they do, via the same remap. Q008's finding generalizes from type-level to
member-level emission rather than being specific to the former.

---

## Round 2 — the selective-enforcement gate, four scenarios

`ClientTP.Provided.Client<SchemaAssemblyPath, SchemaTypeName, DependsOn>` (three string static
parameters). Its instantiation function `Assembly.LoadFrom`s the schema assembly, finds the named
type, reads every property's `FieldProvenanceAttribute` into a `fieldName -> version` map, then
compares **only** the fields listed in `DependsOn` against their expected versions. Every mismatch is
collected (not just the first). On full match it emits two real provided members
(`CheckedFieldCount : int`, `ProvenanceOk : bool`); on any declared-field mismatch it raises from
within the instantiation function, naming every mismatched field with declared and expected versions.
Tested through Q006/Q008's exact harness API (`FSharpChecker.ParseAndCheckFileInProject`, gating
strictly on the diagnostics list). All four scenarios ran on the same process (scenario 1 is the
genuine cold check):

**Scenario 1 — baseline match** (schema `Name:v1;Age:v2;Email:v3`, client `DependsOn Name:v1;Age:v2`):
**1183ms** (genuine cold), **0 diagnostics**. `C.CheckedFieldCount` resolves as
`type Microsoft.FSharp.Core.int` — real generated members, not just absence of error.

**Scenario 2 — irrelevant-field change** (schema `Email` v3→v4; client still `DependsOn Name:v1;Age:v2`,
never mentions `Email`): 58ms, **0 diagnostics**. This is the actual precision claim: a field the
client does not depend on changed provenance and the client stayed clean. PASS.

**Scenario 3 — relevant-field change** (schema `Age` v2→v3; client still expects `Age:v2`): 45ms,
**4 diagnostics (2 unique)**. Verbatim provider diagnostic:

```
The type provider 'ClientTPImplementation.ClientProvider' reported an error: ClientTP provenance
  mismatch (1 field(s)): field 'Age': schema declares 'v3' but client expects 'v2'
```

The diagnostic names the **specific field** (`Age`), its declared version (`v3`), and the client's
expected version (`v2`) — not a generic "schema changed". FCS surfaces the full raised message,
wrapped in the fixed prefix `The type provider 'ClientTPImplementation.ClientProvider' reported an
error: `, exactly as in Q008.

**Scenario 4 — multiple simultaneous relevant changes** (schema `Name` v1→v2 AND `Age` v2→v3; client
depends on both): 19ms, **4 diagnostics (2 unique)**. Verbatim:

```
The type provider 'ClientTPImplementation.ClientProvider' reported an error: ClientTP provenance
  mismatch (2 field(s)): field 'Name': schema declares 'v2' but client expects 'v1'; field 'Age':
  schema declares 'v3' but client expects 'v2'
```

The diagnostic names **both** mismatched fields in one message, with each field's declared and
expected version — the design's explicit requirement (complete information in one check, not just the
first mismatch found).

The same two secondary observations Q008 reported recur and are reported rather than smoothed over:
(1) the provider error appears **twice** (identical text, same `(2,9)` location) — FCS emits the
instantiation failure once during name resolution and once during the dependent member access; the
harness gates on `hasErrors` so duplication does not affect the verdict, but a real editor would show
two squiggles. (2) The mismatch also produces two downstream `(3,16)`/`(4,18)` "does not define
member" errors, because the raised instantiation left `C` with no members — Q006's error-recovery
caveat, and exactly why gating on the diagnostic list (not on "did a symbol resolve") is correct.

**Round 2: PASS** (match → 0 diagnostics with real members; irrelevant change → stays clean; relevant
change → specific diagnostic naming the field; multiple changes → all fields named).

---

## Round 3 — live-edit precision, same `FSharpChecker`, no rebuild, growing field count

Same checker instance across every step of both sequences (and continuous from Round 2 — the very
first check in the process is Round 2 scenario 1's 1183ms cold). **Nothing is rebuilt between steps;
`SchemaAsm.dll` on disk never changes.** See the design correction below for how a "schema change" is
expressed without a rebuild.

**Small schema (3 fields, client depends on 2):**

| step | edit | re-check | result |
|---|---|---|---|
| 1 cold match | baseline, `DependsOn Name:v1;Age:v2` | 32ms | 0 diagnostics |
| 2 live irrelevant | repoint to `SchemaEmailBumped` (Email v3→v4) | 57ms | 0 diagnostics (stays clean) |
| 3 live relevant | repoint to `SchemaAgeBumped` (Age v2→v3) | 15ms | field-specific `Age` diagnostic (4 diags, 2 unique) |
| 4 live cleared | repoint back to baseline | 42ms | 0 diagnostics |

Step-3 diagnostic is byte-for-byte the Round 2 scenario-3 text (`field 'Age': schema declares 'v3'
but client expects 'v2'`).

**Wide schema (12 fields F0–F11, client depends on F0/F3/F7/F9):**

| step | edit | re-check | result |
|---|---|---|---|
| 1 cold match | `WideBaseline`, `DependsOn F0:v1;F3:v1;F7:v1;F9:v1` | 36ms | 0 diagnostics |
| 2 live irrelevant | repoint to `WideIrrelevantBump` (F5 v1→v2, not a dependency) | 33ms | 0 diagnostics (stays clean) |
| 3 live relevant | repoint to `WideRelevantBump` (F3 v1→v2, a dependency) | 20ms | field-specific `F3` diagnostic |
| 4 live cleared | repoint back to `WideBaseline` | 33ms | 0 diagnostics |

Step-3 diagnostic verbatim:

```
The type provider 'ClientTPImplementation.ClientProvider' reported an error: ClientTP provenance
  mismatch (1 field(s)): field 'F3': schema declares 'v2' but client expects 'v1'
```

### Scaling: small vs wide, and comparison to Q008's baseline

```
                     small 3-field    wide 12-field
cold match (warm)    32ms             36ms
live irrelevant      57ms             33ms
live relevant fail   15ms             20ms
live cleared         42ms             33ms
```

Re-check cost is **flat**, not linear and not degrading, as field count goes from 3 to 12 (client
dependencies from 2 to 4). Every live re-check for both sizes sits inside 15–57ms, squarely in Q008's
19–32ms envelope (the couple of 40–57ms readings are ordinary FCS incremental-check jitter, not a
field-count effect — the *wide* schema's numbers are if anything lower than the small one's). The
genuine cold number, 1183ms (Round 2 scenario 1), is essentially identical to Q008's 1161ms cold.

This directly addresses the **REVISE** threshold ("re-check cost scales badly with field count, e.g.
because every recheck re-reflects every declared dependency individually with no batching"): it does
not. The reader reflects the schema type's properties **once per instantiation** into a map and then
does N string comparisons; reflecting 12 properties instead of 3 is not measurable against FCS's own
incremental-check machinery. Note the reader currently reads *all* the type's property attributes and
filters to the `DependsOn` subset afterward, so these numbers are a conservative upper bound — a
reader that read only the named fields' attributes would be at most equal, never worse.

**Round 3: PASS** (both sizes: cold clean; irrelevant edit stays clean live; relevant edit produces
the field-specific diagnostic live with no rebuild; diagnostics clear on edit-back; flat scaling).

---

## The design's assumption that turned out wrong once built

`01-design.md` Round 3 step 2 says: *"Live edit, same checker: bump only `Email`'s version in the
schema's static argument."* Taken literally that is not achievable, for the same reason Q006/Q008
already established: **a type provider only ever sees already-compiled referenced assemblies.** The
schema's per-field versions live in its static argument, which is baked into `SchemaAsm.dll` at build
time; editing that static argument requires rebuilding `SchemaAsm`, which contradicts Round 3's own
"same checker, no rebuild" constraint.

The feasible realization — which preserves the constraint exactly and mirrors Q008's Round 3
faithfully — is to **pre-bake the schema variants** into `SchemaAsm.dll` (baseline, one-irrelevant-
field-bumped, one-relevant-field-bumped, plus the wide equivalents) and express a "schema change" as
**editing only the consumer file's `SchemaTypeName` static argument** to repoint `ClientTP` at a
different, already-compiled variant. This is the same shape as Q008's Round 3 (there, the consumer's
`ExpectedVersion` was edited live; the schema DLL was fixed). It is a genuine live edit of the
consumer buffer, re-checked on the same checker with no rebuild — it just moves the "which schema"
choice to the consumer side, because the schema side is immutable compiled IL. This is the single most
important correction; it does not weaken the result (the precision claim is still tested both
directions, live) but it is worth stating plainly rather than pretending the schema's static argument
was edited in place.

Two smaller notes: (a) the emitted generative type is named after the consumer's abbreviation
(`SchemaAsm.Schemas+SchemaBaseline`, etc.), the same `SchemaAsm.Schemas+X` shape Q008 hit, so
`ClientTP` is told the consumer-side name; (b) no `AppDomain.AssemblyResolve` handler was needed —
`Assembly.LoadFrom` + the co-located `SchemaTP.Runtime.dll` resolve the attribute type on their own,
confirming Q008's same finding.

---

## The competing alternative the design named: N separately-versioned whole types

The validity preconditions require weighing this honestly, not asserting the mechanism works and
skipping it. The competitor is: **reuse Q008's whole-type mechanism unchanged, one type per field**,
and get selective enforcement simply by having the client instantiate `ClientTP` only for the fields
it cares about. Concretely, for the 12-field wide case:

**This quartet (one type + a `DependsOn` string):**

```fsharp
// schema author: ONE cohesive type, one spec string
type Wide = SchemaTP.Provided.Schema<"F0:v1;F1:v1;...;F11:v1">   // 1 declaration

// client: ONE instantiation, dependencies as data
type C = ClientTP.Provided.Client<SchemaAssemblyPath = "...",
                                   SchemaTypeName = "SchemaAsm.Schemas+Wide",
                                   DependsOn = "F0:v1;F3:v1;F7:v1;F9:v1">   // 1 instantiation
// -> ONE provided type C with members, one Assembly.LoadFrom, 12 attrs read, 4 compared
```

**N-separate-types (Q008 mechanism, one whole type per field):**

```fsharp
// schema author: 12 unrelated types, no grouping that says "these are one schema"
type F0 = SchemaTP.Provided.Schema<"v1">
type F1 = SchemaTP.Provided.Schema<"v1">
// ... 10 more ...
type F11 = SchemaTP.Provided.Schema<"v1">                        // 12 declarations

// client: one ClientTP instantiation PER depended field
type CF0 = ClientTP.Provided.Client<"...", "SchemaAsm.Schemas+F0", "v1">
type CF3 = ClientTP.Provided.Client<"...", "SchemaAsm.Schemas+F3", "v1">
type CF7 = ClientTP.Provided.Client<"...", "SchemaAsm.Schemas+F7", "v1">
type CF9 = ClientTP.Provided.Client<"...", "SchemaAsm.Schemas+F9", "v1">   // 4 instantiations
// -> FOUR unrelated provided types, four Assembly.LoadFroms, no single "my view of the schema"
```

Honest reading of the comparison:

- **Selective enforcement is NOT unique to this quartet.** The N-types approach achieves it too — the
  client just doesn't instantiate `ClientTP` for fields it ignores. So the precision *outcome* alone
  does not justify member-level machinery. The design's NULL threshold is exactly this, and it is a
  real risk, not a strawman.
- **On the client side the raw verbosity difference is modest**: one `DependsOn` string vs four
  separate instantiation lines. If verbosity were the only axis, this would lean NULL.
- **The decisive difference is structural, and it is on the schema side and in the type surface.** The
  N-types approach shatters one logical record into 12 unrelated types with no construct that says
  "these twelve belong to one schema", and gives the client four disjoint types (`CF0/CF3/CF7/CF9`)
  with nothing that represents "my dependency view" as a single thing to program against. The
  single-type approach keeps "one record type with N properties" intact: one schema type, one client
  type, dependencies expressed as *data* (a string) rather than as N separate type instantiations.
  That grouping is exactly the ergonomic property the preconditions said field-level granularity must
  preserve to earn its complexity, and it does preserve it.
- **The single-type approach is also cheaper at design time**: one `Assembly.LoadFrom` and one provider
  instantiation reading N attributes, versus N `LoadFrom`s and N instantiations. This is consistent
  with Round 3's flat scaling — the wide client's single re-check reflecting 12 attributes stayed in
  the same band as the small client's.

Net: the mechanism is not NULL — it preserves single-grouped-type ergonomics and is cheaper at
design time — but the win over N-types is structural/ergonomic, not a raw line-count blowout on the
client. Whether that structural win clears the bar for shipping vs. "just use N whole types" is
`03-review.md`'s call; this file records that both work, and where each is better.

---

## What was and wasn't proven, stated plainly

- **Proven:** a custom attribute stamped on an individual `ProvidedProperty` (member-level) survives
  into independently reflectable on-disk IL, with both constructor arguments intact, remapped to the
  runtime assembly via the same `defineCustomAttrs` path as type-level attributes (Round 1). A
  separate provider can read those per-field attributes by ordinary reflection and enforce provenance
  for a **declared subset** of fields: staying clean when a non-depended field changes, and refusing
  to generate with a diagnostic naming the **specific** changed field (or **all** of them) when a
  depended field changes (Round 2). Both directions are caught on a **live consumer edit with no
  rebuild**, at 15–57ms re-checks, and the cost is **flat** from a 3-field to a 12-field schema (Round
  3).
- **Not proven / boundary for the review:** as in Q006/Q008 this ran against `FSharpChecker` (the host
  library), not a literal Ionide/VS/Rider session with FSAC's caching on top. The per-field compare is
  re-run on every instantiation (no caching of the reflected map across checks); it was cheap enough
  not to matter at 12 fields, but a very wide schema (hundreds of fields) reflected on every keystroke
  was not stress-tested.
- **Design correction surfaced (not patched silently):** the schema's static argument cannot be
  live-edited in place (it is compiled IL); "schema changes" are expressed by repointing the consumer's
  `SchemaTypeName` at a pre-baked variant — the faithful, feasible mirror of Q008's Round 3.
- **The competing alternative was measured, not waved away:** N separately-versioned whole types also
  achieve selective enforcement; this quartet's single-type approach earns its place on grouping
  ergonomics and design-time cost, not on client-side line count. Recorded above in full for the
  review's NULL-threshold call.

---

## Reproduction

From the scratch dir `tp-field-provenance-spike` (vendored `ProvidedTypes.fs`/`.fsi` copied from
Q008's `tp-provenance-spike`):

```
# build both providers (each Runtime ref triggers its DesignTime build + colocates the TPDTC)
dotnet build SchemaTP.Runtime/SchemaTP.Runtime.fsproj -c Release
dotnet build ClientTP.Runtime/ClientTP.Runtime.fsproj -c Release

# build the schema carrier — bakes all stamped generative variants into SchemaAsm.dll's IL
dotnet build SchemaAsm/SchemaAsm.fsproj -c Release

# Round 1: independent member-level reflection, no SDK in the loop
dotnet run --project AttrCheck/AttrCheck.fsproj -c Release

# Rounds 2 + 3: FCS host, selective-enforcement gate + live-edit timing (no consumer build)
dotnet build Harness/Harness.fsproj -c Release
dotnet run --project Harness/Harness.fsproj -c Release --no-build
```

Scratch layout: `SchemaTP.Runtime/` (`FieldProvenanceAttribute` + TPRTC), `SchemaTP.DesignTime/`
(`SchemaTP.Provider.fs` — parses the spec, stamps each property), `ClientTP.Runtime/`,
`ClientTP.DesignTime/` (`ClientTP.Provider.fs` — the selective-enforcement provider), `SchemaAsm/`
(bakes the `Tagged` Round-1 type plus the Round-2/3 small and wide variants), `AttrCheck/Program.fs`,
`Harness/Program.fs`. Per Q001–Q008 precedent, only this write-up is durable; the scratch projects are
not committed. No wiring into Myriad's own `Myriad.Sdk` targets — this idea is independent of Myriad by
design.
