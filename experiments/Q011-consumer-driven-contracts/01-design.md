# Q011-consumer-driven-contracts / Movement 2 — Design

**Status:** NOT YET EXECUTED.
**Location:** scratch projects under a temp working directory, not part of any committed repo — same
precedent as Q006/Q008/Q009 (no `artifacts/` subfolder checked in for the provider projects
themselves; save the harness/consumer `Program.fs`(es) under `artifacts/` in this quartet folder, same
as Q010 did, since those are the reproducible test-driver code even though the provider DLLs
themselves are scratch).
**Pins:** `FSharp.Compiler.Service` `43.9.101`; `ProvidedTypes.fs`/`.fsi` from the same
`FSharp.TypeProviders.SDK` commit Q006/Q008/Q009 used
(`0a95768a2247daba80b24a2604f77f89fc88ff1f`). `FSharp.Core` `4.7.2` for provider projects, matching
Q009's pins exactly for comparable numbers.

## Shape of the two providers (read before building)

**`SchemaTP.Provided.Schema<FieldSpec, KnownClientPaths>`** — generative. `FieldSpec` a semicolon-
list of `name:version` pairs, e.g. `"Name:v1;Age:v2;Email:v3"` (identical format to Q009's schema
side). `KnownClientPaths` a semicolon-list of already-compiled client assembly paths (may be empty).
Generation function:
1. Build the provided type exactly as Q009's `SchemaTP` did — one provided property per field, each
   individually stamped via `AddCustomAttribute` with its own `FieldProvenanceAttribute(name,
   version)`.
2. For each path in `KnownClientPaths`: `Assembly.LoadFrom` it, walk its types' members, collect every
   `ConsumesFieldAttribute(fieldName, consumedVersion)` found via `GetCustomAttributesData()`.
3. For each collected `(fieldName, consumedVersion)`: look up `fieldName` in the *current* `FieldSpec`.
   - Not found at all → **removed-field failure**: raise, message must name the client assembly path,
     the field name, and say "removed" (not "version mismatch") — distinguishable text, checked
     explicitly in Round 3, not just "an error occurred."
   - Found but current version ≠ `consumedVersion` → **stale-dependency failure**: raise, message
     names the client assembly path, the field name, the client's recorded version, and the schema's
     current version.
   - Found and version matches → no action, continue.
4. If no failure was raised across all known clients, return the provided type from step 1 normally.

**`ClientTP.Provided.Client<SchemaAssemblyPath, FieldNames>`** — generative, deliberately closer to
Q009's `ClientTP` but **without client-declared version pinning** (a real simplification worth stating
explicitly: the client doesn't predict a future version, it only declares *which* fields it uses; the
version it records is whatever the schema said at the moment the client itself was generated — this is
what makes the "unwedging" move in Round 2 just "recompile the client," not extra reconciliation
logic). `FieldNames` a semicolon-list of field names only, e.g. `"Name;Age"`. Generation function:
`Assembly.LoadFrom(SchemaAssemblyPath)`, for each name in `FieldNames` find that field's *current*
`FieldProvenanceAttribute` on the schema type, generate a real provided property/method giving access
to it (mirroring Q009's real-accessor generation, not a marker-only member), and stamp that same
provided member with `ConsumesFieldAttribute(fieldName, currentVersionReadFromSchema)`.

## Round 1 — falsifier: does a client-emitted member attribute survive into independently-reflectable IL?

Single provider, one provided type, one provided property stamped via `AddCustomAttribute` (called on
the `ProvidedProperty`) with `ConsumesFieldAttribute("Name", "v1")` — no schema reflection involved
yet, just confirming the emission-and-survival half in isolation, exactly mirroring Q009's own Round 1
but with the client's attribute type instead of the schema's. Build once; from a separate consumer
program with zero SDK involvement, `Assembly.LoadFrom` and read back via
`PropertyInfo.GetCustomAttributesData()`; assert both constructor arguments round-trip.

Kill condition for the whole quartet, pre-registered exactly as in Q006/Q008/Q009: if this fails, stop
before building anything further.

## Round 2 — the well-foundedness loop (the capability claim)

Sequence, using the real `SchemaTP`/`ClientTP` providers built above, through the same
`FSharpChecker`-based live-edit harness pattern Q008/Q009 used (real on-disk consumer source files,
gating strictly on the diagnostics list):

1. Build `SchemaTP.Runtime`/`SchemaTP.DesignTime` v1: fields `"Name:v1;Age:v2;Email:v3"`,
   `KnownClientPaths = ""` (empty — no clients yet). Confirm it generates cleanly with zero clients
   registered (sanity check on the empty-list case).
2. Build `ClientCo`, a real consumer project referencing `SchemaTP` v1, instantiating
   `Client<"path/to/SchemaTP.Runtime.dll", "Name;Age">`. Confirm `ClientCo.dll` compiles, and
   independently confirm (via reflection, not just "the build succeeded") that its generated members
   carry `ConsumesFieldAttribute("Name","v1")` and `ConsumesFieldAttribute("Age","v2")`.
3. Point a **new** on-disk consumer source file at `SchemaTP` with `KnownClientPaths` now naming
   `ClientCo.dll`'s path, same field spec (`"Name:v1;Age:v2;Email:v3"`) — check via `FSharpChecker`.
   **Expect zero diagnostics** (`ClientCo`'s recorded dependencies exactly match the current schema).
4. Live edit, same checker: bump `Email` to `v4` in the schema's static argument (irrelevant to
   `ClientCo`, which never named `Email`) — re-check. **Expect continued zero diagnostics.** This is
   the direct analogue of Q009's own irrelevant-field-change precision test, now on the producer side.
5. Live edit, same checker: bump `Age` to `v3` (relevant — `ClientCo`'s compiled attribute still says
   `Age:v1`... **correction check**: `ClientCo` was built against schema v1 where `Age` was `v2`, so
   its recorded attribute is `ConsumesFieldAttribute("Age","v2")`; the schema now says `v3`) — re-check.
   **Expect a stale-dependency failure**, diagnostic naming `ClientCo`'s assembly path, field `Age`,
   client's recorded version (`v2`), schema's current version (`v3`).
6. **The unwedging move.** Rebuild `ClientCo` from source against the *new* schema (`Age` now `v3`) —
   because the client doesn't pin a version (see the design note above), this recompilation alone
   causes `ClientCo`'s regenerated `ConsumesFieldAttribute` for `Age` to read `"v3"` automatically, no
   separate reconciliation step. Re-check the schema-side consumer file again (same live checker
   session if practical, or a fresh one if the rebuilt DLL needs a process boundary to be picked up
   cleanly — report honestly which one was actually needed and why, don't assume). **Expect zero
   diagnostics again** — the block clears once the client catches up, proving the loop doesn't
   permanently wedge a legitimate coordinated upgrade.
7. Re-run step 5's exact check once more, without touching `ClientCo` again, as a sanity re-
   confirmation that the earlier pass in step 6 wasn't a fluke of stale caching masking a real
   continued failure — i.e. deliberately go back to an old, unrelated schema state and confirm the
   check still correctly fails when it should, after having also correctly passed in step 6.

## Round 3 — field removal (closes Q009's own named follow-up 2)

Using `ClientCo` still compiled against a schema where it depends on `Age` (any version): bump the
schema's `FieldSpec` to **remove `Age` entirely** (e.g. `"Name:v1;Email:v3"`, no `Age` at all) while
`ClientCo` is *not* recompiled. Re-check via the live-edit harness. **Expect a removed-field failure**,
diagnostic text distinguishable from Round 2 step 5's stale-dependency wording (must say "removed" or
equivalent, not reuse the version-mismatch phrasing for a field that no longer exists at all) — this is
the specific distinction Q009's own review named as untested and this quartet is the vehicle for
closing.

## Explicitly out of scope (name it, don't silently skip it)

Realistic client-assembly discovery (globbing a real multi-project solution layout, or resolving
`ProjectReference`s at design time) is **not** tested here — `KnownClientPaths` is an explicit static
string, mirroring exactly how Q008/Q009 locate schema assemblies. `BACKLOG.md`'s own framing already
flags this as the more likely source of a REVISE than the coordination semantics; this quartet answers
the coordination-semantics question cleanly and leaves discovery as a named, explicit follow-up, not
something quietly assumed to generalize.

## Reproduction

```
mkdir tp-consumer-driven-contracts-spike && cd tp-consumer-driven-contracts-spike
# reuse ProvidedTypes.fs/.fsi vendored copy from Q006/Q008/Q009's scratch dirs if present
dotnet new classlib -lang F# -o SchemaTP.Runtime
dotnet new classlib -lang F# -o SchemaTP.DesignTime
dotnet new classlib -lang F# -o ClientTP.Runtime
dotnet new classlib -lang F# -o ClientTP.DesignTime
dotnet new console -lang F# -o AttrCheck     # Round 1's independent reflection check
dotnet new console -lang F# -o ClientCo      # Round 2's real consumer project, rebuilt mid-sequence
dotnet new console -lang F# -o Harness       # FSharp.Compiler.Service 43.9.101, Rounds 2 + 3
dotnet build SchemaTP.Runtime SchemaTP.DesignTime ClientTP.Runtime ClientTP.DesignTime
dotnet run --project AttrCheck
dotnet build ClientCo
dotnet run --project Harness
# Round 2 step 6 requires rebuilding ClientCo mid-run: dotnet build ClientCo again, then re-invoke
# the Harness's re-check step against the same or a fresh checker as the results dictate.
```

No MSBuild integration with Myriad's own `Myriad.Sdk` targets — same as Q006/Q008/Q009, this idea is
independent of Myriad by design.

## Explicit instruction to whoever executes this (do not silently deviate)

- Do not edit this file or `00-hypothesis.md` once execution starts. Report design-vs-reality
  corrections honestly in `02-results.md`, mirroring Q010's own precedent for this exactly.
- If Round 1 fails, still report it fully and stop — do not attempt Rounds 2–3 on a foundation that
  doesn't hold, same kill condition as Q006/Q008/Q009.
- Round 2 step 6 is the load-bearing step for the whole capability claim. If the same live
  `FSharpChecker` session doesn't pick up the rebuilt `ClientCo.dll` cleanly (a real, plausible risk —
  FCS may cache assembly reads), try a fresh checker/process for the post-rebuild re-check and report
  which was actually needed. Do not quietly paper over a caching surprise; it would be a genuinely
  useful finding either way (mirrors the kind of correction Q001 and Q010 both surfaced honestly).
- Report exactly what was tested, including any deviation in API names or provider construction from
  this sketch — this design was written from Q008/Q009's own already-proven patterns, so deviations
  should be smaller than Q010's were, but report them regardless if any surface.
