# Q011-consumer-driven-contracts / Movement 3 — Execute + write up

**Status:** DONE. All three rounds built and run to completion, 2026-07-15. Kill condition **not**
triggered (Round 1 passed). Final SHIP/REVISE/NULL verdict is deferred to `03-review.md`; this file
records what actually ran, including three real methodology corrections discovered while building the
harness, all reported here rather than patched quietly into the frozen `01-design.md`.

**Environment:** .NET SDK 9.0.310 (net10.0 host, `dotnet build`/`dotnet run` resolve SDK 10.0.302)
running on Windows. Provider TFMs `netstandard2.0;net8.0` (multi-targeted — see correction 1 below
for why single-targeting was tried first and ruled out as *not* the cause). Consumer/carrier/harness
projects `net8.0`. `FSharp.Compiler.Service` `43.9.101` (pinned). `ProvidedTypes.fs`/`.fsi` vendored by
file copy from the `FSharp.TypeProviders.SDK` checkout at commit `0a95768a2247daba80b24a2604f77f89fc88ff1f`
(same commit Q006/Q008/Q009/Q010 used). Provider projects pin `FSharp.Core` `4.7.2`.

**Scratch layout** (not committed, per Q001–Q010 precedent — only this write-up plus the
`artifacts/` harness/provider sources are durable): `tp-consumer-driven-contracts-spike/` containing
`SchemaTP.Runtime/`, `SchemaTP.DesignTime/`, `ClientTP.Runtime/`, `ClientTP.DesignTime/`,
`SchemaAsmV1/`, `SchemaAsmV2/` (two pre-baked schema-carrier assemblies — see correction 2's
discussion of why the schema's static argument can't be edited in place, same finding Q009 already
made and re-confirmed here), `ClientCo/` (the real consumer project, rebuilt mid-run in Round 2 step 6),
`AttrCarrier/` (Round 1's isolated carrier), `AttrCheck/` (Round 1's independent-reflection console,
reused as a general dump tool), `Harness/` (Rounds 2–3), plus a throwaway `MiniTP.Runtime/DesignTime/`
and `Probe/` pair used only for isolating correction 2 (not part of the quartet's actual mechanism,
kept only as `artifacts/MiniTP-isolation-probe/MiniProvider.fs` for anyone who wants to reproduce the
isolation finding independently).

---

## Read this before the round-by-round results: three real corrections, found by running, not assumed

The design says this quartet "reuses Q008/Q009's provider construction and `FSharpChecker`-based
live-edit harness pattern... to minimize this risk." Two of the three corrections below are exactly
the kind of risk that framing anticipated but did not guarantee against, and are reported in full
rather than smoothed over.

### Correction 1 — `DefineStaticParameters`'s instantiation function is invoked more than once per
check, and an unmemoized callback breaks type identity across those calls

Isolated with a **minimal** generative provider (`MiniTP`, static param `Tag: string`, one property, no
enforcement, no exception possible anywhere) built specifically to rule out anything in `SchemaProvider`'s
own field/enforcement logic as the cause. Instrumenting `createType` with file-based logging (stdout is
not reliable to observe from inside a hosted type provider) showed, for **one single check** of
`Mini<"A">`:

```
13:03:14.330 createType ENTER typeName=M tag=A
13:03:14.335 ProvidedAssembly() created, Location=...\Temp\tmpg3fnds.dll
13:03:14.339 returning t, t.FullName=MiniTP.Provided.M t.Assembly=tmpg3fnds
13:03:14.559 createType ENTER typeName=M tag=A
13:03:14.559 ProvidedAssembly() created, Location=...\Temp\tmp3u1513.dll
13:03:14.560 returning t, t.FullName=MiniTP.Provided.M t.Assembly=tmp3u1513
```

Two invocations, 229ms apart, for the *same* logical instantiation, each constructing a **fresh**
`ProvidedAssembly()` with its own randomly-named temp file. Without memoization the two invocations
return type objects with different backing assemblies, and FCS's diagnostic literally names the
mismatch: `couldn't find type 'MiniTP.Provided.M' in assembly 'tmpg3fnds'` when the type it actually
has on hand belongs to `tmp3u1513` (or vice versa). This generalizes Q008's own secondary observation
("the provider error is reported twice... FCS emits the provider-instantiation failure once during
name resolution and once during the member access that depends on it") from the *failure* path (where
duplication is harmless, since both invocations raise the identical exception) to the *success* path
(where duplication is destructive unless the callback is idempotent by construction).

**Fix:** both `SchemaTP.DesignTime\SchemaProvider.fs` and `ClientTP.DesignTime\ClientProvider.fs` now
memoize their `createType` by a `Dictionary` keyed on the full static-argument tuple, returning the
*same* `ProvidedTypeDefinition`/`ProvidedAssembly` pair for repeat calls with identical arguments. The
failure path is deliberately **not** cached (a repeat call after a raised exception recomputes and
re-raises), matching the "reported twice, identical text" duplicate-diagnostic behavior Q008/Q009 both
already documented and treated as benign.

This fix was necessary but **not sufficient** — see correction 2.

### Correction 2 — `FSharpChecker.ParseAndCheckFileInProject`/`ParseAndCheckProject` (Q008/Q009's own
harness entry points) do not materialize this quartet's generative type even after correction 1, on
the zero-diagnostic success path; `checker.Compile` does

With memoization applied (confirmed via the same log: both invocations now report the *same* assembly
name, e.g. `tmpet5k2x` twice), the **success**-path scenarios (schema state with no client-dependency
violation — Round 2's steps 1, 3, 4) still failed under `ParseAndCheckFileInProject` **and**
`ParseAndCheckProject` (tested both, plus `useTransparentCompiler=true/false`, plus
`keepAssemblyContents=true/false` — none of these changed the outcome), with the identical
"couldn't find type" diagnostic, still naming a *self-consistent* temp assembly this time. Three
independent controls ruled out everything else:

1. The exact same script text, run via a **real `dotnet fsi` session** (zero FCS diagnostics-only
   API involvement), printed `Tag=B` correctly — the provider itself is not the problem.
2. The exact same script text, compiled via **`checker.Compile(["fsc.exe"; "-o"; outDll; ...])`** — a
   real, full FCS-hosted compile, still entirely in-process, no `dotnet build` subprocess — returned
   `exitCode=0`, zero errors, and produced a real output DLL on disk.
3. Only the **diagnostics-only checking APIs** (`ParseAndCheckFileInProject`, `ParseAndCheckProject`)
   fail to resolve the type on the success path, regardless of compiler backend
   (`TransparentCompiler`/`BackgroundCompiler`) or `keepAssemblyContents`.

This is a genuine divergence from what the design's novelty gate assumed would transfer directly: Q008
and Q009 both report success-path zero-diagnostic results (e.g. Q008 Round 2 Direction 1: "1161ms...
0 diagnostics... `C.VerifiedVersion` binding resolves as `type Microsoft.FSharp.Core.string` — real
generated members") using exactly `ParseAndCheckFileInProject`. Whatever made their `ClientTP`
resolve cleanly on that path and this quartet's `SchemaProvider`/`MiniTP` not resolve was not
identified with certainty (candidates ruled out: static-parameter count, explicit vs. implicit script
`module` declaration, single- vs. multi-targeted provider TFM, `keepAssemblyContents`,
`TransparentCompiler` vs. `BackgroundCompiler`) — reported as an open, unexplained discrepancy rather
than a guessed root cause dressed up as a finding.

**Fix, applied and reported, not silently substituted for the design's harness:** the `Harness` used
for every Round 2/3 result below calls `checker.Compile(["fsc.exe"; "-o"; outDll; "--target:library";
"-r:" + schemaRuntimeDll; consumerFsPath])` for each scenario, not `ParseAndCheckFileInProject`. This
is still FCS-hosted, still re-invoked after every live edit of the on-disk consumer file, still using
one `FSharpChecker` instance created once and reused across every call in a phase — it is **not**
`dotnet build`, no MSBuild, no subprocess for the check itself. But it is a real compile (produces an
output DLL each time), not Q008/Q009's incremental, diagnostics-only re-check — so the timing numbers
below are **compile times**, reported as such, and are **not** directly comparable to Q008's 19–32ms or
Q009's 15–57ms live-recheck bands. This is the single largest methodology deviation from the design in
this quartet, stated plainly per the design's own explicit instruction to do so.

### Correction 3 — `Assembly.LoadFrom` holds a Windows file lock for the process's lifetime, blocking
the very rebuild Round 2 step 6 depends on; `Assembly.Load(File.ReadAllBytes(...))` does not

First attempt at Round 2 step 6 (rebuild `ClientCo` from source, repointed at `SchemaAsmV2`, from
*within* the same long-running `Harness` process that had already `Assembly.LoadFrom`'d `ClientCo.dll`
during step 5's check) failed the `dotnet build` subprocess itself, not the re-check:

```
error MSB3027: Could not copy "obj\Release\net8.0\ClientCo.dll" to "bin\Release\net8.0\ClientCo.dll".
Exceeded retry count of 10. Failed. The file is locked by: "Harness (55348)"
```

MSBuild's own diagnostic names the `Harness` process as the lock holder. This is a real, empirically
confirmed obstacle on this system: `SchemaProvider`'s `readClientConsumes` had used
`Assembly.LoadFrom(path)` to reflect over `ClientCo.dll`'s `ConsumesFieldAttribute` stamps during step
5's check, and that load kept the file locked for the remainder of the process's life, preventing the
"just recompile the client" unwedging move from even *producing* a new DLL, let alone testing whether
the checker would pick it up.

**Fix:** both `readClientConsumes` (SchemaProvider) and `readSchemaFields` (ClientProvider) now use
`Assembly.Load(File.ReadAllBytes(path))` instead of `Assembly.LoadFrom(path)` — reads the bytes and
closes the file handle immediately, leaving no lock. `Assembly.Load(bytes)` does not do `LoadFrom`'s
directory-based dependency probing, so each provider also registers a small
`AppDomain.CurrentDomain.AssemblyResolve` handler (netstandard2.0-compatible; `AssemblyLoadContext` is
netcoreapp-only and not available to a `netstandard2.0`-targeted design-time assembly) that resolves a
missing dependency (e.g. `ClientCo.dll` → `ClientTP.Runtime.dll`, both MSBuild-colocated in the same
output folder) by simple-name lookup in the loaded assembly's own directory. This is the mirror image
of Q008's own already-reported finding ("`Assembly.LoadFrom`'s probing context resolves the co-located
runtime DLL on its own... the custom resolver was dead weight") — here the custom resolver is *not*
dead weight, because switching away from `LoadFrom` specifically to avoid the lock costs that
free probing and it has to be replaced by hand.

With this fix applied, the same rebuild-from-within-the-live-process sequence succeeded cleanly (`dotnet
build` exit code 0, no lock, no retries) — see Round 2 step 6 below for the actual result this made
possible.

---

## Round 1 — falsifier: does a CLIENT-emitted member attribute survive into independently-reflectable IL?

`AttrCarrier.dll` was built containing `type Stamped = ClientTP.Provided.Round1Stamped<"Name">` — one
generative type, one provided property (`Name`), stamped via `ClientProvider`'s own
`ConsumesFieldAttribute("Name", "v1")`, **no schema reflection at all** (mirrors Q009's Round 1
exactly, roles swapped: the *client*, not the *schema*, is the one emitting). `AttrCheck` — a separate
console referencing only `ClientTP.Runtime.dll`, zero SDK involvement — did `Assembly.LoadFrom` and
read the property's attribute back via plain `PropertyInfo.GetCustomAttributesData()`:

```
Loading (plain reflection, no SDK): ...\AttrCarrier\bin\Release\net8.0\AttrCarrier.dll
type 'AttrCarrier.Baked+Stamped'
  property 'Name'
    attribute: ClientTP.Runtime.ConsumesFieldAttribute
    attribute assembly: ClientTP.Runtime
    ctor arg [0] (FieldName) = "Name"
    ctor arg [1] (Version)   = "v1"

ConsumesFieldAttribute instances found (client-emitted, member-level): 1
ROUND 1 (independent client-emitted member reflection) verdict: PASS
```

Same four things load-bearing as Q009's Round 1, now confirmed for the *client* side: (1) the attribute
survives into on-disk IL on a property, readable with zero SDK involvement; (2) both constructor
arguments round-trip exactly; (3) the declaring assembly in the emitted metadata is `ClientTP.Runtime`
(the `assemblyReplacementMap` remap fired correctly, same as every prior quartet); (4) this is a
*client*-emitted attribute — the mechanism claim under test — not a re-run of Q009's schema-emitted case.

**Round 1: PASS. Kill condition not triggered.** Attribute-emission-and-survival is confirmed symmetric
across which of the two paired providers does the emitting, closing the mechanism claim's open question.

---

## Round 2 — the well-foundedness loop

Run in two OS processes, `phase1` and `phase2`, per the design's own allowance ("try a fresh
checker/process for the post-rebuild re-check... report which was actually needed"). Both phases use
one `FSharpChecker` instance, created once, reused across every `checker.Compile` call within that
phase (see correction 2 for why `Compile` replaces `ParseAndCheckFileInProject` throughout).

**Step 1 — sanity check, zero known clients.** `SchemaTP.Provided.Schema<"Name:v1;Age:v2;Email:v3", "">`:
**1437ms** (genuine cold, first check in the process), **0 error diagnostics**. Generates cleanly with
no clients registered.

**Step 2 — independent confirmation of `ClientCo`'s recorded dependencies.** `ClientCo` was built
(fresh, from `ClientCo/Program.fs` pointing at `SchemaAsmV1`, i.e. schema state `Age:v2`) and its
compiled DLL read back via `AttrCheck`'s plain-reflection `dump` mode, zero SDK involvement:

```
ClientCo.Main+Dep.Name -> ConsumesField(fieldName="Name", version="v1") [attr assembly: ClientTP.Runtime]
ClientCo.Main+Dep.Age  -> ConsumesField(fieldName="Age", version="v2")  [attr assembly: ClientTP.Runtime]
```

Real generated members (`d.Name`, `d.Age` both print at runtime — `ClientCo` runs, not just compiles),
each independently confirmed to carry the exact recorded dependency, not just "the build succeeded."

**Step 3 — baseline match.** Schema `"Name:v1;Age:v2;Email:v3"`, `KnownClientPaths` naming `ClientCo.dll`:
**188ms**, **0 error diagnostics (CLEAN)**.

**Step 4 — irrelevant field change.** Schema `Email` bumped `v3→v4` (irrelevant — `ClientCo` never named
`Email`): **156ms**, **0 error diagnostics (CLEAN)**. `ClientCo`'s recorded dependencies (`Name:v1`,
`Age:v2`) are untouched by the change and the schema-side check correctly stays clean.

**Step 5 — relevant field change, expect stale block.** Schema `Age` bumped `v2→v3` (relevant —
`ClientCo`'s recorded `ConsumesFieldAttribute("Age","v2")` no longer matches): **156ms**, **exit code
1, 3 error diagnostics**. Verbatim provider diagnostic (deduplicated — see the "reported twice" note
below):

```
The type provider 'SchemaTPImplementation.SchemaProvider' reported an error: SchemaTP
  consumer-driven-contract violation (1 issue(s)): STALE dependency: client
  'C:/Users/Dave/tp-consumer-driven-contracts-spike/ClientCo/bin/Release/net8.0/ClientCo.dll' recorded
  field 'Age' at version 'v2' but the schema now declares 'v3'
```

Names the specific client assembly path, the specific field (`Age`), the client's recorded version
(`v2`), and the schema's current version (`v3`) — all four required by the design.

**Repeat probe.** Re-checking the *identical* step 5 scenario again, same process, no external change:
**106ms**, same 3 diagnostics, byte-identical message. Confirms the block is deterministic, not a
one-shot fluke.

**Step 6 — THE UNWEDGE.** `ClientCo/Program.fs` was rewritten to point `SchemaAssemblyPath` at
`SchemaAsmV2` (the pre-baked schema-carrier where `Age` is `v3` — see the note below on why the
schema's static argument can't be edited in place, same as Q009's own correction) and rebuilt via a
real `dotnet build ClientCo.fsproj -c Release` subprocess, launched **from within the still-running
`Harness` process** that had already checked step 5 above:

```
dotnet build ClientCo (repointed at SchemaAsmV2) exit=0 in 2019ms
ClientCo.dll on disk after rebuild: length=7680 bytes, lastWriteUtc=15/07/2026 12:13:50
```

Exit code 0, no file lock, no MSBuild retries — correction 3's fix holding. Independent confirmation
this was a genuine, meaningful rebuild (not a no-op): `Age`'s recorded version in the rebuilt
`ClientCo.dll`, read back by `AttrCheck`'s plain-reflection dump at the start of `phase2` (a fresh
process, so this reads the DLL fresh off disk with zero possibility of stale in-memory state):

```
ClientCo recorded dependencies (independent reflection): [("Name", "v1"); ("Age", "v3")]
```

`Age` genuinely changed from `v2` to `v3` in the rebuilt DLL, confirming the client's own regenerated
`ConsumesFieldAttribute` picked up the new schema state automatically — exactly the "no client-declared
version pinning" simplification the design specifies (the client doesn't predict a version; recompiling
alone re-reads whatever the schema currently says).

**Post-rebuild re-check, SAME process, SAME checker, no restart:**

```
--- step5-scenario after rebuild, SAME process
    FieldSpec="Name:v1;Age:v3;Email:v4"  KnownClients=ClientCo.dll
    compile: 201ms, exitCode=0, 0 error diagnostic(s) (CLEAN)
```

**Cleared, in the same process, on the very next check, with no fresh checker and no process restart
needed.** This is the load-bearing result the design flags as the single most important step in the
whole quartet: once correction 3's fix (`Assembly.Load(bytes)`, not `Assembly.LoadFrom`) removed the
file-lock obstacle, there was **no further caching surprise** to report — the same `FSharpChecker`
instance that had just reflected the *old* `ClientCo.dll` bytes correctly reflected the *rebuilt*
bytes on the immediately following `checker.Compile` call. (Contrast with correction 2, which *is* a
real methodology limitation of the diagnostics-only checking APIs — but once the harness was switched
to `checker.Compile`, no *additional* caching problem appeared at the rebuild boundary specifically.)

**Step 7 — re-confirm not-stale-pass, sanity check against a fluke.** Without touching `ClientCo`
again, the schema-side check was pointed back at an *old*, now-conflicting state (`Age:v2` — the state
`ClientCo` no longer matches, since it now records `v3`):

```
The type provider 'SchemaTPImplementation.SchemaProvider' reported an error: SchemaTP
  consumer-driven-contract violation (1 issue(s)): STALE dependency: client '...\ClientCo.dll'
  recorded field 'Age' at version 'v3' but the schema now declares 'v2'
```

**133ms, exit code 1, 3 error diagnostics — correctly re-blocked.** This is the sanity re-confirmation
the design specifically requires: step 6's clean pass was not a fluke of stale caching masking a real
continued failure — the exact same provider, same process, correctly distinguishes "matches" from
"doesn't match" on the very next check, in both directions, using the client's *new* recorded state.

**Round 2: PASS**, both directions of the well-foundedness claim: a schema edit conflicting with a
stale client's recorded dependency blocks with a diagnostic naming the client assembly, field, and both
versions (step 5); recompiling that client against the new schema clears the block with no deadlock and
no extra ceremony beyond the rebuild itself (step 6); the clearing is not a caching artifact (step 7).

**Secondary observation, reported not smoothed over (matches Q008/Q009's own recurring note):** each
scenario's raw diagnostics list is length 3, not the "one violation → one diagnostic" a reader might
expect: the provider error text appears **twice** (identical, deduplicated to one line above) plus one
`Invalid use of a type name` downstream error from `let _s = S()` failing to bind after the provider's
instantiation raised. Gating on the diagnostics list (not on symbol resolution) is unaffected by this,
same discipline as every prior quartet in this lineage. This third quartet in a row hitting the exact
same un-deduplicated double-report is worth escalating past a per-quartet footnote (see the review's
own likely verdict on this point).

---

## Round 3 — field removal (closes Q009's own named follow-up 2)

Using `ClientCo` in its **rebuilt** state (still depends on `Age`, now recorded at `v3`, per Round 2
step 6), the schema's `FieldSpec` was changed to remove `Age` **entirely**: `"Name:v1;Email:v4"`, no
`Age` field at all. Re-checked via the same `checker.Compile`-based harness, fresh process (`phase2`,
continuing directly after step 7 above, same checker instance within that process):

```
The type provider 'SchemaTPImplementation.SchemaProvider' reported an error: SchemaTP
  consumer-driven-contract violation (1 issue(s)): REMOVED field: client '...\ClientCo.dll' consumes
  field 'Age' (recorded at version 'v3') but that field no longer exists in the schema
```

**119ms, exit code 1, 3 error diagnostics (same 2-copies-plus-one-downstream shape as Round 2).**

**Diagnostic-text distinctness, checked directly as the design requires (not just "PASS"):**

| scenario | wording |
|---|---|
| Round 2 step 7 (stale, field still exists) | `STALE dependency: client '...' recorded field 'Age' at version 'v3' but the schema now declares 'v2'` |
| Round 3 (field removed entirely) | `REMOVED field: client '...' consumes field 'Age' (recorded at version 'v3') but that field no longer exists in the schema` |

The two messages are lexically distinct at the first differentiating word (`STALE` vs. `REMOVED`), use
different sentence structure (`... but the schema now declares 'X'` vs. `... but that field no longer
exists in the schema`), and would not be confused by a human reading either in isolation or by any
substring/prefix match a tool might use to distinguish them programmatically. This directly closes
Q009's own review-identified follow-up 2 ("test field *removal*, not just version bumps on a fixed
field set").

**Round 3: PASS.**

---

## Timing summary, read against Q008/Q009's numbers honestly

```
                                compile time
step 1 (cold, zero clients)    1437ms
step 3 (baseline match)         188ms
step 4 (irrelevant bump)        156ms
step 5 (stale block)            156ms
step 5-repeat                   106ms
step 6 post-rebuild (cleared)   201ms
step 7 (re-block)               133ms
step 6 rebuild itself (dotnet build, separate subprocess) 2019ms
phase 2 cold (fresh process)   1344ms
round 3 (removed)               119ms
```

These are **`checker.Compile` full-compile times**, not Q008's 19–32ms or Q009's 15–57ms
*incremental-typecheck* re-check times — correction 2 explains why the two are not the same
measurement and should not be read as a regression in the underlying mechanism. Read at face value,
warm compiles here land in the 106–201ms band, an order of magnitude above Q008/Q009's live-recheck
band but two to three orders of magnitude below a real `dotnet build` of a project this size (the
actual `ClientCo` rebuild took 2019ms). Whether a `ParseAndCheckFileInProject`-based path could be
made to work for this quartet's specific generative-type shape (and hit Q008/Q009's faster band) is an
open question this quartet did not resolve — reported as exactly that, not glossed over.

---

## Out of scope, as the design requires stating plainly

Realistic client-assembly discovery (globbing a real multi-project solution, or resolving
`ProjectReference`s at design time) was **not** tested. `KnownClientPaths` was an explicit static
string throughout, naming one hand-built path, mirroring exactly how Q008/Q009 located schema
assemblies. `BACKLOG.md`'s own framing already named this as the more likely source of a REVISE than
the coordination semantics; this quartet answers the coordination-semantics question (Rounds 1–3 above)
and leaves discovery as a named, explicit follow-up.

---

## What was and wasn't proven, stated plainly

- **Proven:** a custom attribute stamped on a member by the *client*-side generative provider survives
  into independently-reflectable IL exactly as Q009 proved for the schema side (Round 1) — attribute
  survival is confirmed symmetric across which paired provider emits. A schema-side provider can load a
  set of known, already-compiled client assemblies, collect their recorded field dependencies via plain
  reflection, and refuse to generate when the current schema state conflicts with any of them — citing
  the specific client, field, and both versions (Round 2, step 5). The block correctly clears once the
  conflicting client is recompiled against the new schema, with the client's own recorded dependency
  updating automatically (no client-side version pinning needed) and with **no additional ceremony
  beyond the rebuild itself** once a genuine Windows file-lock obstacle (correction 3) was fixed (Round
  2, step 6) — and the clearing is confirmed not to be a caching fluke (Round 2, step 7). Field removal
  (not just re-versioning) produces a diagnostic lexically and structurally distinct from the
  stale-version wording, closing Q009's own named follow-up (Round 3).
- **Not proven / boundary for the review:** as in every prior quartet in this lineage, this ran against
  `FSharpChecker` as a library, not a literal Ionide/VS/Rider session with FSAC's own caching layered on
  top. Realistic multi-project client discovery is untested by design (named above). The root cause of
  correction 2 (why this quartet's generative type fails to resolve via `ParseAndCheckFileInProject`/
  `ParseAndCheckProject` even after correction 1's memoization fix, when Q008/Q009's own structurally
  similar `ClientTP` succeeded via the identical API) was **not** identified with certainty — several
  plausible candidates were tested and ruled out, but the true cause remains open. Timing numbers are
  compile times via `checker.Compile`, not incremental-recheck times, and are not directly comparable to
  Q008/Q009's band (see the timing summary above) — whether a faster, `ParseAndCheckFileInProject`-based
  path exists for this quartet's specific provider shape is untested.
- **Corrections surfaced, not patched silently into the frozen files:** three, detailed in full above —
  (1) `DefineStaticParameters`'s instantiation function must be memoized by static-argument tuple, since
  FCS invokes it more than once per logical check and an unmemoized callback creates a fresh
  `ProvidedAssembly` (and therefore a type-identity mismatch) on each call; (2) `checker.Compile` was
  substituted for the design's assumed `ParseAndCheckFileInProject`/`ParseAndCheckProject` harness
  pattern, because the latter two do not materialize this quartet's generative type even on the
  zero-diagnostic success path, with the reason not fully identified; (3) `Assembly.LoadFrom` holds a
  Windows file lock on the loaded client assembly for the process's lifetime, which blocked the Round 2
  step 6 rebuild outright (an `MSB3027` MSBuild error, not a logical check failure) until switched to
  `Assembly.Load(File.ReadAllBytes(...))` plus a manual `AppDomain.AssemblyResolve`-based dependency
  prober (since `Assembly.Load(bytes)` does not do `LoadFrom`'s automatic directory-based probing).

---

## Reproduction

From the scratch dir `tp-consumer-driven-contracts-spike` (vendored `ProvidedTypes.fs`/`.fsi` copied
from the pinned `FSharp.TypeProviders.SDK` commit):

```
# build both providers (each Runtime ref triggers its DesignTime build + colocates the TPDTC)
dotnet build SchemaTP.Runtime/SchemaTP.Runtime.fsproj -c Release
dotnet build ClientTP.Runtime/ClientTP.Runtime.fsproj -c Release

# pre-bake the two schema states client rebuilds move between
dotnet build SchemaAsmV1/SchemaAsmV1.fsproj -c Release   # Age:v2
dotnet build SchemaAsmV2/SchemaAsmV2.fsproj -c Release   # Age:v3

# Round 1: independent client-emitted member reflection, no SDK in the loop
dotnet build AttrCarrier/AttrCarrier.fsproj -c Release
dotnet run --project AttrCheck/AttrCheck.fsproj -c Release --no-build -- round1 AttrCarrier/bin/Release/net8.0/AttrCarrier.dll

# ClientCo starts pointed at SchemaAsmV1 (Age:v2); Harness/phase1 rewrites it to SchemaAsmV2 mid-run
dotnet build ClientCo/ClientCo.fsproj -c Release

# Rounds 2 + 3
dotnet build Harness/Harness.fsproj -c Release
dotnet run --project Harness/Harness.fsproj -c Release --no-build -- phase1   # steps 1,3,4,5 + THE UNWEDGE (step 6)
dotnet run --project Harness/Harness.fsproj -c Release --no-build -- phase2   # step 7 + Round 3, fresh process
```

Scratch layout durable copies under `experiments/Q011-consumer-driven-contracts/artifacts/`:
`SchemaTP.Runtime/Provenance.fs` (`FieldProvenanceAttribute`), `ClientTP.Runtime/Consumes.fs`
(`ConsumesFieldAttribute` — the reversed-arrow's own attribute type), `SchemaTP.DesignTime/SchemaProvider.fs`
(the enforcing schema-side provider, with correction 1's memoization and correction 3's
`Assembly.Load(bytes)` fix both present), `ClientTP.DesignTime/ClientProvider.fs` (the client-side
provider), `AttrCheck/Program.fs` (Round 1 falsifier, doubles as a general attribute dump tool),
`ClientCo/ClientCo.fsproj` (the real, rebuilt-mid-run consumer project — its `Program.fs` is generated
inline by the Harness, both v1/v2 variants are embedded as string templates in
`Harness/Program.fs`), `Harness/Program.fs` (Rounds 2–3, `phase1`/`phase2`),
`MiniTP-isolation-probe/MiniProvider.fs` (the minimal provider used to isolate correction 1, kept for
independent reproducibility of that finding specifically). No wiring into Myriad's own `Myriad.Sdk`
targets — this idea is independent of Myriad by design, same as Q006/Q008/Q009/Q010.
