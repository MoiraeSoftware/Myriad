# Q008-provenance-closed-loop / Movement 3 — Execute + write up

**Status:** DONE. All three rounds built and run to completion, 2026-07-15. Kill condition **not**
triggered (Round 1 passed). Final SHIP/REVISE/NULL verdict is deferred to `03-review.md`; this file
records what actually ran.

**Environment:** .NET SDK 9.0.310 host; provider TFMs `netstandard2.0;net8.0`; consumer/harness
`net8.0`. `FSharp.Compiler.Service` `43.9.101` (pinned, matches `Myriad/paket.lock`).
`ProvidedTypes.fs`/`.fsi` reused by file copy from Q006's vendored `FSharp.TypeProviders.SDK` @
`0a95768a2247daba80b24a2604f77f89fc88ff1f`. Provider projects pin `FSharp.Core` `4.7.2`; FCS
resolved `FSharp.Core` `10.1.301` for the harness (NU1608 warning, benign — identical to Q006's
setup; the providers load with their own FSharp.Core).

**Two genuinely separate provider pairs**, per the design's load-bearing precondition:
`SchemaTP.Runtime`/`SchemaTP.DesignTime` (emits provenance) and `ClientTP.Runtime`/`ClientTP.DesignTime`
(reads and enforces it). Plus `SchemaAsm` (a plain classlib that references *only* `SchemaTP.Runtime`
and bakes the stamped generative type into its own IL), `AttrCheck` (Round 1's independent-reflection
console), and `Harness` (FCS host, Rounds 2+3). The `IsFSharpDesignTimeProvider=true` project
reference colocated each `*.DesignTime.dll` alongside its `*.Runtime.dll`, so FCS's default probing
found both providers with no `typeproviders/fsharpNN` subfolder needed — same as Q006.

The custom attribute under test is a **real** attribute defined in `SchemaTP.Runtime`, not one of the
SDK's built-in helpers:

```fsharp
[<AttributeUsage(AttributeTargets.All, AllowMultiple = false)>]
type SchemaVersionAttribute(version: string) =
    inherit Attribute()
    member _.Version = version
```

`SchemaTP`'s provider stamps it on the generative provided type via `AddCustomAttribute` with a
hand-built `CustomAttributeData` whose `Constructor` points at the (design-time copy of the) runtime
attribute type. The SDK's `System.Reflection.Emit` code-gen path (`ProvidedTypes.fs:15768-15783`,
`defineCustomAttrs`) runs that constructor's declaring type through `transCtorSpec`/`transType`,
which is what applies the `assemblyReplacementMap` remap into `SchemaTP.Runtime` during IL emission.
Verified from source before building; confirmed empirically in Round 1.

---

## Round 1 — falsifier: do generative custom attributes survive into independently-reflectable IL?

`SchemaAsm.dll` was built containing `type SchemaV2 = SchemaTP.Provided.Schema<"v2">`. Building it ran
the generative provider and baked the stamped type into `SchemaAsm.dll`'s real IL. `AttrCheck` — a
separate console program that references **only** `SchemaTP.Runtime.dll` and never touches
`ProvidedTypes.fs` or any design-time assembly — then did `Assembly.LoadFrom` on `SchemaAsm.dll` and
read the attribute back via plain `System.Reflection.CustomAttributeData.GetCustomAttributesData()`:

```
Loading (plain reflection, no SDK): ...\SchemaAsm\bin\Release\net8.0\SchemaAsm.dll
  type 'SchemaAsm.Schemas+SchemaV2'
    attribute: SchemaTP.Runtime.SchemaVersionAttribute
    attribute assembly: SchemaTP.Runtime
    ctor arg [0] (Version) = "v2"

SchemaVersionAttribute instances found: 1
ROUND 1 (independent reflection) verdict: PASS
```

Three things are load-bearing here and all held: (1) the attribute survives into on-disk IL, readable
with zero SDK involvement; (2) the constructor argument `"v2"` round-trips exactly; (3) the attribute's
declaring assembly in the emitted metadata is `SchemaTP.Runtime`, i.e. the `assemblyReplacementMap`
remap fired correctly — the consumer sees the *runtime* attribute type, not a dangling design-time
reference.

**Round 1: PASS. Kill condition not triggered.** The "attribute injection as a metadata bridge"
finding recorded in `BACKLOG.md` (previously assumed, never tested) is now empirically confirmed for
the generative-provider case.

### Surprise #1 — the emitted type name

The generated type's full name is **`SchemaAsm.Schemas+SchemaV2`**, not something under the provider's
own `SchemaTP.Provided` namespace. For a generative provider the emitted type lands where the consumer
*names* it: the `type SchemaV2 = ...` abbreviation in module `SchemaAsm.Schemas` caused FCS to emit the
generative type as a nested type `SchemaV2` under that module. This directly shaped Round 2 — `ClientTP`
must be told this consumer-side name, and a robust reader cannot assume the provider's namespace.

---

## Round 2 — the two-provider gate, both directions

`ClientTP.Provided.Client<SchemaAssemblyPath, SchemaTypeName, ExpectedVersion>` (three string static
parameters). Its instantiation function `Assembly.LoadFrom`s the schema assembly, finds the named type
(falling back to an attribute scan if the name misses), reads `SchemaVersionAttribute` via
`GetCustomAttributesData()`, and compares the declared version to `ExpectedVersion`. On match it emits
two real provided members (`VerifiedVersion : string`, `ProvenanceOk : bool`); on mismatch it raises
from within the instantiation function. Tested through Q006's exact harness API
(`FSharpChecker.ParseAndCheckFileInProject`, gating strictly on the diagnostics list).

**Direction 1 — schema `v2`, client expects `v2` (match):** 1161ms (genuine cold, first check in the
process), **0 diagnostics**. `C.VerifiedVersion` binding resolves as `type Microsoft.FSharp.Core.string`
— real generated members, not just absence of error.

**Direction 2 — schema `v2`, client expects `v3` (mismatch):** 65ms, **4 diagnostics (2 unique)**:

```
Error (2,9): The type provider 'ClientTPImplementation.ClientProvider' reported an error: ClientTP
  provenance mismatch: schema type 'SchemaAsm.Schemas+SchemaV2' declares SchemaVersion 'v2' but the
  client was told to expect 'v3'
Error (2,9): The type provider 'ClientTPImplementation.ClientProvider' reported an error: ClientTP
  provenance mismatch: schema type 'SchemaAsm.Schemas+SchemaV2' declares SchemaVersion 'v2' but the
  client was told to expect 'v3'
Error (3,19): The type 'C' does not define the field, constructor or member 'VerifiedVersion'.
Error (4,18): The type 'C' does not define the field, constructor or member 'ProvenanceOk'.
```

The critical question the design flagged — does FCS surface the raised exception text, or a generic
wrapper? — answered verbatim: **FCS surfaces the full raised message**, wrapped in a fixed prefix
`The type provider 'ClientTPImplementation.ClientProvider' reported an error: `. Both version strings
(`'v2'` declared, `'v3'` expected) and the schema type name are present in the diagnostic. This is a
specific, citable compiler error, not a swallowed "type provider instantiation failed."

Two secondary observations, reported rather than smoothed over:

- The provider error is reported **twice** (identical text, same `(2,9)` location). FCS emits the
  provider-instantiation failure once per... something (likely once during name resolution and once
  during the member access that depends on it); the harness gates on `hasErrors`, so duplication does
  not affect the verdict, but a real editor would show two red squiggles.
- The mismatch also produces the two downstream `(3,19)`/`(4,18)` "does not define member" errors:
  because instantiation raised, the type `C` has *no* members, so the `VerifiedVersion`/`ProvenanceOk`
  accesses fail too. This is exactly Q006's error-recovery caveat in reverse and is why gating on the
  diagnostic list (not on "did a symbol resolve") is correct — here the truth is unambiguous because
  the provider error is itself in the list.

**Round 2: PASS** (match → 0 diagnostics with real members; mismatch → specific real diagnostic naming
both versions).

---

## Round 3 — live-edit violation detection, same `FSharpChecker`, no rebuild

Same checker instance across all three steps, one on-disk consumer file, only the `ExpectedVersion`
static argument edited between checks (version bumped, re-checked — the FSAC/Ionide per-keystroke
buffer semantics Q006 used). **Nothing is rebuilt between steps**; `SchemaAsm.dll` on disk never
changes.

| step | edit | re-check time | result |
|---|---|---|---|
| 1 cold match | `ExpectedVersion="v2"` | 37ms | 0 diagnostics |
| 2 live-edit mismatch | `"v2"` → `"v3"`, same checker | **19ms** | mismatch diagnostic appears (4 diags, 2 unique) |
| 3 live-edit cleared | `"v3"` → `"v2"`, same checker | 32ms | 0 diagnostics |

The step-2 diagnostic is byte-for-byte the Round 2 mismatch text above. The provenance conflict is
caught **the moment the source is edited**, in a build-free re-check, and clears again on editing back
— the central claim under test.

### Comparison to Q006's baseline (1137ms cold / 47ms live re-check)

- **Cold:** the genuine cold cost is Round 2 direction 1's **1161ms** (first check in the process),
  essentially identical to Q006's 1137ms. Round 3 step 1 reads 37ms *only because the same checker was
  already warmed by Round 2* — noted here rather than presented as a second cold number.
- **Live re-check:** **19ms** for the provenance-conflict re-check — **faster** than Q006's 47ms,
  despite this recheck doing genuinely more work per resolution (a second `Assembly.LoadFrom` plus a
  custom-attribute read and string compare). The extra reflection is dominated by FCS's own
  incremental-check machinery and does not move the number out of Q006's order of magnitude. The
  hypothesis's REVISE threshold ("materially worse than baseline, 10x–100x") is not met; the live cost
  is at or below baseline.

**Round 3: PASS** (cold match clean; mismatch caught on live edit with no rebuild; diagnostics clear on
edit-back).

---

## The design's flagged open question, resolved honestly

`01-design.md:34-38` explicitly left open "exactly how `ClientTP` resolves `SchemaTP`'s compiled
assembly path/location across two independently-built scratch projects" and asked for whatever the
actual working mechanism turned out to be.

**Actual mechanism:** the absolute path to `SchemaAsm.dll` is passed as `ClientTP`'s
`SchemaAssemblyPath` static argument (forward-slashed so it embeds cleanly in the consumer's F# source
literal). The instantiation function calls `Assembly.LoadFrom` on that path and reads the attribute.
No `TypeProviderConfig.ReferencedAssemblies` scoping was involved at all — the schema assembly is
deliberately *not* referenced by the consumer, only loaded by path inside the provider.

**One design assumption I built in and then falsified:** I first added an
`AppDomain.CurrentDomain.AssemblyResolve` handler to resolve `SchemaTP.Runtime.dll` (which carries the
attribute type, needed when `CustomAttributeData.AttributeType` is forced) from the schema assembly's
directory, expecting cross-project dependency resolution to need help. I then tested whether it was
load-bearing by removing it and re-running: **all rounds still pass**. `Assembly.LoadFrom`'s probing
context resolves the co-located `SchemaTP.Runtime.dll` on its own, because the build drops it into
`SchemaAsm`'s output folder next to `SchemaAsm.dll`. The custom resolver was dead weight and was
removed from the final provider. The real precondition is simply: **the schema assembly's own
dependencies must sit beside it on disk** (which normal `dotnet build` output satisfies). Reported here
rather than left in as unexplained defensive code.

---

## What was and wasn't proven, stated plainly

- **Proven:** a custom attribute stamped on a generative provided type survives into independently
  reflectable on-disk IL, correctly remapped to the runtime assembly (Round 1). A *second*, genuinely
  separate provider can `Assembly.LoadFrom` the first provider's compiled output, read that attribute
  by ordinary reflection, and **refuse to generate** — with a specific compiler diagnostic naming both
  the declared and expected versions — when provenance disagrees (Round 2). That refusal is caught on a
  **live source edit with no rebuild**, at a 19ms re-check, at or below Q006's 47ms baseline (Round 3).
  End-to-end compile-time supply-chain verification through the type system, no external tooling.
- **Not proven / boundary for the review:** as in Q006, this ran against `FSharpChecker` (the host
  *library*), not a literal Ionide/VS/Rider session with FSAC's caching layers on top. The provenance
  compare is re-run on *every* instantiation (no caching of the reflected version across checks was
  added or measured); it was cheap enough here not to matter, but a design that reflected a large
  schema assembly on every keystroke has not been stress-tested.
- **The cross-package framing is honest, not incidental:** `ClientTP` reads `SchemaTP`'s *compiled*
  output, exactly Q006's established wall (a provider only sees already-compiled referenced code). For
  this idea that wall is the natural shape — a published schema package vs a separately-built client —
  not a limitation. Whether the NULL threshold applies (could a plain build-time version-check script
  do the same job more cheaply, making the through-the-type-system framing not worth its complexity) is
  `03-review.md`'s call; this file only records that the mechanism works and at what cost.
- **Design corrections surfaced (not patched silently):** (1) the emitted generative type is named
  after the *consumer's* abbreviation (`SchemaAsm.Schemas+SchemaV2`), not the provider namespace, so
  `ClientTP` needs the consumer-side type name (or an attribute scan). (2) The `AssemblyResolve` handler
  the design implied might be needed is not needed; `Assembly.LoadFrom` + co-located dependencies
  suffice.

---

## Reproduction

From the scratch dir `tp-provenance-spike` (vendored `ProvidedTypes.fs`/`.fsi` copied from Q006's
`tp-spike`):

```
# build both providers (each Runtime ref triggers its DesignTime build + colocates the TPDTC)
dotnet build SchemaTP.Runtime/SchemaTP.Runtime.fsproj -c Release
dotnet build ClientTP.Runtime/ClientTP.Runtime.fsproj -c Release

# build the schema consumer — bakes the stamped generative type into SchemaAsm.dll's IL
dotnet build SchemaAsm/SchemaAsm.fsproj -c Release

# Round 1: independent reflection, no SDK in the loop
dotnet run --project AttrCheck/AttrCheck.fsproj -c Release

# Rounds 2 + 3: FCS host, two-provider gate + live-edit timing (no consumer build)
dotnet build Harness/Harness.fsproj -c Release
dotnet run --project Harness/Harness.fsproj -c Release --no-build
```

Scratch layout: `SchemaTP.Runtime/` (attribute + TPRTC), `SchemaTP.DesignTime/` (`SchemaTP.Provider.fs`),
`ClientTP.Runtime/`, `ClientTP.DesignTime/` (`ClientTP.Provider.fs` — the enforcing provider),
`SchemaAsm/` (stamped-type carrier), `AttrCheck/Program.fs`, `Harness/Program.fs`. Per Q001–Q006
precedent, only this write-up is durable; the scratch projects are not committed. No wiring into
Myriad's own `Myriad.Sdk` targets — this idea is independent of Myriad by design.
