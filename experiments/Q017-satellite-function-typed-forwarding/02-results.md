# Q017-satellite-function-typed-forwarding / Movement 3 — Results

**Status:** RUNNING (Movement 3 complete; Movement 4 review pending).
**Date:** 2026-07-16.

## Summary

**PASS, clean, first attempt — no new scaffolding, no incantation-finding required.** Unlike Q016's own
four-attempt history, adding a `ProvidedMethod("Map", ...)` wrapping Myriad's real, function-typed-
parameter `map` function worked with exactly the same construction pattern Q016's attempt-4 fix already
established for `Create`/`GetName`/`GetAge`/`GetEmail` — reflect the real target `MethodInfo`, derive
every `ProvidedParameter`/return type directly from its own `GetParameters()`/`ReturnType`, and
`invokeCode = fun args -> Expr.Call(mapMi, args)`. No hand-built `FSharpFunc` conversion thunk, no
`ConvertSourceExprToTarget`/`ConvertSourceTypeToTarget`, no change to `sourceAssemblies` registration.

## What was built

Extended Q016's own surviving scratch build in place (`SampleLib`, the real Myriad CLI run, and
`Satellite.dll` all reused unchanged — `Generated.fs` already contained `map`, Q016 just never wrapped
it):

1. **`SatelliteProvider.fs`** — added `let mapMi = moduleType.GetMethod("map", flags)` alongside the
   existing lookups, and, guarded the same way `emailMi` is (`if mapMi <> null then ...`), added:
   ```fsharp
   let mapParams =
       [ for p in mapMi.GetParameters() -> ProvidedParameter(p.Name, p.ParameterType) ]
   let mapM =
       ProvidedMethod("Map", mapParams, mapMi.ReturnType, isStatic = true,
           invokeCode = fun args -> Expr.Call(mapMi, args))
   t.AddMember mapM
   ```
   `mapMi.GetParameters()` reflects three real parameters off the compiled satellite:
   `FSharpFunc<string,string>`, `FSharpFunc<int,int>`, `SampleNs.Person`. Built the provider
   (`dotnet build -c Release`) — 0 warnings, 0 errors, first try.

2. **`Harness/Program.fs`** — added `round3Generalization ()`: compiles a consumer instantiating the
   same `MyriadSatellite<...>` provided type Q016 already used, calling
   `.Create("Ada", 42)` then `.Map((fun (s: string) -> s.ToUpperInvariant()), (fun (i: int) -> i + 1),
   person)`, and an independent `directSatelliteMapCall` helper that loads `Satellite.dll` directly via
   reflection (outside the provider entirely, `Assembly.Load(bytes)` same as Round 1's own
   `directSatelliteCall`), builds the *same* two lambda values via
   `FSharpFunc<_,_>.FromConverter`, and invokes the real `map` `MethodInfo` directly. Built — 0 errors
   (2 pre-existing `NU1608` FSharp.Core version warnings, identical to Q016's own build, unrelated to
   this change).

## Round 3 (Q017's cheapest falsifier) — PASS

```
consumer text:
module Consumer3Map
type P = SatelliteTP.Provided.MyriadSatellite<".../Satellite.dll", "SampleFields.Person", "SampleNs.Person">
let person = P.Create("Ada", 42)
let mapped = P.Map((fun (s: string) -> s.ToUpperInvariant()), (fun (i: int) -> i + 1), person)
let readMappedName : string = P.GetName(mapped)
let readMappedAge : int = P.GetAge(mapped)

--- Round 3 (Q017) compile ---
  compile: 1515ms exitCode=0 errors=0 outExists=true
  readback via provider-mediated Map: name="ADA" age=43
  direct reflection call to real map (outside provider): name="ADA" age=43
  agreement: name true, age true
ROUND 3 (Q017) verdict: PASS
```
(Full log: `artifacts/run-logs/round3-run.txt`; a second run reproduced identically at 1661ms, both
runs' values byte-identical.)

- **Zero compile diagnostics.** A real F# lambda literal (`fun (s: string) -> s.ToUpperInvariant()`) at
  the consumer call site resolved directly to the `FSharpFunc<string,string>`-typed provided parameter
  with no conversion needed, no different from how it would resolve against an ordinary (non-provided)
  method signature of the same shape. This settles this quartet's first-listed "deviation to report
  honestly if it occurs" — it did not occur.
- **The falsifier's stronger design (per Q016 review objection 2) is satisfied, not just the cheaper
  compile-diagnostics half.** The lambdas chosen (uppercase the name, increment the age) are
  deliberately **not** identity echoes — a reimplementation that got the wrapping wrong (e.g. swapped
  the two `FSharpFunc` arguments, or applied `mapname` to `age` instead of `name`) would produce a
  *different*, checkably-wrong result, not an accidentally-correct one. The provider-mediated result
  (`"ADA"`, `43`) agreeing with an independent direct-reflection call built from scratch, outside the
  provider, using the same lambda *values* (not the same code path) is real evidence this is Myriad's
  actual compiled `map` executing, not a reimplementation — the discriminating check Q016's own review
  said was missing for `name`/`age`'s trivial-echo getters.
- **No regression to Round 1.** Reran Round 1 unchanged after adding `Map` — still PASS, identical
  values to Q016's own original run (`artifacts/run-logs/round1-regression-check.txt`). The `if mapMi <>
  null` guard, mirroring `emailMi`'s own guard, means consumers that don't reference `Map` are
  unaffected.

## What was and wasn't proven

- **Proven:** the `sourceAssemblies` + reflected-`MethodInfo`-typed-`ProvidedParameter`s + `Expr.Call`
  kernel Q016 found is not specific to primitive-and-single-record signatures. It handles at least one
  function-typed-parameter, multi-non-primitive-argument shape (two `FSharpFunc<_,_>` args plus a
  record) with zero additional scaffolding. This directly answers Q016 review objection 1 and follow-up
  1 for the one shape tested.
- **Not proven, scoped honestly:** this is one additional shape, not a proof the kernel is unconditionally
  general. Untested: a function-typed parameter whose input/output types are themselves records or other
  provided types (not just primitives inside the `FSharpFunc`); a member with a `FSharpFunc<_,_>` *return*
  type rather than parameter type; curried functions of arity > 2; a real `Lenses`-shaped getter/setter
  pair (this quartet used `Fields`' own `map`, the cheapest available function-typed shape, not a new
  Myriad generator surface). Also unchanged from Q016: Round 2's file-lock/regeneration problem is not
  retouched here — this quartet is scoped purely to the generalization question, and that problem stands
  exactly as Q016 left it.
- **Single-sample timings**, consistent with this whole repo's own cross-cutting caveat — reran twice
  (1515ms, 1661ms) for basic sanity, not a distribution.

## What a review should press on

1. **Is "no scaffolding needed" actually surprising, or was it predictable from Q016's own attempt-4
   reasoning?** Q016's fix already established that deriving parameter/return types directly from a real
   `MethodInfo`'s own reflection (rather than reconstructing an equivalent type by hand) sidesteps the
   whole source/target type-identity problem that broke attempts 2–3. Arguably `FSharpFunc<string,string>`
   obtained the same way is just another CLR type from the SDK's perspective — was this quartet's PASS a
   real generalization test, or a re-derivation of a conclusion Q016's own design already implied? Worth
   checking directly against `ProvidedTypes.fs`'s own type-conversion code paths for `FSharpFunc`-shaped
   types specifically, not just re-running the spike.
2. **Does the lambda actually cross into the emitted IL as a real closure, or could F#'s own compiler be
   doing something that makes this easier than a "real" cross-provider function value crossing** (e.g. if
   the consumer's lambda is compiled to a class implementing `FSharpFunc` before the provider ever sees
   it, is that meaningfully different from passing any other object-typed argument)? This bears on how
   much weight "function-typed parameters work" should carry for e.g. a future `Lenses`-shaped provider
   where the exposed function might need to flow *out* of the provider, not just in.
3. **The distinguishing-lambda falsifier design** (objection 2's fix) — is uppercase+increment actually
   sufficient to rule out a reimplementation, or could a reviewer construct a plausible reimplementation
   that also passes this specific check? Worth trying at least one more adversarial input pair to see if
   the agreement is robust or coincidental.
4. Same standing caveat every quartet in this lineage carries: never tested through a real IDE host, only
   `FSharpChecker`-as-library.

## Reproduction

`experiments/Q017-satellite-function-typed-forwarding/artifacts/` is self-contained (does not reference
`Q016-satellite-dll-type-provider/artifacts/`): `SampleLib/` (with `Satellite.dll` already built,
containing `map`), `SatelliteBuilder/`, `SatelliteTP.Runtime/`, `SatelliteTP.DesignTime/` (with the
`Map` member added), `Harness/` (with `round3Generalization` added), vendored `ProvidedTypes.fs`/`.fsi`.
Build order: `SatelliteTP.Runtime` (pulls in `SatelliteTP.DesignTime`), then `Harness`; run
`dotnet Harness.dll round3` (or `round1`/`round2`/`all` — `round2` unchanged from Q016, not rerun in this
quartet). Absolute paths in `Harness/Program.fs` (`SPIKE`, `MYRIAD`) point at this session's own scratch
and repo locations and would need updating for a different machine/session, same as Q016's own artifacts.
