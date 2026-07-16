# Q017-satellite-function-typed-forwarding / Movement 2 — Design

**Status:** RUNNING.

## What gets built

This quartet is deliberately a minimal delta on Q016's own surviving scratch build
(`$CLAUDE_JOB_DIR/tmp/q016-satellite-dll-spike/`, confirmed intact at session start) — reusing
`SampleLib`, the real Myriad CLI run, and `Satellite.dll` unchanged (all three already produced by
Q016 and already contain `map`, since `Generated.fs` was never edited to remove it). Two pieces added:

1. **`SatelliteProvider.fs` — one new `ProvidedMethod`.** Reflect `moduleType.GetMethod("map",
   BindingFlags.Public ||| BindingFlags.Static)` alongside the existing `create`/`name`/`age`/`email`
   lookups. Build:
   ```fsharp
   let mapParams =
       [ for p in mapMi.GetParameters() -> ProvidedParameter(p.Name, p.ParameterType) ]
   let mapM =
       ProvidedMethod("Map", mapParams, mapMi.ReturnType, isStatic = true,
           invokeCode = fun args -> Expr.Call(mapMi, args))
   t.AddMember mapM
   ```
   Identical construction pattern to `createM`/`getNameM`/`getAgeM` — the only difference is that two of
   `mapMi.GetParameters()`'s entries are `FSharpFunc<string,string>`/`FSharpFunc<int,int>` rather than
   `string`/`int`/`Person`. No other change to the provider (same `sourceAssemblies` registration, same
   `successCache`/`watchers` memoization, same `assemblyReplacementMap`). Guarded the same way `email`
   is (`if mapMi <> null then ...`) so Q016's own Round 1/2 consumers, which don't reference `Map`,
   still compile unchanged — this quartet must not regress Q016's own passing result.

2. **`Harness/Program.fs` — one new round.** `round3Generalization ()`:
   - Compile a consumer instantiating the same `MyriadSatellite<...>` provided type, calling
     `.Create("Ada", 42)` then
     `.Map((fun (s: string) -> s.ToUpperInvariant()), (fun (i: int) -> i + 1), person)`.
   - Independent check: reflect `map` directly off `Satellite.dll` (outside the provider, same
     `directSatelliteCall`-style helper Q016 already has) and invoke it with the *same* two lambda
     values (constructed as plain `FSharpFunc` values via `FSharpFunc<_,_>.FromConverter` or an F#
     function value boxed appropriately) and the same `Person`, then compare fields.
   - Record whether compilation itself produces any `FSharpFunc`-related type-mismatch diagnostic —
     the falsifier's first, cheaper half — before checking runtime agreement.

## What's measured

- Pass/fail on zero compile diagnostics for the `Map` call.
- Pass/fail on field-by-field agreement between the provider-mediated result and the direct-reflection
  result (name uppercased, age incremented — both distinguishing from a same-shape identity echo, per
  Q016 review objection 2's own suggested stronger falsifier).
- Wall-clock for this round's compile, single-sample, reported for scale only (not a pre-registered
  threshold in this quartet).
- If it fails: the exact diagnostic text or exception, and at which stage (provider construction,
  consumer compile, or runtime invocation) it occurs — this quartet's KILL/REVISE distinction depends on
  exactly where the failure lands.

## How it's run / reproduced

Extends the surviving `$CLAUDE_JOB_DIR/tmp/q016-satellite-dll-spike/` scratch build in place (its
`SampleLib`/`Satellite.dll`/`SatelliteTP.Runtime` are reused unchanged — only `SatelliteTP.DesignTime`
and `Harness` are rebuilt). Final source and run logs copied to this quartet's own `artifacts/` on
completion, same convention as every prior quartet — `artifacts/` here is a self-contained copy, not a
symlink back to Q016's own folder, so this quartet stands alone per the repo's own artifact-durability
lesson (Q006/Q008/Q09's gap). FCS pinned to `43.9.101`, matching every quartet in this repo.

## Deviations expected, to report honestly if they occur

- Whether an `FSharpFunc<_,_>`-typed `ProvidedParameter` accepts a real lambda literal at the F# call
  site with no extra conversion — F# should compile `(fun s -> s.ToUpperInvariant())` directly to an
  `FSharpFunc` value matching the declared parameter type, but this has never been exercised through a
  *provided* method signature in this repo, only through ordinary compiled method signatures. If FCS's
  type-provider consumption path needs the argument converted or wrapped differently than an ordinary
  method call would, that is exactly the KILL/REVISE-determining fact this quartet exists to find.
- Whether `Expr.Call(mapMi, args)`'s `args` — the SDK's own `Var` list for the provided method's
  parameters — type-check as `FSharpFunc`-typed `Expr` nodes without any conversion, mirroring how
  Q016's attempt-4 fix worked for `string`/`int`/`Person`-typed args. If not, the fallback matching
  Q016's own attempt-2/3 history would be to try `ConvertSourceExprToTarget`, which Q016 already found
  breaks IL emission for primitive args — if it also fails here, that is reported as REVISE/KILL
  evidence, not silently worked around with an untested API.
