# Q018-collectible-alc-round2-mitigation / Movement 3 — Results

**Status:** RUNNING (Movement 3 complete; Movement 4 review pending).
**Date:** 2026-07-16.

## Summary

**Round A (foundational OS-level claim): PASS, but with a load-bearing caveat.** A collectible
`AssemblyLoadContext` genuinely releases the Windows file-lock — but only with `TieredCompilation=false`,
a non-default JIT setting outside Myriad's control in a real IDE host. **Round B (provider-lifecycle
fact-finding): the design's own prediction was wrong.** FCS constructs a *fresh* `SatelliteProvider`
instance per `checker.Compile()` call, not the same instance persisting across `Invalidate()` — this
changes which mitigation shape is even relevant. **Round C (the actual mitigation): FAIL, and not in
either pre-registered shape.** Even with an *ideal* externally-triggered eviction — using Round A's exact
proven technique, called *before* the write attempt, deliberately sidestepping the
`FileSystemWatcher`-triggering chicken-and-egg problem this design itself identified — the ALC still
fails to release after 30 forced-GC iterations. Something inside FCS's own checker/type-provider-hosting
layer retains a reference beyond this provider's own code, confirming the exact risk Q016's `02-results.md`
and review both named as a real possibility, not a hypothetical one.

## Round A: does a collectible ALC release the Windows file-lock at all?

Standalone test, no type provider involved (`artifacts/AlcLockProbe/`): load a copy of `Satellite.dll`
into a fresh collectible `AssemblyLoadContext` via `LoadFromAssemblyPath`, hold only a `WeakReference` to
the context, call `Unload()`, then loop `GC.Collect(2, GCCollectionMode.Forced, true, true)` +
`GC.WaitForPendingFinalizers()` (bounded, 30 iterations, 50ms sleep between) until the weak reference
reports dead, then attempt to open the same path for writing from within the same process.

- **First attempt, default JIT settings: deterministic FAIL, 3/3 runs.** The ALC never collected within
  30 iterations (~1.9 seconds of sustained forced-GC pressure) despite holding zero other live references.
- **With `<TieredCompilation>false</TieredCompilation>` added to the probe's own `.fsproj`: deterministic
  PASS, 3/3 runs, collecting on the *first* GC iteration (~55-61ms) every time.** Tier 0 quick-JIT's less
  precise GC liveness reporting was the confound — Tier 0 compiled code can report a broader live range
  for local variables than fully optimized code, keeping the ALC reachable past where a naive reading of
  the source says it should be. This is a real, reproducible, and non-obvious .NET behavior, not a fluke:
  toggling the one setting flipped the result cleanly and repeatably in both directions.
- **This caveat is bigger than a build-config footnote.** `TieredCompilation` defaults to *on* in every
  ordinary .NET process, including whatever hosts a real F# type-provider design-time build (Ionide/FSAC,
  `dotnet build`, MSBuild's own `dotnet.exe`). Myriad does not control that process's runtime settings.
  So Round A's PASS is real but conditional on a setting this whole approach cannot assume in production
  use, only in a harness the experimenter controls.

Full log: `artifacts/run-logs/roundA-run.txt`.

## Round B: does the same provider instance persist across Invalidate(), or is a fresh one constructed?

The design's own hypothesis 2 predicted the same instance persists (matching Q011's memoization
lesson about repeated `DefineStaticParameters` calls *within* one resolution). This was wrong. A
process-wide constructor counter (`DiagCounter.constructedCount`, `SatelliteProvider.fs`) shows:

```
[Q018-DIAG] SatelliteProvider CONSTRUCTED (instance #1)   -- step 1's compile
[Q018-DIAG] SatelliteProvider CONSTRUCTED (instance #2)   -- step 5's compile, a DIFFERENT consumer module
```

A fresh instance is constructed for each separate `checker.Compile()` call (matching each call being a
genuinely new, one-shot compile — the API Q012/Q013 already established as required for generative
providers). This has a real consequence: `sourceAssemblies`-staleness (the concern the original design
worried about, since it's a constructor-time-only argument) is **not actually the mechanism blocking
Round 2** — a fresh instance naturally re-reads whatever's on disk at construction time, no `Invalidate()`
needed for that specific purpose. The blocker is purely and only the *file-lock itself*, held by instance
#1's own loaded assemblies for as long as anything keeps them reachable.

Full log (Round 1, which carries this same diagnostic): `artifacts/run-logs/roundB-round1-run.txt`.

## Fixing Round 1's own regression before Round C could even be attempted

Switching `loadSatellite`/`loadSourceAssemblies` from `Assembly.LoadFrom` to a naive
`AssemblyLoadContext(isCollectible=true).LoadFromAssemblyPath` **broke Round 1's own previously-working
baseline**, twice, for two distinct reasons found by running, not anticipated by the design:

1. **A bare collectible ALC with no `Load` override does not delegate dependency resolution back to
   `AssemblyLoadContext.Default`** — it independently resolves a *second*, distinct copy of a shared
   dependency via ordinary probing. Confirmed via `Object.ReferenceEquals` diagnostics before concluding
   this was the cause (it wasn't, in the end — see below — but ruling it in/out required verifying
   directly, not assuming). Fixed with a `DelegatingCollectibleAlc` subclass whose `Load` override checks
   `AssemblyLoadContext.Default.Assemblies` by simple name first.
2. **The actual cause: `loadSourceAssemblies` was calling `LoadFromAssemblyPath` unconditionally for
   *every* entry in `config.ReferencedAssemblies`**, including `FSharp.Core.dll`'s own path — creating a
   second, isolated `FSharp.Core` instance distinct from the one already loaded by the host process and
   the one the SDK's own `sourceAssemblies` machinery and this provider's reflection actually resolve
   against. `Assembly.LoadFrom` (the original, pre-Q018 code) had transparently reused the
   already-loaded instance for these paths; a raw `AssemblyLoadContext.LoadFromAssemblyPath` call does
   not do this — it always creates a new instance regardless of what's already loaded elsewhere. This
   produced the exact same-printed-name-different-identity "Expected 'FSharpFunc...', but received type
   'FSharpFunc...'" error Q016's own attempt-2 history already documented for a different reason (there:
   the SDK's own `TargetTypeDefinition` wrapper; here: two independently-loaded copies of `FSharp.Core`).
   **Fixed** by checking `AssemblyLoadContext.All |> Seq.collect (fun alc -> alc.Assemblies)` for an
   already-loaded assembly of the same simple name before creating any new ALC — reserving the
   collectible-ALC treatment for the one assembly actually meant to be evictable (`Satellite.dll` itself),
   letting every framework/shared reference just reuse what's already loaded, exactly matching what
   `Assembly.LoadFrom` did for those paths all along.

With both fixes, Round 1 passes cleanly again (`compile: 1361ms exitCode=0 errors=0`, agreement
confirmed both fields) — confirming the collectible-ALC substitution, once correctly scoped, doesn't
regress the baseline mechanism Q016/Q017 established. This did require dropping `netstandard2.0` from
both `SatelliteTP.DesignTime` and `SatelliteTP.Runtime` (`System.Runtime.Loader.AssemblyLoadContext`
isn't available there) — FCS's design-time host still resolved the net8.0-only provider without issue,
itself a minor incidental confirmation, not something this quartet set out to test.

## Round C: the actual mitigation attempt — FAIL

Recognized before running it: the original design's `FileSystemWatcher`-triggered eviction (`onChange`
calling evict, then `Invalidate()`) has a structural chicken-and-egg problem this quartet's design didn't
name. The watcher only fires *after* the file changes — but releasing the lock is a *precondition* for
the file to change, not a consequence of it. Implemented anyway for parity with the original design, but
Round C's real test uses a second, more favorable path: a public `Q018Hooks.evictAndForceCollect`
function, called directly by the Harness (via `Assembly.LoadFrom` on the *same* design-time DLL path FCS
itself loaded — `Assembly.LoadFrom`'s own path-caching returns the identical loaded instance, confirmed
working since the call didn't throw and had an observable, correct effect on `AlcCache`'s own state)
*before* `SatelliteBuilder` is invoked, sidestepping the timing problem entirely.

```
-- step 2.5 (Q018): explicitly evict Satellite.dll's collectible ALC BEFORE the overwrite attempt --
[Q018-DIAG] evictAndForceCollect(...Satellite.dll): released=false after 30 iterations
  eviction hook result: Some false
-- step 3: recompile satellite to the SAME path (overwrite) --
  Error: A problem occurred writing the binary '...Satellite.dll': Could not open file for writing
  file-lock exception observed: true
```

Reproduced twice, deterministic (`artifacts/run-logs/roundC-run.txt`). Using the *exact* GC-forcing
technique Round A proved reliably collects an ALC with nothing else referencing it (same iteration count,
same `TieredCompilation=false` setting on the Harness project), and calling it from outside the provider
*before* the write is attempted — the eviction still fails. `AlcCache.evict` was confirmed to run and
correctly drop this provider's own cache entry (its only self-held reference), so whatever remains
reachable is not this code's own doing. The most plausible explanation, consistent with what Q016's own
`02-results.md` and review both predicted as a real risk: `FSharpChecker`'s own internal type-provider
hosting retains a reference to instance #1 (and everything `sourceAssemblies` registered for it,
including `Satellite.dll`) for the checker's entire lifetime, not just for the duration of the compile
that created it — plausibly for incremental/diagnostic bookkeeping this quartet did not investigate
further. Step 5 then reproduces Q016's original symptom exactly: the DLL was never actually overwritten
(still v1 on disk), so the freshly-constructed instance #2 correctly, faithfully resolves v1's stale
2-field member set — `Create` rejects 3 arguments, `GetEmail` doesn't exist.

## What was and wasn't proven

- **Proven:** collectible `AssemblyLoadContext` unloading is a real, working mechanism for releasing a
  Windows file-lock held by a dynamically-loaded assembly — Round A confirms this cleanly and
  deterministically, once `TieredCompilation=false` and correct GC-forcing technique are both in place.
- **Proven:** the mitigation attempt as designed (and even in its most favorable, timing-problem-free
  form) does not fix Round 2. This is not "needs a workaround" — the review should judge whether it's
  closer to REVISE (Round A's underlying mechanism is real, so this isn't a hard wall on collectible ALCs
  categorically) or closer to KILL for *this specific approach* (the most favorable test this quartet
  could construct — an ideal, pre-write, externally-triggered eviction using a proven-working
  technique — still failed, and neither of the two pre-registered outcomes for Round C's failure mode
  match exactly: this is not a "new scaffolding needed" REVISE, and it's not "the isolated mechanism
  itself fails" KILL either).
- **Not proven, scoped honestly:** *what specifically* inside FCS retains the reference. This quartet
  identified that something does (by process of elimination: this code's own cache was confirmed cleared)
  but did not trace FCS's own source to name the exact retaining structure. A named, concrete follow-up,
  not attempted here: does a *fresh* `FSharpChecker.Create()` instance for the post-regeneration check
  (rather than reusing the same long-lived checker across the whole sequence) avoid the problem entirely
  — a materially different question from "can an existing long-lived checker's provider be evicted,"
  worth testing separately since it bears on whether a real host restarting its checker periodically
  would sidestep this, or whether the reference genuinely lives somewhere else.
- **Not retested:** Q017's function-typed generalization result — Round 1's `Map` member (with its
  `FSharpFunc`-typed parameters) was exercised incidentally via the diagnostic printfns added to
  `createTypeUncached`, and continued to resolve correctly identity-wise throughout, but no dedicated
  Round 3-style test was rerun in this quartet.

## What a review should press on

1. **Is Round C's failure mode actually novel, or predictable from Round B's own finding?** Given a fresh
   provider instance is constructed per compile anyway, was there ever a scenario where THIS harness's
   Round 2 shape could have benefited from ALC eviction at all — or was the whole mitigation always going
   to collide with whatever FCS's checker itself retains, regardless of how cleanly this provider's own
   code released its side? Worth checking against FCS's own source (`FSharpChecker`/type-provider hosting
   internals) rather than treating this as fully unexplained.
2. **Does a fresh `FSharpChecker` per regeneration cycle sidestep the problem?** Named above as untested
   — cheap to check and would meaningfully narrow whether the blocker is checker-lifetime-scoped (fixable
   by a real host simply not reusing one checker forever) or something more fundamental.
3. **Is the `TieredCompilation=false` caveat from Round A actually disqualifying for real-world use, or
   is there a way to trigger Tier-1-equivalent JIT behavior for just the relevant methods** (e.g.
   `[<MethodImpl(MethodImplOptions.AggressiveOptimization)>]`, which forces immediate full-tier JIT
   without a process-wide setting) that a library author (not the hosting process) *could* control?
   Untested here — this quartet used the process-wide setting because it was the first thing that worked,
   not because narrower alternatives were ruled out.
4. **Given Round B overturned the design's own prediction, does the original `FileSystemWatcher`+
   `Invalidate()` mechanism from Q016 do anything useful at all in a `checker.Compile()`-per-invocation
   harness like this one** — or was it always inert here, with real value only in a genuinely long-lived,
   single-checker-instance incremental host (never tested in this repo)?
