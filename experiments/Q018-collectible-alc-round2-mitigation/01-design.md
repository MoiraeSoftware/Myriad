# Q018-collectible-alc-round2-mitigation / Movement 2 — Design

**Status:** RUNNING.

## What gets built

Three rounds, staged cheapest-and-most-foundational first, each gating whether the next is worth
attempting — mirroring this quartet's own three-way claim split.

1. **Round A (cheapest falsifier) — isolated file-lock test, no type provider at all.** A small,
   standalone F# console program (`AlcLockProbe/`):
   - `LoadFromAssemblyPath` a copy of `Satellite.dll` into a fresh
     `AssemblyLoadContext(isCollectible = true)`.
   - Hold a `WeakReference<AssemblyLoadContext>` to it; drop every other reference (the `Assembly`
     object, any `Type`/`MethodInfo` obtained from it) out of scope.
   - Call `alc.Unload()`, then loop `GC.Collect(); GC.WaitForPendingFinalizers()` up to 10 times (or
     until the `WeakReference` reports dead), recording how many iterations/how much wall-clock it took.
   - Immediately after, attempt `File.Open(path, FileMode.Open, FileAccess.Write, FileShare.None)` on the
     same path from within the same process and report success/failure — this simulates what
     `SatelliteBuilder`'s own overwrite needs, without requiring a second process for this isolated
     check.
   - Report: did unload+GC actually null the weak reference (real collection, not just an unload call);
     did the subsequent write-open succeed.

2. **Round B — provider-instance lifecycle check, reusing Q016/Q017's harness.** Add a `static mutable`
   counter incremented in `SatelliteProvider`'s own constructor (a one-line, clearly-marked diagnostic
   addition, not a permanent change to the provider's real logic). Run a sequence matching Q016's own
   Round 2 shape (initial check, real Myriad CLI + satellite regeneration, `Invalidate()`, re-check) and
   report the counter's value at each stage. If it stays at 1 throughout, the same provider instance
   persists across `Invalidate()` — confirming piece 2's predicted structural ceiling empirically rather
   than assuming it. If it increments, a fresh instance is constructed each time, and `sourceAssemblies`
   *could* in principle be made fresh on each construction (a materially different, better situation than
   predicted) — reported honestly either way, this is a fact-finding round, not one with a pre-registered
   pass/fail.

3. **Round C — full mitigation attempt, shaped by whatever Round B finds.** Modify
   `SatelliteTP.DesignTime/SatelliteProvider.fs`'s `loadSatellite` to load via a collectible
   `AssemblyLoadContext` instead of `Assembly.LoadFrom`, with the `ensureWatcher`/`onChange` callback
   extended to: clear `successCache`, unload the current ALC and drive it to real collection (Round A's
   confirmed technique), *then* call `this.Invalidate()` (previously it called `Invalidate()`
   immediately). Two sub-variants depending on Round B's result:
   - **If the same provider instance persists (predicted):** `sourceAssemblies` stays exactly as Q016
     left it (loaded once, via `Assembly.LoadFrom`, at construction — unchanged, since there is no
     exposed hook to alter it later). Only `loadSatellite`'s own per-check load moves to a fresh
     collectible ALC each generation. Run Q016's Round 2 sequence exactly (edit `Person.fs` to add
     `email`, rerun Myriad's real CLI, rebuild `Satellite.dll` to the same path) and observe: does the
     DLL overwrite now succeed with no lock exception (piece 1, applied for real); and does the
     post-`Invalidate()` re-check then succeed, fail with a NEW error (source/target mismatch, the
     REVISE-shaped outcome), or fail some other way. Report the exact diagnostic text if it fails.
   - **If a fresh instance is constructed each time:** attempt the fuller mitigation — both
     `sourceAssemblies`'s own loading and `loadSatellite`'s loading routed through per-generation
     collectible ALCs — and run the same Round 2 sequence.

## What's measured

- Round A: pass/fail on (a) `WeakReference` reporting dead within the bounded GC-poll loop, with
  iteration count and elapsed time; (b) the subsequent same-process write-open succeeding.
- Round B: the constructor-counter's value immediately after Round 2's initial check, immediately after
  regeneration + `Invalidate()`, and immediately after the post-regeneration re-check — a fact, not a
  pass/fail.
- Round C: pass/fail on (a) no file-lock exception on the `SatelliteBuilder` overwrite (this time via the
  real separate-process path, matching Q016's own Round 2 exactly, not the same-process simulation Round
  A used); (b) zero diagnostics on the post-`Invalidate()` re-check of a consumer referencing the new
  `GetEmail` member; if (b) fails, the exact diagnostic text and whether it matches the source/target
  mismatch shape Q016's own attempts 2-3 already produced once before (a mechanistic sanity check on
  whether this is really the same wall recurring).
- Wall-clock for Round C's full sequence, single-sample, reported for scale only.

## How it's run / reproduced

Round A is fully standalone (`Q018-collectible-alc-round2-mitigation/artifacts/AlcLockProbe/`), no
dependency on Q016/Q017's scratch build. Rounds B and C extend the surviving
`$CLAUDE_JOB_DIR/tmp/q016-satellite-dll-spike/` scratch build in place (same convention Q017 used) —
`SatelliteTP.DesignTime/SatelliteProvider.fs`'s `loadSatellite`/`ensureWatcher` are the only production
code touched; `Harness/Program.fs` gets one new entry point per round. Final source and run logs copied
to this quartet's own self-contained `artifacts/` on completion. FCS pinned to `43.9.101` throughout.

## Deviations expected, to report honestly if they occur

- Whether `AssemblyLoadContext.Unload()` plus a bounded GC-poll loop reliably collects within a
  practical number of iterations, or requires an unbounded/much longer wait — if the latter, report the
  actual number needed rather than silently raising the bound until it passes.
- Whether Round B finds the same instance persists (the design's own prediction) or a fresh one is
  constructed — this directly determines which Round C variant is even attemptable, and the prediction
  could be wrong.
- Whether Round C's failure mode (if it fails) is genuinely the source/target mismatch shape predicted,
  or something else entirely (e.g. the `FileSystemWatcher`'s async timing interacting badly with the
  ALC-unload sequencing, a risk this design doesn't specifically anticipate) — reported as found, not
  forced into the predicted shape.
