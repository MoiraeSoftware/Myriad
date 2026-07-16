# Q018-collectible-alc-round2-mitigation / Movement 4 — Adversarial review

## Reproduction status — read this first, it bounds everything below

I was able to execute this quartet independently. The tooling outage that blocked Q016's reviewer was
not in effect: every `dotnet build`/`dotnet run` I issued ran. So the claims below are from observations
I made, not inherited from saved logs. I went past reproduction and ran two new experiments the executor
named as untested (a fresh-`FSharpChecker` variant, and an `AggressiveOptimization` variant) plus one the
executor did not name (isolating FCS's own referenced-assembly reader from the provider's ALC).

What I verified by executing:

- **The saved logs correspond to exactly the checked-in code.** `diff` reports the scratch build's
  `SatelliteProvider.fs`, `Harness/Program.fs`, and `Harness.fsproj` at
  `…/jobs/f0b85ddf/tmp/q016-satellite-dll-spike/` are byte-identical to the copies under
  `Q018-.../artifacts/`. The logs and the checked-in source are the same artifact.
- **Round A reproduces exactly, both directions of the caveat.** With `<TieredCompilation>false</TieredCompilation>`
  the isolated probe collected the ALC on the **first** GC iteration (57/61/55 ms), 3/3 PASS. Forcing
  the default JIT back on at runtime (`DOTNET_TieredCompilation=1`, which overrides the fsproj without a
  rebuild) flipped it to `collected=false after 30 iterations` (~1930 ms), 3/3 FAIL. The split is real,
  clean, and reproducible in both directions.
- **Round C reproduces exactly.** `dotnet Harness.dll roundC`: instance #1 constructed,
  `evictAndForceCollect(...Satellite.dll): released=false after 30 iterations`, file-lock exception on
  the subprocess overwrite, instance #2 constructed, `Create` rejects 3 args, `GetEmail` absent. The
  executor's FAIL is real.
- **Round B reproduces.** The process-wide constructor counter increments (instance #1 at the pre-regen
  compile, instance #2 at the post-regen compile) — a fresh `SatelliteProvider` per `checker.Compile()`,
  overturning the design's own "same instance persists" prediction.

The extra-experiment code (`roundD`, `roundE`, and an `AggressiveOptimization` probe) lives only in a
scratchpad copy of the Harness; I did not modify the checked-in `artifacts/` or the scratch build's own
`Harness`. `roundE` overwrote the scratch `Satellite.dll` to v2 as part of its test; I rebuilt it back to
v1 afterward, so the scratch build is left in the same clean v1 state Round C leaves it.

## Strongest objections

1. **The executor's single most important named follow-up — "does a fresh `FSharpChecker` release the
   lock?" — resolves cleanly NO, and I ran it. This is the biggest thing 02-results left open, and its
   answer removes the executor's most hopeful escape route.** 02-results (press-on #2) framed the fresh-
   checker question as "would tell us whether the retained reference is checker-instance-scoped (fixable
   by any real host that doesn't hold one checker forever) or something deeper," and left it untested. I
   built `roundD`: a **local** `checker1` compiles the pre-regen consumer (constructing instance #1 and
   loading `Satellite.dll` into its collectible ALC), then before the overwrite I call
   `checker1.InvalidateAll()`, `ClearLanguageServiceRootCachesAndCollectAndFinalizeAllTransients()`, set
   `checker1 <- Unchecked.defaultof<_>` (dropping the only non-global reference to instance #1), force a
   full GC, evict the design-time cache, and only then attempt the overwrite — with a brand-new
   `checker2` for the post-regen check. Result: `released=false after 30 iterations`, file-lock
   exception, instance #2 sees stale v1. **A fresh checker does not help.** Two observations pin down why.
   First, the constructor counter still increments across the two *separate* checkers (instance #1 under
   `checker1`, instance #2 under `checker2`) — a process-wide static counter advancing across two
   distinct `FSharpChecker` objects proves FCS's type-provider hosting shares state process-globally, not
   per-checker. Second, a load-context census I added after the eviction shows `Satellite` **still
   present in its collectible ALC** after evict + checker-drop + forced GC. So the retained reference is
   not checker-instance-scoped: a real host that periodically recreates its checker would *not* sidestep
   this. The reference lives somewhere process-global in FCS, exactly as 02-results suspected but could
   not confirm — and the "just don't reuse one checker" repair 02-results floated is now empirically
   dead within `FSharpChecker`-as-library. The only in-process escape the hypothesis itself did not
   already scope out (a genuinely separate design-time *process*) remains the sole untested candidate,
   and it is a much bigger lift than "use a collectible ALC."

2. **The lock really is the provider's ALC, not FCS's own `-r:Satellite.dll` reader — I had to check
   this, because if it were the reader the whole mitigation would have been aimed at the wrong target,
   and it is not.** A live alternative hypothesis 02-results never considered: FCS opens every `-r`
   referenced assembly to read its IL metadata and caches those readers process-globally; if *that*
   held `Satellite.dll`, no amount of ALC discipline in the provider could ever release it, and the
   collectible-ALC approach would be misdirected from the start. I built `roundE`: compile a consumer
   that references `Satellite.dll` directly and *uses* a type from it (`SampleNs.Person`,
   `SampleFields.Person.name`) with **no type provider anywhere** (no `-r:SatelliteTP.Runtime`, no
   `MyriadSatellite<...>`). Result: compile succeeds, `Satellite` is in **no** managed load context (FCS
   reads IL directly, it does not `Assembly.Load` referenced assemblies), same-process write-open
   **succeeds**, and the subprocess overwrite **succeeds** (exit=0, no lock exception). So FCS's own
   referenced-assembly reader does not hold a persistent lock. This confirms the executor's attribution
   is correct — the lock in Round C/D genuinely comes from the provider's `LoadFromAssemblyPath` into the
   collectible ALC — and it rules out the one competing explanation that would have made the mitigation
   pointless for a different reason. This *strengthens* the executor's diagnosis rather than undercutting
   it, but it needed to be shown, not assumed.

3. **The `TieredCompilation=false` caveat from Round A has no demonstrated library-controllable escape —
   I tested the executor's own named candidate (`AggressiveOptimization`) and it fails, which makes the
   caveat more disqualifying than 02-results leaves it, not less.** 02-results (press-on #3) hoped
   `[<MethodImpl(MethodImplOptions.AggressiveOptimization)>]` on the relevant methods might force
   Tier-1-equivalent JIT for just those methods — a knob a *library* author controls, unlike the
   process-wide `TieredCompilation` setting a host controls — and left it untested. I built a probe with
   `AggressiveOptimization` (combined with `NoInlining`) on both the load and the GC-poll methods, and
   ran it with the default tiered JIT (no `TieredCompilation` override). Result: `collected=false after
   30 iterations`, 3/3 FAIL — identical to the no-mitigation case, and unchanged when I additionally
   forced `DOTNET_TieredCompilation=1`. So forcing just the provider's own methods to full-tier JIT does
   **not** restore precise-enough GC liveness to let the ALC collect; only disabling tiering
   process-wide does. A library author cannot buy the Round A behavior with an attribute; it requires the
   *hosting* process (Ionide/FSAC/`dotnet build`/MSBuild's own `dotnet.exe`) to run with a non-default
   JIT setting Myriad does not control. The executor was right to flag the caveat as "bigger than a
   build-config footnote"; the narrower alternative it hoped might exist does not, at least not in the
   natural form.

4. **Round C's failure is not predictable-as-a-contradiction from Round B, because Round B and Round C
   measure orthogonal axes — 02-results is right to treat it as a separate finding, but the reason is
   worth stating precisely.** Round B established that a *fresh* provider instance is *constructed* per
   compile, so `sourceAssemblies` staleness (a construction-time concern) is not the blocker: instance
   #2 naturally re-reads whatever is on disk. But "a fresh instance is constructed" says nothing about
   whether the *old* instances (and their ALC-loaded assemblies) are ever *released*. The blocker is
   old-instance retention, an axis Round B never measured. `roundD` makes this concrete: instance #2 is
   freshly constructed (counter → 2) and would faithfully resolve v2 *if the DLL were overwritten*, but
   the overwrite never happens because instance #1's `Satellite` Assembly is retained process-globally.
   The two findings are fully consistent; there was never a scenario in this harness's shape where ALC
   eviction of the provider's own cache alone could have unblocked Round 2, because the retention is
   FCS's, not the provider's. So Round C's collision was structurally inevitable given how FCS hosts
   providers, independent of how cleanly this provider released its own side — which the executor's own
   evict-confirmed-cleared observation already hinted at and my census confirms (the design-time assembly
   exists as a single copy in the Default ALC, so the Harness's `LoadFrom`-driven eviction did touch the
   same `AlcCache` FCS's provider populated; the eviction was not a no-op on the wrong object).

## Smaller points, checked

- **The verdict does not fit either failure-shaped threshold literally, and that must be stated plainly
  rather than smoothed over.** The KILL threshold fires only if "the isolated ALC-unload test itself
  fails … does not release the file handle within a reasonable bound even with no other live
  references." It did not: Round A releases the lock on iteration 1 when there are genuinely no live
  references and liveness is precise. The REVISE threshold anticipates piece 3 failing "with a
  *different* error than Q016's original file-lock exception — most likely a source/target assembly
  identity mismatch." That is not what happened either: piece 3 failed with the **same** file-lock
  exception recurring, because the reference is process-global, not because a new identity error
  appeared. The pre-registration simply did not anticipate this outcome shape (Round A works, Round C
  fails with the *old* error for a *newly-identified* reason). See the verdict for how I resolve it.
- **The two Round-1 regressions and their fixes are real and correctly scoped.** The
  `DelegatingCollectibleAlc.Load` override (delegate already-in-Default assemblies back to Default) and
  `findAlreadyLoaded` (reserve collectible-ALC treatment for the one evictable assembly, reuse existing
  instances for framework/shared references) are both present in the checked-in source and are the right
  narrowing. The Round 1 log confirms the intended behavior line by line: FSharp.Core and the framework
  facades are "reusing already-loaded assembly (not collectible)"; only `Satellite.dll`,
  `SatelliteTP.Runtime.dll`, and `System.ValueTuple.dll` go into fresh collectible ALCs. Round 1 passes
  clean (`exitCode=0 errors=0`, both agreement fields true).
- **The chicken-and-egg framing is correct and the executor was right to route around it.** A
  `FileSystemWatcher.Changed`-triggered evict fires only *after* the file changes, but releasing the
  lock is a *precondition* for the change, not a consequence. The executor's pre-write, externally-
  triggered `Q018Hooks.evictAndForceCollect` is the most favorable possible in-process test, and it
  still fails — which is the strongest form the negative result could take.
- **KILL/NULL/SHIP are all cleanly out on substance too.** SHIP needed all three conjuncts; Round C
  failed. NULL needed the integration to "happen to work"; it did not work at all. KILL needed the
  isolated mechanism to fail; it works.
- **Standing caveat, unchanged since Q006:** everything here is `FSharpChecker`-as-library, never a real
  IDE host (FSAC/Ionide). My `roundD`/`roundE` are the same library-host shape. Whether a real host's
  own assembly-loading and checker-recycling reproduce this exact retention is untested, but there is no
  reason to expect it to be *more* releasable in a real host, and objection 1 shows even aggressive
  in-process checker recycling does not help.

## Verdict

**REVISE**, but a materially weaker REVISE than Q016's, and the pre-registration did not cleanly
anticipate this outcome shape — I am stating that as a fact, not smoothing it.

Reading the four pre-registered thresholds literally: KILL is **out** because its sole trigger ("the
isolated ALC-unload test itself fails … even with no other live references") did not fire — Round A
releases the lock on the first GC iteration when references are genuinely gone and JIT liveness is
precise. Calling this KILL would assert "collectible ALCs cannot release file locks," which Round A
proves false. SHIP and NULL are out on substance (Round C failed outright). That leaves REVISE, whose
registered meaning is "piece 1 confirmed, piece 3 fails such that the mitigation trades one failure mode
for another rather than eliminating it." Piece 1 is confirmed and piece 3 fails — the only divergence
from the registered REVISE text is the *shape* of the piece-3 failure: the same file-lock exception
recurs, rather than the predicted new source/target identity mismatch. REVISE is therefore the
least-wrong label, chosen because KILL's own narrow definition rules itself out, not because the outcome
matches REVISE's optimism.

Four things travel with this REVISE and matter more than the label:

- **The specific mitigation is exhausted in-process, not merely "unproven."** The collectible-ALC
  approach was the one remaining candidate Q016's review named for Round 2. Q018 shows it fails under the
  most favorable possible conditions (pre-write, externally-triggered, proven technique), and my `roundD`
  shows the executor's fallback hope (a fresh checker) fails too. There is now **no demonstrated
  in-process fix** for Round 2's lock. Do not cite Q018 as "collectible ALCs might work with more
  tuning."

- **The blocker is FCS-process-global retention of the loaded satellite assembly, and I bounded it
  empirically.** It is not the provider's own cache (evicted, confirmed via single-copy census), not
  FCS's `-r` reader (roundE: no lock without a provider), and not the checker instance (roundD: fresh
  checker doesn't release it). It survives checker replacement, cache invalidation, and forced GC. I did
  not trace the exact FCS structure that holds it — that stays a follow-up — but the elimination is
  tight enough to place it squarely in FCS's type-provider hosting, process-wide.

- **Round A's PASS is conditional on a host-level JIT setting a library cannot control (objection 3).**
  `TieredCompilation=false` is required and `AggressiveOptimization` does not substitute for it. Even in
  the counterfactual where the retention problem were solved, the release would still depend on the
  hosting process running with a non-default JIT config Myriad cannot assume in Ionide/FSAC/MSBuild.

- **This tightens Q016 objection 3's conclusion further.** The cross-project satellite-DLL TP's sole
  differentiator over an ordinary `<ProjectReference>` is live re-exposure via `Invalidate()`, and Q018
  now shows that differentiator has no working in-process implementation on Windows. The honest guidance
  the Q016 review reached ("use a `<ProjectReference>` for the cross-project case; the TP adds value only
  if the ALC-unload path works") should now drop its conditional: the ALC-unload path does not work, and
  no in-process path is known.

## Follow-ups, prioritized

1. **Out-of-process design-time host, or drop the live-re-exposure goal.** The hypothesis explicitly
   scoped out a separate design-time process, and after Q018 it is the only in-principle escape left for
   Round 2's lock (a distinct process's ALC/reader is torn down on process exit, sidestepping the
   process-global retention entirely). This is a large lift and may not earn its keep against a plain
   `<ProjectReference>`; the honest alternative is to stop pursuing live re-exposure for the cross-project
   case and record that in BACKLOG item 15. Either way, do not queue another in-process ALC variant.
2. **Name the exact FCS structure that retains the provider/satellite assembly.** I bounded it to
   process-global FCS type-provider hosting by elimination; naming the concrete retaining reference
   (TcImports caching, the design-time-assembly load in the Default ALC, or an ImportedAssembly table)
   from FCS's own source would confirm whether *any* in-process release is even theoretically possible,
   before spending on follow-up 1. Cheap relative to its leverage.
3. **If live re-exposure is pursued at all, treat the `TieredCompilation`/JIT-liveness requirement as a
   hard host-environment constraint, not a tunable.** `AggressiveOptimization` is ruled out (objection 3);
   any solution depends on the host process's runtime config, which a library ships no control over.
   Document this as a precondition, not a footnote.
4. **Real-IDE host test (standing caveat since Q006).** Whether FSAC/Ionide reproduces both the Round 1
   pass and this retention, rather than `FSharpChecker`-as-library. Unchanged from Q016/Q017, and lower
   priority than 1–3 because objection 1 already shows aggressive in-process recycling does not help.
