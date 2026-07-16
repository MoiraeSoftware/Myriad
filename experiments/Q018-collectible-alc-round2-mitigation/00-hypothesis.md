# Q018-collectible-alc-round2-mitigation / Movement 1 — Hypothesis

**Status:** RUNNING.
**Date:** 2026-07-16.
**Repo under test:** this repo, Thread 2 lineage. Direct follow-up to `Q016-satellite-dll-type-
provider`'s own review, follow-up 2 ("test the collectible-`AssemblyLoadContext` escape for Round 2...
this is make-or-break for the capability claim and directly tests objection 4's tension"), and to
`02-results.md`'s own named, untested mitigation.

**Question:** Q016's Round 2 fails because `Assembly.LoadFrom` — required for the source/target identity
match Round 1's `sourceAssemblies` mechanism depends on — holds a Windows file-lock on `Satellite.dll`
for the process's lifetime, blocking the separate `SatelliteBuilder` process from overwriting it on
regeneration. Both Q016's `02-results.md` and its review named a collectible `AssemblyLoadContext`
(load the satellite into an isolated, unloadable ALC instead of the default `LoadFrom` context; unload
it, and confirm real GC collection, before the rebuild) as the one remaining untested mitigation — but
both also flagged, as a real risk not just a formality, that a live `ProvidedTypesContext` inside the
SDK might hold references into the ALC that block collection, "just relocating the tension" rather than
resolving it. Does a collectible ALC actually (a) release the Windows file-lock in practice, and (b) if
so, does the provider still resolve the *new* satellite version's members correctly afterward, or does
detaching `loadSatellite` from the default `LoadFrom` context break the very identity match Round 1's
mechanism needs?

## The claim, split into three separable pieces (this quartet is structured around finding out which
one breaks, not assuming they all succeed together)

1. **Foundational OS-level claim, testable independent of any type provider:** loading an assembly via
   `AssemblyLoadContext(isCollectible = true).LoadFromAssemblyPath(path)`, then calling `Unload()` on
   that context and driving it to actual collection (`GC.Collect()`/`GC.WaitForPendingFinalizers()`
   polled against a `WeakReference` to the context, not just calling `Unload()` and assuming it worked),
   genuinely releases whatever OS-level handle `LoadFromAssemblyPath` held on the file, allowing a
   separate process to open it for writing immediately afterward. This is a general .NET/Windows fact,
   not specific to this repo's provider — worth confirming in isolation before spending anything on the
   harder SDK-specific claim.
2. **Provider-lifecycle claim, needed to know which mitigation shape is even attemptable:** does FCS
   construct a *new* `SatelliteProvider` instance each time a static-parameter instantiation is
   re-resolved after `Invalidate()`, or does it keep re-using the *same* instance (matching Q011's own
   memoization lesson, which was about the same instance's `DefineStaticParameters` firing more than
   once)? This matters because `sourceAssemblies` is a `TypeProviderForNamespaces` **base-class
   constructor argument** — fixed the moment `inherit` runs, with no exposed API to change it afterward.
   If the same provider instance survives across `Invalidate()` calls, `sourceAssemblies` can never be
   refreshed to point at a v2 satellite no matter what loading discipline `loadSatellite` itself uses —
   a structural ceiling this quartet must identify empirically, not assume either way.
3. **The actual mitigation claim:** given whatever piece 2 reveals, does a collectible-ALC-based
   `loadSatellite` (replacing the current `Assembly.LoadFrom`) let Q016's Round 2 pass — DLL overwrite
   succeeds with no file-lock exception, *and* a live-checking consumer sees the new member set with zero
   diagnostics after `Invalidate()` — without a new, different failure mode taking the old one's place?

**Deliberately not attempted here:** no out-of-process design-time host restart, no memory-mapped-copy-
then-load workaround, no attempt to reflect into the SDK's own internal `ProvidedTypesContext` tables to
force-swap a registered source assembly (a real idea, but a materially different, riskier mechanism than
"just use a collectible ALC" — out of scope, worth its own quartet if this one's Round C reveals
`sourceAssemblies` really is the wall). Q016's Round 1 (single-generation forwarding) and Q017's
function-typed generalization are not retested from scratch; this quartet assumes both as given and
targets Round 2 specifically.

## Why this is the right next spike

Both Q016's `02-results.md` and its independent review named this as the single most consequential
untested question left standing: Round 2's failure is what breaks the cross-project satellite-DLL type
provider's *only* differentiator over an ordinary `<ProjectReference>` (live re-exposure), so resolving
it — or finding out precisely why it can't be resolved this way — has more leverage on this whole
sub-line's real-world usefulness than any other queued follow-up.

## Novelty gate

Not covered by any closed quartet's verdict:
- No quartet in this repo has used a collectible `AssemblyLoadContext` for anything. Q011 identified the
  `Assembly.LoadFrom` file-lock risk and used `Assembly.Load(bytes)` to avoid it entirely (accepting a
  weaker identity guarantee, since Q011's providers never needed source/target identity matching against
  an externally-produced satellite the way Q016's does). Q016 needed `LoadFrom`'s identity-matching
  property specifically and so inherited the lock Q011 sidestepped; this quartet is the first to try to
  have both properties (identity match, and release-ability) at once.
- No quartet has investigated FCS's own provider-instance lifecycle across `Invalidate()` calls (piece 2
  above) — Q011 found `DefineStaticParameters` fires more than once per logical check, which is a
  different question (repeated calls *within* one resolution) from whether the whole provider object
  itself persists *across* separate `Invalidate()`-triggered re-resolutions.

## Contradiction gate

Does not contradict any prior verdict. Depends on, and is consistent with:
- Q016's own finding that `Assembly.LoadFrom` (not `Load(bytes)`) is required for the identity match
  Round 1 needs — this quartet does not question that, it asks whether an ALC-based load can provide the
  same identity-matching property while also being releasable.
- Q011's `Assembly.Load(bytes)` vs `LoadFrom` lesson, and Q016 review objection 4's finding that
  `Load(bytes)` specifically cannot satisfy Round 1's identity requirement — the reason this quartet
  reaches for a collectible ALC rather than re-trying `Load(bytes)`, which is already a settled dead end.
- Q016 `02-results.md`'s own explicit prediction that a live `ProvidedTypesContext` "could plausibly
  block collection" — this quartet treats that as a real candidate outcome to test for, not something to
  assume away or assume will happen.

## Validity preconditions

Same as Q016/Q017's: FCS pinned to `43.9.101`; vendored `ProvidedTypes.fs`/`.fsi` copied forward
unchanged; provider genuinely generative (`isErased = false`); Windows-relevant (this session runs on
Windows 11, where the file-lock problem is real and where `AssemblyLoadContext` collection behavior is
worth confirming directly rather than assumed from documentation, which primarily discusses Linux/
cross-platform ALC use).

## Cheapest falsifier

Before touching the type provider at all: in a small, standalone console program, `LoadFromAssemblyPath`
`Satellite.dll` into a fresh collectible `AssemblyLoadContext`, hold a `WeakReference` to the context (no
other live references — no `Type`/`MethodInfo` held past this point), call `Unload()`, then loop
`GC.Collect(); GC.WaitForPendingFinalizers()` (bounded, e.g. 10 iterations / a few seconds) until the
`WeakReference` reports dead, then immediately attempt to open the same path with
`FileAccess.Write`/`FileShare.None` from within the same process (simulating what the separate
`SatelliteBuilder` process needs to do). If this fails to release the lock, piece 1 (the foundational OS-
level claim) is false and the whole mitigation direction is dead before any type-provider-specific
complexity is added.

## Pre-registered decision thresholds

- **SHIP:** all three claims hold — the isolated ALC-unload test releases the file lock; Round 2's DLL
  overwrite then succeeds with the provider's own `loadSatellite` using the ALC-based approach; and a
  live-checking consumer referencing the new (v2) satellite's members resolves with zero diagnostics
  after `Invalidate()` fires. This would mean the collectible-ALC mitigation genuinely closes Q016's
  Round 2 gap, upgrading item 15's real-world usefulness materially.
- **REVISE:** the isolated ALC-unload test passes (piece 1 confirmed — the file-lock problem itself is
  solvable in principle), but the full provider integration (piece 3) fails with a *different* error than
  Q016's original file-lock exception — most likely a source/target assembly identity mismatch traceable
  to `sourceAssemblies` being fixed at construction time (piece 2's predicted structural ceiling) — such
  that the mitigation trades one failure mode for another rather than eliminating it. This is the outcome
  Q016's own `02-results.md` predicted as plausible; confirming it precisely, with the exact error and
  mechanism, is still a genuine, useful result even though it doesn't unblock Round 2.
- **NULL:** the isolated test passes and the full integration also happens to work, but for reasons that
  don't actually generalize (e.g. only because this specific harness never triggers the code path that
  would expose the identity mismatch, not because the mismatch is genuinely resolved) — judged unlikely
  going in, but recorded as a possible outcome if the review finds the "success" is narrower than it
  appears.
- **KILL:** the isolated ALC-unload test itself fails — a collectible ALC does not release the file
  handle within a reasonable bound even with no other live references, meaning the entire mitigation
  direction is a dead end at the most basic level, independent of any type-provider complexity. This
  would close off the collectible-ALC approach specifically (not necessarily every possible fix — an
  out-of-process design-time host restart would remain untested) and mean Round 2's file-lock problem, as
  currently understood, has no known in-process fix.
