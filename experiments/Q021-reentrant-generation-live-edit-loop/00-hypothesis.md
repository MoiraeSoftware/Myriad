# Q021-reentrant-generation-live-edit-loop / Movement 1 — Hypothesis

**Status:** RUNNING. Pre-registered, execution not yet started.
**Date:** 2026-07-16.
**Repo under test:** this repo, Thread 1 lineage (Q001/Q002/Q005/Q010's in-process `FSharpChecker`
harness), promoted from `BACKLOG.md` item 18 as its own named cheapest-falsifier step — a precursor to
item 18's own larger, unscoped claim (an FSAC fork hosting Myriad as a live-editing sidecar, modeled
loosely on rust-analyzer's `proc-macro-srv`).

**Question:** Q010 proved a `DocumentSource.Custom` callback can be made reentrant — the callback
supplying a generated file's text can itself call back into the same checker to typecheck an earlier
file of the same in-progress project, and the result typechecks correctly. But Q010's own harness
built a fresh `FSharpChecker` once, ran exactly one outer check, and exited — a single request against
a cold instance. An LSP-shaped editing session is a different load pattern entirely: one long-lived
checker instance serving a *sequence* of checks across a session, with the watched prefix file's
content changing between checks (the editor equivalent of keystrokes), and no guarantee that a
mid-session re-check of the generated file re-observes the prefix's *current* content, rather than a
result cached from an earlier point in the session. Does Q010's reentrant mechanism keep producing
correct, freshly-recomputed typed results for the generated file across repeated edit-then-recheck
cycles on **one persistent checker instance**, or does it silently serve stale results from an earlier
cycle — the exact class of bug Q001 already found for `BackgroundCompiler` generally (silently serving
stale results across a broken cross-file dependency unless `InvalidateConfiguration` is called
explicitly), now asked specifically of the *reentrant* callback path Q010 introduced, which no prior
quartet has driven this way?

## The claim

One claim, not split into mechanism/capability the way Q001 and Q010 were — this quartet doesn't
introduce a new mechanism, it stress-tests an already-proven one under a load pattern no prior quartet
used:

**Liveness claim:** on a single `FSharpChecker` instance created once and reused for N sequential
edit-then-recheck cycles (edit the prefix file's in-memory text, signal the change, recheck the
generated file through the same reentrant `DocumentSource.Custom` callback), each recheck's generated
text reflects that cycle's *current* prefix content — never a value from an earlier cycle — with zero
diagnostics on the freshly-generated file each time, under `useTransparentCompiler = true` (Q001's
validated default, and the configuration Q010's own SHIP threshold required).

## Why this is the right next step, not a re-tread of Q010

`BACKLOG.md` item 18 (added this session, 2026-07-16) proposed testing whether Q010's mechanism holds
when "the outer driving loop is an LSP `didChange`-shaped edit sequence rather than Q010's own one-shot
harness" as the cheapest falsifier before considering any FSAC fork. This quartet is exactly that
falsifier, scoped to a buildable spike: it reuses Q010's Round 1 harness and callback shape verbatim
where possible, changing only the *load pattern* — one checker, many checks, edits interleaved — rather
than introducing a new compiler-hosting API. If this fails, item 18's entire premise (that Q010's
mechanism is the retired-risk part of an FSAC-hosted approach) is wrong, and item 18 should be
rewritten to say the live-editing case is *not* settled by Q010, before anyone considers forking FSAC.
If it succeeds, item 18's remaining risk really is narrowed to FSAC integration plumbing, as claimed.

## Novelty gate

Not covered by any closed quartet's verdict. Q010 tested reentrancy exactly once per checker instance;
no quartet in this repo has driven a `DocumentSource.Custom` callback (reentrant or not) across
multiple sequential checks of the same living checker instance with the source text changing between
them. Q001's own finding about `BackgroundCompiler` serving stale cross-file results is the closest
prior result, but it was never combined with a *reentrant* callback — this quartet asks whether that
known staleness risk and the reentrancy mechanism interact in some way neither prior quartet observed
in isolation.

## Contradiction gate

Does not contradict any prior verdict. It depends on `DocumentSource.Custom` behaving correctly for
virtual files (validated, Q001-Q010) and on Q010's reentrancy result itself (validated, one-shot form).
It directly extends Q001's own open finding about `TransparentCompiler` avoiding the staleness trap
that `BackgroundCompiler` requires explicit `InvalidateConfiguration` calls to avoid — this quartet is
the first to test whether that finding still holds once the callback supplying the checked file's text
is itself reentrant, a materially different code path through the checker's caching layer than Q001's
own simple (non-reentrant) multi-file staleness test used.

## Validity preconditions

- FCS pinned to `43.9.101`, matching `Myriad/paket.lock` (confirmed unchanged this session).
- Must use a real sequence of edits (at least 3 distinct prefix values across the run), not a single
  before/after pair — a single edit cannot distinguish "correctly re-observes each new value" from
  "happened to skip the cache exactly once."
- Must include the explicit invalidation call the checker instance needs between edits
  (`checker.InvalidateConfiguration` against the project options, or whatever the design's own
  reproduction run finds actually necessary — report the real API used, don't assume it matches this
  hypothesis's guess) and must report what happens with that call *omitted* for at least one cycle, so
  a pass can't be quietly attributed to "the harness always called the right invalidation API and we
  never found out whether it was load-bearing."
- Must reuse Q010's own verified alias-stripping technique (typed-vs-syntactic resolved form) as the
  correctness signal for each cycle's generated text, the same rigor Q001/Q010 used, not a weaker
  string-equality check against the edited value.
- Every timing number is a single sample per this repo's own standing convention; the review must not
  imply otherwise.
- Must explicitly report the failure mode if staleness occurs (which cycle, what stale value was
  served, whether it recovered on a later cycle or stayed wedged) rather than a binary pass/fail.

## Cheapest falsifier

Two sequential edit-recheck cycles on one persistent checker instance, changing only the prefix file's
literal field type between them (mirroring Q010 Round 1's `Id = int` alias, changed to a second type on
cycle 2), with the invalidation call included: does cycle 2's generated text reflect cycle 2's prefix,
or cycle 1's stale value? This is weaker than the full N-cycle design below and run first, exactly as
Q010's own Round 1 was run before its Round 2.

## Pre-registered decision thresholds

- **SHIP:** every cycle in the full run (N >= 3) produces zero-diagnostic generated text whose
  typed-resolved content matches that cycle's current prefix value, with the invalidation call present;
  the omitted-invalidation control cycle produces a *visibly different* (stale or erroring) result,
  confirming the invalidation call is actually load-bearing rather than a no-op the harness didn't need.
- **REVISE:** the mechanism works correctly only when a fresh `FSharpChecker` is constructed per cycle
  (i.e., Q010's own one-shot pattern, just looped) but a single persistent instance serves stale results
  across at least one cycle even with invalidation called correctly — this would mean item 18's premise
  needs real correction: an FSAC-hosted version would need to recreate its checker per edit (a real,
  costly constraint, not a free extension of Q010) rather than reuse one long-lived instance the way an
  actual language server would want to.
- **KILL:** the reentrant callback itself breaks (hangs, throws, or silently stops firing) once reused
  across more than one check on the same instance, independent of the staleness question — i.e., Q010's
  mechanism doesn't survive being asked to run twice on one checker at all, a materially worse and more
  basic failure than staleness.
