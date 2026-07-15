# Q010-prefix-stratified-generation / Movement 1 — Hypothesis

**Status:** RUNNING. Pre-registered, execution not yet started.
**Date:** 2026-07-15
**Repo under test:** this repo (Moirae Software's F# code-generation tool), Thread 1 lineage
(Q001/Q002/Q005's in-process `FSharpChecker` harness), promoted from `BACKLOG.md` item 11.

**Question:** F# has a strict linear file order, so at the point the compiler reaches file N,
"everything before it" is a complete, already-typechecked prefix of the same project. Can a
generator exploit that ordering directly — computing file N's *content* on demand, mid-check, as a
function of the typed check results of files 1..N-1 of the *same in-progress compilation* — with no
fixpoint, no iterate-to-convergence loop, and no separate staging pass? And if so, does that give a
generator real cross-generator visibility (one generator's output informing a later generator's
decisions) that Myriad's current per-file-blind model cannot produce at all?

## The claim

Two separable claims, kept apart the same way Q001 split mechanism from capability:

1. **Mechanism claim:** `FSharpChecker`'s `DocumentSource.Custom` callback (already proven safe and
   correct for virtual, non-disk files in Q001/Q002/Q005) can be made *reentrant* — the callback that
   supplies file N's source text can itself call `checker.ParseAndCheckFileInProject` (or
   `ParseAndCheckProject`) on files 1..N-1 of the same project, from inside an in-flight check of the
   project that contains file N, without deadlocking, stack-overflowing, or returning stale/partial
   results. This is a materially different use of `DocumentSource.Custom` than any prior quartet
   tested: Q001-Q005 all supplied static or externally-mutated text; none called back into the same
   checker instance from inside the callback itself.
2. **Capability claim:** given that mechanism, a third generator can see and correctly typed-resolve
   members of a *second* generator's already-generated (but still virtual, not-yet-on-disk) output,
   and produce code that references those members correctly — the concrete cross-generator
   composition scenario current Myriad structurally cannot do (each generator invocation is blind to
   every other generator's output within the same build).

## Why this is newly viable, and why it supersedes BACKLOG item 10 as the preferred route

`BACKLOG.md` item 10 (multi-pass generation for cross-generator visibility) already named the same
capability gap and proposed closing it with an iterate-to-fixpoint loop: generate → typecheck-partial
→ regenerate against now-visible types → final build. This hypothesis claims something strictly
cheaper if the mechanism claim holds: F#'s own file order already gives a well-founded, paradox-free
notion of "the compilation so far" with no fixpoint required, because file order is a total order, not
a graph that needs iterating to a stable point. If Round 1 below confirms the reentrancy is safe, this
quartet supersedes item 10 as the preferred design; if it doesn't (the callback deadlocks or the
compiler doesn't tolerate reentrant checking of its own in-flight project), item 10's iterative,
staged form remains the fallback, and this quartet's review should say so plainly rather than quietly
declaring victory on a narrower result.

**Deviation from the literal `BACKLOG.md` item 11 write-up, stated honestly up front:** that entry
named two candidate mechanisms, "(a) `FSharp.Compiler.IO`'s `IFileSystem` shim, the mutable static
hook Fable itself uses" and "(b) `TransparentCompiler`'s snapshot API with a lazy per-file source
thunk," and flagged (b)'s exact API shape as unverified. Neither is what this quartet actually tests.
Both were speculative framing from a brainstorming pass, not source-grounded in this repo's own prior
quartets. The design below instead reuses `DocumentSource.Custom` exactly as Q001/Q002/Q005 already
validated it, just with a reentrant callback body — the same public, already-proven API surface, no
new or internal API needed. This is a *narrower, more conservative* mechanism than either backlog
candidate, chosen deliberately because it's buildable on infrastructure this repo has already shipped,
consistent with Q001's own lesson (don't assume compiler-hosting behavior without running it) applied
to skip guessing at an unverified internal API when a proven public one might do the same job.

## Novelty gate

Not covered by any closed quartet's verdict. Q001/Q002 proved typed cross-file resolution works
*within* a single check of a static project; this asks whether the project's own composition can be
computed incrementally, file-by-file, with later files' content depending on earlier files' typed
results *during* the same overall check. Q005 typechecks a generator's candidate output before
emission, but against a static, already-fully-specified virtual project — it never asks one virtual
file's content to be computed from another's typed results. Q006's wall (a type provider can never
see a type from the compilation in progress) does not bind this hypothesis at all: this is Thread 1
(Myriad-CLI-hosted `FSharpChecker`), not a type provider, and `FINDINGS.md`'s own cross-cutting notes
already flag that Thread 1 was never subject to that wall in the first place.

## Contradiction gate

Does not contradict any prior verdict. It depends on `DocumentSource.Custom` behaving correctly for
virtual files (validated, Q001-Q005) and on `TransparentCompiler`'s staleness-detection behavior
(validated, Q001) if `useTransparentCompiler = true` is used for the base harness. It inherits Q001's
open caveat directly: `TransparentCompiler` is still labeled experimental by both FCS and FSAC at the
pinned `43.9.101` line. This quartet adds a genuinely new stress case for that caveat — reentrant
checking from inside the compiler's own document-source callback is a materially different code path
than anything Q001-Q005 exercised, and the review must treat that as a real, not inherited-boilerplate,
risk.

## Validity preconditions

- FCS pinned to `43.9.101`, matching `Myriad/paket.lock`, same as every prior quartet.
- Must test against a scenario that requires *typed*, not syntactic, resolution to succeed — mirroring
  Q001 Round 1's alias-resolution check — so a null result can't be dismissed as "could've been done
  with syntax alone." Concretely: the third file's generated code must reference a member of the
  second file's generated module by its *resolved typed identity* (confirmed via
  `FSharpCheckFileResults`/symbol lookup on the spliced text), not by string-matching a name that
  happens to already be right.
- Must use a realistic cross-generator scenario, not a toy — the design below uses a simplified but
  structurally faithful stand-in for Myriad's real `Lenses` generator output (a record with a
  hand-written companion lens module in the shape `LensesGenerator` actually produces, per
  `Q006-myriad-as-type-provider`'s prior port of that same generator), not an arbitrary unrelated
  example.
- Every timing number must come from code that was actually run, a single sample per Q001's own
  precedent and caveat (this repo has never run repeated trials for any quartet; this one doesn't
  either, and the review should say so rather than imply otherwise).
- Must explicitly test and report the failure mode if reentrancy doesn't work cleanly (hang with a
  timeout, exception, wrong/incomplete typed results) — not just report success/failure as a binary.

## Cheapest falsifier

Before building the cross-generator composition scenario (Round 2, the actual capability claim):
does a `DocumentSource.Custom` callback that calls `checker.ParseAndCheckFileInProject` on an earlier
file of the *same* `FSharpProjectOptions`, invoked while a check of a *later* file in that same
project is already in flight, return at all — or does it hang, throw, or stack-overflow? This is
Round 1 below, deliberately the weakest and cheapest test of the mechanism claim, run first, exactly
as Q001 ran its own weakest capability test (Fields, unchanged output) before investing further.

## Pre-registered decision thresholds

- **Mechanism SHIP:** Round 1's reentrant callback returns correct, complete typed results for the
  prefix with no hang, no exception, no stack overflow, and the third file's content — derived from
  those typed results — typechecks with zero diagnostics when spliced back into the project.
- **Capability SHIP:** Round 2's cross-generator scenario produces a spliced file whose generated call
  into the second file's already-generated module resolves to the correct typed member (verified via
  symbol lookup, not visual inspection of the emitted text) and typechecks with zero diagnostics.
- **REVISE:** the live reentrant form (Round 1) hangs, throws, or returns incomplete/stale results,
  but a staged fallback — checking the prefix as a separate step *before* the `DocumentSource.Custom`
  callback is even invoked, then supplying precomputed text — works correctly. This is a real but
  narrower result: it means item 10's staged/iterative framing was right after all, and this quartet's
  "no fixpoint, done inline" claim doesn't hold, only "one precomputed staging step, done once per
  file" does.
- **KILL:** neither the live reentrant form nor the staged fallback produces correct typed results for
  the prefix, or the mechanism only works for the specific two-file toy case and demonstrably fails to
  scale to the three-file cross-generator scenario for a structural (not incidental) reason.
