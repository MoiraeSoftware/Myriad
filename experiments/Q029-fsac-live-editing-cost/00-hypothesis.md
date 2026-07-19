# Q029 — Does FSAC's real incremental editing path reproduce Q023/Q024's position-dependent cost curve?

## Question

`Q023-scale-cost-reentrant-callback` and `Q024-position-sweep-across-scale` established, by directly
calling `checker.ParseAndCheckProject()` in a hand-rolled harness under a reentrant
`DocumentSource.Custom` callback, that the cost of editing one file and re-checking is a **linear
function of the number of compilation-order successors** after the edited file: editing the last file
is nearly free (cache hit), editing the first is nearly as expensive as a cold check. Both quartets'
own reviews stated, in identical words, that this is a **cost model, explicitly not a measured FSAC
session**, and both named the same never-run top follow-up:

> Does FSAC's actual, real incremental per-file editing path — a live `fsautocomplete` process
> responding to `textDocument/didChange`/`didSave` the way a real editor drives it — reproduce this
> same position-dependent cost curve, or does FSAC's real incremental machinery behave differently
> from the direct whole-project `ParseAndCheckProject` calls Q023/Q024 used?

**Hypothesis (H1):** A live FSAC session's per-edit re-analysis cost is a monotonically increasing
(approximately linear) function of the number of compilation-order successors after the edited file —
i.e. FSAC reproduces the Q023/Q024 curve, editing the last file is cheap and editing the first is
expensive.

**Pre-registered alternatives, all valued equally:**
- **H0 (NULL / flat):** FSAC's cost is roughly constant regardless of edit position — its per-file
  incremental path does *not* re-check the whole compilation-order tail on a value-only edit, behaving
  differently from `ParseAndCheckProject`. This would mean Q023/Q024's cost model does not transfer to
  a real editor session and must not be cited as characterizing FSAC keystroke cost.
- **H2 (inconclusive):** the LSP signal chosen does not cleanly discriminate edit positions (e.g. FSAC
  serves stale cached results, debounces edits, or the fixed per-request overhead swamps the
  position-dependent term at the small N a real FSAC session can sustain). A genuine "the signal can't
  tell" is a valid, reportable outcome.

## Novelty

Not covered by any existing generator in `src/Myriad.Plugins/` (this is a compiler-hosting-cost
question, not a code-generation question). Not covered by any prior closed quartet:
- **Q021, Q023, Q024, Q026, Q027** all measured cost/behavior of the reentrant callback through
  `FSharpChecker`-as-a-library (`ParseAndCheckProject`/`ParseAndCheckFileInProject`), **never** a real
  LSP-driven FSAC process. Q024's review names this exact gap as its top follow-up.
- **Q022** is the only quartet that drove a real `fsautocomplete` process, but it measured a *binary*
  outcome (does a newly-generated member appear at all, yes/no) at project cold-load and reload — it
  never measured *cost as a function of edit position*, and it used Myriad codegen, which this quartet
  deliberately does not (see contradiction check).

This is the first quartet in the lineage to measure per-edit *cost* against a literal LSP-driven FSAC
process.

## Contradiction check

- Does not contradict Q023/Q024: it tests whether their cost *model* transfers to the real editor path
  those quartets' reviews explicitly said they had not measured. Confirming H1 corroborates them in a
  new setting; confirming H0 does not falsify their `ParseAndCheckProject` numbers, it bounds their
  scope (as their own reviews already flagged).
- Does not use the reentrant `DocumentSource.Custom` mechanism at all, and therefore does **not** walk
  into Q010's footgun (`ParseAndCheckFileInProject` with explicit source text bypasses
  `DocumentSource.Custom`). That footgun is about driving Myriad's virtual-file generation through
  FCS; this quartet measures FSAC's *ordinary* incremental typecheck path on real on-disk files with
  no virtual/generated files and no Myriad involvement. The footgun's mechanism is simply not present
  here. This is a deliberate scoping decision: isolate FSAC's own incremental cost from any
  generation confound (Q022 already showed generation-on-edit is a separate, unsolved question).
- Consistent with `FINDINGS.md`'s cross-cutting caution that the first typecheck in a process pays a
  one-time JIT/FCS-init tax: the design discards a warm-up edit cycle and never reports the first
  edit after project load as a data point.

## Validity preconditions

1. Must drive a **real** `fsautocomplete` process over real LSP stdio — not `FSharpChecker` as a
   library. (Reuses Q022's proven hand-rolled LSP client plumbing and its pinned FSAC 0.83.0 tool.)
2. Files must be **genuinely typecheck-weighted** (generic records, `Map`/`List` pipelines,
   recursion), reusing Q023's `mkPrefixSrc` shape — not `let x = 5` padding (BACKLOG.md item 4's named
   prior mistake). Files must form a **real compilation-order dependency chain** (each references the
   previous file's output) so that editing an early file genuinely forces re-checking of its
   successors, matching Myriad's real domain-type-early usage shape.
3. The edit must be **value-only** — it must not change the edited file's exported type/signature
   shape — matching Q023/Q024's own edit shape, so that any measured downstream re-check reflects
   FCS's compilation-order invalidation, not a real signature change that would force re-checking
   anyway.
4. The measured signal must reflect the **downstream** re-check cost (the successors of the edit),
   because that is the quantity Q023/Q024's curve is about. Measuring only the edited file's own
   re-analysis would measure predecessor cost, the wrong axis.
5. Timings are wall-clock over LSP and are single-machine, single-session samples; per the lineage's
   standing caveat, trends across positions matter, not absolute milliseconds. Report medians over
   repeats and disclose the first-edit warm-up discard.

## Cheapest falsifier

Before any position sweep: build **one** real N≈20 chained project, load it in a real FSAC session,
open all files, and run just **two** edit cycles — edit the **first** file, then edit the **last**
file — measuring the time from the edit to FSAC's re-analysis of the last file settling. If those two
positions produce **indistinguishable** cost, H1 is already in serious doubt (either FSAC doesn't
re-check the tail on a value-only edit, i.e. H0, or the signal doesn't discriminate, i.e. H2), and a
full sweep is not worth building. If first-file cost is clearly larger than last-file cost, proceed to
the full position sweep. Run this first, even though two points is the weakest possible test of a
linear-curve claim.
