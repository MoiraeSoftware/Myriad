# Q023-scale-cost-reentrant-callback / Movement 1 — Hypothesis

**Question:** Under `useTransparentCompiler = true` with a reentrant `DocumentSource.Custom` callback
(the exact mechanism `Q010` and `Q021` proved correct), does `ParseAndCheckProject` cost on an
*unchanged* remainder stay roughly flat as project size N grows (explanation **(c)**: the callback is
consulted for text but a content-hash cache skips redundant typecheck work), or does it grow
roughly in proportion to a cold from-scratch check at that N (explanation **(b)**: every call
re-typechecks every file unconditionally, and the callback firing is a symptom of that, not a
cheap cache-probe)?

This is `Q021`'s own review, Objection 1 and Follow-up 1, run for real. It is also `BACKLOG.md`
item 18's single named precondition for deciding whether an FSAC-hosted version of this mechanism
is keystroke-cheap or pays a full-project-recheck cost on every edit — the standing top-priority
item in `experiments/BACKLOG.md` and `experiments/FINDINGS.md` as of this session.

## Novelty

Not covered by any existing Myriad generator (`src/Myriad.Plugins/`) — this is host-compiler
behavior, not generator logic. Not covered by any closed quartet:

- `Q021` proved the reentrant callback produces *correct* results across a multi-cycle edit loop on
  a two-file toy project, but its own adversarial review struck the one claim that would have
  answered this question (Round 5's "unconditional full re-check" conclusion) as unsupported —
  Round 5 showed only that the callback *fires* on a no-op repeat call, which is equally consistent
  with (b) or (c). The review's own words: "A real discriminator (hundreds of files, or
  first-call-vs-repeat latency on the reentrant inner check) was not run." This quartet is that
  discriminator.
- `BACKLOG.md` item 4 flagged a related, still-open gap from `Q001` Round 3: the padding files used
  to test `BackgroundCompiler` vs `TransparentCompiler` scaling were near-free to typecheck
  (`let x = 5`-level), so no prior quartet has actually stress-tested typecheck cost at scale with
  files that do genuine work (opens, generics, real inference). This design deliberately closes
  that gap as a side effect, using files with real generic types, `Map`/`List` operations, and
  recursion — not as its own hypothesis, but because a scale test built on trivial files would
  reproduce Q001 Round 3's own named weakness and prove nothing.

## Contradiction check

Does not contradict any standing verdict. `Q021`'s SHIP (scoped) verdict — correctness holds across
a persistent-checker edit loop — is untouched; this quartet only resolves the *cost mechanism*
question its review explicitly left open, it does not reopen or retest correctness. Consistent with
`Q001`'s finding that `TransparentCompiler` avoids `BackgroundCompiler`'s explicit-
`InvalidateConfiguration` staleness trap — if (c) holds here, that finding extends cleanly to actual
caching efficiency, not just staleness-avoidance; if (b) holds, it doesn't contradict Q001 (Q001
never measured unchanged-file cost, only staleness correctness) but it does narrow how much
practical comfort `TransparentCompiler`'s good staleness behavior actually buys.

## Validity preconditions

- `FSharp.Compiler.Service` pinned to `43.9.101`, matching `Myriad/paket.lock` and every prior
  quartet in this lineage.
- `useTransparentCompiler = true` — matches Q010's and Q021's own SHIP configuration.
  `BackgroundCompiler` is out of scope here exactly as it was for Q021 (its own Round 4 stretch,
  never run); mixing in a second compiler mode would confound the one variable this quartet tests.
- The reentrant `DocumentSource.Custom` callback must be genuinely exercised via
  `ParseAndCheckProject` (Q010/Q021's "forces callback" mode), not bypassed via the
  explicit-source-text form of `ParseAndCheckFileInProject` — Q021 Round 4 already proved that path
  silently skips the callback entirely, which would make any timing measured through it meaningless
  for this question.
- Padding files must carry genuine typecheck weight (generic records, `Map`/`List`-returning
  functions, a recursive fold) — a repeat of Q001 Round 3's near-free files would make a flat result
  ambiguous (flat because caching works, or flat because there was nothing to cache in the first
  place).
- Files are deliberately **not** chained into a cross-file type-dependency graph (no file's exported
  type depends on another prefix file's type). This is a considered design choice, stated up front
  so it isn't mistaken for an oversight: a real dependency chain would make edits to an early file
  *correctly* force rechecking of every downstream file, which is proper compiler behavior, not
  evidence of (b) — and would confound the one thing this quartet isolates. The chained-dependency
  case (closer to how Myriad's own cross-generator visibility, `Q002`/`Q010`, actually behaves) is a
  materially different, harder question, named explicitly as future work, not silently substituted
  for by this simpler design.
- Single-process, in-memory timings via `Stopwatch`, same convention as every prior quartet in this
  lineage. Single-sample vs. multi-sample must be disclosed per this repo's standing convention;
  given the noise Q021's review already flagged at toy scale, this design takes 3 repeat
  measurements per condition, not 1.

## Cheapest falsifier

Run the harness first at the smallest planned N (10 files with genuine typecheck weight — already
heavier than anything Q001/Q010/Q021 tested at that count) and compare a cold `ParseAndCheckProject`
call against a same-checker no-op repeat call. If the repeat call's cost is already indistinguishable
from the cold call's cost even at N=10, that is an early, cheap signal toward (b) and grounds to
still build out to larger N (the question is about *scaling behavior*, not one data point) but with
lowered expectation of a clean (c) result. If the repeat call is already much cheaper than cold at
N=10, proceed to N=50/150/300 to confirm the gap holds or widens with scale, which is the actual
claim under test — a flat gap at one N is not itself suficient evidence of (c) without seeing the
trend across N.

**Kill criterion:** the harness hangs, throws, or the reentrant callback stops firing at any N — same
threshold Q010/Q021 used.

**SHIP criterion:** across N = {10, 50, 150, 300}, no-op-repeat and edit-one-unrelated-file costs stay
within a small, non-growing band while cold cost grows with N — i.e., the repeat/cold cost *ratio*
shrinks as N grows, not just "repeat is cheaper than cold at every N" (which could still be
consistent with (b) if both grow at the same rate with a constant offset).

**REVISE criterion:** repeat/edit-one cost tracks cold cost's growth curve (ratio stays roughly
constant or grows) — i.e. (b) holds, and item 18's cost concern is confirmed real, not just
undetermined.
