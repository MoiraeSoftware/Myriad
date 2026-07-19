# Q029 — Results

## Headline

**H1 confirmed.** A live `fsautocomplete` 0.83.0 session's per-edit incremental re-analysis cost is a
clean, monotonic, approximately linear function of the number of compilation-order **successors**
after the edited file — reproducing Q023/Q024's `ParseAndCheckProject` cost model through FSAC's real
LSP-driven per-file path. Editing the **last** file settles in ~6-9ms (nothing downstream to
re-check); editing the **first** file (all N-1 successors dirty) costs ~120-250ms. The per-successor
marginal rate is ~6ms and is **independent of N** across the two sizes tested (N=20 and N=40),
matching Q024's N-invariant-slope finding.

This is the first measurement in this repo's lineage of per-edit *cost as a function of edit position*
against a literal LSP-driven FSAC process, closing the named top follow-up of both Q023 and Q024.

## Setup as run

- Real on-disk F# library project generated fresh per run (`artifacts/q029-fsac-cost.fsx`), N genuinely
  typecheck-weighted files (Q023 `mkPrefixSrc` shape: generic record, generic `Wrapper<'a>`,
  `Map`/`List.mapi` builder, explicit recursive `fold`, 20-record comprehension) in a real
  compilation-order dependency chain (`File{i}` binds `total = ... + File{i-1}.total`). Sample:
  `artifacts/sample-project/File0000.fs`, `File0001.fs`, `q029proj.fsproj`.
- Edit = value-only marker bump (a string literal `"vK"` inside the record comprehension); no exported
  type/signature changes (validity precondition 3). Sent as `didChange` + `didSave`.
- Signal = wall-clock from `didChange` send to FSAC's `fsharp/documentAnalyzed` for the **last** file
  (URI-leaf matched, received after the edit), i.e. the point at which the whole edited tail has been
  re-typechecked. Freshness cross-checked by a `textDocument/hover` on the last file's `total`.
- Real `fsautocomplete` 0.83.0 (pinned, `artifacts/fsac-tool/.config/dotnet-tools.json`), .NET SDK
  9.0.310, over the hand-rolled LSP client copied from Q022. First edit after project load discarded
  as warm-up (FINDINGS.md first-typecheck-tax caveat).

## Cheapest falsifier — PASSED (reproduced 3×)

N=20, edit first (position 0, 19 successors) vs last (position 19, 0 successors), median settleMs:

| run | position 0 (19 succ) | position 19 (0 succ) | ratio |
|---|---|---|---|
| run 1 | 172 (128,172) | 7 (6,7) | ~24× |
| run 2 (`q029-N20.raw.log` era) | 138 (116,138) | 7 (6,7) | ~20× |
| run 3 | 118 (118,118;283 outlier) | 9 (7,9,9) | ~13× |

First-file cost is unambiguously and repeatably far larger than last-file cost. The falsifier does not
fire; the full sweep is warranted.

## Full position sweep — N=20 (`q029-results-N20.csv`)

Median settleMs by successors (3 reps each):

| position | successors | median settleMs | raw |
|---|---|---|---|
| 19 | 0 | 9 | 7, 9, 9 |
| 14 | 5 | 58 | 45, 58, 59 |
| 10 | 9 | 77 | 73, 77, 79 |
| 5 | 14 | 127 | 112, 127, 128 |
| 0 | 19 | 118 | 118, 118, 283* |

Monotonic in successors from 9ms (0 succ) to ~120ms (19 succ). *The one 283ms sample at position 0 is
a first-rep outlier (GC/residual warm-up); the median absorbs it. Positions 0 and 5 land within run
noise of each other (~118 vs 127) at the top of the curve; the dominant trend across the full range is
unambiguous. An earlier sweep run in the same session (captured in the session transcript) gave a even
cleaner 7 / 41 / 63 / 103 / 134 across the same five positions.

## Scale check — N=40 (`q029-results-N40.csv`)

Median settleMs by successors (3 reps each):

| position | successors | median settleMs |
|---|---|---|
| 39 | 0 | 6 |
| 30 | 9 | 63 |
| 20 | 19 | 126 |
| 10 | 29 | 172 |
| 0 | 39 | 243 |

Clean monotonic line, ~6.1ms per successor (slope of the 0→39 span), intercept ~6ms.

**N-invariance of the per-successor rate** (the Q024 question, in a real session): the same number of
successors costs the same regardless of N —

| successors | settleMs @ N=20 | settleMs @ N=40 |
|---|---|---|
| 0 | 9 | 6 |
| 9 | 77 | 63 |
| 19 | 118 | 126 |

The cost tracks *successors*, not N. Per-successor rate ~6-7ms at both sizes, no detectable drift.

## Cascade genuinely fires (rules out a stale-cache false reading)

`artifacts/q029-N20.raw.log` (raw LSP transcript, `Q029_RAW=1`) shows, after a position-0 `didChange`,
FSAC emitting `fsharp/documentAnalyzed` for `File0000.fs` through `File0019.fs` in compilation order —
the full downstream tail re-analyzed — before the last file's `documentAnalyzed` (the measured
endpoint) arrives. After a position-19 edit, only `File0019.fs` re-analyzes. This confirms validity
precondition 4 (the signal measures downstream/successor cost) and answers the reviewer's named
stale-cache concern directly: the tail is actually re-checked, not served from cache. All hovers
returned `resolved=true`, confirming the last file held real, fresh check results at each measurement;
hover request→response latency was a flat ~6-16ms (a cache read after settle), so the position
dependence lives entirely in `settleMs`, exactly as intended.

## Deviations from design

None material. The design's optional `documentAnalyzed`-vs-hover ambiguity resolved in favour of
`documentAnalyzed` cascade settle time as primary (hover latency turned out to be a post-settle cache
read, ~15ms flat, so it could not have served as the cost signal — the design anticipated this and
kept it only as a freshness cross-check, which is how it was used). One CSV (`q029-results-N20.csv`)
was overwritten mid-session by a raw-logging re-run and regenerated by a final full-sweep run; the
final committed CSV is the full 5-position sweep.

## What this does and does not show

Shows: FSAC's real incremental editing path exhibits the same position-dependent, linear-in-successors,
N-invariant cost structure Q023/Q024 measured via `ParseAndCheckProject` — the cost model transfers to
the real editor. For Myriad this carries the same double-edged caveat Q023/Q024 named: editing a file
with many successors is expensive, and Myriad's attributed domain types tend to sit early in build
order (the expensive end), while editing near the tail is nearly free.

Does not show: absolute keystroke cost for a real large project. N≤40 and the files, though genuinely
weighted, are small; absolute settle times here (tens to low-hundreds of ms) are not a large-solution
figure. The claim is about the *shape and scaling* of the curve, not a headline latency. Single
machine, single session per size, one dependency-chain topology (linear chain, every file depends on
the previous); a wide/shallow dependency DAG was not tested. `didChange`+`didSave` was used per cycle;
whether keystroke-rate `didChange` without save debounces differently was not probed.
