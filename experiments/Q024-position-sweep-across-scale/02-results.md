# Q024-position-sweep-across-scale / Movement 3 — Results

**Status:** EXECUTED. Reused `Q023-scale-cost-reentrant-callback/artifacts/q023-spike/` verbatim, same
built `.exe`, no rebuild, no source changes. `FSharp.Compiler.Service 43.9.101`, `Release`, `net9.0`.
20 runs (5 positions × 4 N values), each a fresh process, matching the design. All 20 raw logs saved
under this quartet's `artifacts/logs/`.

Zero diagnostics on every one of the 20 runs. Callback invocation count was N+1 on every call, every
run, no exception — consistent with every prior quartet in this lineage. `Observer.fs`'s generated
text was stable across every edit, at every position, at every N (the edit never touches the type
`Observer` watches, `Prefix0000.Record`, regardless of which file is being edited).

## Raw data

| N | editIdx | successors (N−1−idx) | cold (ms) | repeat median (ms) | editOne median (ms) |
|---|---|---|---|---|---|
| 10 | 0 | 9 | 1081 | 10 | 59 |
| 10 | 2 | 7 | 1049 | 10 | 38 |
| 10 | 4 | 5 | 1034 | 10 | 32 |
| 10 | 6 | 3 | 1035 | 9 | 27 |
| 10 | 9 | 0 | 1040 | 9 | 21 |
| 50 | 0 | 49 | 1184 | 33 | 242 |
| 50 | 12 | 37 | 1174 | 30 | 182 |
| 50 | 24 | 25 | 1204 | 34 | 136 |
| 50 | 37 | 12 | 1295 | 34 | 138 |
| 50 | 49 | 0 | 1209 | 34 | 42 |
| 150 | 0 | 149 | 1736 | 121 | 743 |
| 150 | 37 | 112 | 1726 | 117 | 630 |
| 150 | 75 | 74 | 1750 | 117 | 461 |
| 150 | 112 | 37 | 1701 | 123 | 257 |
| 150 | 149 | 0 | 1769 | 132 | 146 |
| 300 | 0 | 299 | 2466 | 268 | 1416 |
| 300 | 75 | 224 | 2720 | 331 | 1022 |
| 300 | 150 | 149 | 2607 | 334 | 785 |
| 300 | 225 | 74 | 2634 | 291 | 511 |
| 300 | 299 | 0 | 2444 | 246 | 216 |

## Reproduction check against Q023's own published N=300 numbers

Q023's addendum reported (N=300): idx=0 → 1339ms, idx=150 → 823ms, idx=299 → 255ms. This run's
corresponding rows: idx=0 → 1416ms, idx=150 → 785ms, idx=299 → 216ms. Same order of magnitude, same
qualitative shape (head ≈ 5-6x tail), differences of 60-80ms consistent with ordinary run-to-run
timing noise on a shared machine, not a discrepancy worth investigating further — no divergence
flagged per the design's own instruction to report one honestly if found.

## Per-N linear fit: editOneMedian ≈ intercept + slope × successors

Least-squares fit, 5 points per N:

| N | slope (ms/successor) | intercept (ms) | R² | repeat median (ms), for comparison |
|---|---|---|---|---|
| 10 | 3.86 | 16.9 | 0.852 | 9.6 |
| 50 | 3.60 | 59.5 | 0.912 | 33.0 |
| 150 | 4.20 | 134.8 | 0.990 | 125.4 |
| 300 | 3.89 | 209.3 | 0.993 | 274.0 |

Two findings fall out cleanly:

1. **The slope is essentially N-invariant.** All four fitted slopes fall in a tight 3.60-4.20
   ms/successor band — no systematic drift with N in either direction. This is the central question
   Movement 1 asked: the linear-in-successors model from Q023's single-N=300 spot-check generalizes
   across two full orders of magnitude of project size (N=10 to N=300) with the same marginal rate,
   not a rate that grows or shrinks as the project gets bigger.
2. **The fit quality improves with N** (R² 0.85 at N=10 rising to 0.99 at N=300), which is exactly
   what would be expected if the "true" relationship is linear and the deviation is dominated by
   fixed-magnitude timing noise (a few ms of OS/GC jitter) that matters proportionally less as the
   measured costs themselves grow larger at bigger N. N=10's weaker fit is noise-dominated, not
   evidence the model breaks down at small N — the raw N=10 numbers (59/38/32/27/21ms) are
   individually so small that a few milliseconds of jitter is a large fraction of each one.
3. **The intercept (predicted cost at zero successors, i.e. editing the very last file) tracks the
   independently-measured repeat median at the same N reasonably closely**, and the gap between them
   narrows at larger N in relative terms (N=10: 16.9 vs 9.6, ~1.8x; N=300: 209.3 vs 274.0, actually
   *below* the repeat median). This cross-validates Q023's own addendum finding — a tail edit costs
   about the same as changing nothing at all — using an independently-fitted number (a regression
   intercept) rather than a single directly-measured data point.

## Cross-check against Q023's own original (fixed-position) regression

Q023's original run always edited idx=0 (successors = N−1 by construction) and fit `editOneMedian`
directly against `N`, getting a marginal rate of 4.54 ms/file. This quartet's independent regression,
varying position at fixed N and fitting against `successors` directly, gets slopes of 3.60-4.20
ms/successor across four separate N values — the same quantity, measured a structurally different way
(varying position within one N, rather than varying N at one fixed position), landing in the same
band. Two different experimental designs converge on the same marginal-cost figure, which is stronger
evidence for the underlying model than either alone.

## Against Movement 1's thresholds

**SHIP** ("the model fits cleanly at every N, and the slope stays within the same order of magnitude
across N"): **met, and more strongly than the threshold required** — the four slopes aren't just
same-order-of-magnitude, they're within roughly ±15% of their own mean (3.61-4.20 vs. a mean of
~3.89), and fit quality is good-to-excellent everywhere, improving rather than degrading at the largest
N tested. The cheapest-falsifier check (N=10 vs. N=300 comparison) that Movement 1 named as the first
thing to look at shows the same qualitative shape at both extremes.

## Honest limitations

- Only 5 positions per N and only 3 editOne samples per position — the same sample-size caveat every
  quartet in this lineage discloses. The R² figures above are descriptive of these 20 specific runs,
  not a claim of a tight confidence interval on the true population slope.
- N=300 is still the practical ceiling this lineage has tested (Q023's own limitation, inherited
  unchanged) — nothing here speaks to whether the linear model continues to hold at N in the
  thousands, which is closer to some real large F# codebases.
- This remains the independent-file project shape (Q023's own deliberate design choice, inherited
  here) — a dependency-chained variant (Q023 review Follow-up 3, not attempted here) could show a
  different — plausibly steeper, since real invalidation would stack on top of the conservative
  order-based invalidation this quartet measures — relationship.
