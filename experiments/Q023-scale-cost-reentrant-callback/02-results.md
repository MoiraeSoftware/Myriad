# Q023-scale-cost-reentrant-callback / Movement 3 — Results

**Status:** EXECUTED. `FSharp.Compiler.Service 43.9.101` (matches `paket.lock`), `net9.0`, `Release`
build, run via `dotnet run -c Release --no-build` / the built `.exe` directly. Same `NU1608`
(`FSharp.Core` resolves to `9.0.303` against FCS's requested `9.0.101`) as every prior quartet in
this lineage — not material, the pinned package that matters is FCS itself.

## Two deviations from the design, found and fixed before the numbers below were trusted

Per this repo's own convention ("do not silently patch the frozen design; report corrections
honestly"), both are recorded here rather than quietly repaired.

1. **A real bug in the generated F# source, not a harness bug.** The first build's per-file
   template generated `[ "a"; "b"; string i ]` inside the `records` list comprehension, where the
   comprehension's bound variable was `k`, not `i` — `i` was an unrelated `sprintf` format-string
   placeholder name that leaked into the *generated* F# text as a literal identifier. Every one of
   the N generated files failed to compile (`The value or constructor 'i' is not defined`), so the
   very first run's "results" were 10/10 files erroring at every N and are discarded entirely — not
   used anywhere below. Fixed to `string k`; the corrected project compiles with zero diagnostics at
   every N tested.
2. **The design's own explicit "fresh process per N" instruction (01-design.md, "Conditions, per
   N") was not followed in the first working run.** All four `N` values ran inside one `main`, one
   process, sequentially. This let one-time JIT/assembly-load cost at the very first
   `FSharpChecker.Create` (which landed on the N=10 iteration) inflate N=10's cold measurement in a
   way that didn't recur for N=50/150/300 in the same warm process — the raw symptom was a
   non-monotonic cold column (1048ms at N=10, then *409ms* at N=50, then rising again), which is
   physically implausible for "cold check of more files" and is exactly what fixed per-process
   startup cost landing unevenly looks like. Caught before writing this file up: `Program.fs` was
   changed to take `N` from `argv` and the harness was re-run as four separate process invocations
   instead (`q023-spike.exe 10`, `q023-spike.exe 50`, `q023-spike.exe 150`, `q023-spike.exe 300`).
   The corrected cold column is cleanly monotonic (below) as it should be. **All numbers in this file
   are from the corrected, per-process run** — `artifacts/q023-spike/run-n{10,50,150,300}.txt` and
   `combined-run-output.txt` hold the raw logs; `Program.fs` in `artifacts/q023-spike/` is the
   corrected version, with the deviation itself left as a comment in the source (matching Q021's own
   convention of leaving "DEVIATION FROM DESIGN" comments in place rather than erasing the history).

## Raw results (corrected, per-process run)

Zero diagnostics on every call, every N — confirmed for cold, all 3 repeats, and all 3 edit-ones, at
every N. `Observer.fs`'s generated text was byte-identical before and after every edit-one, at every
N — confirming the edit genuinely never touched the type shape (`Record`) the reentrant callback
resolves, only an unrelated file's internal value list, exactly as the design intended.

| N | cold (ms) | repeat median (ms) | editOne median (ms) | repeat/cold | editOne/cold |
|---|---|---|---|---|---|
| 10 | 1037 | 10 | 54 | 0.0096 | 0.0521 |
| 50 | 1270 | 43 | 274 | 0.0339 | 0.2157 |
| 150 | 1798 | 141 | 736 | 0.0784 | 0.4093 |
| 300 | 2674 | 328 | 1382 | 0.1227 | 0.5168 |

Callback invocation count was exactly N+1 (every prefix file plus `Observer.fs`) on **every single
call** — cold, every repeat, every edit-one, at every N. This reproduces Q021 Round 5's "the callback
re-fires every `ParseAndCheckProject` call" finding at two orders of magnitude larger scale, and adds
the piece Round 5 couldn't measure on a 2-file toy: what that re-firing costs.

Both ratio columns are **monotonically increasing** across all four N, with no exception at any
point. This is the opposite of the pre-registered SHIP prediction (§00-hypothesis.md: "the
repeat/cold ratio shrinks as N grows").

## Marginal-cost regression

A least-squares linear fit of cost against N for each condition (four points each; not enough for a
confidence interval, but the fits are visually tight — see residuals):

| condition | slope (ms/file) | intercept (ms) | fit vs actual (N=10/50/150/300) |
|---|---|---|---|
| cold | 5.62 | 978 | pred 1034/1259/1821/2664 vs actual 1037/1270/1798/2674 |
| repeat | 1.10 | −10 | pred 1/45/155/321 vs actual 10/43/141/328 |
| editOne | 4.54 | 33 | pred 78/260/714/1395 vs actual 54/274/736/1382 |

Two distinct behaviors fall out of this, not one:

- **`cold`'s intercept (≈978ms) is a large, roughly fixed, per-process cost** — almost certainly
  `FSharpChecker`/compiler-service construction and first-use warm-up, not per-file typecheck work.
  This matches why the ratio columns start so low at N=10: a fixed ~978ms cold cost dominates the
  denominator at small N regardless of what the numerator measures.
- **`repeat`'s marginal rate (1.10ms/file) is ≈20% of `cold`'s marginal rate (5.62ms/file), with a
  near-zero intercept.** When *nothing* changes, real work is being skipped — this is genuine
  caching, not zero-cost, but a meaningfully cheaper per-file operation than a first typecheck
  (plausibly: content-hash the callback's returned text and confirm the cached result is still
  valid, without redoing the actual typecheck).
- **`editOne`'s marginal rate (4.54ms/file) is ≈81% of `cold`'s marginal rate (5.62ms/file).** Editing
  exactly **one** of N files that has **zero** downstream dependents (files are deliberately
  independent — see Movement 1) costs, per additional unrelated file in the project, almost the same
  as a first-ever cold typecheck of that file. The caching benefit that clearly exists for the
  "nothing changed" case is largely **absent** once anything, anywhere in the project, changes.

## What this means for the pre-registered (b) vs (c) question

Movement 1 framed this as binary: either unchanged-remainder cost stays flat (c) or tracks cold's
growth (b). The actual result splits the two conditions the hypothesis lumped together:

- **No-op repeat (literally zero edits): closer to (c), but not purely (c).** Real caching benefit
  (≈5x cheaper marginal rate than cold), but not O(1) — cost still grows with N, just more slowly.
  The repeat/cold ratio does not stay flat; it grows from 0.01 to 0.12 across the tested range, and
  the regression gives no reason to expect it to plateau rather than keep climbing toward
  `1.10/5.62 ≈ 0.20` as N→∞ and the fixed intercept's share of the total keeps shrinking.
- **Edit-one-unrelated-file: much closer to (b).** A single edit to a file with zero downstream
  consumers among 299 (at N=300) other files costs 81% of cold's marginal per-file rate to
  re-check the whole project — not the near-zero cost a "only recheck what actually depends on the
  edit" model would predict for an independent-file edit. The unchanged 299 files are not being
  skipped in any way that shows up as savings; whatever the callback-consultation/hash-check
  machinery buys the *no-edit* case, it buys almost nothing once *any* single file's content changes.

This is a genuine correction to Movement 1's own framing, made explicit rather than silently
smoothed over: (b) and (c) are not two exhaustive alternatives for "the unchanged remainder" as a
single category. They are different answers to two different questions — "what does an idle repeat
check cost" and "what does one localized edit cost" — and this project's own data says those two
questions have materially different answers under `TransparentCompiler`'s `ParseAndCheckProject`.

## Against the pre-registered thresholds

- **SHIP** ("ratio shrinks as N grows... the mechanism-favors-caching direction"): **not met.** Both
  ratio columns grow monotonically and by a large factor (repeat: 12.8x from N=10 to N=300; editOne:
  9.9x) across the tested range, with no reversal at any point.
- **REVISE** ("repeat/edit-one cost tracks cold cost's growth... ratio stays roughly constant or
  grows"): **met, decisively for edit-one, moderately for repeat.** Unlike Q021's Round 5, which
  could not distinguish (b) from (c) at N=2, this scale sweep produces an unambiguous monotonic trend
  at four separate points, not a single ambiguous data point.
- **KILL** (hang/throw/callback stops firing): did not fire. Every N built, ran, and completed within
  seconds; the callback fired exactly N+1 times on every single call with no exception.

## Honest limitations

- `cold` is a single sample per N (by design — see Movement 2; it is a one-shot "everything unknown"
  baseline, not something a repeat measurement is meaningful for within one process). `repeat` and
  `editOne` are medians of 3 samples per N, per the design; the raw logs (`run-n*.txt`) contain every
  individual sample, not just the medians reported above.
- N=300 is still two to three orders of magnitude below Myriad's own real project sizes could reach
  in principle, and far below "a real, large monorepo" — the regression fits are tight over the
  tested range but extrapolation beyond N=300 is exactly that, extrapolation, not measurement.
- Files are deliberately structurally independent (Movement 1's stated design choice). A project
  where files form a real dependency chain (the shape `Q002`/`Q010` actually exercise, and the shape
  closer to how Myriad's own cross-generator visibility behaves) would be expected to cost *at least*
  as much as this independent-file result on an edit to an early file, since real invalidation
  propagation would additionally be justified rather than purely wasteful — that variant is a
  separate, not-yet-run question, not a stronger version of this one.
- No attempt was made to determine *why* editOne's cost is so close to cold's — this result
  establishes the cost, not the FCS-internal mechanism producing it (e.g., whether it's the
  `TransparentCompiler` snapshot's dependency-graph invalidation genuinely re-typechecking everything,
  or some coarser-grained cache key that treats "any file changed" as "invalidate the whole project
  snapshot"). That would require instrumenting or reading `TransparentCompiler`'s own source, not
  attempted here.
