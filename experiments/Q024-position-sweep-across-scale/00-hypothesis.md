# Q024-position-sweep-across-scale / Movement 1 — Hypothesis

**Question:** `Q023`'s review found, at a single N (300), that per-edit `ParseAndCheckProject` cost
under the reentrant `DocumentSource.Custom` callback is linear in the number of compilation-order
**successors** of the edited file — near-zero for a tail edit, near-cold for a head edit — spot-checked
at exactly three positions (0, 150, 299). Does that linear-in-successors model **hold consistently
across project size**, or does the relationship between edit position and cost change shape as N grows
(e.g. the marginal per-successor rate drifting, a fixed cost appearing at large N that wasn't visible at
N=300, or the tail-edit-is-free result breaking down)? This is `Q023-scale-cost-reentrant-callback/
03-review.md`'s own Follow-up 1, run for real: "rerun the sweep with the edit position as a variable...
report editOne cost as a function of edit position (head/middle/tail) **at each N**" — the single N=300
spot-check in Q023's addendum answered "is the model real at all" but not "does the model generalize
across scale," which is what item 18 actually needs for a usable cost estimate.

## Novelty

Not covered by `Q023` itself: its own addendum tested exactly one N (300) at exactly three positions,
explicitly flagged in its own text as "closes Follow-up 1 to the extent of a single N; a full N-sweep at
multiple positions remains open." Not covered by any other closed quartet — no prior quartet in either
thread has varied edit position as an independent variable at all before `Q023`'s single-N spot-check.

## Contradiction check

Does not contradict Q023's REVISE verdict or its review's central finding (cost is positional, not
uniformly O(N)) — this quartet either strengthens that finding (if the model holds across N) or sharpens
it with a real caveat (if it doesn't), it does not reopen whether editing an unrelated tail file is
cheap at N=300, which is independently confirmed with a durable artifact already.

## Validity preconditions

- Reuses `Q023-scale-cost-reentrant-callback/artifacts/q023-spike/` **verbatim, unmodified** — the
  `editIdx` argv parameter added during Q023's own addendum already supports this sweep with zero new
  code. This quartet's own `artifacts/` folder holds the run logs, not a second copy of the spike
  source (a `README.md` there points back to the exact unchanged binary/commit-equivalent state used).
- Same `FSharp.Compiler.Service 43.9.101` pin, same `useTransparentCompiler = true`, same independent-
  file project shape (no cross-file `open`s — Q023's own stated, deliberate design choice, inherited
  unchanged here; the dependency-chained variant is Q023 review Follow-up 3, a separate, harder
  question, explicitly not this quartet's scope).
- Fresh process per (N, position) combination, matching the correction already made partway through
  Q023 (the single-process-per-N mistake, caught and fixed there, must not be reintroduced here).
- 3 samples per editOne condition per (N, position), matching Q023's own convention; cold and repeat are
  not re-measured per position (they don't depend on which file gets edited) — reuse Q023's own already-
  measured cold/repeat numbers per N rather than re-running them, since nothing about this quartet's
  question touches those conditions.

## Cheapest falsifier

Run the sweep at the two extreme N values first (10 and 300). If the cost-vs-position relationship's
shape (linear, with intercept/slope roughly matching Q023's own N=300 marginal-rate figures) looks
similar at N=10 and N=300, proceed to fill in N=50/150 for a fuller picture. If it already looks
qualitatively different at the two extremes (e.g. N=10 shows no positional effect at all, everything
near-flat, because 10 files is too small to show a meaningful head/tail gap), that itself is the
answer — a genuine N-dependence in whether the positional model matters in practice — and is worth
reporting as such without needing the middle N values to confirm a trend already visible at the
extremes.

**Kill criterion:** unchanged from Q023 — hang, throw, or the callback stops firing at any (N,
position) combination.

**SHIP criterion:** the linear-in-successors model (cost ≈ intercept + slope × successors) fits cleanly
at every N tested, and the fitted slope stays within the same order of magnitude across N (i.e. the
per-successor marginal rate is roughly N-invariant, matching Q023's own regression finding that cold's
and editOne's marginal per-file rates were themselves roughly constant across N in the fixed-position
version of the test).

**REVISE criterion:** the model's shape or fitted slope changes materially with N (e.g. a
noticeably worse or better fit at large N, or a slope that itself scales with N rather than staying
roughly constant) — meaning the single-N=300 spot-check does not safely generalize to other project
sizes, and item 18's cost estimate needs to account for an N-dependent effect this quartet would then
need to characterize, not just report as unresolved.
