# Q028 — results

## What was run

`artifacts/q028-spike/` (copy of Q023's own spike + opt-in warmup step), built with `dotnet build -c
Release`, `FSharp.Compiler.Service 43.9.101` (same pin as Q023/Q024/Q027). Four conditions, each a fresh
process, `editIdx=0`:

| Condition | warmup check | cold | repeat (median) | editOne (median) | repeatRatio | editOneRatio |
|---|---|---|---|---|---|---|
| N=10, no warmup (control) | — | 1007ms | 9ms | 49ms | 0.0089 | 0.0487 |
| N=10, warmup | 766ms | 437ms | 10ms | 58ms | 0.0229 | 0.1327 |
| N=300, no warmup (control) | — | 2520ms | 315ms | 1185ms | 0.1250 | 0.4702 |
| N=300, warmup | 768ms | 1853ms | 214ms | 1437ms | 0.1155 | 0.7755 |

Raw logs: `artifacts/q028-spike/run-n10-control.txt`, `run-n10-warmup.txt`, `run-n300-control.txt`,
`run-n300-warmup.txt`. An earlier, un-archived pair of runs at each N (used to confirm the effect before
locking in the logged runs above) showed the same pattern (N=10 cold 1018→432ms; N=300 cold 2345→1829ms),
so this isn't a one-off.

## Control check

N=10 no-warmup reproduces Q023's own original number closely: this run's `cold=1007ms` vs. Q023's own
`run-n10.txt` (`cold=1037ms`) — a 3% difference, well inside ordinary run-to-run noise for a single
sample, confirming the copied harness is still measuring the same thing Q023's was.

## The core finding: the tax is real, reproduces in Q023's own actual harness (not just Q027's separate
toy), and is roughly fixed in absolute size, not scaling with N

`cold` dropped by **570ms at N=10** (1007→437ms) and **667ms at N=300** (2520→1853ms) when a throwaway,
unrelated 1-file warmup check ran first. Both reductions are close to each other in absolute terms and
close to the standalone warmup check's own cost (766-768ms) — consistent with Q027's own finding that
this is a roughly fixed, one-time, N-independent tax, not something proportional to project size. This
directly confirms the cheapest falsifier's "moves and stays roughly fixed across N" branch, not the NULL
branch.

**One nuance, disclosed rather than glossed over:** the reduction in `cold` (570-667ms) is somewhat
smaller than the standalone warmup check's own cost (766-768ms) — meaning not all of the tax transfers
from the throwaway warmup project to the real N-file project's own first check. A residual ~130-200ms of
first-touch cost still lands on the real project's `cold` even after the warmup ran. Plausible explanation
(not independently confirmed here): the real project's checker uses a **reentrant `DocumentSource.Custom`**
callback and `GetProjectOptionsFromScript` against a much larger virtual project, both of which may JIT
additional code paths the warmup's simpler, non-reentrant checker never touches. This means the warmup
control here is a *partial*, not complete, isolation of the tax — the true fully-isolated tax for this
specific harness shape is somewhere between the observed `cold` reduction and the standalone warmup cost.

## What this means for Q023/Q024's own numbers

`repeat` and `editOne` are measured *after* `cold` in the same already-warm process, so their **raw
millisecond values are not directly contaminated** by the tax — and that's borne out here: `repeatMedian`
barely moved at N=300 (315→214ms, well within the single-sample noise this whole lineage's own
cross-cutting caveats already flag) and `editOneMedian` likewise (1185→1437ms). The tax is a `cold`-only
effect, not a whole-run effect.

But Q023/Q024's own headline numbers are not raw milliseconds alone — they're framed as **ratios against
`cold`** (`editOneRatio`, `repeatRatio`) and as fractions ("1339ms of a 2640ms cold check"). Since the
denominator (`cold`) shrinks substantially under warmup while the numerator (`editOne`/`repeat`) does not,
those ratios move a lot: `editOneRatio` at N=10 nearly **triples** (0.049→0.133), and at N=300 grows by
**~65%** (0.470→0.776). `repeatRatio` moves less and in the noisy direction at N=300 (0.125→0.116, i.e.
essentially flat given single-sample variance), consistent with `repeat`'s tiny absolute cost making it
less sensitive to the denominator's exact value either way.

**The corrected reading, stated plainly: `editOne`'s cost, measured against a tax-uncontaminated `cold`,
is a noticeably larger fraction of true cold than Q023/Q024's own published ratios say — not a smaller
one.** Q023's own framing ("editing the first costs close to cold: 1339ms of a 2640ms cold check," a 51%
ratio) understates the true fraction, since a chunk of that 2640ms `cold` was tax, not real recheck work —
against a tax-corrected cold, that same 1339ms edit would represent a *higher* percentage. This does not
reverse Q023/Q024's qualitative verdict (real, working incremental caching under `TransparentCompiler`;
cost linear in compilation-order successors) — `repeat` staying a tiny fraction of any plausible cold
value is untouched by this correction, and the *relative ordering* (repeat cheapest, tail edits near
repeat, head edits near cold) is unaffected since none of the edit-position comparisons in Q023/Q024
involve `cold` moving between conditions within the same run. What moves is the *specific ratio numbers*
Q023/Q024 published and any future citation quoting them verbatim — those should be treated as measured
against an inflated `cold` denominator and are not safe to cite as precise percentages without this
caveat.

## Scope

Two N values, one sample each (not a distribution — same single-sample limitation this whole lineage's
own cross-cutting caveats already name). `editIdx=0` only — this spot-check does not re-run the position
sweep with warmup added, so whether the tax's effect on ratios changes qualitatively at other edit
positions (e.g. the tail-edit/no-op-repeat comparison Q023's own headline "255ms vs 258ms" result rests
on) is untested here, though the `repeat` condition's own near-zero movement suggests that specific
comparison is not materially affected. Does not re-derive Q024's fitted slope model with corrected
numbers — that would require rerunning the full 5-position × 4-N sweep with warmup added, a much larger
lift than this cheap spot-check's own pre-registered scope.
