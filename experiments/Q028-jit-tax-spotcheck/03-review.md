# Q028-jit-tax-spotcheck / Movement 4 — Adversarial review

## Reproduction status — read this first

I confirmed the harness first, then reran everything from a clean build. `Program.fs`'s measurement
region (`open System` through the end of `runForN`) is **byte-identical** to
`Q023-scale-cost-reentrant-callback/artifacts/q023-spike/Program.fs` — I diffed it directly; the only
differences in the shared region are three lines of header comment. The additions are `runWarmup ()`,
a third `[| nStr; editIdxStr; "warmup" |]` argv branch, and a call site gated on that token. So the
validity precondition ("unchanged in its untouched code paths, opt-in warmup only") holds literally,
not just in spirit. Pin is `FSharp.Compiler.Service 43.9.101` (same `NU1608 FSharp.Core 9.0.303 vs
9.0.101` as every prior quartet; not material).

I then `rm -rf bin obj`, rebuilt `-c Release`, and ran all four conditions as fresh processes. Log:
`artifacts/q028-spike/review-rerun.txt`. My numbers against the results table:

| Condition | warmup (mine/theirs) | cold (mine/theirs) | editOneMed (mine/theirs) | editOneRatio (mine/theirs) |
|---|---|---|---|---|
| N=10 control | — | 1018 / 1007 | 52 / 49 | 0.051 / 0.049 |
| N=10 warmup | 765 / 766 | 441 / 437 | 61 / 58 | 0.138 / 0.133 |
| N=300 control | — | 2365 / 2520 | 1244 / 1185 | 0.526 / 0.470 |
| N=300 warmup | 759 / 768 | 1795 / 1853 | 1214 / 1437 | 0.676 / 0.776 |

The result reproduces. Every load-bearing empirical claim holds under an independent rebuild:

1. **Control reproduces Q023.** My N=10 no-warmup `cold=1018ms` lands on the executor's `1007ms` and
   on Q023's own `run-n10.txt` (`1037ms`), all within ~2-3%. The copied binary measures the same thing.
2. **The tax is real and roughly fixed in absolute size.** Cold dropped **577ms at N=10** (1018→441)
   and **570ms at N=300** (2365→1795) under warmup. In my run those two reductions are essentially
   identical — a *cleaner* confirmation of "fixed absolute, N-independent" than the executor's own
   570/667 pair. This is the cheapest-falsifier's "moves and stays roughly fixed across N" branch, not
   the NULL branch.
3. **The ratios shift, non-uniformly across N.** `editOneRatio` at N=10 rose ~2.7x (0.051→0.138),
   matching the executor's "nearly triples" almost exactly. At N=300 it rose in the same direction but
   by less. The mechanism is exactly as claimed: `editOne` raw ms barely move under warmup (they are
   measured warm, after cold), the `cold` denominator shrinks, so the ratio rises, and it rises more at
   small N because a ~575ms fixed tax is a larger fraction of a smaller cold.

So the frozen result's raw numbers are not in dispute. What I dispute is one specific claim about
*where* this correction lands, and it is load-bearing enough to gate the verdict.

## Objection 1 (headline): the correction does NOT touch "Q024's regression" — Q024 is fit on editOne milliseconds, not on editOneRatio, so it is essentially immune to the cold tax. The hypothesis's own framing overstates this spot-check's reach.

`00-hypothesis.md` (line 33) names the at-risk quantities as `"1339ms of a 2640ms cold check,"
editOneRatio values feeding Q024's regression`. The first is right — that is Q023 prose and it is
genuinely affected. The second is wrong on two counts, both checkable in Q024's own frozen docs:

- **Q024 does not fit a ratio.** `Q024-position-sweep-across-scale/02-results.md` line 46: *"Per-N
  linear fit: editOneMedian ≈ intercept + slope × successors"*, with the slope reported in
  **ms/successor** (line 50 table). The `cold` denominator never enters that regression. Q023's own
  original regression is likewise ms-based (`editOneMedian` vs `N`, 4.54 ms/file — cross-checked in
  Q024 results line 79-80).
- **Q024 does not even publish an `editOneRatio`.** Its results table (line 15) has columns
  `cold (ms) | repeat median (ms) | editOne median (ms)` — raw milliseconds only, no ratio column.

Since `editOne` and `repeat` milliseconds are measured *after* `cold` in an already-warm process — the
executor's own results confirm they "barely moved" under warmup — Q024's fitted slope model and its
SHIP verdict are untouched by this tax correction. The correction lands on **Q023's** ratio column and
its "51% of cold" prose, and on any future citation quoting a `cold`-denominated percentage. It does
**not** land on Q024's cost model. The executor's bottom-line sentence is actually compatible with this
("does not reverse Q023/Q024's qualitative verdict … what moves is the specific ratio numbers
Q023/Q024 published"), but the specific "editOneRatio values feeding Q024's regression" phrase reaches
past the evidence and implies Q024's fitted numbers are in question when they are not. This is the
lineage's recurring overclaim shape (asserting a consequence broader than shown), caught here in a mild
form: the executor overstated the correction's *reach into Q024*, in the safe direction (Q024 is more
robust than implied, not less). It must be corrected before citation.

## Objection 2: the "corrected reading" direction is right, and is if anything conservative — verified independently.

The executor claims `editOne`, measured against a tax-uncontaminated `cold`, is a *larger* fraction of
true cold than Q023/Q024's published ratios say. Checking the logic against my own numbers: same
numerator (`editOne` ≈ constant), smaller denominator (`cold` with tax removed) → larger ratio. N=10:
0.051 → 0.138. The direction is correct, not backwards. And it is conservative: the executor's own
disclosed residual (~185-190ms of tax still lands on the warmup-condition `cold`, so warmup `cold` is
*not* fully tax-free) means the true tax-free `cold` is even lower, and the true ratio even higher than
0.138. So the finding understates itself, which is the honest direction to err. Confirmed.

One scope note the results state correctly and I reinforce: this raises the *worst-case* head-edit
(idx 0) ratio specifically. It does not disturb Q023's rescued finding (tail edits collapse to repeat
cost, real incremental caching), because that finding rests on `repeat` and on position-varying editOne
*within a single run's fixed cold*, where the tax cancels. `repeat` staying a tiny fraction of any
plausible `cold` (0.008-0.13 here) is untouched. The qualitative verdicts of both quartets survive.

## Objection 3: `runWarmup` genuinely isolates a process-global tax and does not contaminate the measurement — verified by reading it against the measurement path.

I read `runWarmup` (Program.fs:173-190) specifically for the failure mode the design flags as the one
that would invalidate it (the warmup silently priming the *measured* project's own cache rather than a
process-global tax):

- It builds its own `checker` (local binding, `keepAssemblyContents = true`, its own
  `DocumentSource.Custom` serving only `Warmup0000.fs`) and **never assigns `checkerRef`/`optsRef`**.
  `buildProject` later mints a brand-new `FSharpChecker` and overwrites those globals. FCS caches are
  per-instance, so the measured project's `cold` cannot be a cache hit on the warmup checker's work.
- It never touches the `files` dictionary the measurement uses (its source is a local `src`), and
  `buildProject` calls `files.Clear()` regardless.
- Its project (`Warmup0000.fs`, 3 lines, `warmup.fsproj`) shares no virtual file or path with the
  Prefix/Observer project.

What it *does* share with the real checker is exactly what the tax is made of: process-global JIT, FCS
static initialization, and referenced-assembly (`FSharp.Core`/framework) metadata reads — Q027's own
enumerated components. Warming those is the intended mechanism, identical to Q027's own
`warmup-reentrant` control, not contamination of the measured recheck work. The isolation is sound.

## Objection 4: partial-transfer limitation is accurate, and it strengthens rather than weakens the finding.

The executor discloses that the standalone warmup cost (766-768ms) exceeds the `cold` reduction
(570-667ms), so ~130-200ms of first-touch cost still lands on the real `cold`. My run agrees: warmup
759-765ms, cold reduction 570-577ms, residual ~185-190ms. The proffered explanation (the real
project's reentrant `DocumentSource.Custom` + `GetProjectOptionsFromScript` over a 300-file virtual
project JITs paths the warmup's simple non-reentrant checker never exercises) is plausible and is
correctly flagged as unconfirmed. It does not undercut the verdict: partial isolation still proves the
tax exists and inflates `cold` by ~570ms; full isolation would only enlarge the reduction and the ratio
shift, in the same direction. The executor draws exactly that conclusion (true tax lies between the
observed reduction and the standalone cost). Accurate.

## Objection 5: single sample per cell — the specific ratio-shift *magnitudes* are not stable, only their direction is.

Every cell is one process, one `cold`, three-median `repeat`/`editOne`. The qualitative direction
reproduced robustly (cold drops ~570ms both N; editOneRatio rises, more at small N). But the specific
magnitudes are noise-sensitive: my N=300 `editOneRatio` shift was +29% (0.526→0.676) where the executor
reported +65% (0.470→0.776), driven by run-to-run variance in the N=300 `editOne` median (my 1244 vs
their 1185/1437) and `cold`. The executor discloses single-sample in Scope, so this is not a hidden
defect, but the headline "at N=300 grows ~65%" should be cited as "grows materially (order tens of
percent)," not as a stable figure. The "nearly triples at N=10" claim is more robust — both runs land
~2.7x.

## Verdict: REVISE

The spot-check does its job and the empirical core ships, reproduced independently from a clean build:
the first `ParseAndCheckProject` in Q023/Q024's *own* harness carries the same ~570ms one-time
JIT/FCS-init tax Q027 found in its separate toy; the tax is roughly fixed in absolute size across
N=10↔N=300 (not proportional); it inflates the `cold` denominator, so the `cold`-denominated ratios and
percentages Q023 published overstate how large `cold` "really" is and correspondingly understate
`editOne` as a fraction of a tax-corrected `cold`; and the warmup step genuinely isolates a
process-global tax without contaminating the measured recheck work. BACKLOG item 23's credibility
question is answered: **yes, Q023's `cold` numbers ate the tax; correcting it moves the ratio framings
but not either quartet's qualitative verdict.**

It is REVISE, not SHIP, for one specific reason a citing write-up must carry forward: the hypothesis's
own framing that this touches "editOneRatio values feeding Q024's regression" is wrong. Q024's
regression is fit on `editOne` **milliseconds** against successors (ms/successor slope) and Q024
publishes no `editOneRatio` at all; `editOne`/`repeat` milliseconds are measured warm and the tax is a
`cold`-only effect. So this correction lands on **Q023's ratio column and "51% of cold" prose**, and on
any percentage-of-cold citation — **not on Q024's cost model, whose SHIP verdict is untouched.** The
executor's own summary sentence is compatible with this narrower scope; the pre-registered phrasing
reaches past it.

It is not KILL because nothing false was asserted as *proven*: the numbers reproduce, the isolation is
sound, the corrected-reading direction is correct and conservatively stated, and the single-sample and
partial-transfer limitations were disclosed, not hidden. This is Movement 4 doing its job — tightening
the scope of a correct finding's *consequence* — exactly as Q027 (whose byproduct spawned this quartet)
tightened its own headline.

### What travels with this verdict, scoped

- **Ships (reproduced independently):** the ~400-570ms first-in-process JIT/FCS-init tax is present in
  Q023/Q024's actual spike, is roughly N-invariant in absolute size, inflates `cold`, and is isolable
  by a throwaway unrelated-project warmup. Any future citation of a Q023 `cold`-denominated ratio or
  "% of cold" figure must treat the denominator as tax-inflated (true `editOne`-as-fraction-of-cold is
  *higher* than published, most at small N).
- **Does NOT travel:** the claim that this alters Q024's fitted slope/regression or any of Q024's
  published numbers. Q024 is ms-based and cold-independent; its verdict stands unmodified. Also do not
  cite the specific "+65% at N=300" magnitude — single-sample, not reproduced (I got +29%); cite the
  direction and the robust "~2.7x at N=10."
- **Still open (unchanged):** the residual ~185-190ms non-transferring tax's exact source (reentrant
  callback vs `GetProjectOptionsFromScript` on a large virtual project — plausible, unconfirmed); a
  distribution rather than single samples; and whether the correction changes anything at edit
  positions other than idx 0 (the `repeat`-condition's near-zero movement suggests the tail-edit ≈
  repeat comparison is unaffected, but it was not rerun with warmup).
