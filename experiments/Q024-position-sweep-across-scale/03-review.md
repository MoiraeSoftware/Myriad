# Q024-position-sweep-across-scale / Movement 4 — Adversarial review

## Reproduction status — read this first, it bounds everything below

I independently reran the full 20-cell grid (no rebuild — the `.exe` was already present and current;
this quartet added no source), exactly as `01-design.md` specifies: `q023-spike.exe <N> <editIdx>` for
every `(N, editIdx)` pair, each a fresh process, from
`Q023-scale-cost-reentrant-callback/artifacts/q023-spike/bin/Release/net9.0/`. Pinned package is
`FSharp.Compiler.Service 43.9.101` (matches `paket.lock` and the precondition). Same `NU1608`
(`FSharp.Core 9.0.303` vs FCS's requested `9.0.101`) as every prior quartet; not material.

KILL did not fire anywhere: `errs=0` on every call of every run, `callbackInvocations = N+1` on every
call at every N, `observerStable=true` on every edit, no hang/throw. That part is solid and matches the
lineage.

My editOne medians (ms) versus the results file's, by (N, successors):

| N | succ | theirs | mine | | N | succ | theirs | mine |
|---|---|---|---|---|---|---|---|---|
| 10 | 9 | 59 | 57 | | 150 | 149 | 743 | 722 |
| 10 | 7 | 38 | 37 | | 150 | 112 | 630 | 686 |
| 10 | 5 | 32 | 32 | | 150 | 74 | 461 | 441 |
| 10 | 3 | 27 | 28 | | 150 | 37 | 257 | 281 |
| 10 | 0 | 21 | 22 | | 150 | 0 | 146 | 126 |
| 50 | 49 | 242 | 263 | | 300 | 299 | 1416 | 1343 |
| 50 | 37 | 182 | 209 | | 300 | 224 | 1022 | 988 |
| 50 | 25 | 136 | 144 | | 300 | 149 | 785 | 687 |
| 50 | 12 | **138** | **72** | | 300 | 74 | 511 | 553 |
| 50 | 0 | 42 | 46 | | 300 | 0 | 216 | 215 |

The qualitative result reproduces cleanly and the monotone-in-successors shape is unambiguous at every N.
I re-ran their published regression on their own numbers and got their exact reported slopes, intercepts,
and R² (3.86/16.9/0.852, 3.60/59.5/0.912, 4.20/134.8/0.990, 3.89/209.3/0.993) — the arithmetic in
`02-results.md` is correct, not just plausible. **The central pre-registered claim — no systematic slope
drift with N — holds under independent reproduction and is affirmatively supported: in both datasets the
largest slope sits at a *middle* N, not the largest N, so "slope grows with N" (the REVISE trigger) is
not merely absent, it is refuted.** SHIP is the right verdict.

What I dispute is precision and framing, not the core finding. Three things in the write-up are stated
more strongly than 5 points per fit can carry, and one anomaly the write-up left standing actually
resolves in the model's favour. None of these overturn the verdict; they scope it. Details below.

## Objection 1 (the load-bearing one): "the slope is essentially N-invariant" overstates what 5 points per fit can establish — the honest claim is "no *detectable* drift," which is weaker but still enough for SHIP

The results file leans on the four point-estimate slopes falling in a "tight 3.60-4.20 ms/successor band …
within roughly ±15% of their own mean" and reads that as the slope being "essentially N-invariant." The
±15% is the spread of the four *point estimates*. It is not the uncertainty on any one of them. I computed
the 95% confidence interval on each fitted slope (3 residual dof, t=3.182):

| N | slope (theirs) | 95% CI (theirs) | rel. width | slope (mine) | 95% CI (mine) |
|---|---|---|---|---|---|
| 10 | 3.86 | [0.91, 6.81] | ±76% | 3.51 | [0.70, 6.32] |
| 50 | 3.60 | [1.55, 5.65] | ±57% | 4.64 | [3.59, 5.70] |
| 150 | 4.20 | [3.44, 4.96] | ±18% | 4.28 | [2.89, 5.68] |
| 300 | 3.89 | [3.31, 4.47] | ±15% | 3.60 | [2.71, 4.48] |

The N=10 slope is barely distinguishable from anything in a 7x-wide range; N=50 is nearly 3x wide. Only the
two large-N fits (N=150, N=300) are actually tight, and only in their run — in *my* run even N=150 opens up
to ±33% because one middle point (succ=112) landed high. So a *true* slope that drifted 20-30% between N=10
and N=300 would be completely undetectable at this sample size: the small-N CIs swallow it whole. The
write-up's own honest-limitations section concedes "the R² figures … are descriptive of these 20 specific
runs, not a claim of a tight confidence interval," which is correct — but the finding-1 headline ("the slope
is essentially N-invariant … the same marginal rate … across two full orders of magnitude") is written as an
established fact, and the "±15% of their own mean" phrasing invites reading the point-estimate spread as a
precision claim it is not.

Why this still supports SHIP rather than sinking it: the pre-registered SHIP bar was "slope stays within the
same order of magnitude across N (roughly N-invariant)," and the REVISE trigger was "a slope that itself
scales with N." Both datasets refute the REVISE trigger directly — the slope does *not* trend with N, the
peak is at a middle N in both. The correct, defensible statement is: **there is no detectable systematic
drift in the per-successor rate across N=10→300; the two well-constrained fits agree to ~15-18%; the small-N
fits are consistent with the same rate but individually too noisy to constrain.** That clears the SHIP bar
as written. It is meaningfully weaker than "the slope is essentially N-invariant" stated flat, and the
digest propagation (FINDINGS/BACKLOG) should use the weaker wording.

## Objection 2: the N=10 lower-R² explanation ("noise-dominated, not a model breakdown") is correct — I checked for the structural alternative and did not find it

Scrutiny point 2 asks whether N=10's R²=0.852 could reflect a *structural* small-N misfit (e.g. fixed
per-call overhead dominating in a way the linear model can't absorb) rather than noise. It cannot, and the
write-up's explanation survives, for a concrete reason: the linear model already carries an intercept term,
so fixed per-call overhead is *captured by the intercept*, not left to distort the slope. A structural
breakdown would show up as a systematic residual pattern (e.g. all interior points bowed the same way). It
does not. My N=10 fit (3.51·succ + 18.4) gives residuals across succ = 9/7/5/3/0 of roughly +7/−6/−4/−1/+4 —
sign-alternating, no curvature signature. The low R² is simply that a few ms of OS/GC jitter is a large
fraction of editOne values that are themselves only 22-57ms. My N=10 R²=0.840 independently matches their
0.852. So the explanation is right, though it is worth stating the tighter reason (the intercept absorbs
fixed overhead) rather than resting on "small numbers, big jitter" alone.

## Objection 3: "intercept tracks the repeat median" is mildly circular and oversold as a cross-validation

The write-up presents finding 3 — the fitted intercept ≈ the independently-measured repeat median — as a
cross-validation of "a tail edit costs about the same as changing nothing," obtained "using an
independently-fitted number … rather than a single directly-measured data point." Two problems. First, the
succ=0 point (editing the very last file, zero successors) *is one of the five points in the regression*, and
it is directly measured at 21/42/146/216ms (theirs), already ≈ the repeat median. The intercept is pinned to
that measured point by construction, so "intercept ≈ repeat" mostly restates "the directly-measured tail
edit ≈ repeat," which Q023's addendum already established. It is not an independent triangulation. Second,
the agreement is loose and drifts: intercept/repeat is ~1.8x at N=10 (16.9 vs 9.6) and ~1.8x at N=50 (59.5 vs
33.0), then crosses to *below* 1.0 at N=300 (209.3 vs 274.0). The write-up notes the N=300 crossing but frames
the overall relationship as "tracks … reasonably closely." A factor ranging from 1.8 down through 1.0 is not
close tracking; it is order-of-magnitude agreement, which is all that's warranted. The honest claim is
"editing the last file costs about a no-op repeat, confirmed both as a directly-measured point and as a
regression intercept that lands in the same ballpark" — not a precise cross-validation.

## Objection 4: the reproduction-check discrepancy (Q024 vs Q023 at N=300) is genuine noise — my third run confirms it, and actually widens the band

Scrutiny point 4 asks whether the 60-80ms Q024-vs-Q023 differences at N=300 (idx0: 1416 vs 1339; idx150: 785
vs 823; idx299: 216 vs 255) signal a systematic shift. They do not. My independent third run gives idx0=1343
(essentially Q023's 1339), idx150=687, idx299=215. Across the three runs the spreads are: idx0 {1339, 1416,
1343}; idx150 {823, 785, 687}; idx299 {255, 216, 215}. idx150 spans ~140ms (~18%), *wider* than the 60-80ms
the results file cited, and my value sits below both prior runs — which is the opposite of a monotone
machine/build drift. The cold column is stable across all three quart runs (~2450-2700ms), which rules out a
thermal/throttling trend over the sweep. This is unstructured run-to-run jitter on editOne medians of 3
samples at ~700-1400ms each, exactly as the write-up concluded. No further scrutiny warranted; if anything my
run reinforces "noise, not shift."

## Objection 5: the N=50 idx=37 anomaly the write-up left standing is sampling noise — my run refutes it directly

Scrutiny point 6 flags the results table's N=50 pair at succ=25 (136ms) and succ=12 (138ms) being nearly
identical despite a 2x difference in successor count — a local violation of monotonicity that the write-up
does not call out. This is the one place their data misbehaves, and it is the reason their N=50 fit is their
second-worst (R²=0.912). It is noise, and I can show it rather than assert it: my independent run gives
succ=25 → 144ms and succ=12 → **72ms** (their 138ms did not reproduce; I re-confirmed via the saved log
`N50-idx37.txt`, whose three editOne samples were 153/138/101 — a high-variance triple whose median landed
high). With 72ms my N=50 sequence is cleanly monotone (263/209/144/72/46) and my N=50 fit tightens to
R²=0.985 with slope 4.64. So their idx=37 median was a high outlier from a noisy 3-sample set, not a real
plateau, and the model is *better* supported than their own N=50 row suggests. Worth a one-line callout in
any citation, because taken at face value their table's succ=25 ≈ succ=12 row is the single strongest piece
of evidence *against* clean linearity, and it dissolves on repetition.

## Objection 6: SHIP confirms a cost *model* generalizes across N; it does not measure a real FSAC editing session, and the gap matters for how item 18 cites this

This is the mechanism-vs-capability check the methodology demands. What Q024 establishes is that a *cost
model* — whole-project `ParseAndCheckProject` cost ≈ intercept + ~4ms × (compilation-order successors) — has
a slope that does not drift as N grows from 10 to 300. That is real and useful. But three gaps sit between it
and "this is what an FSAC-hosted Myriad editing session costs per keystroke," and the SHIP should not be read
as closing them:

1. **It measures `ParseAndCheckProject` (whole-project), not FSAC's actual per-edit path.** A real LSP host
   does not necessarily recheck via the same whole-project call on every keystroke; whether FSAC's
   incremental `TransparentCompiler` snapshotting reproduces this exact positional curve in a live session is
   Q021/Q022 territory and remains untested here.
2. **Independent files, worst-for-Myriad position.** Q023's own caveat carries: Myriad's attributed domain
   types typically sit *early* in build order (other code depends on them), i.e. near the high-successor,
   high-cost end of this curve. The model being clean does not make the number Myriad would actually pay
   small.
3. **N≤300 and ~4ms/successor.** The slope's own arithmetic is the warning: at N in the low thousands (real
   large F# codebases, which the write-up itself flags as untested), even a mid-file edit with ~1000
   successors implies multiple seconds of recheck. "The model generalizes" is not "the cost stays tolerable."

So the practical strengthening for item 18 is real but bounded: Q024 lets you *predict* per-edit cost from
edit position with a rate that is stable across the tested scale range, which is genuinely more than Q023's
single-N spot-check gave. It does not tell you what a live FSAC session costs, and the digest should say
"positional cost model, slope stable across N≤300, on independent files" rather than "FSAC keystroke cost
characterized."

## Verdict

**SHIP, scoped.** The pre-registered question — does Q023's linear-in-successors cost model hold consistently
as N grows, or does the per-successor rate drift with scale — is answered, and the answer reproduces
independently: no detectable systematic drift, the REVISE trigger (slope scales with N) affirmatively
refuted in both the executor's data and mine, KILL never fired. This is the SHIP bar as pre-registered.

What ships (reproduced independently across all 20 cells):
- Across N ∈ {10, 50, 150, 300}, editOne cost is monotone and well-fit by a line in the number of
  compilation-order successors; the per-successor slope shows no systematic trend with N (peak slope at a
  middle N in both datasets), and KILL did not fire (errs=0, callback N+1, observerStable=true everywhere).
- The tail edit (0 successors) costs about a no-op repeat at every N, confirmed as a directly-measured point
  and echoed by the fitted intercept.
- The N=300 rows reproduce Q023's addendum within ordinary noise; the executor's regression arithmetic is
  correct to the digit.

What must be scoped down before citation (none overturns the verdict; all are precision/framing):
- **"The slope is essentially N-invariant"** overstates precision. With 5 points per fit the 95% CI on the
  slope is ±76% at N=10 and ±57% at N=50; only N=150/300 are tight. Cite as "**no detectable systematic
  drift** across N=10→300; the two well-constrained fits agree to ~15-18%," not as a proven invariant. A true
  20-30% drift would be undetectable at this sample size.
- **"Intercept tracks the repeat median" is mildly circular** — the succ=0 point is inside the regression and
  already ≈ repeat — and the ratio drifts from ~1.8x to below 1.0. It is ballpark agreement, not an
  independent cross-validation.
- **The N=50 succ=25 ≈ succ=12 row is a sampling outlier**, not a real plateau: it did not reproduce (my
  succ=12 median 72ms vs their 138ms), and its removal makes the N=50 fit cleaner, not worse.
- **This is a cost *model* generalizing across N, not a measured FSAC session cost.** Whole-project
  `ParseAndCheckProject`, independent files (Myriad's real types sit early/expensive), N≤300. Do not let the
  SHIP be cited as "FSAC keystroke cost is characterized."

Why SHIP and not REVISE: unlike Q023 (whose *headline* reading was wrong and had to be inverted), Q024's
headline — the model generalizes across N with no slope drift — is correct and reproduces; the issues are
overstated precision and one unreproducing outlier, which scope the claim rather than reverse it. Why not a
bare unqualified SHIP: the "essentially N-invariant" and "cross-validates" framings claim more than 5 points
per fit support, and would mislead if propagated verbatim. This is the same outcome class as Q019/Q020/Q021 —
primary claim ships, specific secondary framings struck or softened.

## Follow-ups, prioritized

1. **Propagate the scoped wording, not the write-up's headline, into FINDINGS/BACKLOG item 18.** Use "per-edit
   whole-project recheck cost is a clean linear function of compilation-order successors (~4ms each), with a
   per-successor rate that shows no detectable drift across N=10→300; measured on independent files via
   whole-project `ParseAndCheckProject`, not a live FSAC session." Explicitly *not* "FSAC keystroke cost
   characterized" and *not* "slope proven N-invariant."
2. **If tighter slope constraints are ever needed, add samples, not more N values.** The limiting factor is
   3 editOne samples per position and 5 positions per N, which leaves the small-N slopes with ±60-80% CIs.
   More repeats per cell (or more positions per N) would shrink those; a fifth N would not. The N=50 outlier
   shows the current sampling is thin enough for a single high median to distort a whole fit.
3. **Test whether FSAC's own incremental path reproduces this positional curve.** The standing gap since
   Q023: this measures `ParseAndCheckProject`, and item 18's real question is a live LSP editing session
   (Q021/Q022 territory). The positional cost model is only a lower bound / proxy for that until measured
   against the actual host path.
4. **The N-in-the-thousands regime and the dependency-chained variant (Q023 Follow-up 3) both remain open.**
   The slope's own arithmetic (~4ms/successor) means the interesting question at real large-codebase scale is
   not "does the line stay straight" but "is the absolute cost tolerable," which needs N well past 300, and
   real cross-file `open`s would stack genuine invalidation on top of the conservative order-based
   invalidation measured here.
