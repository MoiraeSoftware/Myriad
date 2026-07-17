# Q023-scale-cost-reentrant-callback / Movement 4 — Adversarial review

## Reproduction status — read this first, it bounds everything below

I independently rebuilt nothing (the `.exe` was already present and current) and reran the checked-in
spike as four separate processes, exactly as the results file describes: `q023-spike.exe 10`, `50`,
`150`, `300` from `artifacts/q023-spike/bin/Release/net9.0/`. The pinned package is
`FSharp.Compiler.Service 43.9.101` (confirmed in `q023-spike.fsproj`), matching `paket.lock` and the
precondition. Same `NU1608` (`FSharp.Core 9.0.303` vs FCS's requested `9.0.101`) as every prior quartet;
not material.

My numbers versus the results file's table:

| N | cold (theirs/mine) | repeat med (theirs/mine) | editOne med (theirs/mine) | editOne/cold (theirs/mine) |
|---|---|---|---|---|
| 10 | 1037 / 1087 | 10 / 10 | 54 / 56 | 0.052 / 0.052 |
| 50 | 1270 / 1200 | 43 / 31 | 274 / 228 | 0.216 / 0.190 |
| 150 | 1798 / 1845 | 141 / 125 | 736 / 802 | 0.409 / 0.435 |
| 300 | 2674 / 2457 | 328 / 251 | 1382 / 1308 | 0.517 / 0.532 |

The qualitative result reproduces cleanly: zero diagnostics on every call, callback fires exactly N+1
times on every call at every N, `Observer.fs`'s generated text is byte-stable across every edit, and
**both ratio columns increase monotonically across N**. Absolute timings differ within ordinary
machine/load noise (my repeat and cold numbers run a little faster; the trend and the ratios are the
same). Nothing in the raw measurement is in dispute. The two disclosed deviations are accurately
described in source (see Objection 4).

What I dispute is the **interpretation** the results file builds on the editOne column — specifically
the claim that editOne is "much closer to (b)" and that "the caching benefit … is largely absent once
anything, anywhere in the project, changes." I ran a controlled variant of the spike's own harness and
found that conclusion is an artifact of *which* file the design always edits. That is Objection 1, and
it is load-bearing enough to change the verdict's reasoning.

## Objection 1 (the load-bearing one): editOne's cost is a compilation-order-*position* artifact, not evidence of "(b) / caching absent"

The spike always edits `Prefix0000` — file **index 0**, the **first** entry in `SourceFiles`
(`Program.fs:102`, `Array.append [| for i in 0 .. n-1 -> prefixPath i |] [| observerPath |]`; the
editOnes loop at `Program.fs:138-143` hardcodes `prefixPath 0`). The results file describes this as
"a single edit to a file with zero downstream consumers among 299 other files" and treats the resulting
near-cold cost as showing the unchanged remainder "is not being skipped."

That reasoning conflates two different notions of "independent." The files are independent at the
**source** level — no file `open`s another, confirmed in `mkPrefixSrc` (`Program.fs:24-42`), each is a
self-contained `module PrefixNNNN`. But F# is **order-dependent**: every file implicitly sees the
accumulated environment of all files before it in compilation order, so any file *could* reference an
earlier one, and FCS's `TransparentCompiler` must conservatively invalidate the compilation-order
**tail** after an edited file. `Prefix0000` has zero *symbol* consumers but **N−1 order-successors**.
Editing it is the single worst-case edit position in the whole project, not a representative "one
independent file."

I tested this directly. I copied the spike's own harness, changed only the edit target (a second argv
selects the edited file's index; the edit operation — `mkPrefixSrc idx r`, add extra tags to the
`records` value, bump the version — is byte-for-byte the spike's own), and swept the edit position at
N=300:

| edit target | order-successors | editOne median (ms) | vs no-op repeat (~250-290ms) |
|---|---|---|---|
| idx 0 (first) | 299 | 1246-1382 | ~5x more expensive |
| idx 150 (middle) | 149 | 742 | ~2.5x more expensive |
| idx 299 (last) | 0 | 226-241 | **indistinguishable from repeat** |

The edit operation and its magnitude are held constant; only the edited file's compilation-order
position changes. Cost is **linear in the number of files after the edit point** and collapses to no-op-
repeat cost when the edited file has no successors. Editing the *last* file — a genuinely
downstream-free edit — is as cheap as changing nothing at all.

This inverts the quartet's central conclusion. FCS is **not** re-typechecking the whole project on any
edit. It is caching correctly and re-typechecking only the compilation-order tail below the edit, which
is exactly the incremental behavior (c) predicts. The reason the spike saw ~cold cost is that it always
edited the head of the file list, forcing the maximal tail. The 299 files *before* an edit are skipped;
the results file only ever measured the case where there are zero files before the edit and 299 after
it. The honest statement is: **per-edit whole-project recheck cost ≈ (files after the edit point) ×
per-file rate, ranging from ~free for a tail edit to ~half-cold for a head edit — not O(N)
unconditionally.** "editOne ≈ (b)" is the worst-case endpoint reported as the general case.

To be fair to the write-up: its own honest-limitations section *does* hedge the mechanism ("No attempt
was made to determine why editOne's cost is so close to cold's … whether it's … genuinely
re-typechecking everything, or some coarser-grained cache key"). But that hedge sits under a "What this
means" section and an "Against thresholds" section that both commit firmly to "edit-one … much closer
to (b)" and "REVISE … met, decisively for edit-one." The hedge is on the mechanism; the pessimistic
*conclusion* is stated without it. A one-line change to the spike's own edit target — which I made —
resolves the hedged question and refutes the committed conclusion.

## Objection 2: the pre-registered ratio-based SHIP threshold was structurally unmeetable, so "SHIP not met" is partly a threshold-design defect

The frozen SHIP criterion is "the repeat/cold ratio **shrinks** as N grows." Given that cold carries a
large fixed intercept (~978ms of `FSharpChecker` construction / first-use warm-up, correctly identified
in the regression section) and repeat has a near-zero intercept, the ratio is
`(repeat_slope·N) / (cold_slope·N + cold_intercept)`, which *necessarily* rises from ~0 toward
`repeat_slope/cold_slope` as N grows. It can only shrink if repeat's marginal rate is literally zero —
i.e. perfect O(1) caching. The pre-registered SHIP bar therefore demanded not "caching works" but
"caching is free per file," a much stricter and arguably wrong operationalization. The *repeat* result
(marginal rate ~20% of cold's, ~5x cheaper per file) is strong real caching, yet scores as "SHIP not
met" purely because of the intercept arithmetic. The results file's own regression section navigates
this correctly by switching to the marginal-rate comparison, but the verdict line ("REVISE …
moderately for repeat") still inherits the ratio framing and undersells that repeat demonstrates
substantial, working caching. This is a Movement-1 pre-registration weakness, not an execution fault,
and it is worth recording so the ratio-shrinks test isn't reused as-is.

## Objection 3: the marginal-cost regression is thin, but that is not the reason the editOne claim fails

Four points, single cold sample per N, no confidence interval. On its own that is enough to distrust
the precise "editOne marginal rate is 81% of cold's" figure — my own run puts it nearer 90%, and either
way it is a slope through four points with one cold sample each. But this is a secondary concern.
Even granting the regression, Objection 1 shows the editOne slope is a measurement of the **worst-case
edit position repeated at every N**, so a tighter fit or more N values would only pin down the
worst-case number more precisely; it would not make the worst-case representative of "an edit." The
regression's thinness is real and honestly flagged; it is not what sinks the editOne conclusion — the
fixed edit position is.

## Objection 4: the two disclosed deviations are accurately described (verified against source)

- The `string k` fix is genuinely in the current file: `mkPrefixSrc` builds the tag list with
  `string k` (`Program.fs:26`) and the `records` comprehension binds `for k in 1..20` (`Program.fs:40`).
  No stray `string i` in generated text. The bug description matches.
- The argv / four-separate-processes correction is real: `main` reads N from `argv` (`Program.fs:160-170`)
  and the DEVIATION-FROM-DESIGN comment (`Program.fs:161-166`) is left in place, matching this repo's
  convention of recording rather than erasing the history. My reproduction ran it exactly as four
  process invocations and got the monotonic cold column the correction was meant to produce.

Both deviations are described honestly. No silent revert.

## Objection 5: the Observer-stability claim is sound, and it isolates the correct thing (verified)

The edit adds entries to the `records` **value** via `extraTags` (`Program.fs:24-26`); the `Record`
**type** definition (`type Record = { Id: int; Name: string; Tags: string list; Meta: Map<string,int> }`,
`Program.fs:29`) is textually invariant to `extraTags`. `synthesizeObserver` emits
`rec'.QualifiedName` of that record (`Program.fs:59-65`), which cannot change when the type shape does
not. `observerStable=true` on every run confirms it. So the edit is a genuine value-level change that
leaves the reentrant callback's watched type untouched — exactly what the design intended, and it means
the editOne cost is *not* inflated by Observer regeneration. Good. (It also, incidentally, reinforces
Objection 1: with the edit at idx 0, Observer watches idx 0, so the edit's cost is the compilation tail,
not Observer churn.)

## Objection 6: does this answer BACKLOG item 18's question? Partly — and the correct answer is more favorable than the quartet drew

Item 18 / Q021 Follow-up 1 asked for exactly this test: hundreds of files behind the reentrant callback,
edit one, measure `ParseAndCheckProject` cost on the remainder. The spike runs that test. But the useful
answer for item 18 is not the quartet's "cost tracks cold, keystroke-cost concern confirmed real." The
corrected answer is: **an FSAC host that drives this mechanism via whole-project `ParseAndCheckProject`
pays a per-edit cost proportional to the number of files *after* the edited file in compilation order,**
from near-free (editing a leaf/late file) to roughly half a cold check (editing a root/early file). That
is both more precise and, for the common editing case, more favorable than the quartet concluded — with
one caveat that cuts the other way and should be stated plainly: Myriad's attributed types are often
domain/model types that live *early* in build order precisely because other code depends on them, so the
expensive head-edit case is not a rare pathology for Myriad specifically. The right framing is "cost is
positional, worst near the top of the file order, and Myriad's own types tend to sit near the top," not
"caching is absent."

Note also that the reentrant callback contributes negligibly to the measured cost (one extra
single-file check of `Prefix0000` per outer call, a few ms against hundreds); the cost result is really a
property of `ParseAndCheckProject` + `TransparentCompiler` tail-invalidation, with the callback along
for the ride. The quartet's title frames the callback as central to the *cost*; it is central to the
*use case*, not the cost.

## Verdict

**REVISE.** The mechanism facts and raw numbers ship; the quartet's headline interpretation of the
editOne column does not, and must be corrected before citation.

What ships (reproduced independently):
- Across N ∈ {10, 50, 150, 300}, the reentrant `DocumentSource.Custom` callback fires exactly N+1 times
  on every `ParseAndCheckProject` call, with zero diagnostics and no hang/throw at any N — KILL did not
  fire, and Q021 Round 5's "callback re-fires every call" reproduces at two orders of magnitude larger
  scale.
- The no-op **repeat** case shows real, working caching: marginal rate ~20% of cold's (~5x cheaper per
  file), with a near-zero intercept. This is (c)-consistent.
- Cold cost grows roughly linearly in N over the tested range with a large (~1s) fixed construction
  intercept.
- The two disclosed deviations, the Observer-stability control, and the FCS pin are all accurate and
  verified against source.

What does **not** ship, and must not be cited:
- **"editOne tracks cold / edit-one is much closer to (b) / the caching benefit is largely absent once
  anything changes / REVISE met decisively for edit-one."** This is an artifact of the spike always
  editing file index 0, the worst-case (whole-tail) edit position. A controlled variant of the spike's
  own harness shows editOne cost is linear in the number of compilation-order successors of the edited
  file and collapses to no-op-repeat cost for a tail edit (N=300: idx 0 ≈ 1300ms, idx 150 ≈ 740ms,
  idx 299 ≈ 230ms ≈ repeat). FCS **does** incrementally skip the unchanged prefix; it re-typechecks only
  the tail below the edit, which is correct order-dependent behavior, not a caching failure. The real
  finding is (c)-with-positional-cost, not (b).
- The precise "81% of cold's marginal rate" figure — thin four-point fit, single cold sample, and in any
  case a measurement of the worst-case position, not of "an edit."

Why REVISE and not SHIP: the quartet's own load-bearing conclusion is wrong in a way that would mislead
anyone citing it about FSAC keystroke cost (it would read as "the mechanism is unavoidably O(N) per
edit," which is false). Why not KILL: the spike is correct, reproducible, and honestly reported; the
error is interpretive and fully recoverable from the same artifact with a one-line change — the data was
right, the reading of it was not. This is the same class of outcome as Q021 (primary reproduces,
headline secondary conclusion struck) except that here the struck conclusion *is* the quartet's headline
verdict, which is why it lands at REVISE rather than scoped-SHIP.

## Addendum — Objection 1's positional claim independently re-verified with a durable artifact

This review's Objection 1 rested on a variant harness the reviewer ran but did not save — a real gap
against this repo's own hard-learned rule that "a `02-results.md` with no re-runnable code behind it
is prose, not evidence" (`README.md`, following the Q006/Q008/Q09 credibility episode). Closed here:
`Program.fs` in `artifacts/q023-spike/` was extended with a second, optional `editIdx` argv parameter
(default 0, so `q023-spike.exe 300` still reproduces the primary run's own numbers unchanged), and
re-run at N=300 for three edit positions, each saved as a checked-in log
(`run-n300-editidx{0,150,299}.txt` in `artifacts/q023-spike/`):

| editIdx | successors | cold (ms) | repeat median (ms) | editOne median (ms) | editOne/repeat |
|---|---|---|---|---|---|
| 0 (first) | 299 | 2640 | 309 | 1339 | 4.3x |
| 150 (middle) | 149 | 2530 | 258 | 823 | 3.2x |
| 299 (last) | 0 | 2550 | 258 | 255 | **1.0x** |

This independently reproduces the review's claim a third time (after the original executor run and
the reviewer's own unsaved variant), now with durable, re-runnable evidence: editIdx=0 matches the
primary run closely (1339ms here vs. 1382ms in `02-results.md`, both far above repeat cost); editIdx=299
— the last file, zero compilation-order successors — is statistically indistinguishable from a no-op
repeat (255ms vs. 258ms). Objection 1's central claim is confirmed, not just argued: per-edit cost is
positional, collapsing to cache-hit cost for a tail edit and rising toward cold cost for a head edit.
This closes Follow-up 1 below to the extent of a single N; a full N-sweep at multiple positions remains
open.

## Follow-ups, prioritized

1. **Rerun the sweep with the edit position as a variable, not fixed at index 0.** The single most
   informative change, already prototyped in this review's variant: report editOne cost as a function of
   edit position (head / middle / tail) at each N. This directly gives item 18 the number it needs —
   per-edit cost ≈ (files below the edit) × per-file rate — and replaces the current worst-case-only
   reading. This should be folded back into how BACKLOG item 18 and FINDINGS summarize Q023.
2. **Correct the propagation into FINDINGS/BACKLOG before this result is cited.** The one-liner should be
   "FCS's `TransparentCompiler` does incremental compilation-tail invalidation under the reentrant
   callback; per-edit whole-project recheck cost is positional (worst for early files), not unconditional
   O(N)" — not "edit-one confirms (b), cost concern real."
3. **Test the dependency-chained variant the design explicitly deferred.** With real cross-file `open`s,
   a head edit's tail-recheck is *justified* invalidation rather than the conservative order-based kind;
   comparing chained vs independent at the same edit position would separate "conservative order
   invalidation" from "genuine dependency invalidation" and is the shape closest to Myriad's own
   cross-generator visibility (Q002/Q010).
4. **Re-examine the SHIP threshold design for any future scale quartet.** A ratio-shrinks bar is
   unmeetable whenever the baseline carries a large fixed intercept; a marginal-rate (slope) comparison
   is the right operationalization and should be pre-registered as such next time.
