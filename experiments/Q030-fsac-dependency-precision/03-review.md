# Q030-fsac-dependency-precision / Movement 4 — Adversarial review

Independently restored FSAC (`dotnet tool restore`, confirmed `0.83.0+96fabed8...` actually running —
the same pin as Q022/Q029; SDK 9.0.310) and reran `q030-fsac-precision.fsx` from scratch into a fresh
scratch dir with the full command (`30 2 7,16,25 2,25,27 3`). The result reproduces exactly on the first
try: Hub edit = **28** distinct files (all of File0002..File0029) across 3/3 reps, dependent edit (25) =
**5** ({25,26,27,28,29}), unrelated edit (27) = **3** ({27,28,29}). I then did the thing this quartet's
own design says is where the claim is really settled — recounted the `documentAnalyzed` set by hand from
the raw transcript, independently regenerated — and read the generated F# source off disk to confirm the
topology rather than trusting the write-up. Everything holds. Unlike most quartets reviewed this way,
there is no overclaim to strike; the one thing I'd add strengthens the conclusion rather than qualifying
it.

## What reproduced

**Topology, verified from generated source (not the write-up).** `grep -l "File0002.total"` over the
generated files returns exactly `File0007`, `File0016`, `File0025` and nothing else; every other file's
`total` line ends `+ 0`, including Hub itself (`File0002`) and the two pre-Hub files. The three
dependents are genuinely scattered and interleaved among unrelated files (7, 16, 25 sit inside the runs
3-6 / 8-15 / 17-24 / 26-29), so H_precise (~4 files) and H_conservative (~28 files) predict sets that
differ by 24 files, far above any noise floor. The edit is value-only by construction: `mkFileSrc`
changes only a string-literal element (`"%s"`) inside the 20-record `records` comprehension; `total`'s
exported type stays `int` (the `+ 0` tail and the `Meta.Count` fold are untouched, and the 4-element
`tags` list never dedupes because markers never collide with `"a"`/`"b"`/`string k`). Precondition 2, 3,
4 met.

**Per-file event multiplicity, hand-counted from the raw transcript.** Grouping every
`documentAnalyzed` in `q030-N30.raw.log` by file gives the decisive picture, cleaner than the aggregate
CSV:

- **File0000, File0001 (pre-Hub prefix): exactly 1 event each, ever** — their initial `didOpen`
  analysis, never once more across all 10 edit cascades. This is the load-bearing negative control and
  it is airtight: the cascade is a genuine compilation-order *suffix*, not a position-blind global
  refresh.
- **File0003..File0024 (mix of unrelated and the 7/16/25 dependents): ~5 events each** = 1 initial + 4
  Hub cascades (warm-up + 3 reps). The 24 unrelated files re-analyze on every Hub edit exactly like the 3
  real dependents — no observable difference between a file that references Hub and one that does not.
- **File0028, File0029: 11 events each** = 1 initial + 4 Hub + 3 (p=25 cascade) + 3 (p=27 cascade). They
  are order-successors of all three edit positions, and the arithmetic closes exactly. File0026 = 8
  (initial + 4 Hub + 3 from p=25), File0027 downstream of p=25 and p=27 both. Every count reconciles to
  "fires once per edit whose position precedes it in compilation order, plus one initial."

`successors+1` holds at all three edit positions: Hub@2 → 27+1=28, dependent@25 → 4+1=5, unrelated@27 →
2+1=3. Cascade arrives in strict compile order with monotonic offsets (~6-13ms/successor, matching
Q029), unrelated files at full cost indistinguishable from dependents.

## Strongest objections

### 1. Is "all order-successors re-analyze" real FCS invalidation, or the harness re-requesting each file? — Real FCS, and the harness structurally cannot be the cause.

This is the measurement-vs-mechanism confound this lineage has been burned by (Q021's "always fully
re-checks" struck for exactly this reason). It does not apply here. `editCollect` sends `didChange` +
`didSave` on the **edited file only**; it issues no hover, no per-file re-open, no analysis request on any
successor. The 27 suffix `documentAnalyzed` events are therefore emitted by FCS of its own accord, not
solicited. The negative control seals it from the other side: if `didSave` triggered a blanket
project-wide refresh, editing File0025 would re-fire File0000..File0029 (all 30); instead it fires exactly
{25..29}, and File0000/File0001 never fire on a Hub edit. A blanket-refresh mechanism cannot produce a
strict suffix that starts precisely at the edited file's index and moves with it across three positions.
Both the "harness re-requested it" and the "didSave = global refresh" alternatives are ruled out by the
data, not by assertion.

### 2. Could the suffix cascade be cheap re-parse pings rather than genuine re-typecheck? — Inherited from Q029, corroborated by cost, but Q030 does not independently re-verify it per-file (the one honest scoping gap).

Q030 leans on Q029's review having established that `documentAnalyzed` tracks real re-analysis 1:1 (in
the linear chain the last file provably cannot be analyzed without the edited slot, and freshness hovers
resolved `true` with real latency). That inheritance is reasonable and the ~6-13ms/successor inter-arrival
cost here is the same order as Q029's confirmed-real per-file cost, so these are not zero-cost
notifications. But Q030 itself runs no freshness hover on the 27 suffix files to prove each was
semantically re-checked (not merely re-parsed) in *this* topology. This is the only place the finding
rests on prior work rather than its own transcript. It does not change the verdict — the event set is the
invalidation set under Q029's established 1:1 property, and the finding is a set-membership claim the
cost corroborates — but a future spike wanting to be fully self-contained would add per-successor
freshness hovers. Noted so a citation does not silently upgrade "27 files re-analyzed" past what this
quartet alone proves.

### 3. Does "compilation-order-conservative" name a fixable FSAC limitation, or F#'s actual semantics? — The latter, and this makes the pessimistic conclusion more durable than the executor argued.

The write-up frames the result as FSAC/FCS *choosing* order-conservatism over dependency-precision, which
reads as a limitation a smarter FCS might one day relax. The mechanism is stronger than that and worth
stating: F# compiles files in order and every file is checked against the accumulated signature
environment of **all** preceding files. `File0027` can write `File0002.total` with no `open` and no
explicit reference edge, because module `File0002` is in scope for every later file by construction. So
"does File0027 actually depend on File0002" is not answerable without re-checking File0027 — the
would-be optimization H_precise describes costs exactly the work it tries to avoid. The compilation-order
suffix *is* the genuine dependency set in F#'s ordered-file model; H_precise is not merely unobserved, it
is close to ill-defined for F#. This means the pessimistic Myriad reading is not "true in FSAC 0.83.0,
watch for a fix" — it is structural. The executor didn't articulate the why; stating it removes the
implicit hope that a later FCS relaxes this. This is an addition in the finding's favor, not a
correction against it.

### 4. Does it generalize past N=30, hub@2, 3 dependents? — Not swept, but the three-position control substantially de-risks it, and objection 3's mechanism explains why.

Fair scope challenge, and the lineage's own history (Q023's single edit-position mistake) says take it
seriously. Q030 tests one N, one hub position, one dependent count. What guards against a Q023-style
artifact is that the rule was independently confirmed at **three** different edit positions with
**different** reference relationships in the same session: Hub@2 (referenced by 3), dependent@25
(referenced by nothing), unrelated@27 (referenced by nothing). The dependent-edit control is the sharpest
single fact in the quartet — File0025 is referenced by no file, yet editing it still cascades to its
entire order-suffix {25..29}, proving the rule is purely positional and reference-blind, at a position
unrelated to Hub. Combined with the mechanism in objection 3 (position *is* the dependency graph in F#),
the generalization risk is low. What is genuinely not shown: behavior at large N, whether a hub at index 0
vs mid-project changes anything (it should not, by the mechanism), and any non-generated real project.

### 5. JIT-tax / keepAssemblyContents / first-check confounds (Q027/Q028)? — Not applicable; this is set-membership, not timing.

The Q027/Q028 first-check tax inflates absolute ms on the first typecheck in a process. Q030's headline
is *which files fire*, not *how long*, so the tax cannot change the answer even in principle. The design
controls it anyway (warm-up edit discarded, all measurement after `workspaceLoad` + `didOpen`-all), and
the offsets are post-warmup single-to-low-double-digit ms. There is no two-checker comparison here, so no
`keepAssemblyContents` mismatch is possible — this is FSAC's own single internal checker. This is the one
lineage-wide caution the quartet gets right for free.

## Verdict: SHIP, scoped

The pre-registered question — does a value-only edit to an early file re-analyze only its true transitive
dependents (H_precise) or the whole compilation-order suffix (H_conservative) — is answered
**H_conservative**, cleanly, stably (3/3 reps + independent full rerun), and it survives a hostile
hand-recount of the raw transcript and a from-source verification of the topology. The headline claim
(compilation-order-conservative, not dependency-precise) is correct as stated. This resolves the
order-vs-dependency ambiguity that Q023, Q024, and Q029 each disclosed and named as their top follow-up,
and resolves it on the pessimistic side for Myriad: an early attributed type pays the full order-suffix
re-check cost regardless of how few files truly reference it. It confirms and generalizes Q029's caveat
rather than relieving it, and overturns nothing — Q029's cost *curve* stands; Q030 only fixes the
identity of its x-axis to "order-successors," which is now the correct model for a real Myriad DAG, not an
over-pessimistic proxy.

SHIP, not REVISE, because the executor's conclusion is right, independently reproduced, and honestly
scoped in `02-results.md`'s own Limits section (one FCS version, one generator, N=30, value-only edit,
single machine). There is no framing overshoot to trim of the kind Q029's "N-invariant" or Q023's
edit-position artifact — a rare clean pass for this discipline, and I looked hard for the overclaim rather
than assuming its absence. The single caveat (objection 2) is an inheritance from Q029, not a defect, and
the single addition (objection 3) makes the result more durable, not less.

### What travels with this verdict, scoped

- **SHIPs:** In a live `fsautocomplete` 0.83.0 session, a value-only edit to a file re-analyzes the entire
  compilation-order suffix after it — every order-successor, whether or not it references the edited file
  — and nothing before it. Verified at three edit positions including a file referenced by nothing;
  confirmed by a prefix-silent negative control (File0000/File0001 fire exactly once, ever) that rules out
  both harness-solicited re-analysis and a blanket `didSave` refresh. For Myriad: attributed types sitting
  early in build order pay the full order-suffix cost even with few real dependents. This is F#'s
  ordered-file compilation semantics, not a fixable FSAC quirk (objection 3), so treat it as structural.
- **Does NOT travel:** any claim that each of the 27 suffix files was independently proven
  re-*typechecked* (not re-parsed) in this topology — that rests on Q029's 1:1 property, corroborated by
  cost but not re-verified per-file here (objection 2); any absolute-latency claim (that is Q029's
  measurement, not re-litigated); any N-scaling or hub-position-invariance claim (one N, one hub position
  tested — the mechanism predicts invariance but the sweep was not run).
- **Still genuinely open:** whether a *signature-changing* edit to an early type behaves differently
  (untested; value-only only); large-N and real (non-generated) multi-hundred-file projects; and the
  standing lineage gap — whether hosting Myriad's own generation inside this path is keystroke-cheap,
  which Q022 left unsolved and this quartet does not touch. Nothing here changes that: it characterizes
  the cost an early-sitting attributed type would pay, and confirms it is the expensive end.
