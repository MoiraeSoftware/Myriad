# Q029-fsac-live-editing-cost / Movement 4 — Adversarial review

Independently restored the FSAC 0.83.0 tool (`dotnet tool restore`, confirmed
`0.83.0+96fabed8...` actually running, matching Q022's pin) and reran `q029-fsac-cost.fsx` from
scratch into fresh scratch dirs: the cheapest falsifier (N=20, positions 0/19, 3 reps), the full
N=20 sweep, and the N=40 scale check. Every number reproduces within run noise. I then went past
the CSVs to the one place a claim like this hides its artifacts — the raw LSP transcript
(`q029-N20.raw.log`, the executor's own `Q029_RAW=1` run, which my reruns did not touch) — and
decomposed the `documentAnalyzed` cascade by hand. The headline holds, and the mechanism check is
stronger than the executor claimed: this is the best-controlled real-FSAC measurement in the
lineage so far, because the cascade transcript gives a direct, positional control that Q022's
binary-appearance test never had.

Independent reproduction, medians (executor's in parentheses):

| successors | settleMs N=20 (exec) | settleMs N=40 (exec) |
|---|---|---|
| 0 | 7 (9) | 8 (6) |
| 5 | 50 (58) | — |
| 9 | 76 (77) | 70 (63) |
| 14 | 129 (127) | — |
| 19 | 137 (118) | 139 (126) |
| 29 | — | 186 (172) |
| 39 | — | 248 (243) |

Monotonic, position-dependent, ~6ms/successor, reproduced on a second machine-session. Falsifier
ratio (p0/p19) reproduced at ~15x (my 136/9) against the executor's 13-24x. Nothing in the raw
numbers is in question.

## Strongest objections

### 1. Does `documentAnalyzed` for the last file mean real successor re-checking, or a notification that fires regardless? — DECISIVELY the former, and this is the finding's load-bearing control.

This is the objection the whole quartet lives or dies on, and the raw transcript settles it cleanly.
If `documentAnalyzed` re-fired for all open files on any edit (a debounce/refresh side-effect rather
than dependency-driven re-analysis), editing the last file would still cascade through
File0000..File0019. It does not. Parsing `q029-N20.raw.log`, the cascade is strictly gated on edit
position:

- p=10 edit → re-analyzes File0010 (twice), then File0011..File0019 in compilation order. 10 files.
- p=14 edit → re-analyzes File0014 (twice), then File0015..File0019. 6 files.
- p=19 edit → re-analyzes File0019 only. 1 file.

The number of `documentAnalyzed` events after each edit equals `successors + 1` exactly, every time,
and the events arrive in compilation order with the last file last (~4-13ms apart per file). A refresh
that fired regardless of position could not produce this. The dependency chain is genuinely real in
source (verified: `File0001.total = ... + File0000.total`, every file references the previous file's
`total`), so the last file's re-analysis provably cannot complete without the edited file's slot being
re-checked. Validity precondition 4 (signal measures downstream/successor cost) is met, and the
"stale-cache false reading" concern the design named is answered directly — the tail is re-typechecked,
not served from cache. All freshness hovers resolve `true` with flat ~6-16ms latency. This is a
better mechanism control than the executor's write-up leans on; it should be the headline evidence,
not a footnote.

### 2. "Per-successor rate is N-invariant" overreaches on two N values and three shared points — scope it to "no detectable drift across N=20 and N=40," exactly the correction Q024 took.

`02-results.md`'s headline says "independent of N," and `quartet.json` says "N-invariant." The
supporting evidence is three overlapping successor counts (0, 9, 19) across exactly two N values. That
is thinner than Q024's own five-N regression, and Q024's review already knocked "essentially
N-invariant" down to "no detectable drift" on the grounds that a handful of points cannot support an
invariance claim. The same discipline binds here, harder: two N values cannot establish invariance of
a slope, only consistency. The overlapping points are genuinely close (succ=9: 76 vs 70 in my run,
77 vs 63 in the executor's; succ=19: 137 vs 139 mine, 118 vs 126 executor's), so "no detectable
systematic drift across the two sizes tested" is well-supported and citable. "N-invariant" as a proven
property is not. The body of `02-results.md` actually uses the correct "no detectable drift" language;
the headline and `quartet.json` verdict do not, and those are what a downstream citation copies. Trim
them.

### 3. `settleMs` (LSP wall-clock to an event) is not the same signal as Q023/Q024's `ParseAndCheckProject` ms — but the executor scopes this correctly, and the confound is bounded small.

Q023/Q024 time an in-process `ParseAndCheckProject` call; Q029 times didChange→last-file
`documentAnalyzed` over stdio LSP, which layers serialization, IPC, and any FSAC debounce on top. The
right question is whether that overhead lives in the *slope* (which would corrupt the position
dependence) or the *intercept* (which would not). The evidence says intercept: the successors=0 point
is ~6-9ms and the hover round-trip is a flat ~6-16ms regardless of position, so the fixed LSP tax is
~single-digit ms and constant, absorbed into the intercept. The position-dependent term (the ~6ms per
successor) is genuine downstream re-analysis — confirmed independently by the per-file inter-arrival
gaps inside the cascade (~4-13ms each), which are the same quantity measured a different way and are
not IPC. So "same shape and scaling" is a sound claim; the executor is right to say the absolute ms
are *not* comparable to Q024's fitted slope and to make no such comparison. This is correctly scoped
already — noted here only so a citation does not silently upgrade "same curve shape" to "same cost."

### 4. Does the Q027/Q028 first-check JIT/FCS-init tax contaminate the "first file ~120-243ms" numbers? — No, and unlike Q023's cold measurement it structurally cannot.

Q027/Q028 found a ~400-500ms one-time tax on the first typecheck in any FCS process, and it inflated
Q023's *cold* baseline because that baseline WAS a first-in-process check. Q029 is immune by
construction: by the time the first measured edit fires, the process has already run `workspaceLoad`,
`didOpen` on all N files (full initial analysis, which the log shows settling ~11s in), and a discarded
warm-up edit. Every measured value (6-248ms) sits an order of magnitude below the tax scale, and the
warm-up settle times that DO carry residual first-touch cost are visibly larger and discarded
(N=20 warm-up 290ms, N=40 warm-up 501-530ms). One residual artifact remains and is handled correctly:
the position-0 rep-1 sample occasionally spikes (executor's 283ms vs its own 118/118; my reruns showed
no such spike, 118/137/140), a GC/residual-warm effect the median absorbs — the executor flagged it as
an outlier rather than burying it. The tax is paid outside the measurement window. This is the one
lineage-wide caution (Q028's) that this quartet actually gets right by design.

### 5. The linear-chain topology conflates "compilation-order successors" with "actual dependents" — an inherited limitation, and it cuts toward the pessimistic Myriad reading, not away.

Because every file references the immediately previous file's `total`, editing file p makes
compilation-order-successors and true transitive-dependents identical by construction. So Q029 (like
Q023/Q024) cannot distinguish "FSAC invalidates the whole compilation-order suffix" from "FSAC
invalidates only true dependents (which here happen to be the whole suffix)." A real Myriad project is
a DAG where an early attributed type may have far fewer actual dependents than order-successors; if
FSAC were dependency-precise, editing such a type could be much cheaper than this worst-case chain
suggests. Q029 does not resolve which regime FSAC is in — this is disclosed ("one linear
dependency-chain topology; a wide/shallow DAG was not tested") and is inherited from Q023/Q024, not a
new overclaim. What Q029 *does* add on this axis strengthens the pessimistic reading: the edit is
value-only (the edited file's exported signature is byte-identical before/after — only a string
literal inside `records` changes, and `total`'s type is untouched), yet FSAC still re-checks the entire
tail. That means FSAC/FCS invalidates on file content, not on whether the exported signature actually
changed — so even a trivial edit to an early file pays the full successor cost. For Myriad, whose
attributed domain types sit early in build order, that is the expensive end of the curve, confirmed
live.

## Verdict: SHIP, scoped

The pre-registered question — does FSAC's real, LSP-driven, per-file incremental editing path reproduce
Q023/Q024's position-dependent, linear-in-successors cost curve — is answered YES (H1), soundly, and
survives independent rebuild and a hostile read of the raw transcript. The core mechanism claim is not
just reproduced but better-controlled than the executor argued: the position-gated cascade directly
rules out the "fires regardless / stale cache" failure mode that would have made this an H2
inconclusive. This is the first per-edit-cost-vs-position measurement against a literal
`fsautocomplete` process in the lineage, and it closes Q023's and Q024's most-cited never-run
follow-up on the favorable side (the cost model does transfer to the real editor).

It is SHIP, not REVISE, because the executor's headline conclusion is correct and independently
reproduced, and because `02-results.md` already discloses the right scope in its body (small N, small
files, single topology, single session, curve shape not absolute latency, "no detectable drift"). The
one framing to trim — "N-invariant" → "no detectable drift across the two N tested" — is a
headline/`quartet.json` wording overshoot of the kind Q024 corrected without reversing its own SHIP,
not a wrong result. Nothing false is asserted as proven.

### What travels with this verdict, scoped

- **SHIPs:** FSAC's real per-file incremental path exhibits the same position-dependent,
  linear-in-successors re-analysis cost Q023/Q024 measured via `ParseAndCheckProject`. Editing the last
  file settles in ~6-9ms; the first file costs ~120ms (N=20) / ~243ms (N=40). The `documentAnalyzed`
  cascade is genuinely dependency-gated (successors+1 files re-analyzed, in compilation order),
  verified in the raw transcript — not a position-blind refresh. Cite this as the cost model
  *transferring to a real LSP editor session*, closing the Q023/Q024 follow-up.
- **Does NOT travel:** "N-invariant per-successor rate" as a proven property (two N values, three shared
  points support only "no detectable drift"); any comparison of Q029's absolute `settleMs` to Q024's
  fitted ms/successor slope (different signals — LSP settle vs raw checker time); any claim about
  whether FSAC invalidates by compilation order or by precise dependency (the topology forces them
  equal and cannot discriminate).
- **Still genuinely open:** absolute keystroke cost on a real large solution (N≤40, small though
  genuinely-weighted files — this is curve shape/scaling, not a headline latency); wide/shallow DAG
  topologies where order-successors exceed true dependents; whether `didChange`-only keystroke-rate
  edits (no save) debounce differently; and — the standing lineage gap — whether hosting Myriad's own
  generation inside this path is keystroke-cheap, which Q022 showed is a separate, unsolved question
  (generation-on-edit never fired live in a running FSAC session).

## Follow-ups (for `BACKLOG.md`, if the frontier keeps moving)

1. The one unresolved axis worth a cheap spike: build a *wide/shallow* dependency shape (one early
   file that only a few later files reference, with unrelated files between) and check whether editing
   the early file re-analyzes only its true dependents or the whole compilation-order suffix. That is
   the single test that would tell whether Myriad's "attributed types sit early = expensive" caveat is
   as bad as the linear chain implies, or softened by dependency-precise invalidation. Q023/Q024/Q029
   all inherit this blind spot.
2. If a real large-solution absolute-latency figure is ever wanted, it needs a real (not generated)
   multi-hundred-file F# project driven through the same harness; the generated small-file shape here
   deliberately measures scaling, not magnitude.
