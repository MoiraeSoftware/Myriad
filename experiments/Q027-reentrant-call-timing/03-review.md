# Q027-reentrant-call-timing / Movement 4 — Adversarial review

Independently rebuilt `q027-spike` from clean (`dotnet build ... -c Release`) and reran it as 5
separate fresh `dotnet run --no-build` processes. The three-way pattern reproduces essentially
byte-for-byte: reentrantA ~515-517ms, coldA ~118-120ms, reentrantB ~67-72ms, coldB ~120-145ms,
warmA/B 2-3ms, `reentrantCallbackFired=true` every run. The frozen result is real and stable; nothing
in `02-results.md`'s raw numbers is in question.

I then did what `02-results.md`'s own "what a review should press on" section explicitly invited (it
named the direct-first-use control as the missing measurement) and built a control spike,
`artifacts/q027-control/` (same FCS `43.9.101` pin, same `mkSrc` weight, 7 modes, each run as 5 fresh
processes; raw log `artifacts/review-logs/control-runs.txt`). The controls decompose reentrantA's
516ms and settle the A-vs-B asymmetry the executor left as open inference. The load-bearing conclusion
survives and is strengthened. The secondary framing does not, and that is the headline of this review.

## Strongest objections

### 1. HEADLINE: "reentrantA is 4.2x *more expensive* than cold" is almost entirely a measurement artifact — reentrantA is the very first typecheck in the process and eats one-time JIT/FCS-init cost that coldA (run 5th, fully warm) never pays. Reentrancy does not add cost; it *removes* it.

The spike's own execution order makes reentrantA the **first** `ParseAndCheckFileInProject` performed
in the process (it fires during the first `ParseAndCheckProject`), while coldA is the 5th-plus check,
by which point process-global JIT, FCS static initialization, and referenced-assembly metadata reads
are all already paid. That is not "which checker instance and whether other files were processed" (the
design's claimed single variable) — it is a straight position-in-process confound, and it dominates.

Control `warmup-reentrant` pays that one-time cost up front with a throwaway full project check on a
separate checker, then runs the identical reentrant sequence on `checker1`:

| condition | reentrantA | reentrantB |
|---|---|---|
| baseline (reentrantA is 1st check in process) | ~515ms | ~84ms |
| after a throwaway warmup project | **~48ms** | **~17ms** |

About 91% of reentrantA's 516ms evaporates once the process is warmed. It was never reentrancy cost.

Control `direct-first` drives the point home from the other side. Create `checker1` exactly as the
spike does (`keepAssemblyContents=true`, same reentrant `DocumentSource.Custom`) but call
`ParseAndCheckFileInProject(A)` **directly** as the first operation — no outer `ParseAndCheckProject`,
no reentrancy at all:

| condition | first check of A |
|---|---|
| baseline reentrantA (reentrant, inside outer call) | ~515ms |
| direct-first directA (non-reentrant, same cold checker) | **~929ms** |

A direct, non-reentrant first check on the same checker costs **~410ms more** than the reentrant one.
So being reentrant does not layer overhead on top of first-touch cost — the executor's inference
("`reentrantA` costs more than cold... consistent with genuine reentrancy overhead on top of ordinary
first-touch cost") is backwards. The outer `ParseAndCheckProject` has already begun building the shared
project snapshot / resolving references by the time file C's callback fires, so the reentrant call
inherits that partly-built state and is *cheaper* than a from-absolute-cold direct check. The correct
reading of "reentrantA (516) > coldA (124)" is not "reentrancy is expensive"; it is "the first check in
any process is expensive, and the spike happened to make the reentrant call be that first check while
making the cold baseline a warm-process check."

### 2. The cold baseline is not apples-to-apples: it uses `keepAssemblyContents=false`, the reentrant/warm checker uses `true`. Small, but it is a second uncontrolled variable the design claimed it had eliminated.

`checker1` is created with `keepAssemblyContents = true` (line 85); both cold checkers are created
without it (lines 121, 123), i.e. the default `false`. `02-results.md` asserts "the *only* difference
between cold and reentrant/warm is which checker instance and whether other files were already
processed — not project shape." That is not quite true — `keepAssemblyContents` differs too. Control
`cold-kac` measures the cost, both after warmup:

| cold check of A | ms |
|---|---|
| `keepAssemblyContents=true` (matches checker1) | ~148ms |
| `keepAssemblyContents=false` (what the spike measured as "cold") | ~122ms |

~26ms, roughly 20%. Minor next to objection 1's ~460ms, but it means the nominal "cold" number the
whole comparison hangs on is itself measured against a checker configured differently from the one
under test. A fair cold baseline (kac=true, warm process) is ~148ms, and `direct-first-warm` confirms
`checker1`'s own first warm check lands there too (~148ms). Both switching cold from
`DocumentSource.Filesystem` to `DocumentSource.Custom` (the disclosed correction) *and* the kac flag
sit on this axis; the docsource switch turns out immaterial (the reentrant checker's own fileA branch
returns identical text), the kac flag does not.

### 3. The A-vs-B asymmetry the executor flagged as unexplained inference is a property of call ORDER, not file identity — and the corrected magnitudes fully vindicate "cache-hit ruled out" while landing squarely on the pre-registered REVISE outcome, not either clean bound.

`02-results.md` puzzled over why reentrantA > cold but reentrantB < cold, offering "checker-instance
first-touch cost, partially amortized between calls" as disclosed-as-unproven inference. Control
`reverse` (check B first, then A, inside C's callback) settles the direction:

| order | 1st reentrant call | 2nd reentrant call |
|---|---|---|
| baseline `[A; B]` | A = ~515ms | B = ~84ms |
| reverse `[B; A]` | B = ~597ms | A = ~10ms |
| third `[A; B; D]` | A = ~528ms | B = ~82ms, D = ~24ms |

Whichever file goes first pays the bulk one-time cost; whichever goes second (or third) rides the
amortization. It is order, not A-ness or B-ness. The executor's amortization *direction* was right; what
they misattributed is *what* is being amortized — not "reentrancy overhead" or a per-file property, but
the same process-global + per-instance one-time cost objection 1 isolates.

Crucially, this does **not** rescue the benign "cache hit / already-resolved by the SourceFiles walk"
hypothesis the quartet set out to kill. That hypothesis predicts near-warm cost (2-4ms) regardless of
order. Instead:
- warmed reentrantA/B = 48/17ms — an order of magnitude above warm (2-4ms) on the same instance;
- the 3rd reentrant call (`third`, D) = ~24ms, still ~8x warm, monotonically decreasing but never
  collapsing to warm;
- reverse's 2nd call (~10ms) is the closest any reentrant call gets to warm and is still ~3-5x it.

So "the reentrant call lands on already-cached/idle typecheck state, indistinguishable from a warm
repeat" is decisively ruled out — and it stays ruled out after every confound in objections 1-2 is
removed. That conclusion is the quartet's real, durable finding, and it is *more* robust than
`02-results.md` showed (they only demonstrated it through the confounded first-in-process numbers; the
controls show it holds warm, at the 3rd call, and under reversed order). The task's suggested "different
kind of cache" alternative (per-instance JIT/assembly-load state rather than per-project typecheck
state) turns out to be exactly what inflated the raw magnitudes — but it explains the *size* of
reentrantA, not the rule-out, because stripping it out still leaves 17-48ms of genuine per-instance
work well above warm.

The honest corrected picture matches the design's own pre-registered **REVISE** trigger verbatim:
"reentrant sits meaningfully between cold and warm, not close to either." With the process warmed,
both reentrant calls (17-48ms) sit between warm (2-4ms) and cold (~122-148ms). That is the result;
the raw-number "reentrant *above* cold" framing was an artifact.

## Verdict: REVISE

The pre-registered question — is the reentrant call a cheap cache hit or genuinely fresh work — is
answered soundly and the answer (fresh work; cache-hit decisively ruled out) survives independent
reproduction and every control I added. The mechanism claim this sub-lineage cares about is intact and
sharper: a `DocumentSource.Custom` callback reentrantly checking an earlier virtual file, from inside
the unreturned outer `ParseAndCheckProject`, does real per-instance typecheck work (17-48ms warmed,
monotonically decreasing with call order), never a 2-4ms cache hit. Q010/Q021/Q023/Q024/Q026's
"reentrant does real work, not a free ride on already-resolved state" reading is confirmed.

It is REVISE, not SHIP, because `02-results.md`'s headline framing outruns its evidence in two specific
ways that a citing write-up must not carry forward:

1. **"reentrantA is 4.2x more expensive than an isolated cold check... outside what either
   pre-registered outcome anticipated... consistent with genuine reentrancy overhead" is not
   supported and is contradicted by controls.** ~91% of that number is first-typecheck-in-process
   one-time cost (JIT / FCS static init / referenced-assembly reads) that the cold baseline, run warm
   later in the same process, never pays; a further ~20% of the residual gap is an uncontrolled
   `keepAssemblyContents` mismatch. A *direct*, non-reentrant first check on the same checker costs
   ~929ms — more than the reentrant path — so reentrancy lowers first-touch cost (the outer call
   pre-warms the shared snapshot), it does not add overhead. The correct one-liner is "warmed, the
   reentrant call sits between warm and cold," not "the reentrant call is more expensive than cold."

2. **The A-vs-B asymmetry is a call-order amortization effect, now demonstrated (reverse: B-first
   ~597ms, A-second ~10ms), not an open mystery about A vs B.** The executor's amortization guess was
   directionally correct but misnamed the amortized quantity as reentrancy/first-touch overhead.

It is not KILL because nothing false was *asserted* as proven: `02-results.md` explicitly disclosed the
mechanistic explanation as "inference, not direct observation," flagged the exact controls I ran as the
missing work, and its load-bearing conclusion is correct. This is Movement 4 doing precisely its job —
resolving a disclosed-open inference and correcting a magnitude framing — not overturning a shipped
claim.

### What travels with this verdict, scoped

- **SHIPs (unchanged, strengthened):** the reentrant `DocumentSource.Custom` call does genuine
  per-instance work, never a warm cache hit; every "already resolved before the callback fired /
  cheap cache hit" reading of this lineage is ruled out, robustly, warm and across call order and to a
  3rd call.
- **Does NOT travel:** any claim that the reentrant call is "more expensive than a cold check," that
  reentrancy imposes measurable overhead, or that A-vs-B cost reflects anything other than call order.
  Cite the warmed numbers (~17-48ms, between warm and cold), not the raw first-in-process ~516ms.
- **Still genuinely open (unchanged from the frozen scope):** whether the outer walk is literally
  concurrent with the callback (no thread/stack instrumentation added, by design); cross-file-dependent
  content; scale beyond 3-4 files; anything about a real FSAC editing session. This is a hand-typed toy
  cost characterization, orthogonal to Q023/Q024's scale model and Q026's real-generator composition.

## Follow-ups (for `BACKLOG.md`, if the frontier keeps moving)

1. This quartet's timing lesson generalizes to every prior quartet in the lineage that reported a
   single "first check" latency: the first typecheck in any FCS process carries a ~400-500ms one-time
   JIT/init tax that has nothing to do with the quantity being measured. Any future timing spike here
   must warm the process first (a throwaway `ParseAndCheckProject`) before its first timed number, or
   explicitly report position-in-process. Q023/Q024's cost models should be spot-checked for whether
   their first data point ate this tax.
2. If a genuine mid-flight-vs-warm-contention answer is ever wanted (the deeper question behind Q010's
   Objection 1), wall-clock cost cannot give it — the controls here show cost is dominated by one-time
   setup and order amortization, both benign. It needs FCS-internal thread/queue instrumentation, the
   heavier lift the cheapest-falsifier framing deliberately declined.
3. The `direct-first` finding (reentrant path cheaper than a direct first check because the outer call
   pre-builds the shared snapshot) is mildly encouraging for the FSAC-sidecar / persistent-host vision
   (item 18/22): reentrant prefix queries ride an already-warm project snapshot rather than paying
   fresh setup. Worth confirming that property holds on FCS's real incremental per-file path, not just
   `ParseAndCheckProject` on independent files — the same "cost model vs real session" gap Q024's review
   named as its top follow-up.
