# Q027 — Movement 3: Execute + write up

## What was built

Exactly the design in `01-design.md`: `artifacts/q027-spike/`, a single console app with three
virtual files (A, B, C) built from Q023's own `mkPrefixSrc` template (genuine typecheck weight, not
padding), a persistent checker (`checker1`) whose `DocumentSource.Custom` callback times two
reentrant `ParseAndCheckFileInProject` calls (to A, then B) from inside file C's source-request
callback, plus a post-check warm repeat and two cold single-instance baselines.

## Corrections discovered while running (reported honestly, per this repo's own discipline)

1. **First attempt crashed**: a "cold" checker with default `DocumentSource.Filesystem`, given the
   real 3-file `opts`, tried to read the *other* project files from actual disk (they only exist as
   in-memory strings) even though explicit source text was supplied for the one file being timed —
   `TransparentCompiler`'s snapshot construction needs every `SourceFiles` entry resolvable, not
   just the one under test. `DirectoryNotFoundException` on `C:\virt-q027\A.fs`, 5/5 runs.
2. **First fix (shrink cold's project to a single file) was rejected before running**, caught while
   writing the fix: that would confound "which checker instance" with "project shape" — two
   variables instead of one, defeating the design's own stated goal of isolating just the checker
   instance. Fixed instead by giving each cold checker the identical 3-file `opts` plus a **static**
   `DocumentSource.Custom` (returning each file's known text, no reentrant logic needed since a cold
   checker never touches file C) — same project shape as reentrant/warm, only the checker instance
   and its prior-use history differ.

## Real result (5 independent fresh processes, all exit 0)

| Run | reentrant A (ms) | warm A (ms) | cold A (ms) | reentrant B (ms) | warm B (ms) | cold B (ms) |
|---|---|---|---|---|---|---|
| 1 | 516 | 2 | 123 | 67 | 3 | 123 |
| 2 | 528 | 2 | 122 | 74 | 3 | 128 |
| 3 | 518 | 2 | 129 | 75 | 4 | 155 |
| 4 | 527 | 2 | 124 | 72 | 4 | 126 |
| 5 | 526 | 2 | 125 | 82 | 4 | 137 |
| **median** | **526** | **2** | **124** | **74** | **4** | **128** |

Full logs: `artifacts/run-logs/run-1.txt` through `run-5.txt`.

`reentrantCallbackFired = true` on all 5 runs (no silent `DocumentSource.Custom` bypass, Q010's own
named footgun).

Ratios (median): `reentrantA/coldA = 4.24`, `reentrantA/warmA = 263`, `reentrantB/coldB = 0.58`,
`reentrantB/warmB = 18.5`, `warmA/coldA = 0.016`, `warmB/coldB = 0.031`.

## The pre-registered "two clean outcomes" framing turned out to be a false dichotomy — a third,
## more informative pattern showed up instead, tight and reproducible across all 5 runs

`00-hypothesis.md`/`01-design.md` named exactly two clean, falsifiable outcomes before running:
"reentrant ≈ warm, both ≪ cold" (benign cache-hit explanation) or "reentrant ≈ cold, both ≫ warm"
(genuine fresh-work explanation). **Neither holds precisely.** What actually happened:

- **`reentrantA` (526ms median) is not close to `coldA` (124ms) — it is 4.2x *more expensive* than
  an isolated, fresh-instance cold check of the identical file.** This is outside the space either
  pre-registered outcome anticipated (both assumed reentrant would land *at or below* cold, never
  above it).
- **`reentrantB` (74ms median) sits *below* `coldB` (128ms)** — roughly 0.58x — closer to (but still
  clearly distinguishable from) the "matches cold" outcome, on the cheap side rather than the
  expensive side.
- **`warmA`/`warmB` (2ms/4ms median) are utterly distinct from both** — 20-260x cheaper than their
  respective reentrant calls, and this part of the result is unambiguous and highly reproducible
  (2-4ms every single run, no exceptions).

## What this settles, precisely, and what it doesn't

**Decisively ruled out: "the reentrant call lands on already-cached/idle state, indistinguishable
from a warm repeat."** This is the specific, more-benign explanation Q010's own review (Objection
1) named as equally consistent with that quartet's own aggregate timing. It is not consistent with
this quartet's per-call instrumentation: if FCS's own `SourceFiles`-order internal walk had already
fully resolved A and B by the time it requested C's source text, both reentrant calls should cost
close to `warmA`/`warmB` (2-4ms) — a cache hit is cheap regardless of which file is checked first.
Instead both reentrant calls cost tens to hundreds of milliseconds, two orders of magnitude above
warm, every single run. **The reentrant calls are doing real, non-cached work when they run**, not
free-riding on work FCS already finished elsewhere.

**Not fully settled, and reported honestly as an open, secondary question this quartet did not
pre-register a clean way to interpret:** *why* `reentrantA` costs more than an isolated cold check
of the same content, while `reentrantB` (made moments later, on the same checker instance) costs
less than an isolated cold check. The design's own precondition 6 disclosed upfront that this
quartet observes *cost*, not FCS's internal scheduling directly, so the mechanistic explanation
below is inference, not direct observation:

- `checker1` has done essentially nothing before the reentrant callback fires except a lightweight
  `GetProjectOptionsFromScript` call — so `reentrantA` is very plausibly where `checker1` pays real,
  first-ever setup cost specific to *this checker instance* (initial project-graph/assembly-
  reference-resolution machinery under `TransparentCompiler`) — cost an isolated cold checker for A
  *also* has to pay once, which is why both are the same order of magnitude, not orders apart.
- That `reentrantA` costs *more* than the isolated cold check of the same content, rather than the
  same amount, is consistent with (but does not conclusively prove) genuine reentrancy overhead on
  top of ordinary first-touch cost: `reentrantA` is invoked while the outer `checker1.
  ParseAndCheckProject` call is still unreturned on the async call stack, a state a standalone cold
  checker is never in.
- `reentrantB`'s lower-than-cold cost is consistent with `checker1` having already amortized some of
  that first-instance setup cost during `reentrantA`, moments earlier — a real, if partial, warming
  effect *within the reentrant sequence itself*, distinct from the "already resolved before the
  callback ever fired" explanation this quartet rules out.

None of this is direct proof of FCS's internal execution model (no thread/stack instrumentation was
added, per the design's own disclosed scope limit) — it is an inference from cost alone, reported
as exactly that.

## What a review should press on

- **This is a 3-file, 2-reentrant-call toy, run once per process type.** Whether the "first
  reentrant call costs more than cold, second costs less" pattern holds with more files, a different
  file order, or genuinely cross-file-dependent content (rather than three independent
  same-shape files) is untested.
- **The mechanistic explanation above (checker-instance first-touch cost, partially amortized by the
  second call) is inference from aggregate cost, not directly observed.** A review with more time
  could attempt to instrument FCS's own internal request queue/thread activity (if accessible) rather
  than relying on wall-clock cost alone, though this quartet's own cheapest-falsifier framing
  deliberately avoided that heavier lift.
- **The specific numeric ratios (4.2x, 0.58x) are this-machine, this-run numbers**, consistent with
  every quartet in this lineage's own standing cross-cutting caveat (`FINDINGS.md`) that no quartet
  has run repeated trials across separate machine states — 5 same-machine repeats is the discipline
  this quartet added, not a claim of cross-environment generality.
- **This is a hand-typed 3-file toy, not a real-generator composition (Q026) or a scale sweep
  (Q023/Q024).** It answers a narrower, orthogonal question (is the reentrant call cache-like or
  work-like) and should not be read as adding to or changing either of those quartets' own findings.
