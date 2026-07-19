# Q028 — hypothesis

## Question

`Q027-reentrant-call-timing`'s review found, as a byproduct of settling a different question, that the
*first* `ParseAndCheckFileInProject`/`ParseAndCheckProject` call performed in any fresh process carries a
large (~400-500ms in Q027's own minimal toy), one-time JIT-warmup/FCS-static-init/referenced-assembly-
metadata-read tax that has nothing to do with whatever is actually being measured — a "cold" baseline run
later in the same, already-warm process never pays it. `BACKLOG.md` item 23 named the direct, cheap
follow-up this raises: `Q023-scale-cost-reentrant-callback` and `Q024-position-sweep-across-scale` both
measured their `cold`/`repeat`/`editOne` numbers as **fresh-process-per-N runs** (their own corrected
methodology, fixing an earlier same-process contamination bug) — meaning every one of those quartets'
`cold` measurements is, by construction, the *first* `ParseAndCheckProject` call in its process. If the
tax Q027 found is real and general, it should show up in Q023/Q024's own `cold` numbers too, and since
both quartets' headline claims are framed as ratios against `cold` (`editOneMedian`/`coldMs`), an inflated
`cold` denominator could be quietly distorting those ratios.

Does Q023/Q024's own `cold` measurement carry the same one-time tax, and if so, does correcting for it
change the reported `cold`/`repeat`/`editOne` numbers or the ratios built from them?

## Novelty

Not covered by any closed quartet: Q027 found the tax exists in its own unrelated 3-file toy harness, but
never touched Q023/Q024's actual spike code or numbers. No prior quartet checks a first-party credibility
question about a *sibling* quartet's own already-published numbers this directly.

## Contradiction check

Does not contradict any prior verdict. It is explicitly a **precision/credibility check** on Q023
(REVISE) and Q024 (SHIP, scoped) — those quartets' own qualitative conclusions (real caching under
`TransparentCompiler`; linear-in-successors cost; no detectable slope drift across N) are not
pre-registered as being at risk here. What's at risk is the *specific numbers* cited when those
conclusions are quoted (e.g. "1339ms of a 2640ms cold check," `editOneRatio` values feeding Q024's
regression), per `BACKLOG.md` item 23's own framing: "a credibility check, not a new mechanism question."

## Validity preconditions

- Must reuse Q023's own checked-in spike code unmodified in its untouched code paths, adding only an
  opt-in warmup step — not a rewrite, so a no-warmup run must reproduce Q023's original numbers within
  ordinary run-to-run noise (confirms the modified binary is still testing the same thing).
- Must run against the same pinned `FSharp.Compiler.Service 43.9.101` Q023/Q024/Q027 all used.
- The warmup check must be a genuinely separate, unrelated 1-file project (not a data point in the real
  N-file project) — otherwise it's not isolating a JIT/init tax, it's just another primed cache entry
  for the same project being measured.

## Cheapest falsifier

Rerun N=10 (where a ~500ms fixed tax would be the largest fraction of `cold`) with and without the added
warmup step. If `cold` doesn't move, the tax either doesn't apply to this harness or is small enough not
to matter — NULL, and a fast one. If it does move, extend to N=300 (the other end of Q023/Q024's tested
range) to see whether the tax's *absolute* size stays roughly fixed (as Q027 found) or scales with N,
which would change how much it matters for the larger, more expensive measurements.
