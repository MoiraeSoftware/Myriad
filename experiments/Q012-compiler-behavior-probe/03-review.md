# Q012-compiler-behavior-probe / Movement 4 — Adversarial review

## Verified directly before writing this review

Read `artifacts/ProbeSimple.DesignTime/ProbeProvider.fs` in full: `ProbeErased` differs from the
generative `Probe` type by exactly `isErased = true` vs `false`, same single static parameter, same
instrumentation, same namespace — a genuinely minimal-difference control, not a strawman. Read
`artifacts/run-logs/run-sharpen.txt` directly: `erased-defn` resolves with 0 errors under
`ParseAndCheckFileInProject`; `gen-defn` fails with the identical "couldn't find type" diagnostic
`02-results.md` quotes. Read `artifacts/run-logs/run-round3.txt` directly: both of Q011's real
providers reproduce the exact same fail-PC/succeed-Compile/2-firings-then-1-firing pattern as the toy
probes, diagnostic text byte-for-byte the same shape Q011 documented. The isolation is real, not
oversold in the write-up.

## Three strongest objections

1. **This result now stands in direct, unreconciled contradiction with Q008's own claimed success-path
   numbers, and the honest thing to do is escalate that, not just note it.** `Q008-provenance-closed-
   loop/02-results.md` (quoted in `FINDINGS.md`'s Thread 2 digest and in Q009's own novelty gate) reports
   a generative provided type resolving via the diagnostics-only checking path with real generated
   members accessible ("`C.VerifiedVersion` binding resolves as `type Microsoft.FSharp.Core.string` —
   real generated members") at a cold cost of ~1161ms. Q012 found, reproducibly, determinism-checked,
   against three toy shapes *and* both of Q011's real providers, that **no generative provided type
   resolves via `ParseAndCheckFileInProject` in this environment, ever** — it always fails with
   "couldn't find type," and only a real `checker.Compile` materializes it. These cannot both be true of
   the same mechanism under the same pinned FCS version unless something about Q008's actual harness
   differed from what its prose describes. Q008 has no saved artifacts (the exact gap `FINDINGS.md`
   already flags), so this cannot be mechanically resolved here. **The single most valuable, cheapest
   next test this finding suggests, which Q012 itself never ran:** does `ParseAndCheckFileInProject`
   succeed for a generative type if `checker.Compile` for that *same* type/scenario already ran earlier
   in the *same* process? Every scenario in Q012's own harness (`runShape`, `artifacts/Harness/Program.fs:176-218`)
   calls PC *before* Compile for a given shape, never the reverse — so a charitable, mechanistically
   plausible reconciliation (a prior compile "warms" something PC alone doesn't) is untested, not ruled
   out. Until that's checked, Q008/Q09's SHIP verdicts should be held with materially lower confidence
   than Q010/Q011/Q012's, not just flagged as "no durable artifact" — this is now a *specific,
   reproducible conflict*, not merely an absence of evidence.

2. **The literal pre-registered thresholds say NULL, and the write-up's own honesty about that should
   be preserved in how this quartet gets cited, not smoothed into a clean SHIP.** `00-hypothesis.md`'s
   SHIP condition named a factor correlating with failure "on shapes (b) and/or (c) but not (a)"; the
   NULL condition was explicitly "no discernible difference between shapes (a), (b), and (c)... must lie
   elsewhere." What happened is the NULL pattern, exactly as pre-registered — both named candidates
   (parameter count, instantiation I/O) are cleanly falsified. What makes this quartet valuable anyway
   is that the *design itself* mandated a positive control for precisely this branch, and that control,
   not a post-hoc rationalization, found a real, different, confirmed answer. That is worth real credit
   — it is exactly what "cheapest falsifier, then broaden systematically" is for — but it should be
   described as what it is: a NULL result on the pre-registered question that led to a genuine discovery
   outside the pre-registration, not a SHIP that happens to have an asterisk.
3. **Correction 2's boundary quietly halves the mechanism claim's novelty, and it's worth stating
   plainly rather than letting "the probe mechanism works" stand unqualified.** The hypothesis's whole
   pitch for this being more than Q011's own ad hoc `MiniTP` was exposing invocation data via **real
   provided members**, not file logs. What was actually delivered: the provided-member channel only
   ever reports a post-hoc snapshot of a *successful* compile (`InvocationCount = 1`, since a successful
   compile only fires once) — it can say nothing about the 2-firing behavior on the failing path, which
   is the behavior that actually matters for this whole investigation. Every piece of evidence about
   *multiple* invocations, in every round including the load-bearing Round 3 confirmation, comes from
   the file-log fallback, the exact technique Q011's `MiniTP` already used. The "reusable, queryable
   diagnostic type" vision is real for the narrow case of "how many times did a *successful* compile's
   instantiation fire" and not yet real for the case that actually mattered here.

## Verdict

**SHIP** the mechanism claim (Round 1) cleanly: a provider's instantiation function can capture a
`StackTrace` containing genuine, identifiable `FSharp.Compiler.*` frames (the exact call chain —
`ApplyStaticArguments` ← `TryApplyProvidedType` ← `TcProvidedTypeAppToStaticConstantArgs` ←
`TcTyconDefnCore_TryAsGenerateDeclaration` — verified directly from the saved stack-trace artifact),
a real, useful window into the host's own behavior, not just a count. Scoped down per objection 3: the
provided-member exposure channel only covers the successful-compile case, not the failing case that
turned out to matter most.

**NULL** the capability claim exactly as pre-registered against its two named candidate factors
(parameter count, instantiation-time I/O) — both cleanly falsified, the simplest possible shape already
fails. **But the mandated positive control this quartet's own design built in for exactly this outcome
delivered a real, different, reproducible, artifact-confirmed answer**: the axis is generative-vs-erased,
not either named factor. `ParseAndCheckFileInProject` resolves erased provided types cleanly and never
resolves generative ones, confirmed against three minimal probes and — the strongest part of this
quartet — against both of Q011's actual, unmodified provider implementations. This positively answers
Q011's own follow-up 1, on a different axis than either quartet anticipated, and does so with evidence
this repo can actually check (full `artifacts/`, raw logs, a vendored SDK copy), unlike the claim it now
contradicts.

Three things travel with this verdict, most consequential first:

- **Q008 and Q009's SHIP verdicts now carry a specific, reproducible, unreconciled contradiction, not
  just an absence of independent verification.** This changes how they should be treated going forward
  — from "trust the pattern shown elsewhere" (this session's earlier framing) to "actively disputed by
  a later, artifact-backed quartet, pending the untested compile-then-PC ordering hypothesis in
  objection 1." `FINDINGS.md` should say this plainly, not bury it under the existing "no durable
  artifact" note.
- **This is a NULL-that-led-to-a-discovery, not a clean SHIP** — cite it as "found the real cause via
  its own mandated fallback," not as "the pre-registered hypothesis was confirmed."
- **The "expose via provided members" half of the mechanism claim only covers the success path.** A
  future probe wanting to observe a *failing* instantiation's behavior through queryable output, not
  file logs, would need a different technique — this one doesn't generalize there.

**Follow-ups, in priority order:**

1. **Test the compile-then-PC ordering hypothesis (objection 1) directly** — the cheapest, highest-value
   remaining question in this whole lineage. If a prior `checker.Compile` of the same scenario makes a
   subsequent `ParseAndCheckFileInProject` call resolve the generative type, that reconciles Q008/Q09
   with Q012 (their harnesses may have compiled something first for an unrelated reason) and would be a
   genuinely new, useful finding about FCS's own caching behavior across API boundaries. If it does
   *not* resolve, that hardens the contradiction with Q008/Q09 further and makes retroactive
   reconstruction of their source (already recommended in `FINDINGS.md`) the clear next priority.
2. **Retroactively reconstruct and re-verify Q008/Q09**, now with elevated urgency given objection 1 —
   this was previously offered as an optional trust-repair task; it is no longer optional-feeling given
   a specific, reproducible conflict now exists.
3. **Locate the actual FCS code path** responsible for the diagnostics-only checking API not emitting a
   generative provider's backend assembly — named as out of scope for a probe spike in `02-results.md`,
   still worth doing if anyone wants to fix rather than merely characterize this.
4. Real-IDE-host testing remains untested, same standing caveat every Thread 2 quartet has carried since
   Q006.
