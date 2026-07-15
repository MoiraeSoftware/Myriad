# Q011-consumer-driven-contracts / Movement 4 — Adversarial review

## Four strongest objections

1. **The primary motivating value claim — "moved from CI-time to keystroke-time in the producer's
   editor," the actual reason this idea was picked as the most valuable next spike over Q007 or Q010's
   own follow-ups — is not what got proven, and correction 2 puts it in genuine doubt rather than
   merely leaving it untested.** Q008 and Q009 both demonstrated their check firing live, incrementally,
   in 15–57ms, through the exact `ParseAndCheckFileInProject`/`ParseAndCheckProject` APIs an IDE-hosted
   checker would use. `02-results.md` reports, honestly and in detail, that those same APIs **do not
   resolve this quartet's generative type even on the zero-diagnostic success path**, for a reason three
   independent controls ruled several candidates out for but never identified. The substituted harness
   (`checker.Compile`, a real full compile to an output DLL each time) proves the coordination-semantics
   claim the hypothesis pre-registered — and that claim does hold, cleanly — but at 106–201ms warm,
   roughly an order of magnitude above Q008/Q09's band, using a structurally different code path than a
   real editor would exercise. `BACKLOG.md`'s own framing of this idea, and the recommendation that
   picked it for this session, explicitly cited "keystroke-time" as the differentiator over a CI script
   doing the crude version of the same check. That differentiator is currently unproven for this
   quartet's specific provider shape, not merely "not yet tested" the way, say, real-IDE-host testing
   has been an accepted standing caveat since Q006 — this is a *regression from a previously working
   path*, on a structurally similar provider, with an unidentified cause. Any citation of this quartet
   going forward must lead with the coordination-semantics result and treat the keystroke-time framing
   as open, not settled.

2. **The clean "unwedge" result (Round 2 step 6) is real, but part of why it worked cleanly is a design
   choice in the executing provider's own memoization strategy, not purely a demonstration of FCS
   robustness — and the review should name this so it doesn't get cited more broadly than it earned.**
   Reading `SchemaTP.DesignTime/SchemaProvider.fs` directly: `successCache` is deliberately **not**
   populated on the exception path (`createType`'s `match ... | false, _ -> ... createTypeUncached ...`
   always recomputes on a cache miss, and a failure never reaches the `successCache.[key] <- t` line).
   That specific choice is exactly what makes the post-rebuild re-check correctly recompute rather than
   replay a cached failure. It is a genuinely correct and well-reasoned design decision, made and
   explained clearly in the artifact's own comments — but "no additional ceremony beyond the rebuild
   itself" (the results file's phrase) reads as though this is a property of the mechanism in general.
   It isn't: a provider that cached failures too (a very plausible implementation choice — "cache
   everything to avoid redundant reflection work" is a reasonable first instinct) would wedge itself
   exactly the way `01-design.md`'s REVISE threshold worried about, and this quartet doesn't test that
   failure mode. Worth stating as an explicit design lesson for any follow-on build: **don't cache
   negative results in a reversed-arrow (or any live-reactive) provider**, not just an incidental
   implementation detail.

3. **Correction 1 (the `DefineStaticParameters` instantiation function firing more than once per
   logical check, breaking type identity without memoization) is a genuinely new finding with real
   teeth for this whole lineage, and it's unclear whether Q008/Q09 were exposed to the same risk and got
   lucky, or whether something about this quartet's provider shape specifically provokes it.** Neither
   Q008 nor Q009 kept their provider source as durable artifacts (`Q009-field-level-provenance` has no
   `artifacts/` `.fs` files at all — checked directly, confirmed empty), and neither quartet's
   `02-results.md` mentions memoization, caching, or repeated `DefineStaticParameters` invocation
   anywhere. Three explanations are all live: (a) Q008/Q09 hit the identical double-invocation and it
   was silently harmless because their generated members were verification-only markers rather than
   real accessors depending on cross-call type identity; (b) something about Q011's two-static-parameter,
   enforcement-carrying shape provokes a second invocation that Q008/Q09's simpler shape didn't trigger;
   (c) Q008/Q09 quietly avoided it by some implementation detail lost with their un-kept source. This
   quartet's own isolation (the `MiniTP` probe) is convincing evidence the behavior is real and general
   — but the probe's *current* saved artifact already has the fix applied (`cache` dictionary present
   in `MiniProvider.fs` as committed), so the "before" state that produced the quoted duplicate-log
   evidence isn't independently reproducible from what's checked in, only described. Not a reason to
   doubt the finding (the quoted timestamps and distinct temp-assembly names are concrete, specific
   evidence, not a vague claim), but worth naming as a small methodological gap: this finding rests on
   log output quoted in prose, not a reproducible artifact showing the failure mode.

4. **The duplicate-diagnostic defect (provider error reported twice, identical text, plus one
   downstream error) has now recurred a third time across this lineage (Q008, Q09, Q011), unfixed, and
   this quartet's own results file explicitly asks for it to be escalated past a per-quartet footnote.**
   Taking that request seriously here rather than letting it recur a fourth time.

## Verdict

**SHIP**, the claims as literally pre-registered. Round 1's falsifier passed cleanly (client-emitted
member attributes survive into independently-reflectable IL, symmetric with Q009's schema-side
finding). Round 2 proved the well-foundedness claim in both required directions — a schema edit
conflicting with a stale client's recorded dependency blocks, citing the client assembly, field, and
both versions; recompiling that client against the new schema clears the block on the very next check,
confirmed not to be a caching fluke by a deliberate re-block probe. Round 3 closed Q009's own named
follow-up (field removal) with a diagnostic lexically and structurally distinct from the stale-version
wording, verified by direct text comparison, not asserted as "PASS" alone. None of the pre-registered
KILL or REVISE conditions, as literally written, were triggered.

Say plainly what the pre-registration itself didn't anticipate and therefore doesn't formally gate on,
because it matters more than a clean SHIP label suggests: **this quartet's own pre-registered
thresholds never accounted for the possibility that the fast, incremental-checking API path Q008/Q09
relied on might fail outright for a structurally similar provider.** That happened here (objection 1),
was reported with real rigor (three independent controls, not a shrug), and was worked around with a
methodologically sound substitute (`checker.Compile`) that still answers the pre-registered question.
But it means this quartet's practical contribution to the "keystroke-time, not CI-time" pitch that made
it the highest-priority pick this session is currently **zero**, not merely "narrower than hoped" — and
that gap should travel with any future citation of this result at least as prominently as the SHIP
label does.

Four things travel with the ship:

- **The coordination-semantics/well-foundedness claim is proven; the keystroke-time value claim is
  not, and is now specifically in doubt (objection 1), not just untested.** Don't conflate the two when
  describing what this quartet accomplished.
- **The clean unwedge result depends on a real, correct design choice (don't cache failures) that this
  quartet made deliberately but doesn't test the failure mode of getting wrong (objection 2).** Name
  this explicitly in any reuse of the pattern.
- **Correction 1's double-invocation finding is real and probably lineage-wide, but unconfirmed against
  Q008/Q09's own (unpreserved) provider code, and not independently reproducible from what's checked in
  (objection 3).**
- **The duplicate-diagnostic defect is now a three-quartet-old, unescalated pattern (objection 4).**

**Follow-ups, in priority order:**

1. **Identify correction 2's root cause.** Before citing this quartet's mechanism as a template for any
   future Thread 2 work, determine why `ParseAndCheckFileInProject`/`ParseAndCheckProject` fail to
   resolve this provider's generative type on the success path when Q008/Q09's structurally similar
   `ClientTP` succeeded via the identical API. This is the single highest-value unresolved question in
   the whole quartet — it gates whether the keystroke-time value proposition is recoverable at all for
   this idea, or whether reversed-arrow enforcement is stuck at full-compile granularity for a reason
   yet to be found.
2. **Retrofit the double-invocation check against Q008 and Q09's own provider logic**, if their source
   can be reconstructed from the quartets' `02-results.md` write-ups closely enough, to settle whether
   objection 3's three explanations narrow to one.
3. **Fix the duplicate-diagnostic defect once, across the whole Q008/Q009/Q011 pattern**, per this
   quartet's own request and the same follow-up named (and not yet done) after both prior occurrences.
4. **Test the failure mode objection 2 names**: a provider that (incorrectly) caches negative
   enforcement results, to confirm it wedges the way the design's REVISE threshold anticipated, and use
   that as a concrete cautionary example in any documentation of this pattern going forward.
5. **Realistic client-assembly discovery**, named as out of scope from the start and still open —
   unchanged priority from the original pre-registration.
