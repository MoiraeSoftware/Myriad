# Q008-provenance-closed-loop / Movement 4 — Adversarial review

## Three strongest objections

1. **The headline "faster than Q006's baseline" figure compares an error path to a success path,
   and that's not quite apples to apples.** Round 3's 19ms live-recheck is the *mismatch* case — FCS
   raises from `ApplyStaticArguments` and short-circuits before doing the type-inference and member
   resolution work a *successful* instantiation requires. The genuinely comparable number is Round 3
   step 3 (32ms — a live edit that restores a match, so a real success path, same shape as Q006's own
   live-edit test). That number still beats Q006's 47ms baseline, which is the claim that actually
   matters and survives scrutiny — but `02-results.md`'s framing leads with the 19ms figure without
   flagging that a failing recheck and a succeeding recheck aren't doing comparable work, and the
   review shouldn't repeat that imprecision. Net effect on the verdict: none — even the fair
   comparison (32ms vs 47ms) clears the SHIP threshold by a wide margin — but the reasoning behind
   *why* it's fast needs correcting: it is not evidence that cross-assembly reflection is free, only
   that it's cheap enough not to dominate FCS's own recheck cost, and a failing path is inherently
   cheaper to typecheck than a succeeding one regardless of what the provider does internally.

2. **Every timing number in this quartet (and in Q006) is a single sample, not a distribution.**
   Not a new problem — Q006 had the same gap and it wasn't a blocker there either — but worth
   restating rather than letting a second quartet's clean numbers make it feel settled. The margin
   here is large enough that it doesn't threaten the SHIP call (19–37ms sits nowhere near the
   pre-registered REVISE threshold of "materially worse," which by the hypothesis's own 10x–100x
   framing would mean 470ms–4700ms), but a claim resting partly on "cheap enough to run on every
   keystroke" deserves repeated-trial measurement before it's treated as load-bearing for anything
   beyond this quartet's own pass/fail gate.

3. **The NULL threshold question was explicitly deferred to this review by `02-results.md`, and
   deserves a real answer, not a restatement of the deferral.** Could a plain build-time
   version-check script achieve the same outcome more cheaply, with none of this machinery? For the
   narrow case tested — one monolithic version string, checked once — yes, functionally: a CI step
   comparing a schema package's declared version against a client's expected version already exists
   as ordinary practice and would also fail the build on mismatch. What a version-check script
   structurally cannot do, which this quartet demonstrated rather than merely asserted: surface the
   conflict as a real compiler diagnostic, cited at the exact source line where the mismatched
   dependency is consumed, the moment a developer edits code that creates the mismatch — before a
   save, before a build, before CI ever runs. That's a difference in *when and where* the failure
   surfaces, not *whether* it can be caught at all. The capability claim should be scoped to that
   axis precisely: this is not "supply-chain verification became possible," it's "supply-chain
   verification moved from CI-time to keystroke-time and got a precise source citation for free by
   riding the type checker instead of a side-band tool." That is a real, meaningful, measured
   difference — Round 3 proved it, not just argued it — but it's the correct scope for the SHIP
   below, not the more sweeping "no external tooling required at all" framing `02-results.md`'s
   closing section leans toward.

## Verdict

**SHIP.** Every threshold this quartet's own `00-hypothesis.md` pre-registered was met, not just
approximately: the cheapest falsifier passed cleanly (a custom attribute on a generative provided
type survives into independently-reflectable on-disk IL, correctly remapped to the runtime
assembly — confirmed by a program that never touched the SDK at all); a second, genuinely separate
provider read that attribute and gated real member generation on it, in both directions, with a
specific diagnostic naming both versions on mismatch rather than a swallowed generic error; and the
conflict was caught on a live source edit through the same `FSharpChecker` instance, no rebuild, at
a cost at or below Q006's established baseline in the fair (success-path) comparison. This is the
first quartet in the type-provider lineage (Q006–Q008) to hit every pre-registered SHIP criterion
without a caveat forcing a downgrade — Q006 was capped by its own validity preconditions explicitly
requiring a real IDE host, which never got tested; Q008's own preconditions required only the
same-checker live-recheck test, which did get tested, fully.

Scope precisely, because two things travel with the ship:

- **The differentiating value is latency and integration, not existence of the check.** Section 3
  above is the load-bearing correction to carry forward: don't cite this result as "type-level
  supply-chain verification is now possible" — some version of that possibility already existed via
  ordinary build/CI tooling. Cite it as "the same verification, moved to keystroke-time, with a
  source-cited diagnostic, by riding the type-provider protocol instead of a side-band script" — a
  real, demonstrated, narrower claim.
- **Untested against a literal editor session, same as Q006, but a smaller residual risk here.**
  This quartet's core claim (does the compiler produce a citable diagnostic, live, without a
  rebuild) is answerable directly through `FSharpChecker`, since that's the layer that owns
  diagnostic generation — FSAC is substantially a translation of FCS's own diagnostics into IDE
  chrome, unlike Q006's claim, which was fundamentally about IDE-visible behavior. The residual risk
  — FSAC's own caching of type-provider instantiation results across recheck cycles, a specifically
  complex area, un-exercised here — is real but secondary, not central, to what this quartet set out
  to prove.

Two loose ends surfaced honestly in `02-results.md` and worth naming as real, not swept in with the
main result: the mismatch diagnostic is reported twice (identical text, same location) — cosmetic
for this quartet's pass/fail gate, but a real editor would show two red squiggles, worth a fix if
this ever ships for real; and the emitted generative type is named after the *consumer's*
abbreviation rather than the provider's own namespace, which `ClientTP` had to work around with a
name-or-attribute-scan fallback — correctly handled here, but worth remembering as a general
gotcha for any future generative-provider work in this lineage, not just this quartet's problem.

**Follow-ups, if the frontier keeps moving (recorded in `BACKLOG.md`'s general type-provider
section, not Myriad-specific):**

1. Fine-grained, per-field provenance instead of one whole-type version tag — this quartet tested
   the coarsest possible version of the claim; a schema with per-field provenance (some fields
   changed since v2, others didn't) would test whether the enforcement can be as precise as the
   diagnostic citation already is.
2. Stress-test with a large schema assembly (hundreds of provided members) to confirm the
   `Assembly.LoadFrom` + attribute-read cost stays flat under real-world schema size, not just the
   two-property toy case tested here.
3. Resolve the duplicate-diagnostic reporting — worth a real fix before this pattern gets used
   anywhere an editor experience matters, even though it didn't affect this quartet's verdict.
