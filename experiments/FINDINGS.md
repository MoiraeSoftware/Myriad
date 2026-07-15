# Findings so far — what's real, what's not

Cross-quartet digest as of 2026-07-15, twelve quartets in (Q001–Q012, nine closed, three planned).
This is not a replacement for reading a closed quartet's own `03-review.md` — each is written to stand
alone — it's a synthesis for deciding what to do next without re-reading all twelve. Two intertwined
but distinct threads share this quartet discipline; keep them separate, because they answer different
questions and one is Myriad-specific while the other explicitly isn't.

**Read this before trusting anything in the Thread 2 section below:** Q012 found a specific,
reproducible, artifact-backed contradiction with Q008's core claimed result. This is not a scoping
caveat like the others in this file — it's an open dispute between two quartets' actual measurements,
and Q008 cannot currently defend itself because it saved no source. See the Thread 2 section and "A
gap in this file's own credibility" below for the full picture before citing any Thread 2 SHIP verdict.

## Thread 1: should Myriad's own architecture change?

The question: could Myriad's current model (untyped-AST parsing → disk-written `.fs` files →
separate `dotnet build` re-typechecks them) be replaced by in-process, typed FCS hosting, and would
that actually buy real capability or just be novelty?

**Established, with real evidence:**

- In-process typed FCS hosting mechanically works, including a real correctness trap worth
  remembering on its own: `FSharpChecker`'s default `BackgroundCompiler` silently serves *stale*
  results across a broken cross-file dependency unless you explicitly call
  `InvalidateConfiguration`. The opt-in `useTransparentCompiler = true` path avoids this and was
  faster in every quartet that measured it (Q001).
- Typed access is not automatically an improvement over syntax. Ported onto the real `Fields`
  generator, it changed *nothing* — `Fields` is pure structural echo, and syntax-only was already
  correct. This NULL result matters as much as any SHIP in this file: it's the standing check
  against overclaiming everywhere else in this thread (Q001).
- Typed access *does* earn its keep for a harder, real class of problem: detecting that one field's
  type is itself another attributed type, resolved across files. A real syntax-only alternative was
  built (not a strawman) and found to flip between correct and silently-wrong depending only on file
  processing order — a genuine correctness bug class, not a convenience gap. But this required
  **both** pillars together — whole-project in-process hosting *and* typed field resolution — not
  typed access bolted onto Myriad's current per-file plugin model (Q002).
- FSI (`FsiEvaluationSession`, the mechanism type providers run on) can be hosted in the same
  process as `FSharpChecker` with no collision, and real F#-typed data — not just primitives —
  crosses into host code correctly (Q003). Untested: `AssemblyLoadContext` isolation, which is the
  configuration Myriad's actual plugin loader (`McMaster.NETCore.Plugins`) uses; the full
  evaluate→generate→splice→typecheck loop was never built.
- A real Myriad generator (`Lenses`) was ported onto the type-provider protocol instead of the
  FCS-hosting path, to attack Myriad's IDE-invisibility gap directly. The mechanism worked cleanly —
  47ms live re-check with zero rebuild — but hit a **structural wall, not an engineering gap**: type
  providers can only resolve types already compiled in a separately referenced assembly, never a
  type from the compilation currently in progress. Myriad's actual dominant usage (`[<Lenses>]` on a
  record in the file you're editing, in the project you're building) is exactly the case this rules
  out (Q006).
- F#'s own strict linear file order gives a route around Q006's wall that a type provider structurally
  cannot take: a `DocumentSource.Custom` callback made **reentrant** — calling back into the same
  checker to check earlier files while supplying a later file's content — can compute a virtual file's
  text from the already-typechecked prefix of the same in-progress project, no fixpoint, no separate
  staging pass. Proven for a real payoff: a third generator correctly reused a second generator's
  already-generated (still virtual) output, confirmed by symbol resolution two independent ways, not
  visual inspection. This is Myriad's own MSBuild-hosted pipeline, not a type provider, so Q006's wall
  never applied to it in the first place (Q010).

**Honest net position:** three SHIPs (Q002 fully, Q003 narrowly, Q010 scoped) prove the mechanism is
sometimes genuinely valuable; two REVISE/NULL results (Q001, Q006) prove it's easy to overclaim and
that at least one promising-looking route (type providers) hits a hard wall for Myriad's real usage
pattern. **Nothing has been built that would replace Myriad's current pipeline end to end**, and
nothing in `experiments/` has been merged into `src/`. Q010's own review holds back from the
hypothesis's strongest framing: unresolved whether the reentrancy tested was genuinely mid-flight or
landing on an already-idle checker, and it settles only the acyclic case (a later generator depending
on an earlier one), not mutual cross-generator dependency. Q004 (cross-assembly typed access) and Q005
(self-verifying generators — typecheck-before-emit) remain the next pre-registered steps from the
original queue and neither has been run.

## Thread 2: general F# type-provider headroom, independent of Myriad

A separate line, started from the question "excluding how Myriad is configured, can type providers
themselves be pushed further" — genuinely independent research using the same `FSharp.TypeProviders.SDK`
checkout, not gated on Thread 1's outcome either way.

**Originally logged as a three-quartet SHIP streak (Q008, Q009, Q011). As of Q012, that framing no
longer holds without a major caveat — read the whole section, not just this list:**

- One generative provider can embed derivation provenance (a version tag) as a real custom
  attribute; a **second, independently-compiled** provider can read that attribute via plain
  reflection and refuse to generate when its own declared expectation disagrees — with a specific
  compiler diagnostic naming both versions, not a swallowed generic error. **Claimed**: the conflict is
  caught live, on a source edit, through the same `FSharpChecker` instance, no rebuild, at 19–32ms
  (Q008) — **but see below: Q012 found this specific claim (generative-type resolution via the live
  checking API) is not reproducible in an artifact-backed environment, and Q008 saved no source to
  check against.**
- The same mechanism generalizes to **field-level** granularity: a client declaring a dependency on
  only a subset of a schema's fields stays clean when an unrelated field's provenance changes, and
  fails with a diagnostic naming the *specific* changed field(s) — including all of them at once in
  the multi-mismatch case — when a depended-on field changes. Re-check cost stayed flat from a
  3-field to a 12-field schema (Q009) — **inherits Q008's same now-disputed dependency on live-API
  resolution, for the same reason.**

**Scoped precisely — what these two claimed SHIPs were said not to be:** neither result was claimed to
mean "type-level supply-chain verification is now possible." A CI script comparing version numbers
already does that, more crudely. What was claimed is that the same check moves from CI-time to
keystroke-time, with a diagnostic cited at the exact consuming source line, by riding the type-provider
protocol instead of a side-band tool. **Q012 now specifically calls the keystroke-time half of that
claim into question** — see below. Also unproven regardless: both quartets generate verification-only
marker members (`ProvenanceOk`, `CheckedFieldCount`), never real per-field data accessors.

**The streak continued to a third SHIP (Q011), which is what first exposed a crack, before Q012 turned
that crack into a specific, artifact-backed contradiction.** The enforcement arrow was reversed: a
client-side provider records its own consumed fields as IL attributes, and a schema-side provider
refuses to compile a change that breaks a known, already-compiled client's recorded dependency —
blocking correctly on a stale client (naming the client assembly, field, and both versions), clearing
correctly once that client is recompiled with no deadlock, and producing a diagnostic for field
*removal* distinct from one for re-versioning, closing Q009's own named gap (Q011). The coordination-
semantics claim holds cleanly. But Q008/Q09's own fast, incremental `FSharpChecker` checking API — the
exact mechanism that makes "keystroke-time" true rather than aspirational — **failed to resolve this
quartet's generative type even on the success path**, for a reason investigated with real rigor (three
independent controls) but not identified at the time, forcing a fallback to full `checker.Compile`
(106–201ms, not Q008/Q09's claimed 15–57ms band).

**Q012 then found out why, and the answer is bigger than Q011's own regression.** Built specifically to
resolve Q011's open question, Q012 ran a controlled, minimal-difference comparison and — via a positive
control its own pre-registration mandated for exactly this outcome — found the actual axis:
`ParseAndCheckFileInProject` resolves **erased** provided types cleanly and **never** resolves
**generative** ones, confirmed against three toy shapes and, decisively, against both of Q011's real,
unmodified providers (same "couldn't find type" diagnostic, same 2-firings-then-1-firing pattern,
determinism-checked). Every provider in this Thread 2 lineage that generates real members (Q008, Q009,
Q011 — all generative, by necessity, since erased types can't carry the custom attributes this whole
enforcement mechanism depends on) should, per Q012's reproducible finding, **never** have resolved via
the live checking API at all. Q008's own `02-results.md` claims exactly that resolution happened,
successfully, with real generated members accessed. **This is a direct, specific contradiction between
two quartets' measurements, not a scoping gap** — see the credibility section below for what this means
and the one cheap, untested reconciling hypothesis (does a prior `checker.Compile` "warm" a later
`ParseAndCheckFileInProject` call for the same scenario?) that could resolve it either way.

**Still open, pre-registered but not run:** Q007 — can a provider host FSI *inside* its own
static-parameter instantiation function, evaluating a string argument as real F# code to sidestep
the literal-only static-argument restriction, and does that survive being invoked from inside the
compiler's own live type-checking call stack (a nesting of compiler-service hosts no prior quartet
tested)? This is the best-supported unrun hypothesis in the whole backlog — it composes two already-
proven pillars (Q003's FSI hosting, Q006/Q008's working string-static-parameter pattern) rather than
needing new infrastructure.

**Still just brainstorming, not evidence — recorded in `BACKLOG.md`, none spiked:** minting new
`[<Measure>]` phantom types for zero-cost compile-time tagging (currency, tenant IDs — mechanism
plausible, unverified whether the language's `typ<unit>` syntax extends past numeric primitives);
`FileSystemWatcher`-driven `Invalidate()` (real, but less novel than first framed — sample-file
providers already do this); a provider reflecting on its own just-emitted IL mid-session (real risk
around `AssemblyLoadContext` identity, unverified); proof-carrying witness types via an external
SMT/theorem prover (viable only if narrowed to prove things about *external* artifacts, since a
provider can never see the compilation it's part of — the same wall Q006 hit); reachability-derived
session types via full protocol-composition model checking (the most ambitious idea on the list and
the least tractable soon — blocked on real state-space-explosion complexity, not an SDK gap);
property-tested/adversarial types (breaks build reproducibility as originally pitched; a
content-derived deterministic seed would fix that, not yet tried); optimization-as-static-parameter
(weakest idea recorded, a real unfixed structural flaw — the type itself changes shape when the
optimum moves, fighting the point of a stable interface).

## Cross-cutting limitations — true of nearly everything in this file, not quartet-specific

- **Every timing number anywhere in this lineage is a single sample**, not a distribution. The
  margins involved (order-of-magnitude gaps between measured numbers and the pre-registered REVISE
  thresholds) mean this hasn't mattered for any verdict yet, but no quartet has run repeated trials.
- **Every "works live in the IDE" claim has only ever been tested through `FSharpChecker` as a
  library, never a literal Ionide/FSAC/VS session.** This was judged disqualifying for Q006's
  specific claim (which was fundamentally about IDE-visible behavior) and judged real-but-secondary
  for Q008/Q009's narrower claims (which are fundamentally about diagnostic generation, a layer FCS
  owns directly). No quartet has closed this gap for any claim.
- **`TransparentCompiler` — the mechanism underpinning the entire in-process-hosting foundation — is
  still labeled experimental by both FCS and FSAC** as of the pinned version (`43.9.101`). Every
  result in Thread 1 inherits this caveat; Q005, if it ships, would be the first quartet to give a
  design-time-hosted compiler veto power over what gets emitted, which raises the stakes on this
  caveat rather than lowering them.
- **The generative-type-provider wall (Q006) is structural, not an engineering gap, and it bounds
  every idea in Thread 2 that routes through a type provider**, not just Q006 itself: a provider can
  never see a type from the compilation currently in progress, only already-compiled referenced
  code. Q008 and Q009 both live comfortably inside this wall (schema-package-vs-client-package is
  naturally a cross-compilation scenario); any future idea that needs same-project visibility will
  hit it again.
- **A specific, reproducible defect has now recurred three times unfixed**: the mismatch diagnostic in
  Q008, Q009, and now Q011 is reported twice (identical text, same location), plus one downstream
  error in Q011's case. Harmless to all three quartets' pass/fail gating, real in an actual editor (two
  red squiggles), worth fixing once rather than re-discovering a fourth time.
- **`DefineStaticParameters`'s instantiation function is invoked more than once per logical check, and
  an unmemoized callback breaks type identity across those calls** — a real, likely lineage-wide FCS
  behavior found while building Q011 (isolated cleanly with a minimal provider), generalizing Q008's
  own "reported twice" observation from the failure path (harmless duplication) to the success path
  (destructive unless the instantiation function is memoized by its static-argument tuple). Whether
  Q008 and Q009 were exposed to the same behavior and avoided it by luck, by a simpler static-parameter
  shape, or by an implementation detail lost with their un-kept provider source is unconfirmed — neither
  quartet saved its provider code as a durable artifact. Any future provider in this lineage should
  memoize its instantiation function defensively regardless.
- **RESOLVED by Q012, and the answer is more consequential than the open question was:** the fast,
  incremental `FSharpChecker` checking path (`ParseAndCheckFileInProject`/`ParseAndCheckProject`)
  **never** resolves a generative provided type in this environment, full stop — not "not guaranteed
  for every shape," but never, for any of the four generative shapes tested (three toy probes plus
  Q011's own two real providers), while an erased provided type resolves cleanly every time. The
  failure isn't silent once you check for it (a specific "couldn't find type" diagnostic names the
  reason), but nobody had built the erased-vs-generative control until Q012. This directly means the
  "keystroke-time" claim behind Q008/Q09's SHIP verdicts should not have been possible via the API they
  report using — see the credibility section below.
- **`FSharpChecker.ParseAndCheckFileInProject` silently bypasses `DocumentSource.Custom` for a file
  when called with explicit source text.** No exception, no diagnostic — it just checks whatever text
  was passed and never invokes the callback for that file. Q010 hit this directly: its own frozen
  design's literal Round 1 trigger did exactly this and produced a false pass (0 errors on a trivially-
  valid placeholder) with nothing to indicate the callback never fired. A real, silent footgun for any
  quartet or future engineering effort that uses `DocumentSource.Custom` — guard against it explicitly
  (e.g. a sentinel only the callback would produce), don't assume the callback fired just because the
  check returned clean.

## A gap in this file's own credibility — found by user scrutiny, then escalated by Q012 from
"unverified" to "actively disputed"

**Q006, Q008, and Q009 — three closed, verdict-bearing quartets — have zero saved source under their
`artifacts/` folders**, confirmed directly from the filesystem on 2026-07-15. Every claim in those
three quartets' `02-results.md` — quoted diagnostic text, timing numbers, "the attribute survives into
independently-reflectable IL" — exists only as prose. Q001–Q003 partially kept artifacts (Q001's own
note: reconstructed after the fact and re-verified to match, not the literal original run); Q010, Q011,
and Q012 all kept full, independently-readable source, and every review of those three actually read
and cross-checked the code against the write-up before ruling.

**This stopped being a purely hypothetical trust gap the moment Q012 ran.** Q012 built a controlled,
artifact-backed, determinism-checked comparison specifically to answer an open question Q011 left
behind, and found that `ParseAndCheckFileInProject`/`ParseAndCheckProject` **never** resolve a
generative provided type in this pinned environment (`FSharp.Compiler.Service 43.9.101`, the same
`FSharp.TypeProviders.SDK` commit every Thread 2 quartet uses) — confirmed against four different
generative shapes, including both of Q011's real providers, with a clean erased-vs-generative control
proving the checking API itself isn't broken in general. **Q008's own `02-results.md` claims the
opposite for a generative provider**: successful resolution via the same live checking path, real
generated members accessed, at a reported cold cost of ~1161ms. These cannot both be true of the same
pinned mechanism unless something about Q008's actual harness differed from what its prose describes —
and there is no way to check, because Q008 saved nothing.

**One cheap, specific, untested hypothesis could resolve this either way, named in `Q012-compiler-
behavior-probe/03-review.md`'s first objection: does a prior `checker.Compile` of the same scenario
"warm" a later `ParseAndCheckFileInProject` call for that same scenario, in the same process?** Q012's
own harness always checked PC before Compile for a given shape, never the reverse, so this ordering was
never tested. If a prior compile does make PC succeed afterward, that reconciles Q008/Q09 with Q012
cleanly (their harnesses may have compiled something first for an unrelated reason) and is itself a
genuinely new, useful finding about FCS's own caching behavior across API boundaries. If it doesn't,
the contradiction hardens and retroactively reconstructing Q008/Q09's source becomes the clear next
priority, not an optional trust-repair task.

**Until one of those happens, treat Q008 and Q009's SHIP verdicts as actively disputed by a later,
artifact-backed quartet — a materially different, more serious status than "unverified."** Q006's
missing artifacts remain a plain unverified gap (nothing has specifically contradicted it), not a
dispute.

## Starting the next session

Read this file for the digest, `experiments/README.md`'s Index for the one-line-per-quartet detail,
then `experiments/BACKLOG.md` for what's queued and why. Q010 and Q011 (planned at the end of the
2026-07-15 session's first half) are both closed. Q011's review named finding its own open regression's
root cause as the clear top priority; that became Q012, which is also now closed — and answered the
question with something bigger than expected: not a Q011-specific bug, but a lineage-wide finding that
directly contradicts Q008's own claimed measurement. See "A gap in this file's own credibility" above
for the full picture.

**If picking one next step, this is again not a close call: test the compile-then-PC ordering
hypothesis named in `Q012-compiler-behavior-probe/03-review.md`'s first objection.** Concretely: take
any generative provider (Q012's own `ProbeSimple` artifact works), run `checker.Compile` on a scenario
first, then run `checker.ParseAndCheckFileInProject` on the *same* scenario in the *same* process
afterward, and see whether the second call now resolves the type where a cold `ParseAndCheckFileInProject`
alone does not. This is cheap (an afternoon on Q012's own saved harness, not a new quartet's worth of
infrastructure), and it's the one test that could resolve the Q008-vs-Q012 contradiction in either
direction — reconciling them (a real, useful finding about FCS caching across API boundaries) or
hardening the case that Q008/Q09 need retroactive reconstruction before being cited again.

Second priority, unchanged and still open: **Q010's own follow-up 1** (instrument the reentrant call to
determine whether it was landing on genuinely in-flight state or an already-idle checker). **Q007**
remains the best-supported unrun hypothesis from the original queue if none of the above is preferred.
Two smaller, cheap items now worth doing regardless of what's picked next: fix the duplicate-diagnostic
defect that has recurred unfixed across Q008, Q009, and Q011 (three occurrences); and locate the actual
FCS code path responsible for the diagnostics-only checking API not emitting a generative provider's
backend assembly, named as out of scope for Q012's own probe spike but not located anywhere else
either.
