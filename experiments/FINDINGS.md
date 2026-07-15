# Findings so far — what's real, what's not

Cross-quartet digest as of 2026-07-16, sixteen quartets in (Q001–Q016, twelve closed, three planned,
one running). Separately, on 2026-07-16, Q008 and Q009's missing artifacts were filled in by recovering
and re-verifying their actual original source — see "A gap in this file's own credibility" below; this
was a reconstruction of existing verdicts, not a new quartet, so it doesn't change the quartet count.
This is not a replacement for reading a closed quartet's own `03-review.md` — each is written to stand
alone — it's a synthesis for deciding what to do next without re-reading all fifteen. Two intertwined
but distinct threads share this quartet discipline; keep them separate, because they answer different
questions and one is Myriad-specific while the other explicitly isn't.

**Update, 2026-07-16: the Q008-vs-Q012 contradiction below is RESOLVED, not merely disputed.** Q008 and
Q009's actual original scratch source was recovered intact (found, unintentionally preserved, in a
session job's own temp directory), rebuilt and rerun completely unmodified, and reproduced both
quartets' claimed results exactly, timings included. A direct same-checker, same-provider isolation
then confirmed the specific mechanistic reason Q012/Q013 saw failure where Q008/Q09 saw success: Q008/
Q09's real harness drives `ParseAndCheckFileInProject` with a **hand-built, non-script
`FSharpProjectOptions`**; every Q012/Q013 run instead used a `.fsx` script via
`GetProjectOptionsFromScript` — the one axis both of those quartets' own reviews flagged as untested.
Given identical checker, identical provider, identical consumer text, the script route fails with
Q012's exact diagnostic and the real-project-options route resolves cleanly. Full account:
`Q008-provenance-closed-loop/RECONSTRUCTION.md` and `Q009-field-level-provenance/RECONSTRUCTION.md`.
The paragraphs below are left as originally written (including the now-resolved dispute) so the
reasoning trail stays intact; treat every "actively disputed" framing in this section as superseded by
the reconstruction notes, not as the current status.

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
- A compiler-built F# quotation — obtained via `[<ReflectedDefinition>]` + `TryGetReflectedDefinition`,
  deliberately avoiding hand-built `Expr.Call` construction — can be specialized by a rewrite and
  rendered by Unquote's `decompile` into real F# source text that reparses, typechecks, and, verified by
  actually compiling and executing that text (not just the quotation), runs correctly with a recursive
  general implementation fully unrolled away. That's *code*, not just Q003's *data*, crossing the
  generation-time-to-source boundary, a real advance over Q003 on its own narrow terms. But it does not
  earn the "FSI staged compilation" framing it was pre-registered under: no `FsiEvaluationSession` ran
  anywhere in the spike (the SHIP threshold's own "at FSI time" conjunct), and both the general
  implementation and its specializing static config were source-level literals in the same program, so
  the dynamic-origin staging boundary that would make this a Myriad capability — a plugin-loaded
  implementation, config from attributed source neither known at host-compile-time — was never crossed.
  The partial evaluator is also a hand transcription of one recursive function's three node shapes, not
  the generic `ExprShape` expander the design's own recon named but didn't build, so the capability
  claim generalizes zero inches past that one shape (Q014).
- The specific gap Q014's review named — no `FsiEvaluationSession` ever ran, both the general
  implementation and its config were host-compile-time literals — closes for real: a `MethodInfo` for a
  function defined only inside a live FSI session (source read from a separate file the host never
  compiles) crosses back to host code, confirmed cross-assembly (`mi.Module.Assembly` genuinely differs
  from the host's own, not asserted), and `Expr.TryGetReflectedDefinition` returns the identical
  quotation shape the host-compile-time case gets — a real, previously-untested mechanism fact that
  could have failed and didn't. Q014's unchanged reification pipeline then specializes and correctly
  executes it, driven by a config value read from an environment variable, not a literal. But the
  adversarial review — independently reproducing the result at a third `n` value — found this earns only
  a Q010-shaped scoped SHIP, not the thing the lineage's framing implies: **FSI performs none of the
  actual staging.** It compiles the plugin and hands back a `MethodInfo` via a quotation-destructure
  one-liner that never executes the general implementation; every line of the actual partial-evaluation
  logic is host-compiled code byte-identical to Q014's. The origin proven dynamic is the implementation
  *body* and a scalar config value only — the host still hardcodes the plugin's function name, arity, and
  argument types at its own compile time, and the specializer is still hand-matched to one function's
  exact shape. Composition with Myriad's real architecture (ALC-isolated plugin loading, config parsed
  from attributed source) remains untested; FSI's non-isolated dynamic assembly is the friendly slice of
  the dynamic-origin space, not the one Myriad would actually need (Q015).

**Honest net position:** four SHIPs (Q002 fully, Q003 narrowly, Q010 scoped, Q015 scoped) prove the
mechanism is sometimes genuinely valuable — with Q015 now the second quartet, after Q010, to earn a SHIP
only once heavily scoped, a pattern worth noticing on its own: this thread's positive results keep
shrinking on inspection, not just its negative ones. Three REVISE/NULL results (Q001, Q006, Q014) prove
overclaiming is easy — Q014 in a distinct way: a spike can reproduce cleanly and still not be the thing
its own pre-registration named, because the mechanism that made it into the harness quietly substituted
for the mechanism in the hypothesis's title, and neither the design nor the results write-up caught the
swap before adversarial review did; Q015 then showed that even the corrected follow-up, built specifically
to close that exact gap, still smuggled in a second, subtler substitution (an inert FSI call standing in
for real generation-time computation) that only surfaced under a review briefed to look for it. Combined
with Q006's hard wall for type providers on Myriad's real usage pattern, this thread's overclaiming risk
is now demonstrated across three structurally different mechanisms. **Nothing has been built that would
replace Myriad's current pipeline end to end**, and nothing in `experiments/` has been merged into
`src/`. Q010's own review holds back from the hypothesis's strongest framing: unresolved whether the
reentrancy tested was genuinely mid-flight or landing on an already-idle checker, and it settles only the
acyclic case (a later generator depending on an earlier one), not mutual cross-generator dependency. Q004
(cross-assembly typed access) and Q005 (self-verifying generators — typecheck-before-emit) remain the
next pre-registered steps from the original queue and neither has been run. Q015's own review named the
most direct unrun next step in this sub-line: make FSI do work that is actually computation (run the
general implementation against the config and return a value/quotation the host could not have produced
structurally), not a quotation-destructuring one-liner — only then would "FSI evaluates real computation
at generation time" be demonstrated rather than assumed.

## Thread 2: general F# type-provider headroom, independent of Myriad

A separate line, started from the question "excluding how Myriad is configured, can type providers
themselves be pushed further" — genuinely independent research using the same `FSharp.TypeProviders.SDK`
checkout, not gated on Thread 1's outcome either way.

**Originally logged as a three-quartet SHIP streak (Q008, Q009, Q011). Q012 then found a specific
contradiction, Q013 hardened it, and as of 2026-07-16 the contradiction has been resolved by recovering
and re-running Q008/Q09's actual original source (see the credibility section below) — the streak
stands, on reproduced evidence, not just prose. Read the whole section for how that happened, not just
this list:**

- One generative provider can embed derivation provenance (a version tag) as a real custom
  attribute; a **second, independently-compiled** provider can read that attribute via plain
  reflection and refuse to generate when its own declared expectation disagrees — with a specific
  compiler diagnostic naming both versions, not a swallowed generic error. **Claimed**: the conflict is
  caught live, on a source edit, through the same `FSharpChecker` instance, no rebuild, at 19–32ms
  (Q008) — **confirmed by direct reconstruction on 2026-07-16: Q008's recovered original source
  reproduces this exactly, live-recheck timings inside the claimed band. See "A gap in this file's own
  credibility" below for the full account, including why Q012 saw the opposite result on a different
  harness shape.**
- The same mechanism generalizes to **field-level** granularity: a client declaring a dependency on
  only a subset of a schema's fields stays clean when an unrelated field's provenance changes, and
  fails with a diagnostic naming the *specific* changed field(s) — including all of them at once in
  the multi-mismatch case — when a depended-on field changes. Re-check cost stayed flat from a
  3-field to a 12-field schema (Q009) — **also confirmed by direct reconstruction on 2026-07-16, same
  outcome as Q008: reproduced exactly, including the flat 3-to-12-field scaling.**

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
two quartets' measurements, not a scoping gap** — see the credibility section below for what this means.

**Q013 then tested the one cheap, named reconciling hypothesis — and it failed.** Does a prior
`checker.Compile` "warm" a later `ParseAndCheckFileInProject` call for the same scenario, same checker
instance? No: on the exact generative shape Q012 proved fails cold, a same-instance Compile-then-PC
sequence still fails with Q012's identical diagnostic, deterministically across repeats, even though
the Compile half resolves cleanly every time. Each PC call mints a fresh temp assembly and reuses
nothing from the prior Compile — the mechanistic reason there's no warming. This closes the specific
same-checker/same-scenario/toy-shape reconciliation, not "warming" in general (cross-instance warming,
a stricter same-project-object PC form, and `TransparentCompiler` remain untested) — but it removes the
cheapest charitable explanation for Q008's numbers. See Q013 in the credibility section below.

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

## A gap in this file's own credibility — found by user scrutiny, escalated by Q012 to "actively
disputed," then RESOLVED on 2026-07-16 by recovering the missing evidence itself

**Update, 2026-07-16: Q008 and Q009's actual original scratch source was found intact** in a session
job's own temp directory (`$CLAUDE_JOB_DIR/tmp/tp-provenance-spike/` and
`tp-field-provenance-spike/`) — apparently never deleted after the original spikes were run, and never
copied into `experiments/Q008-.../artifacts/` or `Q009-.../artifacts/` before those quartets were
closed. Provider names, the custom attribute, and harness structure all match each quartet's own
`02-results.md` description exactly. Both project sets were rebuilt with `dotnet build -c Release`, no
source changes, and rerun: **every claimed result reproduced exactly**, including live-recheck timings
inside each quartet's originally claimed band (Q008: 18-26ms vs claimed 19-32ms; Q009: 15-56ms vs
claimed 15-57ms, flat across 3-to-12 fields as claimed). Full write-ups:
`Q008-provenance-closed-loop/RECONSTRUCTION.md` and `Q009-field-level-provenance/RECONSTRUCTION.md`;
recovered source, run logs, and a new isolation program now live under each quartet's `artifacts/`.

**The Q008-vs-Q012 contradiction is explained, not just closed by reproduction.** A minimal isolation
program (`Q008-provenance-closed-loop/artifacts/Isolate/Program.fs`) checked the identical recovered
provider, identical consumer text, on the *same checker instance*, two ways: via
`GetProjectOptionsFromScript` (a `.fsx` script — every route Q012 and Q013 ever tested) and via a
real, hand-built, non-script `FSharpProjectOptions` (`mkOptions` in Q008's own recovered `Harness/
Program.fs` — explicit `--noframework` plus ref-pack and `-r:` compiler args). The script route failed
with Q012's exact "couldn't find type ... in assembly `tmpXXXXXX`" diagnostic; the real-project-options
route resolved with zero diagnostics. Reproduced twice, deterministic both times. **This is the exact
axis Q013's own review named as the most likely place a Q008 reconstruction would differ from what
Q012/Q013 tested** ("exercise `ParseAndCheckFileInProject` over a real, non-script `FSharpProjectOptions`
project, not the `.fsx` script both Q012 and Q013 used") — and it is confirmed, not merely plausible.

**What this means for each quartet's own verdict, precisely:**
- **Q012's NULL-on-the-named-factors / generative-vs-erased finding stands, exactly as scoped.** Every
  shape Q012 tested was checked via a `.fsx` script; Q012 never claimed otherwise, and its own write-up
  flagged the untested axis honestly. Q012's result describes the script configuration correctly.
- **Q013's NULL (compile-then-PC warming) also stands, exactly as scoped.** It closed a same-checker/
  same-scenario/toy-shape *script* reconciliation — a different, narrower question than the one this
  reconstruction answers.
- **Q008 and Q009's SHIP verdicts are no longer disputed.** They are reproduced on recovered original
  source, and the mechanistic reason Q012 saw a contradiction is now identified and confirmed by direct
  isolation rather than argued from absence of evidence.
- **A real, previously unrecorded FCS behavior is now on the record for future quartets in this
  lineage:** whether `ParseAndCheckFileInProject` resolves a generative provided type depends on
  whether `FSharpProjectOptions` came from a real (non-script) project or from
  `GetProjectOptionsFromScript`, independent of and in addition to Q012's generative-vs-erased axis.
  The two are not mutually exclusive and neither one alone was previously known to be sufficient.

**What is still genuinely open, not resolved by this reconstruction:** whether Q011's own two real
providers (structurally close to Q008/Q09 but never tested via the real-project-options PC route, only
via `checker.Compile`) would also resolve that way is untested — a cheap, named follow-up. Whether
Q012's own toy probes would resolve via the real-project-options route is also untested (plausible,
given the axis found here, but not run — would fully cross-check the "generative vs erased" framing
against the "script vs real-project-options" framing on the same toy shapes). Q006 remains a plain
unverified gap (no source ever found for it, and nothing has specifically contradicted its claimed
numbers) — this reconstruction does not touch Q006.

**Historical framing kept below, for the reasoning trail, now superseded by the above:**

Q006, Q008, and Q009 — three closed, verdict-bearing quartets — had zero saved source under their
`artifacts/` folders, confirmed directly from the filesystem on 2026-07-15. Every claim in those three
quartets' `02-results.md` — quoted diagnostic text, timing numbers, "the attribute survives into
independently-reflectable IL" — existed only as prose. Q001–Q003 partially kept artifacts (Q001's own
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

**That reconciling hypothesis has now been tested, in Q013, and it failed.** Q012's own harness always
checked PC before Compile for a given shape, never the reverse — Q013 built exactly that missing
ordering test: same checker instance, `checker.Compile` of a generative scenario immediately followed
by `checker.ParseAndCheckFileInProject` of the identical scenario. Verified cell-by-cell against raw
run logs (not just the write-up's summary), the result is a clean, deterministic NULL across 3
in-process repeats and a second full process run: PC still fails with Q012's exact "couldn't find type"
diagnostic every time, while the preceding Compile resolves cleanly every time. A prior Compile does
not warm a subsequent PC for the same scenario. Q013's own review scoped this precisely: it kills the
*specific* same-checker/same-scenario/toy-shape reconciliation, not every conceivable warming mechanism
— cross-instance/process-global warming, a stricter same-project-object PC form (as opposed to the
`.fsx`-script form both Q012 and Q013 tested), and `TransparentCompiler` all remain untested and are not
logically excluded. The review also flagged a blind spot shared by the whole lineage: neither Q012 nor
Q013 has ever exercised PC over a real non-script `.fs` project, only a `.fsx` script via
`GetProjectOptionsFromScript` — if Q008's unknown harness used a real project, this axis is where a
reconstruction should look first, since nothing so far has tested it.

**Q008/Q09 have since been reconstructed and re-verified (2026-07-16) — see the update at the top of
this section.** Their SHIP verdicts are no longer disputed; both reproduced exactly on recovered
original source, and the specific axis this paragraph's own last sentence named ("if Q008's unknown
harness used a real project, this axis is where a reconstruction should look first") is exactly what
turned out to explain the contradiction, confirmed by direct isolation rather than assumed. Q006's
missing artifacts remain a plain unverified gap (nothing has specifically contradicted it, and this
reconstruction did not touch Q006), not a dispute.

## Starting the next session

Read this file for the digest, `experiments/README.md`'s Index for the one-line-per-quartet detail,
then `experiments/BACKLOG.md` for what's queued and why. Q010 and Q011 (planned at the end of the
2026-07-15 session's first half) are both closed. Q011's review named finding its own open regression's
root cause as the clear top priority; that became Q012, which answered it with something bigger than
expected: not a Q011-specific bug, but a lineage-wide finding that directly contradicts Q008's own
claimed measurement. Q012's review then named the cheapest reconciling hypothesis for that contradiction
(compile-then-PC ordering); that became Q013, which tested it and came back NULL — the reconciliation
is dead, not merely untried. See "A gap in this file's own credibility" above for the full picture.
Separately, in Thread 1: Q014 (FSI staged compilation, promoted from `BACKLOG.md` item 1's stretch goal)
was built, run, and reviewed in the same pass — its own adversarial review caught, on independent
re-execution, that the spike never actually ran FSI, quietly substituting `[<ReflectedDefinition>]`
compile-time quotation capture for the generation-time FSI evaluation its own title and SHIP threshold
named. **CLOSED, REVISE**; see the Thread 1 section above and `Q014-fsi-staged-compilation/03-review.md`.
That review's first-named follow-up — actually hosting `FsiEvaluationSession` and reifying a result whose
origin isn't a host-compile-time literal — became Q015 in the same session, also designed, built, and
reviewed in one pass. Its review, independently reproducing the result at a third `n` value, found all
four SHIP conjuncts genuinely met (**CLOSED, SHIP, narrowly scoped**) — Q014's specific "no FSI ran" gap
is closed for real — but surfaced a second, subtler substitution the design and results write-up both
missed: FSI hands back a `MethodInfo` via a quotation-destructure one-liner that never executes anything,
so 100% of the actual partial-evaluation logic is still host-compiled code identical to Q014's. See
`Q015-fsi-dynamic-origin-staging/03-review.md` for the full picture and four named follow-ups, the first
of which — making FSI perform real computation rather than handing back a pointer to code the host
already knows the shape of — is the direct next step if this sub-line continues.

**Update, 2026-07-16: the top-priority item named above — reconstruct Q008/Q09's actual harness and
re-verify — is DONE, and it succeeded.** Q008 and Q009's original scratch source was found intact
(unintentionally preserved in a session job's own temp directory), rebuilt unmodified, and reproduced
both quartets' claimed results exactly. A direct isolation then confirmed the reconciling axis named
above — script-derived vs real, hand-built `FSharpProjectOptions` given to
`ParseAndCheckFileInProject` — as the actual explanation: same checker, same recovered provider, same
consumer text, script route fails with Q012's exact diagnostic, real-project-options route resolves
cleanly, deterministic across repeats. See "A gap in this file's own credibility" above and
`Q008-provenance-closed-loop/RECONSTRUCTION.md` / `Q009-field-level-provenance/RECONSTRUCTION.md` for
the full account. Both SHIP verdicts stand, no longer disputed.

**Next priorities, now that the reconstruction is closed:**
1. **Retest Q011's own two real providers via the real-project-options PC route** (they were only ever
   checked via `checker.Compile`, never via the route that now resolves Q008/Q09's providers) — cheap,
   would settle whether Q011's specific regression is provider-shape-specific or was simply hitting the
   same script-vs-real-project axis all along.
2. **Retest Q012's toy probes via the real-project-options route** — would fully cross-check
   "generative vs erased" (Q012's axis) against "script vs real-project-options" (this reconstruction's
   axis) on the same minimal shapes, confirming both are real and independent rather than one
   subsuming the other.
3. Unchanged and still open: **Q010's own follow-up 1** (instrument the reentrant call to determine
   whether it was landing on genuinely in-flight state or an already-idle checker). **Q007** remains the
   best-supported unrun hypothesis from the original queue if none of the above is preferred.

Two smaller, cheap items still worth doing regardless of what's picked next: fix the duplicate-diagnostic
defect that has recurred unfixed across Q008, Q09, and Q011 (three occurrences); and locate the actual
FCS code path responsible for the script-route PC failure to resolve a generative provider's backend
assembly — Q013 narrowed where to look on the script route specifically (PC never consults Compile's
output, so the gap is in PC's own emit path, not cross-API cache sharing), but the actual code path is
still unlocated, and it's now known to matter only for the script-options configuration, not universally.

Separately, Q016 (satellite-DLL type provider, `BACKLOG.md` item 15) was started in the same session
this reconstruction happened in — check its own status in `README.md`'s Index before assuming it's
still open.
