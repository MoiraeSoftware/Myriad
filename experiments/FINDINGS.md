# Findings so far — what's real, what's not

Cross-quartet digest as of 2026-07-19, thirty quartets in (Q001–Q030, twenty-eight closed, two
planned).
Separately, on 2026-07-16, Q008 and Q009's missing artifacts were filled in by recovering
and re-verifying their actual original source — see "A gap in this file's own credibility" below; this
was a reconstruction of existing verdicts, not a new quartet, so it doesn't change the quartet count.
This is not a replacement for reading a closed quartet's own `03-review.md` — each is written to stand
alone — it's a synthesis for deciding what to do next without re-reading all twenty-eight. Two intertwined
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
- Q010's reentrant `DocumentSource.Custom` mechanism was proven only for one check per checker
  instance; `BACKLOG.md` item 18 (the FSAC-as-live-sidecar idea, itself modeled on rust-analyzer's
  out-of-process proc-macro architecture) needed to know whether it survives the load pattern a real
  editing session actually produces — one persistent checker serving many sequential checks, with the
  watched file changing between them. **It does**, confirmed and independently reproduced: on a
  persistent `FSharpChecker` (`useTransparentCompiler = true`), four sequential edit-then-recheck
  cycles — including a deliberate return to a previously-seen value, to rule out a fixed one-cycle-lag
  artifact — each produced correct, freshly-recomputed, zero-diagnostic results, with no staleness and,
  unanticipated, no explicit version-bump or invalidation discipline required. That retires item 18's
  actual correctness premise. One genuine new constraint surfaced alongside it: Q010's own footgun
  (`ParseAndCheckFileInProject` with explicit source text silently bypasses `DocumentSource.Custom`)
  reproduced independently in a fresh harness, confirming it isn't scenario-specific — the natural
  per-file incremental API an LSP host has in hand on `didChange` cannot drive this mechanism at all,
  only a whole-project check can. A secondary claim in the quartet's own results write-up — that the
  whole-project check "always fully re-checks every file, unconditionally," implying a real
  keystroke-cost problem — was struck by adversarial review as unsupported: the evidence shown is
  equally consistent with a cheap cache hit under `TransparentCompiler`'s content-hash model, and the
  two-file toy tested cannot discriminate the two. Whether an FSAC-hosted version of this mechanism
  would be keystroke-cheap or expensive is therefore still genuinely open, pending a real scale test
  (Q021).
- `BACKLOG.md` item 8 — hooking Myriad's own `MyriadSdkGenerateCode` MSBuild target into the
  design-time-build (DTB) path directly, rather than routing around Myriad's IDE-invisibility gap from
  the type-provider side the way every prior attempt (Q006, Q016-21) did — was finally spiked as Q022,
  and for the first time in this repo's history the "works live in the IDE" half of a claim was tested
  against a literal `fsautocomplete` (FSAC) process over real LSP, not `FSharpChecker`-as-a-library.
  Both halves of the mechanism hold, independently reproduced: removing the target's
  `Condition="'$(DesignTimeBuild)' != 'true'"` gate makes Myriad's real CLI run during a genuine DTB
  (`SkipCompilerExecution=true`, the real compiler never invoked, confirmed by a frozen `bin/*.dll`
  mtime under a moved canary) and correctly regenerate a changed attributed type's output; a fresh FSAC
  0.83.0 session then genuinely shows the new member with zero `dotnet build`, matching a gated negative
  control that correctly fails the same check. But an already-*running* FSAC session does not pick up a
  bare source-file save — codegen only re-runs when the `.fsproj` itself is touched (even a no-op mtime
  bump) and the project is reloaded, a real, demonstrated in-session trigger the quartet's own first pass
  wrongly concluded didn't exist until independent review found it. Net: **closes the IDE-invisibility
  gap at project load/reload time, not during live source editing** (Q022, REVISE). *Since applied for
  real*, 2026-07-17: the gate removal was carried from Q022's scratch copy into the actual shared
  `src/Myriad.Sdk/build/Myriad.Sdk.targets` and re-verified against a real DTB invocation — this
  confirms the mechanism outside the quartet's own harness but does not change the verdict above (the
  live-edit gap it names is still open). See `BACKLOG.md`'s known-engineering-gaps section for the
  applied-fix account.
- `BACKLOG.md` item 18's own named top-priority follow-up — a real scale test of Q010/Q021's reentrant
  callback, "a few hundred files, edit one, measure `ParseAndCheckProject` cost on the unchanged
  remainder" — was finally run as Q023, at N up to 300 with genuinely weighted files (not `let x = 5`
  padding), and the honest finding only emerged after adversarial review corrected the executor's own
  headline claim. The executor's write-up concluded editing one file costs close to a full cold rebuild
  regardless of project size — apparent evidence for "no effective caching, unconditional full recheck."
  Independent review found this was an artifact of always editing file index 0 (the *first* file, with
  the maximum number of compilation-order successors) — a real methodological blind spot, not a
  fabrication, but one that inverted the conclusion. A position sweep at N=300 (the review's own variant,
  then independently re-confirmed with a durable, checked-in artifact) shows per-edit cost is linear in
  the number of files *after* the edited one: editing the **last** file is statistically indistinguishable
  from a no-op repeat (255ms vs. 258ms), editing the **first** costs close to cold (1339ms vs. 2640ms
  cold). FCS's `TransparentCompiler` genuinely does skip the compilation-order prefix before an edit and
  only re-checks the tail below it — real, working incremental caching, not the absence of it. The
  caveat that keeps this from being unambiguously good news for Myriad specifically: attributed domain
  types often sit early in build order precisely because other code depends on them, which is close to
  the worst-case position actually measured, so the *expensive* case isn't a rare pathology for Myriad's
  own real usage even though it isn't the universal case the executor's write-up first implied (Q023,
  REVISE).
- Q023's own review Follow-up 1 — does the linear-in-successors cost model hold at more than the one
  N it was spot-checked at — was run for real as Q024: a full sweep of 5 edit positions at each of
  N ∈ {10, 50, 150, 300} (20 runs, reusing Q023's own spike verbatim, no new code). **SHIP, scoped.**
  The per-successor marginal cost fits a clean line at every N, and the fitted slope shows no
  detectable systematic drift from N=10 to N=300 — independently reproduced, with the peak slope
  landing at a *middle* N in both the executor's and the reviewer's own rerun, affirmatively refuting
  the pre-registered "slope scales with N" REVISE trigger. The review scoped three framings down
  without reversing the verdict: "essentially N-invariant" overstates what 5 points per fit can
  support (95% CI ±76% at N=10, only N=150/300 tightly constrained — the honest claim is "no
  *detectable* drift," not a proven invariant); "intercept tracks the repeat median" is mildly
  circular, since the zero-successor point sits inside the same regression that produced the
  intercept; and one N=50 data point the write-up itself flagged as anomalous was confirmed, on
  rerun, to be a sampling outlier rather than a real plateau. The most consequential scoping for
  citation purposes: **this measures a cost model (whole-project `ParseAndCheckProject`, independent
  files), not a real FSAC live-editing session** — cite as "per-edit cost is a stable linear function
  of compilation-order successors, slope constant across N≤300, on independent files," never as
  "FSAC keystroke cost characterized." Myriad's real attributed types still tend to sit early in
  build order, near the expensive end of the measured curve (Q024, SHIP scoped).

- Q015's own review named the sharpest unmet goal in this whole sub-line: FSI never performed real
  computation, only compiled a plugin and handed back a `MethodInfo`. Q025 attacks the same goal by a
  different route — never host FSI at all, hand-walk FCS's own resolved `FSharpExpr` tree directly
  (reflection-invoking already-compiled reference methods for the leaves), prompted by inspecting
  `FSharp.Compiler.PortaCode`'s dead 2018-era interpreter (not ported; hand-built fresh against the
  current `43.9.101` pin). **SHIP, scoped.** A hand-written interpreter matching exactly six
  `FSharpExprPatterns` cases (`Const`, `NewRecord`, `Call`, `Let`, `Value`, `IfThenElse`) against a real,
  checker-produced expression tree (`keepAssemblyContents = true`, never a hand-built expr or quotation)
  correctly reflection-constructs records — including nested ones, a generalization the review confirmed
  the executor never tried — and reflection-invokes already-compiled reference functions across a
  `Let`/`Call`/`IfThenElse` chain, with the interpreted result driving a genuinely differing generated-
  member-name list. Zero `FsiEvaluationSession`/`Reflection.Emit` anywhere, grep-confirmed by review, not
  just self-reported — this is the first `FSharpExpr`-body interpretation in either research thread, and
  it reaches Q015's unmet goal by construction rather than by adding FSI-usage discipline to Q015's own
  mechanism. But the review, going past the two pre-registered fixtures, found a real accuracy defect in
  the frozen docs: both `01-design.md` and `02-results.md` claim generic method calls fall through to the
  interpreter's named `NotImplementedException` safety net. They don't — a generic call matches the
  ordinary `Call` arm and dies with a raw, unspecific `InvalidOperationException` from `mi.Invoke` on an
  open generic method, because the `Call` arm discards the resolved `methodTypeArgs` and never calls
  `MakeGenericMethod`. That defeats the exact "fails loudly and specifically, never silently" guarantee
  the design leaned on, for a case both docs explicitly listed as covered — a write-up-accuracy defect of
  the class this whole review discipline exists to catch, not a result failure (it doesn't touch any SHIP
  conjunct, since neither pre-registered round used a generic call). The scope that travels with the
  SHIP, unchanged by the correction: this is a mechanism proof over six patterns and two hand-picked
  fixtures, not "generation-time computation solved" — `Lambda`/`Application` (interpreting a function's
  *body*, as opposed to a value binding that merely calls functions), DU pattern matching
  (`NewUnionCase`/`UnionCaseGet` — i.e. any of Myriad's own union-based generators), recursion
  (`LetRec`), and now confirmed generic calls are all unsupported, exactly the hypothesis's own
  REVISE ceiling ("a production interpreter would need to reimplement a materially large fraction of F#'s
  semantics"). Cite Q025 as "direct `FSharpExpr` interpretation is viable for a small closed set of
  shapes with no FSI," never as "generators can now compute at generation time" (Q025, SHIP scoped).
- `BACKLOG.md` item 22 named the first cross-mechanism integration milestone in this thread: every prior
  reentrant-composition result (Q010, Q021, Q023, Q024) exercised the mechanism against hand-typed
  stand-ins, never Myriad's own real generator code — Q010's own "B.fs" is a hand-typed transcription of
  what `LensesGenerator` produces, and its "second generator" is a bare inlined function that never
  implemented `IMyriadGenerator` at all. Q026 closed item 22's own cheapest falsifier (deliberately
  narrower than its full live-watcher vision): the real, unmodified `LensesGenerator` — discovered via
  `MyriadGeneratorAttribute` reflection and `Activator.CreateInstance`, exactly mirroring
  `src/Myriad/Program.fs`'s own plugin-discovery code, then formatted through the real Fantomas pipeline —
  is reentrant-queried inside one in-process `FSharpChecker` by a second, real, similarly-discovered
  `IMyriadGenerator` (`ReentrantJsonGenerator`), verified at Q010's own bar: both callback sentinels fired
  (no silent `DocumentSource.Custom` bypass), zero diagnostics, and two independent symbol-resolution
  checks confirming the generated serializer's references resolve to the real generated file, not a
  textual coincidence. Reproduced 12/12 by review, plus a three-field, mixed-type shape the executor never
  tried, generalizing cleanly. **SHIP, scoped — with review adding two new, concrete findings rather than
  merely reproducing.** First: a nested non-primitive field shape shows the reentrant *mechanism* still
  correctly surfaces the real nested type by typed inspection, but the second generator's own toy
  type-sniffing then emits code that fails to typecheck — caught as a real diagnostic at composition time,
  evidence for the in-process-hosting thesis even as it scopes the SHIP down to "the composition mechanism
  works," not "the composed generator is production-grade." Second, and more consequential for anything
  built on this: the static side-channel standing in for the checker/options handle
  `IMyriadGenerator.Generate` has no parameter to carry races roughly 50% of the time when reused for a
  second composition in the same process (`FSharpChecker`'s `DocumentSource` callback gives no cross-file
  ordering guarantee) — concretely demonstrating that a real persistent, multi-composition host (the
  deeper ask behind items 18/22) needs `GeneratorContext` itself widened, not a global mutable standing in
  for it. Review also converted the disclosed `Assembly.LoadFrom`-vs-`PluginLoader` ALC gap from
  speculation to evidence (the side-channel's single shared assembly load is exactly why it works today,
  and an isolated ALC would not guarantee it) and reconfirmed directly from `src/Myriad/Program.fs` that
  Myriad's real CLI still has zero `FSharpChecker` usage — this closes item 22's cheapest falsifier
  precisely, without showing Myriad's actual CLI can host any of this today (Q026, SHIP scoped).
- Q010's own review (Objection 1) named a gap every quartet since inherited unresolved: is the
  reentrant `DocumentSource.Custom` call, made from inside a later file's callback while the outer
  `ParseAndCheckProject` is still on the stack, genuinely fresh work, or a cache hit landing on state
  FCS's own file-order walk had already resolved before ever asking for the later file's source? Q021,
  Q023, Q024, and Q026 all restated this caveat by name and added no instrumentation. Q027 finally
  built it, deliberately isolated in a minimal hand-typed 3-file toy (not a real-generator harness, so
  generator-invocation cost couldn't confound the FCS-internal signal). **REVISE — the load-bearing
  conclusion is confirmed and strengthened, but the executor's own magnitude framing was overturned by
  review.** Reproduced across 5 independent processes: a genuinely warm repeat costs 2-4ms, while both
  reentrant calls cost tens to hundreds of milliseconds — decisively ruling out "reentrant lands on
  already-cached/idle state," exactly the benign explanation Q010's review flagged as equally
  consistent with that quartet's own aggregate timing. But the executor's own headline number — the
  *first* reentrant call costs 4.2x *more* than an isolated cold check, read as possibly "reentrancy
  overhead on top of first-touch cost" — did not survive review's own added controls: ~91% of that gap
  is a pure position-in-process artifact (the reentrant call happens to be the very first typecheck
  performed in the process, paying a one-time ~400-500ms JIT/FCS-static-init/assembly-metadata-read
  tax that a cold baseline measured 5th, in an already-warm process, never pays), with a further ~20%
  of the residual gap traced to an uncontrolled `keepAssemblyContents` mismatch between the checkers
  being compared. Most tellingly, a *direct*, non-reentrant first check on the identical checker costs
  *more* than the reentrant one (~929ms vs ~515ms) — proving reentrancy makes the first touch *cheaper*,
  not more expensive, because the outer call has already begun building the shared project snapshot by
  the time the callback fires. Review also closed the executor's own disclosed-as-inference,
  unresolved question — why the first reentrant call (A) cost more than cold while the second (B) cost
  less — by reversing the call order and adding a third call: it is a call-order amortization effect,
  not a property of which file is checked, confirmed directly rather than left as a guess. Warmed, both
  reentrant calls (17-48ms) land squarely on the design's own pre-registered REVISE outcome — "reentrant
  sits meaningfully between cold and warm, not close to either" — the correct citable claim, never the
  raw first-in-process numbers the results write-up led with. A new, lineage-wide caution came out of
  this as a side effect: any single "first check" timing this whole thread has ever reported carries
  this same one-time tax and should be re-read with that in mind — `BACKLOG.md` item 23 now names the
  cheap follow-up (spot-checking whether Q023/Q024's own cost-model numbers were affected) (Q027,
  REVISE).
- `BACKLOG.md` item 23's own follow-up — does the tax Q027's review found also inflate Q023/Q024's own
  `cold` numbers, since both quartets measured `cold` as the first `ParseAndCheckProject` call in a
  fresh process — was run as Q028, reusing Q023's own checked-in spike code unmodified plus an opt-in
  throwaway warmup step. **REVISE.** Independently reproduced on a clean rebuild: yes, the tax is real
  in Q023's own actual harness (not just Q027's separate toy), roughly fixed in absolute size across
  N=10 and N=300 (~570-577ms both, not proportional to N), and it inflates `cold` enough to shift
  Q023's own published `cold`-denominated ratios materially (`editOneRatio` at N=10 rises ~2.7x once
  isolated by warmup, reproduced within a few percent independently). The corrected direction is
  conservative: `editOne` is a *larger* fraction of a tax-corrected `cold` than Q023 published, not
  smaller, so neither quartet's qualitative verdict is disturbed. REVISE rather than SHIP for one
  specific reason: the pre-registered hypothesis wrongly claimed the correction reaches "`editOneRatio`
  values feeding Q024's regression" — Q024's own fitted slope is ms/successor, not ratio-based, and
  Q024 publishes no ratio column at all, so it's cold-independent and untouched; the correction lands
  only on Q023's own ratio/percentage prose (Q028, REVISE).
- `BACKLOG.md` item 18's own single highest-priority named follow-up, restated by every quartet in this
  sub-line since Q021 — does FSAC's real, LSP-driven, per-file incremental editing path (a literal
  `fsautocomplete` process, not a direct `ParseAndCheckProject` call) reproduce Q023/Q024's
  position-dependent, linear-in-successors cost curve — was finally run as Q029, reusing Q022's own
  hand-rolled LSP harness and Q023's genuinely-weighted file generator. **SHIP, scoped.** Independently
  reproduced from a clean FSAC 0.83.0 restore and rerun: a live FSAC session's per-edit re-analysis cost
  is a monotonic, ~linear function of compilation-order successors after the edited file, matching
  Q023/Q024's cost model through FSAC's real path (last file ~6-9ms, first file ~120ms at N=20 / ~243ms
  at N=40, ~6ms/successor). The load-bearing control is stronger than a simple number match: the raw LSP
  transcript shows the `documentAnalyzed` cascade is strictly position-gated — exactly `successors+1`
  files re-analyzed, in compilation order, verified by hand at three edit positions — decisively ruling
  out a position-blind refresh or stale-cache false reading, the one failure mode that would have made
  this an artifact. This is the best-controlled real-FSAC measurement in the lineage, closing item 18's
  own top-named follow-up on the favorable side: the cost model transfers to a real editor session, not
  just a direct-API harness. Review trimmed one framing overshoot, the identical correction Q024's own
  review made for the same claim: "N-invariant per-successor rate" overreaches on two N values — the
  supported claim is "no detectable drift across N=20 and N=40." One finding cuts toward the pessimistic
  Myriad reading: the tested edit was value-only (exported signature byte-identical) yet FSAC still
  re-checked the whole tail — FSAC invalidates on file content, not on whether the exported signature
  changed. Scoped hard: small N (≤40), one linear dependency-chain topology that conflates
  compilation-order-successors with true dependents (can't yet tell whether FSAC invalidates by order or
  by precise dependency — inherited from Q023/Q024), single session per size, curve shape/scaling shown,
  not absolute large-project keystroke latency (Q029, SHIP scoped).
- Q023, Q024, and Q029 all disclosed the same blind spot as their own top follow-up: every one of them
  used a linear dependency chain (file N always references file N-1), making "compilation-order
  successors" and "true dependents" identical by construction, so none could tell whether FSAC/FCS
  invalidates by real dependency or by raw compilation-order position. Q030 built the lineage's first
  wide/shallow topology to separate the two — N=30, an early "Hub" file referenced by only 3 of the 27
  later files, scattered among 24 unrelated ones — and answered it. **SHIP, scoped, a rare clean pass:
  the reviewer found no framing overshoot to trim.** A value-only edit to Hub re-analyzed **all 27
  order-successors**, not just the 3 true dependents — the entire compilation-order suffix — reproduced
  3/3 reps and confirmed independently by hand-recounting the raw LSP transcript against a topology
  verified directly from generated source, not the write-up. Two controls close off the obvious
  alternative explanations: editing a file referenced by *nothing* still cascades to its full
  order-suffix, proving the rule is purely positional and reference-blind; the two pre-Hub files never
  re-fire across any of 10 edit cascades, ruling out a blanket project-wide refresh. The harness solicits
  analysis only on the edited file itself, so the measurement-vs-mechanism confound that struck Q021 (an
  earlier "always fully re-checks" claim the review there found unsupported) doesn't apply here — these
  are FCS-driven events, not harness-solicited ones. **This resolves the order-vs-dependency ambiguity
  Q023/Q024/Q029 each named as their own top follow-up, on the pessimistic side for Myriad**: an early
  attributed type pays the full order-suffix re-check cost regardless of how few files truly reference
  it. The review went past the executor's own framing and found the mechanism is stronger than "an FSAC
  choice that might be relaxed later": F# checks every file against the accumulated signature environment
  of *all* preceding files, so the compilation-order suffix genuinely *is* the dependency set in F#'s
  ordered-file model — the pessimistic reading is structural to the language, not a version-specific FCS
  limitation a later release might fix. Overturns nothing: Q029's cost curve stands, its x-axis is now
  confirmed to be order-successors, not an over-pessimistic proxy. Scoped: value-only edit only
  (signature-changing edits untested), one N, one hub position — though three different edit positions in
  the same session, including a file referenced by nothing, substantially de-risk the generalization
  concern this lineage has been burned by before (Q023's own single-edit-position mistake) (Q030, SHIP
  scoped).

**Honest net position:** ten SHIPs (Q002 fully, Q003 narrowly, Q010 scoped, Q015 scoped, Q021 scoped,
Q024 scoped, Q025 scoped, Q026 scoped, Q029 scoped, Q030 scoped) prove the mechanism is sometimes
genuinely valuable — with Q015, Q021, Q024, Q025, Q026, Q029, and now Q030 all earning a SHIP only once
heavily scoped, a pattern worth noticing on its own: this thread's positive results keep shrinking on
inspection, not just its negative ones. Notably, Q030's own scoping cuts the other way from most of that
list — it isn't a narrower capability than first claimed, it's the same clean finding with a harder,
structural explanation attached, landing unfavorably for Myriad rather than favorably. Seven REVISE/NULL
results (Q001, Q006, Q014, Q022, Q023, Q027, Q028) prove overclaiming is
easy. Q001 is the odd one out in that list: its NULL ("typed beats syntax" changed nothing for a pure
structural-echo generator) is the standing check *against* overclaiming everywhere else in this file, not
an instance of it. The overclaiming itself shows up in six distinct shapes across five of the other
REVISE/NULLs plus one SHIP (Q015, which overclaimed on the way to earning its verdict) — named here by
quartet rather than by a
loose running count, since an earlier draft of this paragraph numbered them inconsistently against a
different six-item list (the REVISE/NULL tally above, which is not the same six quartets), a confusion
exactly of the kind this file's own "gap in this file's own credibility" section exists to prevent:
**Q006** — a structural wall (type providers can never see a type from the compilation currently in
progress) initially read as an engineering gap a cleverer static parameter might route around, not a hard
boundary. **Q014** — mechanism substitution: the spike quietly substituted host-compile-time
`[<ReflectedDefinition>]` capture for the generation-time `FsiEvaluationSession` evaluation its own title
and SHIP threshold named, uncaught by its own design or results write-up before adversarial review. **Q015**
— a second, subtler substitution inside the very follow-up built to close Q014's gap: FSI genuinely ran
this time, but only to compile a plugin and hand back a `MethodInfo` via a one-line quotation destructure
that never executes anything, so the real partial-evaluation logic stayed 100% host-compiled code
identical to Q014's. **Q022** — negative-claim overreach: a results write-up's own sharpest claim was a
*negative* one ("no in-session reload signal exists, short of a process restart"), stated more absolutely
than the evidence supported — the executor had ruled out several specific signals but generalized to "no
signal works," and review, by trying one the executor hadn't (an actual `.fsproj` mtime change, not just a
notification claiming one happened), found a working in-session reload path and corrected the claim back
down to what the design's own pre-registration had predicted. **Q023** — a single, unrepresentative
worst-case condition (always editing the compilation-order *first* file) reported as the general case —
real data, real numbers, correct code, wrong scope — caught only when review swept the one variable (edit
position) the design held fixed. **Q027** — a real, reproducible, correctly-measured number whose
comparison was confounded by two uncontrolled variables (position-in-process, and an uncontrolled
`keepAssemblyContents` mismatch between the checkers being compared), making a directionally surprising,
reportable-sounding magnitude claim ("reentrant costs *more* than cold") look like a finding when it was
an artifact — caught only when review built a warmed-process control and a direct-first-check control the
executor's own design never attempted. Q028 and Q029 both recurred an already-named shape rather than
adding a new one, worth noting because it's evidence the taxonomy above is stabilizing, not still growing
one quartet at a time: Q028's own review flagged its hypothesis's claim that the correction reached Q024's
regression as "the lineage's recurring overclaim shape" (a consequence asserted broader than what was
shown, the same genus as Q022's negative-claim overreach, though milder and in the safe direction); Q029's
"N-invariant per-successor rate" was the identical overreach Q024's own review had already scoped down for
the same underlying claim, recurring a third time. **Nothing has been built that would replace Myriad's current
pipeline end to end**, and nothing in `experiments/` has been merged into `src/`. Q010's own review named
a gap every quartet since inherited unresolved — whether the reentrancy tested was genuinely mid-flight or
landing on an already-idle checker — and **Q027 has since settled the load-bearing half of it**: five
independently reproduced process runs decisively rule out "reentrant lands on already-cached/idle state,"
the specific benign explanation Q010's review flagged as equally consistent with its own aggregate timing.
What Q027 does not settle, by its own explicit design (no thread instrumentation added): literal
concurrency of FCS's internal file-order walk relative to the reentrant call. Q010's review also settles
only the acyclic case (a later generator depending on an earlier one), not mutual cross-generator
dependency. Q004 (cross-assembly typed access) and Q005 (self-verifying generators — typecheck-before-emit)
remain the next pre-registered steps from the original queue and neither has been run. Q015's own review
named "make FSI do work that is actually computation" as the most direct unrun next step in this sub-line —
**Q025 has since closed that goal, by a different route (direct `FSharpExpr` interpretation, no FSI hosted
at all)**; the most direct unrun next step now is Q025's own named gaps (`Lambda`/`Application`, DU pattern
matching, recursion, generic method calls), not Q015's superseded one.

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

**Q007 then ran the best-supported unrun hypothesis in the whole backlog — and came back REVISE, on
cost and on capability, not on the mechanism.** Composing two already-proven pillars (Q003's FSI/
`FSharpChecker` coexistence, Q006's working string-static-parameter provider) for the first time: a
generative provider's `DefineStaticParameters` instantiation function hosts a fresh
`FsiEvaluationSession` and evaluates its string static argument as real F# code, genuinely nested on
`fsc.exe`'s own live compilation call stack. The cheapest falsifier passed cleanly (real `dotnet
build`, 0 errors, independently-reflected members matching the FSI-evaluated value exactly) and a
richer `string list` value drove a variable-count, named set of generated members with no
`InvalidCastException` on the `FsiValue.ReflectionValue` downcast — both reproduced byte-for-byte by
review. But SHIP requires all three conjuncts, and cost fails the one that matters in practice: the
live-edit re-check (~185-231ms) lands ~4-5x Q006's 47ms baseline, and the review's own isolated
`FsiEvaluationSession` create+eval+dispose benchmark (no type provider, no `FSharpChecker` in the
process) found ~130-140ms of that is fixed per-instantiation session lifecycle, expression-independent
(an arithmetic expression and a 3-element list cost the same) and therefore cacheable — turning REVISE's
registered "should ship as an opt-in/cached feature" wording from a hedge into a specific, actionable
finding, not a vague caveat. The review also separated mechanism from capability more sharply than the
executor did: the richer-value case actually tested (one generated member per name in a string list) is
byte-for-byte replicable by a CSV-splitting provider with zero FSI, which is precisely the hypothesis's
own pre-registered NULL description — no case was built where FSI's arbitrary-code evaluation bought
something a delimiter convention couldn't, and neither a record nor a function (the SHIP clause's own
named payload) was ever evaluated. And the provider's failure path, never exercised by the executor and
triggered directly by the review, turned out not to be low-risk in the way "inferred low-risk" implied:
an invalid or unsupported static string hard-crashes `fsc.exe` (`0xE0434352`) under `dotnet build` and
throws an escaping `AggregateException` out of `ParseAndCheckFileInProject` under a persistent checker —
though the checker does recover cleanly on the next valid edit, so KILL's state-corruption arm never
fires. Net: the nested-hosting mechanism is real, safe, and new; do not cite Q007 as "type providers can
now take rich typed static arguments" as a capability, only as "the mechanism to do so safely exists, at
a cost, on a case that didn't need it." One unplanned, valuable side effect: building this quartet's
`artifacts/` in place (rather than an out-of-repo scratch dir, unlike every predecessor) surfaced a real
bug in the repo's root `.gitignore` — an unanchored `artifacts/` rule silently matching every
`experiments/Qxxx/artifacts/` folder, very likely the actual mechanical explanation for Q006's artifacts
going missing outright and Q008/Q09 needing `RECONSTRUCTION.md` — now fixed (anchored to `/artifacts/`,
root-only), verified by the review to correctly expose every quartet's evidence while leaving `bin/`/
`obj/` still ignored (Q007, REVISE).

**Q016 then took the lineage somewhere new: real data accessors forwarding into Myriad's own compiled
output, not synthetic markers — and came back REVISE, with the mechanism win narrower than it looks and
the capability loss landing on the provider's one reason to exist.** `BACKLOG.md` item 15 asked whether
a generative provider could re-expose members that are really Myriad's own compiled generator output,
via `Assembly.LoadFrom` of a satellite DLL that Myriad's real, unmodified CLI produced and
`checker.Compile` built — the cross-project usage shape, sidestepping Q006's same-compilation wall by
construction rather than testing against it again. Round 1 passed: `Expr.Call(realMethodInfo, args)`
forwards into the real compiled DLL, four attempts deep to find that `sourceAssemblies =
Assembly.LoadFrom`-at-construction is the required incantation (`Assembly.Load(bytes)` and
`RegisterGeneratedTargetAssembly` each fail differently). Round 2 (regeneration + live pickup) failed:
the same `LoadFrom` that Round 1's identity match needs holds a file-lock that blocks the separate
builder process from overwriting the DLL, so `Invalidate()` never fires. The adversarial review — unable
to independently rebuild or rerun anything for its entire window due to a sustained command-safety-
classifier outage on effectful commands, a disclosed gap against this lineage's own independent-
reproduction standard, not a silent one — still sharpened the verdict materially from source inspection
alone: the provider is hand-fit to one record shape, with the one available differently-shaped member
(`map`, function-typed parameters) never tried, so generalization is a hypothesis, not a result; Round
1's "two independent checks agree" forwards trivial identity-echo getters indistinguishable from a
reimplementation, so the forwarding claim currently rests more on `Expr.Call` being present in the
source than on runtime evidence that could actually discriminate the two (the same NULL-adjacent trap
Q001 named for pure structural echoes); and, most consequentially, Round 2's lock breaks the *one thing*
this cross-project shape needs a type provider for at all — `BACKLOG.md` item 15's own text already
conceded a TP is barely earning its keep over an ordinary `<ProjectReference>` except for live
re-exposure, and live re-exposure is exactly what the lock defeats. See `Q016-satellite-dll-type-
provider/03-review.md` for the full four-objection breakdown and prioritized follow-ups (rerun Round 1
on a function-typed shape first; only then is a collectible-`AssemblyLoadContext` Round 2 mitigation
worth attempting).

**Q017 then closed that top follow-up — SHIP, with the generalization confirmed but reframed, and a new
brittleness found that matters more than the headline.** Q016's review named the function-typed-parameter
gap as its single highest priority; Q017 wrapped Myriad's own real `map` function (two
`FSharpFunc<_,_>` parameters plus a record) with the identical kernel and it passed cleanly, zero new
scaffolding. Unlike Q016's own review, Q017's reviewer *could* execute — it independently rebuilt and
reran the falsifier, then went further, building two shapes the executor never tried: a genuine
`FSharpFunc` **return** value (a function flowing *out* of the provider) and an `FSharpFunc` whose
element type is the real record rather than a primitive. Both passed with no new API. But the review
reframes what the SHIP actually means, in three ways worth carrying forward: the PASS was predictable,
not a discovery — the vendored SDK's own `convType` has no `FSharpFunc` special-case, so this is Q016's
attempt-4 "derive types from real reflection" decision working exactly as that decision implied, not new
machinery; "function-typed" carries no more weight than "reference-typed" — the provider forwards
function values as opaque object references in both directions and never touches a closure; and, the
most consequential finding, the kernel is **brittle to F#'s curried-arity flattening** — a source-level
function-returning-function usually compiles to a flat multi-arg method rather than an `FSharpFunc`
return, invisible from the source signature, and a `ProvidedParameter` list built from a real but
arity-mismatched `MethodInfo` throws and **poisons the entire provided type**, not just the offending
member. This is a live, previously unknown risk for any future attempt to reflection-forward `Lenses`'
own getter/setter-shaped output this way (`BACKLOG.md`'s general-ideas section names exactly that
possibility) — compiled arity, not source arity, would need checking first. See
`Q017-satellite-function-typed-forwarding/03-review.md` for the full account.

**Q018 then tested Q016's other named follow-up — the collectible-`AssemblyLoadContext` mitigation for
Round 2's file-lock — and closed it REVISE, but a materially weaker REVISE than Q016's own, with the
mitigation now effectively exhausted in-process rather than merely unproven.** Staged in three rounds:
Round A (isolated, no type provider) confirmed a collectible ALC genuinely releases the Windows file-lock
— but only with `TieredCompilation=false`, a non-default JIT setting no host under Myriad's control would
set; with default tiered compilation it deterministically failed to collect 3/3 even under sustained
forced-GC pressure. Round B overturned the design's own prediction: FCS constructs a *fresh* provider
instance per `checker.Compile()` call, not the same instance persisting across `Invalidate()`, so
`sourceAssemblies` staleness was never the real blocker — old-instance retention is. Round C, the actual
mitigation, failed even in its most favorable possible form: an ideal, externally-triggered eviction
called *before* the write attempt (deliberately sidestepping a chicken-and-egg problem in the original
`FileSystemWatcher`-triggered design, since a change-triggered watcher can't release a lock that blocks
the very change it's waiting for) still couldn't release the lock. The review — which, unlike Q016's own
reviewer, could execute everything — reproduced all three rounds and then closed off the one escape route
the executor left open: it tested a fresh `FSharpChecker` per regeneration cycle (the executor's own named
"single most informative untested follow-up") and found it *also* fails, because the retained reference is
process-global inside FCS's own type-provider hosting, not scoped to any one checker instance. It also
ruled out a competing explanation (FCS's own `-r` referenced-assembly reader isn't the culprit — confirmed
by a no-provider control that compiled and overwrote cleanly) and tested a narrower JIT-tuning alternative
(`[<MethodImpl(AggressiveOptimization)>]`) that also failed, confirming the `TieredCompilation` caveat has
no library-controllable workaround. Net effect, stated plainly by the review: there is now no demonstrated
in-process fix for Round 2's lock, and this tightens Q016's own objection 3 further — the cross-project
satellite-DLL type provider's sole differentiator over an ordinary `<ProjectReference>` (live re-exposure)
has no known working in-process implementation on Windows. The review's own top follow-up, if this
sub-line continues at all, is an out-of-process design-time host (untested, a large lift that may not beat
a plain `<ProjectReference>`) or simply dropping the live-re-exposure goal for this case. See
`Q018-collectible-alc-round2-mitigation/03-review.md` for the full account, including the review's own
three additional experiments (fresh-checker, FCS-reader isolation, `AggressiveOptimization`) that the
executor's own results left as named-but-untested follow-ups.

**Q019 then took the lineage in a different direction from Q016-18: instead of routing a generative
provider around Q006's wall by construction (cross-project, already-compiled), it asked whether an
*erased* provider could sidestep the wall by never needing to cross it at all — and came back SHIP,
scoped, with the write-up's own headline mechanism finding struck by review as non-reproducing.**
`BACKLOG.md` item 14 asked whether a provider could parse a source file itself (with Myriad's own real
parser, `Myriad.Core.Ast.fromFilename`/`extractRecords`, called as a library) and expose the discovered
record's field names as erased members, entirely sidestepping the wall because nothing asks FCS to
resolve the target type at all. All three pre-registered rounds shipped and were independently
reproduced by review: design-time member resolution with the target record provably absent from every
reference path (0 diagnostics, ~1.2s cold); a live on-disk field edit picked up via
`FileSystemWatcher`+`Invalidate()` with zero diagnostics and no rebuild (~40ms); and runtime correctness
against a genuinely independent, separately-compiled consumer (dynamic-reflection reads matching direct
field access). This is genuinely new ground in Thread 2 — the first erased provider in either thread,
the first to call Myriad's own parser from inside a provider rather than reimplementing AST-walking
(Q002) or forwarding into Myriad's *compiled* output (Q016-18) — and it never touches the
`Assembly.LoadFrom` file-lock that dead-ended Q016/Q018's live re-exposure, so the live edit simply
works where Q018 proved no in-process fix exists. But the review found the write-up's own single
biggest "found by running" correction — that `assemblyReplacementMap` is load-bearing even for an
erased provider — does **not** reproduce: removing it as a controlled single-variable test left all
three rounds passing identically, including a clean full `dotnet build`/`dotnet run`. The map is
harmless but unproven necessary for an all-`obj` erased provider; the original claim was most likely a
post-hoc misattribution against a multi-variable change, and this repo's own `02-results.md` and the
provider's source comment were both corrected after review to say so — a live example that this
lineage's "found by running, not assumed" discipline is not itself immune to being wrong, which is
exactly what the independent-review gate exists to catch. The review also scoped the SHIP hard on three
other axes: every provided member is `obj`-typed, so the win is design-time member *names*, not typed
IntelliSense; this is a parallel `Fields<file, name>` preview mechanism the user points at a file path,
not Myriad's actual `[<Lenses>]`-on-the-record usage shape made live — it sidesteps Q006's wall by not
being the thing behind it, and should not be cited as closing Myriad's IDE-invisibility gap; and Round 3
in isolation proves only that reflection works (the erased getter is a one-line `GetProperty`/
`GetValue` call), not that the provider adds runtime capability — the genuine result lives entirely in
Rounds 1-2. See `Q019-erased-self-parsing-provider/03-review.md` for the full account and four
prioritized follow-ups, the first being to type the provided members with the field's real declared
type instead of `obj` — trivial for primitives, but a field typed by another type from the same
uncompiled file walks straight back toward Q006's wall, and is the genuinely open question this quartet
leaves behind.

**Q020 then pushed the TP protocol into diagnostics, a dimension no prior quartet touched — and came
back SHIP, scoped, with its own headline claim struck as tautological.** A Fable-brainstormed idea
(`BACKLOG.md` Round 5, itself informed by an external finding recorded in `BACKLOG.md` item 9 — F# has
no Roslyn-source-generator equivalent and the F# team's own stated answer is "use Myriad,"
`fsharp/fslang-suggestions#864`) asked
whether one shared self-parsing analysis function could feed two diagnostic channels that "cannot
disagree": a Myriad-CLI-style emitter printing a canonical MSBuild line at a field's declaration, and an
erased provider (`Q019`'s shape) stamping the affected member with `AddObsoleteAttribute` so FCS surfaces
the identical message live at the consumer's use site — routing around, not disputing, this repo's
standing finding that a provider can only throw, anchored at the whole static-argument expression. All
three pre-registered rounds reproduced independently, and the review went further than the executor,
driving eight additional consumer shapes (pipes, lambdas, nested arithmetic, an `if`/`then`/`else`) to
find a clean, previously-unstated rule: the diagnostic anchors the *member-access node* — receiver plus
`.member` — never the containing expression, tight for an identifier receiver and only as wide as the
receiver itself in the worst case. That's genuinely new ground for Thread 2 — the first non-fatal,
severity-controlled, use-site-anchored diagnostic a provider has produced here, where Q008/Q09/Q011 only
ever threw at the static-argument site. But the review struck the idea's own headline finding: "one
analysis, two channels that cannot disagree" is **tautological**, not a capability — two callers of the
identical pure function trivially agree, the same mechanism/capability conflation Q019's own Round 3
hit — and found the two halves are **un-entangled**: the build-time diagnostics half needs zero
type-provider machinery at all, and despite being the half the idea itself calls more broadly useful, it
was only ever demonstrated as a 25-line standalone format script, never wired into `IMyriadGenerator`
for real (confirmed directly from `src/Myriad.Core/Types.fs` and `src/Myriad/Program.fs`: Myriad plugins
genuinely have no diagnostics channel today). The review also did the honest weighing the hypothesis's
own novelty gate asked for and left open: an `FSharp.Analyzers.SDK` analyzer calling the identical shared
function **strictly dominates** this result in Ionide — true arbitrary-range anchoring at the actual
problem, no obsolete-attribute workaround needed — leaving the type-provider channel's real niche narrow
twice over: Visual Studio specifically, and only for a developer who has adopted a `Q019`-style preview
provider and references its members; a VS user editing their own `[<Lenses>]`-attributed record directly
gets nothing from this channel. See `Q020-shared-analysis-diagnostic-channels/03-review.md` for the full
account and four prioritized follow-ups, the first being to build the real `IMyriadGenerator`
diagnostics API this quartet's own hypothesis named as the more valuable half, needing none of the TP
machinery this quartet built.

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
- **The first `FSharpChecker` typecheck performed in any fresh process carries a large (~400-500ms in
  Q027's own toy), one-time JIT-warmup/FCS-static-init/referenced-assembly-metadata-read tax that has
  nothing to do with whatever is actually being measured, discovered by `Q027`'s review** while
  settling an unrelated question. A "cold" baseline measured later in the same, already-warm process
  never pays this tax, so any single-process comparison that times its "first" and "cold"/"baseline"
  conditions at different positions in the same process's call sequence risks reading this tax as
  signal. `Q027` itself fell into exactly this trap before its own review caught it. **`BACKLOG.md`
  item 23's follow-up has since run, as `Q028` (CLOSED, REVISE, 2026-07-19): yes, `Q023`'s own `cold`
  measurement carries this same tax**, confirmed on an independent rebuild of `Q023`'s actual spike code
  — a roughly fixed ~570-577ms one-time cost at N=10 and N=300 alike, inflating `Q023`'s own published
  `cold`-denominated ratios (`editOneRatio` at N=10 rises ~2.7x once the tax is isolated by a warmup
  control). The correction is conservative (`editOne` is a *larger* fraction of a tax-corrected `cold`
  than published, not smaller) and doesn't disturb either quartet's qualitative verdict — but it does
  **not** touch `Q024`'s own fitted regression, which the review found is ms/successor, not ratio-based,
  and publishes no `editOneRatio` column at all; `Q024`'s SHIP verdict is untouched, and this correction
  lands only on `Q023`'s ratio/percentage prose. See `Q028-jit-tax-spotcheck/03-review.md`.
- **Every "works live in the IDE" claim was tested through `FSharpChecker` as a library, never a
  literal Ionide/FSAC/VS session, until Q022 — which partially, not fully, closes this gap.** This was
  judged disqualifying for Q006's specific claim (which was fundamentally about IDE-visible behavior)
  and judged real-but-secondary for Q008/Q009's narrower claims (which are fundamentally about
  diagnostic generation, a layer FCS owns directly). Q022 is the first quartet in this lineage to drive
  a real `fsautocomplete` process over genuine LSP rather than `FSharpChecker`-as-library, and the part
  of its claim that held (a cold FSAC session showing a newly-generated member with zero `dotnet
  build`) is now real, literal-process-tested evidence, not an inference from library behavior. But it
  is `fsautocomplete` directly, not the literal VS Code + Ionide UI, and only the cold-load/reload half
  of the claim was tested this way — the live-source-edit half failed, and Visual Studio/Rider (different
  project systems) remain untouched. Treat future "works live in the IDE" claims the same way this one
  was judged: real, literal-process evidence for the specific slice actually driven, not a license to
  claim the whole gap is closed. **Q029 extended real-FSAC-process testing to a second, distinct axis
  (edit *cost*, not code *visibility*)** — a live `fsautocomplete` session's per-edit re-analysis timing,
  not just whether a member appears — and found the same discipline pays off again: its own reviewer
  went past the headline numbers to the raw LSP transcript and found a stronger mechanism control (the
  `documentAnalyzed` cascade's position-gating) than the executor's write-up had leaned on. Still
  `fsautocomplete` directly, not literal VS Code + Ionide, and still one topology/small-N — the same
  scope discipline applies.
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
- **External corroboration that the IDE-extension-point gap is real and long-standing, not just
  unexplored by this repo.** `FSharp.Compiler.PortaCode` (Don Syme/Fabulous, added as an adjacent
  working directory 2026-07-17) independently attacked the same problem this repo's IDE-invisibility
  gap describes: its `LiveCheckEvaluation.fs` writes a `.fsharp/<file>.info` sidecar meant to feed
  extra diagnostics/tooltips into Intellisense, but doing that required "an experimental FCS
  modification" per the project's own `README.md` — i.e. patching the compiler, not using a stock
  extension point. That modification never shipped; the repo's last commit is 2020-11-23. **A second,
  more concrete data point, found the same day this note was first written:** `origin/feature/analyzers`
  (also dead, last touched 2021-02-17) went further and actually prototyped a compiler-service-level
  analyzer hook — `FSharpAnalyzer` with `OnCheckFile`/`TryAdditionalToolTip`, able to inject arbitrary
  hover content from inside FCS's own checking pipeline, not just diagnostics — for
  `FST-1033-analyzers.md` in `fsharp/fslang-design`, Don Syme's own RFC for exactly this capability. It
  required a patched `fsc.exe` built from that personal fork branch (`--compilertool:`, the branch's own
  README: "Requires branch feature/analyzers from dotnet/fsharp") and never shipped either — confirmed
  directly against current `dotnet/fsharp` source (`FSharpAnalyzer`/`AnalyzerAttribute`: zero
  occurrences), and the RFC doc itself ends on an open TODO list, no implemented status. Six years on,
  independent of Myriad and this repo's own findings, the same wall (`fsharp/fslang-suggestions#864`,
  "F# has no Roslyn-source-generator equivalent") stopped a more resourced team twice, at two different
  levels of ambition. Treat any future idea premised on "get FCS/FSAC to read a side-channel file" *or*
  "hook a compiler-internal analyzer extension point" as presumptively dead on arrival unless it routes
  through what's actually shipped: a stock `FSharp.Analyzers.SDK` analyzer (diagnostics only, no
  hover/tooltip injection — confirmed by the above, not assumed, as in Q020/Q019/item 21) or Myriad's
  own MSBuild/DTB hook (as in Q022) — never a compiler fork. See `BACKLOG.md` items 19-21 for
  spike-shaped ideas PortaCode's mechanism (not its dead sidecar/analyzer channels) suggested instead.

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

**Next priorities, now that the reconstruction is closed (written 2026-07-16; both items 3's named
follow-ups have since closed — kept for the reasoning trail, not as current status, see "Starting the
next session (updated)" at the end of this file for what's actually next):**
1. **Retest Q011's own two real providers via the real-project-options PC route** (they were only ever
   checked via `checker.Compile`, never via the route that now resolves Q008/Q09's providers) — cheap,
   would settle whether Q011's specific regression is provider-shape-specific or was simply hitting the
   same script-vs-real-project axis all along. Still not run.
2. **Retest Q012's toy probes via the real-project-options route** — would fully cross-check
   "generative vs erased" (Q012's axis) against "script vs real-project-options" (this reconstruction's
   axis) on the same minimal shapes, confirming both are real and independent rather than one
   subsuming the other. Still not run.
3. **CLOSED since this was written.** "Q010's own follow-up 1" (instrument the reentrant call to
   determine whether it was landing on genuinely in-flight state or an already-idle checker) was run as
   `Q027-reentrant-call-timing` (CLOSED, REVISE, 2026-07-18) — the load-bearing half (ruling out
   already-cached/idle state) is settled; literal thread concurrency remains untested by design. "Q007"
   (best-supported unrun hypothesis at the time this was written) was itself run and closed the same
   session this note was written in (CLOSED, REVISE) — see the Thread 2 section above.

Two smaller, cheap items still worth doing regardless of what's picked next: fix the duplicate-diagnostic
defect that has recurred unfixed across Q008, Q09, and Q011 (three occurrences); and locate the actual
FCS code path responsible for the script-route PC failure to resolve a generative provider's backend
assembly — Q013 narrowed where to look on the script route specifically (PC never consults Compile's
output, so the gap is in PC's own emit path, not cross-API cache sharing), but the actual code path is
still unlocated, and it's now known to matter only for the script-options configuration, not universally.

Separately, Q016 (satellite-DLL type provider, `BACKLOG.md` item 15) was built, run, and reviewed in
the same session this reconstruction happened in. **CLOSED, REVISE** — Round 1 (real forwarding into a
Myriad-compiled satellite DLL) passed, Round 2 (regeneration + live pickup) failed on a file-lock that
the review traced to breaking the type provider's only differentiator over an ordinary
`<ProjectReference>` for this case. The review itself could not independently rebuild/rerun (disclosed
tooling outage), so its top follow-up — rerunning Round 1 on a function-typed member shape to settle
whether the forwarding mechanism generalizes — became Q017 in the same session. **Q017: CLOSED, SHIP** —
the forwarding kernel generalizes to function-typed parameters and returns with no new scaffolding
(this time the review *could* execute, and extended the test itself), but it surfaced a real brittleness
(F#'s curried-arity flattening can silently turn a function-returning member into a flat multi-arg
method, and a mismatched forward poisons the entire provided type) that is now the standing risk for any
future `Lenses`-shaped forwarding attempt. Round 2's file-lock problem was untouched by Q017 and became
Q018 in the same session: does a collectible `AssemblyLoadContext` fix the lock that blocks Round 2's
regeneration? **Q018: CLOSED, REVISE, weaker than Q016's own** — the underlying OS mechanism works
(confirmed in isolation) but only with a JIT setting no real host controls, and the actual mitigation
fails even in its most favorable in-process form; the review closed off the one hopeful escape the
executor left open (a fresh `FSharpChecker` doesn't help either — the retained reference is process-global
inside FCS's own provider hosting). There is now no demonstrated in-process fix for Round 2, and Q016's
cross-project satellite-DLL idea has lost its last untested path to the one capability (live re-exposure)
that would have justified it over a plain `<ProjectReference>`. See the Thread 2 section above,
`Q016-satellite-dll-type-provider/03-review.md`, `Q017-satellite-function-typed-forwarding/03-review.md`,
and `Q018-collectible-alc-round2-mitigation/03-review.md`.

**Update, 2026-07-16 (same session): `BACKLOG.md` item 14 (erased, self-parsing provider) was promoted
to `Q019` and closed the same session.** `Q019: CLOSED, SHIP, scoped` — a different route around Q006's
wall than Q016-18's cross-project satellite-DLL forwarding: an erased provider that self-parses a source
file with Myriad's own real parser (`Myriad.Core.Ast`) and exposes the discovered record's fields as
`obj`-typed, reflection-backed members, never asking FCS to resolve the target type at all. All three
pre-registered rounds (design-time resolution with the target type provably uncompiled, a live on-disk
edit with no rebuild, and independent runtime correctness) were independently reproduced by review — but
the review struck the write-up's own headline "found by running" mechanism claim
(`assemblyReplacementMap` being load-bearing) as non-reproducing, and scoped the SHIP down to "member
names, not typed IntelliSense" and "a parallel preview mechanism, not Myriad's own `[<Lenses>]` shape
made live." See the Thread 2 section above and `Q019-erased-self-parsing-provider/03-review.md`.
Separately in the same session, a second model (Fable, not the model that wrote this repo's earlier
brainstorming passes) was briefed on Q016-19 and asked to find a combination of type providers and
Myriad that divides labor rather than competing over the same type surface — its output, "tiered
erasure" (an erased-TP façade whose runtime fallback Myriad harvests and retires via a runtime attribute
registry, reversing the information flow relative to every combination tried so far), is recorded
verbatim in `BACKLOG.md`'s Round 4 section, not yet spiked.

**Update, 2026-07-16 (same session, continued): an external finding reframed the next brainstorm, which
then promoted directly to `Q020`, closed the same session.** User scrutiny surfaced a real, current,
authoritative external fact — `fsharp/fslang-suggestions#864` confirms F# has no Roslyn-source-generator
equivalent and the F# team's own stated answer is "use Myriad" (recorded in full in the
`project_fsharp_no_source_generators` memory note and cited in `BACKLOG.md` item 9). Briefed on that plus
`Q019`, Fable produced a Round 5 brainstorm naming what a type provider could borrow from source
generators' *design* (precise diagnostics, a staged pipeline) since it structurally cannot borrow the
*mechanism*. Its stronger idea — one shared analysis function feeding a Myriad-CLI declaration-anchored
emitter and a provider-side use-site-anchored emitter that "cannot disagree" — was promoted directly to
`Q020` and closed in the same session. `Q020: CLOSED, SHIP, scoped`. The genuine result is Round 1: an
erased provider's member stamped with `AddObsoleteAttribute` produces a live, non-fatal,
severity-controlled diagnostic anchored at the member-access node — the first such diagnostic in either
thread, and the review drove eight extra consumer shapes to confirm it generalizes cleanly past the one
the executor tested. But the review struck the quartet's own headline framing: "cannot disagree" is
tautological (two callers of one pure function agreeing proves nothing), the build-time and
type-provider halves are un-entangled (the more valuable build-time diagnostics API needs zero TP
machinery and was never actually wired into `IMyriadGenerator`), and an `FSharp.Analyzers.SDK` analyzer
calling the identical shared function would strictly dominate this result in Ionide, leaving the TP
channel's genuine niche narrow to Visual Studio plus an adopted `Q019`-style preview provider. See the
Thread 2 section above and `Q020-shared-analysis-diagnostic-channels/03-review.md`.

**Update, 2026-07-16 (new session): `BACKLOG.md` item 18's own cheapest falsifier was promoted to
`Q021` and closed the same session.** Item 18 (FSAC-owned virtual generated file, modeled loosely on
rust-analyzer's out-of-process proc-macro architecture, itself informed by a second external model's
research pass into rust-analyzer's `proc-macro-srv` design) named a narrow, cheap precursor question
before committing to the large lift of forking FSAC: does Q010's reentrant `DocumentSource.Custom`
mechanism, proven only for one check per checker instance, survive the load pattern a real editing
session produces — one persistent checker serving many sequential checks as the watched file changes.
`Q021: CLOSED, SHIP, scoped.` It does, independently reproduced: four sequential edit-then-recheck
cycles on one persistent checker, including a deliberate return to a prior value, all produced correct,
fresh, zero-diagnostic results, confirming item 18's actual correctness premise. Alongside that, Q010's
own footgun reproduced independently in a new harness, sharpening rather than dissolving item 18's
"purely plumbing" framing: the natural per-file incremental API an LSP host has on `didChange`
(`ParseAndCheckFileInProject` with explicit current text) cannot drive this mechanism at all, only a
whole-project check can. A secondary claim in the quartet's own results write-up — that the whole-project
check "always fully re-checks, unconditionally" — was struck by adversarial review as unsupported (the
evidence is equally consistent with a cheap cache hit under `TransparentCompiler`'s content-hash model,
and the toy project tested cannot discriminate the two), leaving whether an FSAC-hosted version would be
keystroke-cheap or expensive as the single most consequential open question this thread now has, resolvable
only by a real scale test. See `Q021-reentrant-generation-live-edit-loop/03-review.md` and `BACKLOG.md`
item 18's own updated entry.

**Update, 2026-07-16/17 (new session): `BACKLOG.md` item 8 — the DTB/MSBuild-hook route named across
three sessions as "the more direct candidate" and never spiked — was promoted to `Q022` and closed.**
`Q022: CLOSED, REVISE.` This is the quartet that finally attempted to close this file's own standing
cross-cutting caveat (immediately above) rather than merely naming it: a hand-rolled LSP client drove a
real `fsautocomplete` 0.83.0 process, not `FSharpChecker`-as-library. Round 1 (removing
`MyriadSdkGenerateCode`'s DTB gate on a scoped local copy of `Myriad.Sdk.targets`, the real shared file
never touched) fully confirmed the mechanism: Myriad's real `<Exec>` runs during a genuine DTB with the
compiler itself never invoked, and the existing rebuild cache still correctly no-ops an unchanged repeat
DTB. Round 2a fully confirmed the capability against a literal LSP process both ways: a cold FSAC session
shows a newly-generated field's lens with zero `dotnet build`, and a gated negative control correctly
fails the identical check. But Round 2b (an already-*running* FSAC session, ordinary source save, no
restart) failed — and independent review, dispatched specifically to press on this, caught that the
executor's own sharpest negative claim ("no in-session reload signal exists short of a full restart") was
itself an overclaim: an actual `.fsproj` mtime change (not just a notification claiming one) plus a
reissued `workspaceLoad` does re-trigger codegen live, in the same running process. Corrected, the result
lands exactly on the design's own pre-registered REVISE wording: **this closes the IDE-invisibility gap
at project load/reload time, independently confirmed against a real editor-facing process for the first
time in this repo's history, but not during live source editing** — an ordinary save to the attributed
`.fs` file alone still never refreshes generated code, since it never touches the `.fsproj` a project
reload is keyed on. See `Q022-dtb-generation-hook/03-review.md`.

**Separately, the same session: the diagnostics-API design sketch named below (and in `BACKLOG.md`) was
actually built, not just designed.** `IMyriadGeneratorWithDiagnostics`/`MyriadDiagnostic`/
`DiagnosticSeverity` now exist in `src/Myriad.Core/Types.fs`, with rendering in the new
`src/Myriad.Core/Diagnostics.fs` and CLI wiring in `src/Myriad/Program.fs`'s `runGenerator` — a
Warning-severity diagnostic no longer fails the build, an Error-severity one still does (old exit
behavior preserved), and existing `IMyriadGenerator` plugins (`Fields`, `Lenses`, `DUCases`) are
untouched through the old interface path. Five new tests cover it, including a real end-to-end MSBuild
round trip and a CLI-subprocess test of the error path; all 58 tests in
`test/Myriad.IntegrationPluginTests` pass. This was engineering, not a hypothesis, per this file's own
existing carve-out, and needed no quartet.

**Update, 2026-07-17 (new session): `BACKLOG.md` item 18's own top-priority scale test was promoted to
`Q023` and closed — with the corrected answer more favorable to item 18 than the quartet's own headline
claim first suggested.** Q010/Q021's reentrant `DocumentSource.Custom` callback was scaled to N ∈
{10, 50, 150, 300} genuinely-weighted files (generic records, `Map`/`List` pipelines, recursion — not
Q001 Round 3's near-free padding), measuring cold vs. no-op-repeat vs. edit-one-unrelated-file cost.
Two deviations were caught and honestly corrected before the results were trusted: a real bug in the
*generated* F# source (an unbound identifier leaking from a `sprintf` template), and a violation of the
design's own "fresh process per N" instruction that let JIT warm-up contaminate the N=10 baseline —
both disclosed in `02-results.md` rather than silently patched. The corrected run showed both ratio
columns growing monotonically with N, which the executor read as "editOne tracks cold, caching is
largely absent once anything changes" — a REVISE against the pre-registered thresholds, on its face a
materially pessimistic answer for item 18. **Independent review found this conclusion doesn't survive
scrutiny of *which* file the design always edited.** The spike hardcoded `Prefix0000` — index 0, the
*first* file, with the maximum possible number of compilation-order successors — as the sole edit
target at every N. The reviewer ran a position sweep at N=300 and found cost is linear in the number of
files *after* the edited one, collapsing to no-op-repeat cost for a tail edit (idx 299: 255ms, ≈ repeat's
258ms) and rising toward cold only for a head edit (idx 0: 1339ms of a 2640ms cold check). This was then
independently re-confirmed a third time with a durable, checked-in artifact (`run-n300-editidx{0,150,299}.txt`),
closing a real gap this repo has been burned by before (Q006/Q008/Q09's missing-artifacts episode) —
the reviewer's own finding didn't just get argued, it got hardened into re-runnable evidence before the
quartet closed. **The corrected finding: FCS's `TransparentCompiler` genuinely does skip the
compilation-order prefix before an edit and re-checks only the tail — real, working incremental caching,
not its absence.** `Q023: CLOSED, REVISE` — REVISE not because the mechanism fails, but because the
quartet's own headline interpretation was wrong and must not be cited as written. One caveat keeps this
from being unambiguous good news for Myriad specifically, named by the review: attributed domain types
often sit early in build order because other code depends on them, which is close to the worst-case
position actually measured — the expensive case isn't a rare pathology for Myriad's real usage, even
though it isn't the universal case first claimed. See `Q023-scale-cost-reentrant-callback/03-review.md`.

**Update, 2026-07-17 (same session, continued): `Q023`'s review's own top follow-up — does the
linear-in-successors cost model hold across scale, not just the single N=300 spot-check — was run
immediately as `Q024` and closed the same session.** `Q024: CLOSED, SHIP, scoped.` A full sweep (5 edit
positions × N ∈ {10, 50, 150, 300}, 20 runs, reusing Q023's own spike binary verbatim) found the
per-successor marginal cost fits a clean line at every N with no detectable systematic drift in the
slope from N=10 to N=300 — independently reproduced, with the peak slope landing at a *middle* N in
both the executor's and the reviewer's own separate rerun, which affirmatively refutes the
pre-registered "slope grows with N" REVISE trigger rather than merely failing to find it. The review
scoped three specific framings down without touching the verdict: "essentially N-invariant" overstates
what 5 points per fit can support statistically (only the two largest-N fits are tightly constrained);
"intercept cross-validates the repeat median" is mildly circular, since the zero-successor point is
itself one of the points inside that regression; and one N=50 data point the write-up itself flagged as
an anomaly was confirmed, on independent rerun, to be a sampling outlier rather than a real plateau in
the model. Most important for how this gets cited: **this measures a cost model — whole-project
`ParseAndCheckProject` cost as a function of edit position, on independent files — not a real FSAC
live-editing session**, and must not be described as "FSAC keystroke cost characterized." See
`Q024-position-sweep-across-scale/03-review.md`.

## Starting the next session (updated)

With Q022, Q023, and Q024 all closed, `BACKLOG.md` items 8 and 18's own named cheapest-falsifier/
scale-test questions are resolved as far as this repo's tooling can currently test them, and the
diagnostics API is shipped. The most direct next steps, in rough priority order: (1) Q024's own review's
top follow-up — test whether FSAC's own incremental per-file editing path actually reproduces this
positional cost model, since everything measured so far (Q023, Q024) drives whole-project
`ParseAndCheckProject` directly, not a live LSP session's actual per-edit call pattern — this is the
real remaining gap between "the cost model is well-characterized" and "item 18's FSAC-hosted keystroke
cost is known"; (2) Q023/Q024's shared follow-up — test the dependency-chained variant both quartets'
own design deliberately deferred (real cross-file `open`s, closer to Myriad's actual `Q002`/
`Q010`-shaped cross-generator visibility), to separate "conservative compilation-order invalidation"
(what's been measured so far) from "genuine dependency-forced invalidation" (a different, and for
Myriad's own multi-generator case, more realistic question); (3) Q022's own most direct follow-up,
named by its review: test whether Ionide's real project-file watcher (not a hand-rolled LSP
notification) actually fires reliably on an ordinary `.fsproj` save in a literal VS Code + Ionide
session; (4) Q021's own two smaller named follow-ups: test whether any `ParseAndCheckFileInProject`
calling pattern honors `DocumentSource.Custom` at all, and test concurrent/interleaved access.

## Starting the next session (updated again, 2026-07-19 — the above predates Q025, Q026, and Q027)

Three more quartets closed since the priority list directly above was written, and it was never
revised to account for them — flagged here rather than left silently stale. `Q025` (SHIP, scoped)
closed a different open item, Q015's "make FSI do real computation" gap, by a route (`FSharpExpr`
interpretation) orthogonal to the FSAC/scale-cost thread the list above is about. `Q026` (SHIP, scoped)
closed `BACKLOG.md` item 22's cheapest falsifier and surfaced a new, concrete engineering finding: a
real persistent multi-composition host needs `GeneratorContext` itself widened to carry a checker/
options handle, since the process-global static side-channel standing in for one races roughly 50% of
the time on reuse. `Q027` (REVISE) settled Q010's own inherited "mid-flight vs idle-checker" question's
load-bearing half (genuinely fresh work, not a cache hit) and added a lineage-wide caution: any
single "first check" timing this thread has ever reported carries a ~400-500ms one-time JIT/FCS-init
tax, which the list above's own Q023/Q024 numbers were measured before this was known to matter.

Corrected priority order, folding in `BACKLOG.md` item 23 (named the same day as Q027):
(1) **DONE, 2026-07-19: item 23 ran as `Q028` (CLOSED, REVISE)** — confirmed `Q023`'s own `cold`
numbers do carry the tax, corrected the ratio framing, and confirmed `Q024`'s ms-based regression is
untouched; see `Q028-jit-tax-spotcheck/03-review.md` and the Thread 1 section above. This was a gate
on item (2) below (a live FSAC comparison needs a cost-model baseline already known to be
uncontaminated) and it's now cleared. (2) **DONE, 2026-07-19: the list's own item (1) also ran, as
`Q029` (CLOSED, SHIP, scoped)** — a real `fsautocomplete` 0.83.0 session, driven over hand-rolled LSP,
confirmed FSAC's own per-file incremental editing path reproduces the same position-dependent,
linear-in-successors cost curve Q023/Q024 found via direct `ParseAndCheckProject` calls, with a
stronger control than either prior quartet had: the raw LSP transcript shows the `documentAnalyzed`
cascade is strictly position-gated (exactly `successors+1` files re-analyzed, in order), ruling out a
position-blind-refresh artifact directly rather than by inference. This closes item 18's own
single-most-cited follow-up, on the favorable side — see `Q029-fsac-live-editing-cost/03-review.md`.
Scope that travels forward: small N (≤40), one linear dependency-chain topology (can't yet distinguish
"FSAC invalidates by compilation order" from "FSAC invalidates by true dependents," since this topology
makes them identical), single session per size. **DONE, 2026-07-19: the review's own named next step
also ran, as `Q030` (CLOSED, SHIP, scoped) — and answered on the unfavorable side.** A wide/shallow
topology (N=30, an early "Hub" referenced by only 3 of 27 later files) showed a value-only edit
re-analyzes **all 27 order-successors, not just the 3 true dependents** — the whole compilation-order
suffix, reproduced 3/3 reps and confirmed by hand against the raw LSP transcript, with controls ruling
out both a harness-solicited-request explanation and a blanket project-wide refresh. Review found this
is not a fixable FSAC limitation but F#'s own ordered-file compilation semantics (every file checks
against the accumulated signature environment of all predecessors), making the pessimistic reading
structural, not version-specific. See `Q030-fsac-dependency-precision/03-review.md`. **This closes the
order-vs-dependency question that was the single largest remaining unknown in the whole FSAC-cost
sub-line (Q023→Q024→Q029→Q030) — settled, unfavorably for Myriad, not softened.** (3) and (4) above,
unchanged (dependency-chained variant now subsumed by Q030's own topology). (5) `Q026`'s own named
follow-up — widen `GeneratorContext` to carry a real checker/options handle — is a prerequisite
engineering change, not a quartet, before any future persistent multi-composition host can be attempted
safely; do this before, not after, extending Q026's composition mechanism to a third generator or a
live-watcher wiring (item 19). With items (1)-(2) of this list and the FSAC-cost sub-line both now
closed, item (5) — the `GeneratorContext` engineering prerequisite — is the most direct remaining step
if this thread continues toward an actual persistent-host attempt, rather than another cost-model
spike.
