# Q014-fsi-staged-compilation / Movement 1 — Hypothesis

**Status:** CLOSED. **REVISE** — see `03-review.md`. Executed, reproduced independently on review, but
the SHIP threshold's own "at FSI time" conjunct was not satisfied: no `FsiEvaluationSession` ran
anywhere in the spike.
**Date:** 2026-07-15
**Repo under test:** this repo (Moirae Software's F# code-generation tool), Thread 1 lineage
(Q001/Q002/Q003/Q010's in-process `FSharpChecker`/FSI harness), promoted from `BACKLOG.md` item 1's
stretch goal.

**Procedural note, not part of the science:** the adversarial review (Movement 4) for this quartet is
to be run against Opus, not the Fable model used for some prior quartets' reviews — a resourcing
constraint for this week, not a methodology change. Record this here so it isn't lost by the time
Movement 4 is reached.

**Question:** Q003 proved an FSI-evaluated F#-typed *value* (a record, a list — data) crosses cleanly
into host code with no cast failure. No quartet has gone further and asked the harder question: can
FSI evaluate real *computation* at Myriad-generation-time — running a general implementation against
concrete, statically-known configuration — and have the *result of that computation* reified back into
real F# source text that splices into the generated file and typechecks? That's the difference between
"compile-time-evaluated config drives a template" (already proven, Q003) and actual staged compilation:
baking a *specialized* piece of code into the build, derived by partially evaluating a general
implementation at compile time rather than interpreting it at runtime.

## The claim

Two separable claims, kept apart the same way Q001 and Q010 split mechanism from capability.

1. **Mechanism claim:** an FSI-evaluated computation that produces an `Expr<'T>` (a real F# quotation,
   not a raw closure — see the scoping note below on why the target is a quotation, not an arbitrary
   function value) can be mechanically translated into valid F# source text, spliced into the same
   `FSharpChecker` virtual project Q001/Q002/Q010 already use, and reparsed and typechecked with zero
   diagnostics.
2. **Capability claim:** for a genuine specialization scenario — a general, parameterized
   implementation (e.g. a small interpreter or a combinator-built computation) evaluated at FSI time
   against one concrete, statically-known configuration, producing a quotation for the *specialized*
   result — the spliced generated code is both correct (matches the general implementation's runtime
   behavior for the specialized case) and structurally different from what the general implementation
   would produce if just called at runtime (no residual interpretation loop, no unused generality left
   in the emitted code).

**Scoping decision, stated up front so the review doesn't have to reverse-engineer it later:** the
target of reification is deliberately an `Expr<'T>` quotation, not an arbitrary FSI-evaluated closure.
A closure captures an arbitrary runtime environment with no general source-level representation —
reifying one back into `SynExpr` is very likely structurally impossible for the general case, not just
unbuilt. A quotation is already a data structure that *represents* code, which sidesteps that wall
entirely and stays inside F#'s own sanctioned metaprogramming surface (quotations, same family as
`ProvidedTypeDefinition`'s `Expr`-based method bodies) rather than reopening the AST-injection back
door `BACKLOG.md`'s "closed door" note already found removed from FCS's public surface
(`FSharpChecker.CompileToDynamicAssembly`). If the design work in Movement 2 finds that the interesting
specialization scenarios can't be expressed as a quotation, that is a real, reportable constraint, not
something to route around by attempting closure introspection instead.

## Why this is the right next spike

- **Q003's own review named exactly this as the next step** ("build the full loop... that is the spike
  that actually earns the type-provider comparison") and it has sat unaddressed for eleven quartets
  since. Q010 later built a different piece of Thread 1's unfinished business (cross-generator
  visibility); this is the other half Q003 left open.
- **`BACKLOG.md` item 1 already named this exact idea as a stretch goal** and deliberately scoped it
  out of the base FSI-loop claim: "this needs reifying arbitrary evaluated FSI values (potentially
  closures, captured environments) back into a hygienic `SynExpr`, which is a materially harder problem
  than the primitive/record-shaped data Q003 already proved crosses the boundary cleanly." This quartet
  is that follow-on hypothesis, run directly rather than gated behind first re-proving the (already
  Q003-adjacent, lower-value) base loop — the base loop's core claim ("an FSI value can drive template
  text") is not materially in doubt given Q003 and Q006's string-static-parameter result; the reify-code
  question is the one with real uncertainty and real payoff.
- **Nothing in Q001–Q013 has ever attempted to move *code*, as opposed to *data*, across the FSI
  boundary.** Q003 crossed data. Q007 (planned, not yet run) asks about richer *static parameters*, not
  about reifying a compile-time result into emitted source. This is a genuinely open cell in the space
  this session has been mapping, not a re-tread of any closed or planned quartet.
- **Ties Thread 1 back to a real, named piece of prior art** rather than inventing the idea from
  nothing: Eirik Tsarpalis's `QuotationCompiler` (cited in `BACKLOG.md`'s closed-door note) already
  demonstrated lowering F# quotations to a compilable form works in principle, via the now-removed
  `CompileToDynamicAssembly(ParsedInput list)` entry point. This quartet asks whether the same
  quotation-to-compilable-form idea survives going through the text-splice-and-reparse round trip every
  other quartet in this repo already uses, since the AST-injection entry point Tsarpalis used is
  confirmed gone.

## Novelty gate

Not covered by any closed or planned quartet's scope. Q001/Q002/Q010 proved typed *resolution* across
virtual files; none of them evaluate anything at FSI time or reify computed code. Q003 proved FSI data
crosses into host *code* (the harness's own top-level code reads the value), never that an
FSI-computed *result* becomes new *source text* fed back into the compiler. Q006/Q007/Q008 all operate
inside the type-provider protocol and are bound by Q006's structural wall (a provider can never see a
type from the compilation in progress); this hypothesis is Thread 1 (Myriad-CLI-hosted `FSharpChecker`,
the same harness Q001/Q002/Q010 use), and `FINDINGS.md`'s own cross-cutting notes already establish
that Thread 1 was never subject to that wall in the first place.

## Contradiction gate

Does not contradict any prior verdict. It depends on: `DocumentSource.Custom` behaving correctly for
virtual files (validated, Q001–Q005, Q010); FSI and `FSharpChecker` coexisting in one process with no
collision (validated, Q003); and the reparse-and-typecheck round trip on spliced text working correctly
(validated across every closed Thread 1 quartet). It deliberately does **not** depend on
`FSharpChecker.CompileToDynamicAssembly` or any other AST-injection entry point `BACKLOG.md` already
confirmed is gone from the pinned FCS's public surface — the translation path this quartet tests
produces *text*, reparsed the ordinary way, not a direct AST handoff. It inherits Q003's still-open
`AssemblyLoadContext`-isolation gap (backlog item 2) without attempting to close it — see validity
preconditions below.

## Validity preconditions

- FCS pinned to `43.9.101`, matching every prior quartet and `Myriad/paket.lock`.
- The FSI-side computation must be a genuine specialization — a general implementation, parameterized
  over something the general case can't know at definition time, evaluated at FSI time against one
  concrete static input — not a constant fold or a value read straight off a record. Evaluating
  `<@ 1 + 2 @>` is acceptable only as the cheapest-falsifier smoke test below, never as the quartet's
  positive result, the same discipline Q007 held itself to for its own richer-value precondition.
- The reified quotation must be verified correct via the compiler's own reparse-and-typecheck of the
  spliced text (zero diagnostics) *and* a behavioral check (the generated specialized code's output
  matches the general implementation's output for the specialized case), not visual inspection of the
  emitted text — the same standard every closed Thread 1 quartet already holds itself to for typed
  resolution.
- Must report explicitly which `Expr` node shapes the translation handles and which it doesn't.
  `FSharp.Quotations.Expr` has many constructors (`Lambda`, `Application`, `Call`, `IfThenElse`,
  `PropertyGet`, `NewObject`, `Let`, `Var`, and more); a full, general-purpose quotation-to-source
  pretty-printer is a nontrivial project on its own. Scope Movement 2's design to a named, deliberately
  limited subset (the shapes the chosen specialization scenario actually produces) and say so plainly —
  do not imply general coverage from one scenario's success, the exact overclaiming trap Q001's own
  NULL result exists to guard against.
- Explicitly out of scope, inherited unresolved from Q003 rather than newly closed here: FSI hosted
  under `AssemblyLoadContext` isolation (Myriad's real plugin-loading configuration, per
  `McMaster.NETCore.Plugins`). This quartet reuses the no-isolation harness config Q003/Q010 already
  validated. If this quartet ships, ALC isolation remains the next gate before the result is
  production-relevant, exactly as Q003's own review already flagged.
- Every timing number is a single sample, per this repo's own established (if imperfect) precedent —
  the review must say so rather than imply otherwise.

## Cheapest falsifier

Before attempting any real specialization scenario: does *any* F# quotation, however trivial
(`<@ 1 + 2 @>` or equivalent), translate to valid F# source text that FCS accepts — reparses and
typechecks with zero diagnostics — when spliced into the existing Q001/Q002/Q010 harness? This is the
single riskiest unverified assumption behind the entire idea (does a working `Expr`-to-source path
exist at all, whether via a hand-written translator over a small node subset or an existing library),
and it should be tested cheaply, first, exactly as Q001 tested its own weakest capability claim before
investing further.

## Pre-registered decision thresholds

- **SHIP:** the cheapest falsifier passes; a genuine specialization scenario (general implementation
  partially evaluated at FSI time against concrete static configuration, producing a quotation for the
  specialized result) translates to spliced source that typechecks with zero diagnostics; the generated
  specialized code is verified correct against the general implementation's behavior; and the
  specialized output is demonstrably different in shape from the general implementation (no residual
  interpretation loop, no unused generality) — not just re-templated data, which Q003 already proved.
- **REVISE:** the mechanism works but only for a narrow, explicitly-bounded subset of quotation shapes
  (e.g. arithmetic and simple conditionals, but not closures-over-mutable-state, computation
  expressions, or object expressions) — real and worth keeping, but scoped to "staged compilation over
  a defined quotation subset," not the general claim.
- **NULL:** the translation works and typechecks, but for the specific scenario tested the specialized
  output isn't meaningfully different from what an ordinary template-driven (non-staged) generator
  could already produce — the same shape of null result Q001 got porting typed access onto a
  structural-echo generator. This would be a legitimate, informative result, not a failed spike.
- **KILL:** the cheapest falsifier itself fails — no `Expr`-to-source path, however narrow, produces
  text FCS will accept for even a trivial quotation. This would be a significant, general finding: it
  would mean F# quotations, despite being a documented, sanctioned metaprogramming surface, don't have
  a practical route back to compilable source text outside the now-removed
  `CompileToDynamicAssembly`-style AST-injection API, closing this entire line of attack on staged
  compilation for Myriad, not just this quartet's specific scenario.
