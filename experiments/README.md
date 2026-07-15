# QUARTET — pre-registered experiment units (Myriad edition)

A QUARTET is one experiment run through four movements, each a separate file in its folder. The
point is gating, not idea generation: a spike that would take an afternoon (or a design decision
that would shape months of follow-on work) should not get spent chasing a re-tread or a claim that
doesn't survive its own write-up. Each movement is a checkpoint a human can veto.

Adapted from an ML-training quartet methodology (pre-registered hypothesis, kill criteria,
adversarial review) used in a different repo. The four-movement discipline transfers directly to
software-architecture spikes; the GPU-hour/seed/MLflow mechanics do not, so they've been replaced
below with the equivalent for this repo: build/typecheck pass-fail, benchmark deltas, and honest
comparison against what Myriad already ships.

## The four movements

| File | Movement | Gate it must pass |
|---|---|---|
| `00-hypothesis.md` | Hypothesize | **Novelty** (cite why this isn't already covered by an existing generator in `src/Myriad.Plugins/` or a closed quartet's verdict) + **contradiction** (does it conflict with a prior quartet's verdict, or with an architectural decision already made in `DEVNOTES.md`?) + **validity preconditions** (what must be true for a result to mean anything — e.g. "must run against the FCS version Myriad is actually pinned to in `paket.lock`," "must test against a real shipping generator, not a synthetic strawman built to flatter the hypothesis") + **cheapest falsifier** (the cheapest thing that could kill this before deeper investment; run it first, even if it's the weakest test of the claim) |
| `01-design.md` | Design | Concrete method: what gets built, what's measured, how it's run, and how to reproduce it (commands, package versions pinned). No unstated assumptions about API behavior — if a sketch is based on documentation rather than a verified run, say so explicitly. |
| `02-results.md` | Execute + write up | What actually happened, including corrections to the design's assumptions discovered while running it. Numbers from code that was actually run, not estimated. Null and negative results reported with the same weight as positive ones — a spike that disproves its own premise is not a failed spike. |
| `03-review.md` | Adversarial review | The strongest objections to the claimed result, honestly argued — including "this only tested one narrow slice of the real question." SHIP / KILL / REVISE verdict. Must check: does the result generalize past the one case tested? Does a positive mechanism result get mistaken for a positive capability result (or vice versa)? Does it contradict a standing conclusion from a prior quartet? |

Movements 1 and 4 are the same hostile review applied before and after the work. Movement 1 is
the effort-saver: a hypothesis that fails its own pre-registration never gets built.

## quartet.json

```json
{
  "id": "Q001",
  "status": "closed",
  "verdict": "one-paragraph, fast-read summary of what shipped and what didn't",
  "required_files": [
    "00-hypothesis.md",
    "01-design.md",
    "02-results.md",
    "03-review.md"
  ],
  "completion_check": "strict",
  "allow_partial_state": false,
  "last_validated": null
}
```

Rule: a quartet is only valid if all required files exist AND are non-empty. Status-aware:

| status | required non-empty files |
|---|---|
| planned | 00 |
| running | 00, 01 |
| parked | 00 |
| closed | 00, 01, 02, 03 |

Run `dotnet fsi experiments/check-quartets.fsx` to validate every `Qddd-*` folder against this
table (`--stamp` writes `last_validated` into passing quartets).

**Exemptions:** a folder under `experiments/` that does not match `Qddd-*` is not part of the
gated lineage and is skipped by the checker — useful for one-off notes or infra scratch that
doesn't warrant the full pre-registration discipline. Anything that IS pre-registered as a
quartet should use the numbering and go through all four gates; don't half-adopt the format the
way an earlier pass of Q001 itself did before being backfilled (see its `00-hypothesis.md`).

## Folder schema

```
experiments/
  README.md              <- this file
  check-quartets.fsx      <- validator (dotnet fsi)
  Q001-<slug>/
    quartet.json
    00-hypothesis.md
    01-design.md
    02-results.md
    03-review.md
    artifacts/            <- spike source code, benchmark scripts, generated-output samples
```

One quartet per folder, small files. Numbering is sequential across the whole `experiments/`
folder, not per-topic — Q002 is "the second quartet run in this repo," whatever its subject.

**`artifacts/` is required for any quartet that reaches `closed`, not optional.** Discovered missing
for Q006/Q008/Q009 on 2026-07-15 by direct user scrutiny, not caught by this project's own review
process — see `FINDINGS.md`'s "A gap in this file's own credibility" section. A `02-results.md` with
no re-runnable code behind it is prose, not evidence, however carefully the review checks it for
internal consistency; `check-quartets.fsx` does not currently enforce this (it only checks the four
movement files exist and are non-empty), which is itself worth fixing so this can't recur silently.
**Update, 2026-07-16: Q008 and Q009's missing artifacts have since been filled in** — their actual
original scratch source was found intact in a session job's own temp directory, rebuilt, and reproduced
exactly; see `Q008-provenance-closed-loop/RECONSTRUCTION.md` and
`Q009-field-level-provenance/RECONSTRUCTION.md`. Q006's artifacts remain missing and unrecovered.

## Index

| ID | Question | Status | Verdict |
|---|---|---|---|
| Q001 | Can an in-process, typed FCS host (Fable-style, zero disk writes) correctly run a real Myriad generator, and does typed access change anything for it? | **CLOSED** | **REVISE.** Mechanism (in-process typed hosting + `TransparentCompiler` correctly auto-detecting stale dependencies) validated with real numbers. Capability claim ("typed beats syntax") came back NULL for the one generator tested (Fields) — it's a pure structural-echo generator that never needed semantic reasoning. Foundation survives; the motivating claim is still an open hypothesis. Detail: `Q001-fcs-typed-codegen/03-review.md`. |
| Q002 | Can a generator use typed access to detect that one field's type is itself another Myriad-attributed type (nested/recursive dispatch), across virtual files — the capability class Q001 left untested? | **CLOSED** | **SHIP, measured on both sides.** Typed cross-file resolution + recursive dispatch both worked. A real syntax-only resolver (built using Myriad's own `Ast.fs` matching code) was then tested head to head: correct on the unambiguous case, but flips between correct and silently-wrong answers on an adversarial case (two same-named types, different modules) depending only on file processing order. Required both Q001 pillars together — Myriad's current per-file plugin invocation model wouldn't reproduce this from typed access alone. Detail: `Q002-typed-nested-dispatch/03-review.md`. |
| Q003 | Can `FsiEvaluationSession` (FSI) be hosted in the same process as `FSharpChecker`, and can compile-time-evaluated F# data (not just primitives) be used directly by host code — the mechanism type providers run on, and the last untested pillar from Q001/Q002? | **CLOSED** (falsifier only) | **SHIP the narrow claim.** FSI and FSharpChecker coexist with no collision; an FSI-evaluated `(string*string) list` crossed into host code via direct cast with no `InvalidCastException`, because FSI resolved to the host's own `FSharp.Core` identity. Untested: `AssemblyLoadContext`-isolated hosting (the production-relevant case), and the full evaluate→generate→splice→typecheck loop. Detail: `Q003-fsi-comptime-eval/03-review.md`. |
| Q004 | Can typed access resolve the shape of a type defined only in a referenced *compiled* assembly (no source, no AST at all in current Myriad) — and if so, does that need FCS, or does plain reflection already solve it more cheaply? | **PLANNED** | Not yet run. Detail: `Q004-cross-assembly-typed-access/00-hypothesis.md`. |
| Q005 | Can a generator splice its own output into the live typed project and typecheck it *before* emission, refusing to write code that doesn't compile — and is the round-trip cost cheap enough to pay by default? | **PLANNED** | Not yet run. Detail: `Q005-self-verifying-generators/00-hypothesis.md`. |
| Q006 | Can a real Myriad generator (Lenses) be reimplemented as a generative F# type provider via `FSharp.TypeProviders.SDK`, so its output appears live in the IDE with zero build step — and where does this stop working for the rest of Myriad's generator surface? | **CLOSED** | **REVISE.** Mechanism validated with real numbers (47ms live re-check vs 1137ms cold, zero `dotnet build` of the consumer) on a faithful, runtime-correct port of `LensesGenerator`'s logic. Held short of SHIP: never tested against a real IDE host (only `FSharpChecker` as a library — pre-registered as insufficient), and type providers structurally can't reach a record in the same project currently being compiled, only ones already published elsewhere — a narrower capability than "fix Myriad's IDE-invisibility gap" implies. **No durable `artifacts/` — see `FINDINGS.md`'s credibility-gap note.** Detail: `Q006-myriad-as-type-provider/03-review.md`. |
| Q007 | Can a generative provider host `FsiEvaluationSession` inside its own static-parameter instantiation function to evaluate a string static argument as real F# code — sidestepping the literal-only static-argument restriction — and does that survive being invoked from inside the compiler's own live type-checking call stack? | **PLANNED** | Not yet run. General type-provider question, independent of Myriad's configuration — composes Q003 (FSI/FSharpChecker coexistence) and Q006 (a working string-static-parameter provider) for the first time. Detail: `Q007-fsi-static-parameters/00-hypothesis.md`. |
| Q008 | Can one generative type provider read a second, independently-compiled provider's emitted custom attributes via plain reflection and refuse to generate when declared provenance disagrees — compile-time supply-chain verification through the type system, caught live on a source edit with no rebuild? | **CLOSED** | **SHIP, reconstructed and re-verified 2026-07-16.** Attribute survives into independently-reflectable IL; two genuinely separate providers gate on it correctly in both directions with a specific diagnostic naming both versions; conflict caught live at 19-32ms (reproduced on recovered original source at 18-26ms), at or below Q006's 47ms baseline. Scoped: the real win is when/where the check fires (keystroke-time, source-cited), not that such a check can exist at all — a build-time script already does the latter. Originally shipped with no durable `artifacts/`; original scratch source was later found intact and used to fill the gap. Detail: `Q008-provenance-closed-loop/03-review.md` and `Q008-provenance-closed-loop/RECONSTRUCTION.md`. |
| Q009 | Can provenance enforcement work at *field* granularity instead of Q008's whole-type grain — a client depending on a subset of fields stays clean when an unrelated field changes and fails naming the specific field(s) when a depended one does? | **CLOSED** | **SHIP, reconstructed and re-verified 2026-07-16.** Member-level custom attributes survive into independently-reflectable IL, generalizing Q008's finding. Selective enforcement proven live in both directions (multi-field diagnostics name every mismatch, not just the first); re-check cost flat across a 3-to-12-field schema, matching Q008's band (reproduced on recovered original source, 15-56ms, still flat). The design-mandated "just use N whole types" alternative was measured honestly and also works — this quartet's edge is design-time cost and one dependency declaration as data, not a raw verbosity win. Originally shipped with no durable `artifacts/`; original scratch source was later found intact and used to fill the gap. Detail: `Q009-field-level-provenance/03-review.md` and `Q009-field-level-provenance/RECONSTRUCTION.md`. |
| Q010 | Can a generator compute a virtual file's content on demand, mid-check, as a function of the already-typechecked prefix of the same in-progress project — no fixpoint, no separate staging pass — by making `FSharpChecker`'s `DocumentSource.Custom` callback reentrant, and does that give real cross-generator visibility (one generator's output informing another's) Myriad's per-file-blind model can't produce? | **CLOSED** | **SHIP, scoped.** Reentrant callback returned correct typed results with zero diagnostics, no hang/exception, under both `TransparentCompiler` and `BackgroundCompiler`; a third generator correctly reused a second generator's already-generated output via symbol resolution verified two independent ways, not visual inspection. Held short of the hypothesis's strongest framing: unresolved whether the reentrancy tested was genuinely mid-flight or landing on an already-idle checker; proven only for the acyclic later-depends-on-earlier case, not mutual cross-generator dependency (item 10's territory); a real silent API footgun found (explicit source text bypasses `DocumentSource.Custom` for that file, no error). Detail: `Q010-prefix-stratified-generation/03-review.md`. |
| Q011 | Can the Q008/Q009 provenance-enforcement arrow be reversed — a client-side provider records what it consumed as its own IL attributes, and a schema-side provider refuses to compile a change that breaks any known, already-compiled client's recorded dependency — and is the resulting bidirectional loop well-founded (blocks on a stale client, clears once that client is recompiled, no deadlock)? | **CLOSED** | **SHIP, but the pre-registration missed the biggest finding.** Coordination-semantics claim proven cleanly (blocks on stale client naming assembly/field/both versions, clears on recompile with no deadlock, field removal gets a distinct diagnostic). But Q008/Q09's fast incremental `FSharpChecker` API path failed to resolve this provider's type even on the success path for an unidentified reason, forcing a fallback to full `checker.Compile` (106-201ms, not 15-57ms) — meaning the idea's actual selling point, keystroke-time not CI-time, is unproven and now specifically in doubt, not just untested. Also found: `DefineStaticParameters` fires more than once per check and must be memoized (probably lineage-wide, unconfirmed against Q008/Q09); the clean "unwedge" result depends on the provider's own choice not to cache failures. Detail: `Q011-consumer-driven-contracts/03-review.md`. |
| Q012 | Can a generative provider instrument and expose (via real provided members, not file logging) its own invocation count and call-stack context — and does a staged, minimal-difference comparison (one static param/no I/O vs. two params/no I/O vs. two params/with I/O) isolate which factor causes `ParseAndCheckFileInProject` to fail to resolve Q011's generative type on the success path? | **CLOSED** | **SHIP the mechanism; NULL on the two named factors; but the mandated positive control found the real, confirmed answer.** Neither parameter count nor instantiation-time I/O explains it — the simplest shape already fails. The axis is **generative vs. erased**: `ParseAndCheckFileInProject` resolves erased provided types cleanly and *never* resolves generative ones, confirmed against 3 toy shapes and both of Q011's real, unmodified providers. **This directly and specifically contradicted Q008's own claimed success-path resolution of a generative type via the same API** (real generated members accessed, ~1161ms cold). **Resolved 2026-07-16:** Q008's recovered original source shows it drove `ParseAndCheckFileInProject` over a real, hand-built `FSharpProjectOptions`, never a `.fsx` script — every shape Q012 tested here used `GetProjectOptionsFromScript`. Q012's own finding stands correctly scoped to the script configuration; see `FINDINGS.md`'s credibility section and `Q008-provenance-closed-loop/RECONSTRUCTION.md`. Detail: `Q012-compiler-behavior-probe/03-review.md`. |
| Q013 | Does a prior `checker.Compile` of a generative-provider scenario "warm" a subsequent `ParseAndCheckFileInProject` call for the *same* scenario, same checker instance — the cheapest reconciliation Q012's review named for the standing Q008-vs-Q012 contradiction? | **CLOSED** | **NULL, as pre-registered, verified cell-by-cell against raw logs.** Compile-then-PC ordering makes no difference: PC fails with Q012's identical "couldn't find type" diagnostic every time, deterministically across 3 in-process repeats and a second full process run, even though the preceding Compile resolves cleanly every time. Mechanistic reason found: each PC call mints a fresh temp assembly and reuses nothing from the prior Compile. Scoped precisely by review: this kills the *specific* same-checker/same-scenario/toy-shape reconciliation, not "warming" in general — cross-instance warming, a stricter same-project-object PC form, and `TransparentCompiler` remain untested. Also surfaced a shared blind spot: both Q012 and Q013 exercise PC only via a `.fsx` script, never a real `.fs` project. **That blind spot turned out to be exactly the answer:** the 2026-07-16 reconstruction (`Q008-provenance-closed-loop/RECONSTRUCTION.md`) confirmed, by direct same-checker isolation, that a real non-script `FSharpProjectOptions` resolves the identical generative type the script route fails on. Q013's own NULL (compile-then-PC warming, tested via script) stands correctly scoped. Detail: `Q013-compile-then-pc-warming/03-review.md`. |
| Q014 | Can FSI evaluate real computation at generation time — a general implementation partially evaluated against concrete static config, producing a quotation for the specialized result — and have that quotation reified into real F# source text that splices into the project and typechecks, i.e. actual staged compilation rather than data-driven templating? | **CLOSED** | **REVISE.** A compiler-built quotation (`[<ReflectedDefinition>]`/`TryGetReflectedDefinition`, no hand-built `Expr.Call`) was specialized by a hand-matched rewrite of one recursive shape (`power`, unrolled at n=4) and rendered by Unquote's `decompile` into F# source that reparsed, typechecked, and — verified by actually compiling and executing the text via `checker.Compile`, not just the quotation — ran correctly with zero residual recursion. Real: *code*, not just Q003's *data*, crosses the generation-time-to-source boundary. But the adversarial review, which independently re-ran the spike and reproduced it byte-for-byte, found the SHIP threshold's own "at FSI time" conjunct unmet — no `FsiEvaluationSession` ran anywhere in the spike — and that both the general implementation and its static config were source-level literals in the same program, so the dynamic-origin staging boundary Myriad would actually need was never crossed. The partial evaluator is also a bespoke transcription of `power`'s own three node shapes, not the generic `ExprShape` expander the design's own recon named but didn't build. Detail: `Q014-fsi-staged-compilation/03-review.md`. |
| Q015 | Direct follow-up to Q014's review, follow-up 1: can a real `FsiEvaluationSession` evaluate source for both the general recursive implementation (read from a separate plugin file, never in the host's own compiled source) and its specializing config (read from an environment variable), and can the resulting quotation still be reified via Q014's unchanged pipeline into real, correctly-executing spliced source? | **CLOSED** | **SHIP, narrowly scoped — like Q010, not like Q014.** All four SHIP conjuncts met and independently reproduced at a third `n` value (7): a `MethodInfo` obtained from a live FSI session, confirmed cross-assembly (`mi.Module.Assembly` genuinely differs from the host's own), fed `Expr.TryGetReflectedDefinition` correctly (identical shape to Q014's host-compile-time case — the one genuinely new, could-have-been-KILL mechanism fact), then Q014's unchanged reification pipeline specialized and executed it correctly. Closes Q014's specific "no FSI ran / both pieces were compile-time literals" gap for real. But the review's strongest objection: **FSI performs none of the actual staging** — it compiles the plugin and hands back a `MethodInfo` via a quotation-destructure one-liner that never executes `power`; 100% of the partial evaluation is host-compiled code byte-identical to Q014. The origin is dynamic-*body*/static-*interface* only — the host still hardcodes the plugin's name, arity, and types, and `unroll` is still hand-matched to `power`'s exact shape. Composition with Myriad's real architecture (ALC-isolated plugin loading, config from parsed attributed source) remains untested; FSI's non-isolated dynamic assembly is "the friendly slice of the dynamic-origin space, not the Myriad-relevant one." Detail: `Q015-fsi-dynamic-origin-staging/03-review.md`. |
| Q016 | Can a generative type provider re-expose members that are really Myriad's own compiled generator output — via `Assembly.LoadFrom` of a satellite DLL that Myriad's real, unmodified CLI produced and `checker.Compile` built — for Myriad's cross-project usage shape (`BACKLOG.md` item 15), fully sidestepping Q006's same-compilation wall by construction? | **PLANNED** | Hypothesis written; design not yet started. Detail: `Q016-satellite-dll-type-provider/00-hypothesis.md`. |
