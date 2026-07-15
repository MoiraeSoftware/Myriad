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
| Q008 | Can one generative type provider read a second, independently-compiled provider's emitted custom attributes via plain reflection and refuse to generate when declared provenance disagrees — compile-time supply-chain verification through the type system, caught live on a source edit with no rebuild? | **CLOSED** | **SHIP.** Attribute survives into independently-reflectable IL; two genuinely separate providers gate on it correctly in both directions with a specific diagnostic naming both versions; conflict caught live at 19-32ms, at or below Q006's 47ms baseline. Scoped: the real win is when/where the check fires (keystroke-time, source-cited), not that such a check can exist at all — a build-time script already does the latter. **No durable `artifacts/` — see `FINDINGS.md`'s credibility-gap note.** Detail: `Q008-provenance-closed-loop/03-review.md`. |
| Q009 | Can provenance enforcement work at *field* granularity instead of Q008's whole-type grain — a client depending on a subset of fields stays clean when an unrelated field changes and fails naming the specific field(s) when a depended one does? | **CLOSED** | **SHIP.** Member-level custom attributes survive into independently-reflectable IL, generalizing Q008's finding. Selective enforcement proven live in both directions (multi-field diagnostics name every mismatch, not just the first); re-check cost flat across a 3-to-12-field schema, matching Q008's band. The design-mandated "just use N whole types" alternative was measured honestly and also works — this quartet's edge is design-time cost and one dependency declaration as data, not a raw verbosity win. **No durable `artifacts/` — see `FINDINGS.md`'s credibility-gap note.** Detail: `Q009-field-level-provenance/03-review.md`. |
| Q010 | Can a generator compute a virtual file's content on demand, mid-check, as a function of the already-typechecked prefix of the same in-progress project — no fixpoint, no separate staging pass — by making `FSharpChecker`'s `DocumentSource.Custom` callback reentrant, and does that give real cross-generator visibility (one generator's output informing another's) Myriad's per-file-blind model can't produce? | **CLOSED** | **SHIP, scoped.** Reentrant callback returned correct typed results with zero diagnostics, no hang/exception, under both `TransparentCompiler` and `BackgroundCompiler`; a third generator correctly reused a second generator's already-generated output via symbol resolution verified two independent ways, not visual inspection. Held short of the hypothesis's strongest framing: unresolved whether the reentrancy tested was genuinely mid-flight or landing on an already-idle checker; proven only for the acyclic later-depends-on-earlier case, not mutual cross-generator dependency (item 10's territory); a real silent API footgun found (explicit source text bypasses `DocumentSource.Custom` for that file, no error). Detail: `Q010-prefix-stratified-generation/03-review.md`. |
| Q011 | Can the Q008/Q009 provenance-enforcement arrow be reversed — a client-side provider records what it consumed as its own IL attributes, and a schema-side provider refuses to compile a change that breaks any known, already-compiled client's recorded dependency — and is the resulting bidirectional loop well-founded (blocks on a stale client, clears once that client is recompiled, no deadlock)? | **CLOSED** | **SHIP, but the pre-registration missed the biggest finding.** Coordination-semantics claim proven cleanly (blocks on stale client naming assembly/field/both versions, clears on recompile with no deadlock, field removal gets a distinct diagnostic). But Q008/Q09's fast incremental `FSharpChecker` API path failed to resolve this provider's type even on the success path for an unidentified reason, forcing a fallback to full `checker.Compile` (106-201ms, not 15-57ms) — meaning the idea's actual selling point, keystroke-time not CI-time, is unproven and now specifically in doubt, not just untested. Also found: `DefineStaticParameters` fires more than once per check and must be memoized (probably lineage-wide, unconfirmed against Q008/Q09); the clean "unwedge" result depends on the provider's own choice not to cache failures. Detail: `Q011-consumer-driven-contracts/03-review.md`. |
| Q012 | Can a generative provider instrument and expose (via real provided members, not file logging) its own invocation count and call-stack context — and does a staged, minimal-difference comparison (one static param/no I/O vs. two params/no I/O vs. two params/with I/O) isolate which factor causes `ParseAndCheckFileInProject` to fail to resolve Q011's generative type on the success path? | **CLOSED** | **SHIP the mechanism; NULL on the two named factors; but the mandated positive control found the real, confirmed answer.** Neither parameter count nor instantiation-time I/O explains it — the simplest shape already fails. The axis is **generative vs. erased**: `ParseAndCheckFileInProject` resolves erased provided types cleanly and *never* resolves generative ones, confirmed against 3 toy shapes and both of Q011's real, unmodified providers. **This directly and specifically contradicts Q008's own claimed success-path resolution of a generative type via the same API** (real generated members accessed, ~1161ms cold) — Q008 has no saved artifacts, so this can't be mechanically reconciled yet. Treat Q008/Q09's SHIP verdicts as actively disputed, not merely unverified, until the untested compile-then-PC ordering hypothesis is checked. Detail: `Q012-compiler-behavior-probe/03-review.md`. |
