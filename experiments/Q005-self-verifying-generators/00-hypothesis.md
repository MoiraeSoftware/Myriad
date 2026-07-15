# Q005-self-verifying-generators / Movement 1 — Hypothesis

**Status:** PLANNED. Pre-registered only — not yet built. Awaiting go-ahead.
**Date:** 2026-07-14

**Question:** Current Myriad emits generated code blind: `Program.fs` calls
`instance.Generate(context)` (`Program.fs:174`), formats it, and writes it straight to disk via
`File.WriteAllLines` (`Program.fs:235`/`:245`) with no typecheck step anywhere in between —
correctness is only discovered later, when the *separate* `dotnet build` that follows the
pre-build step re-typechecks the whole project. Can a generator, hosted on the Q001 in-process
`FSharpChecker`/`TransparentCompiler` harness, splice its own candidate output into the live
virtual project and typecheck it *before* emission, so that a generator with diagnostics is
refused (or flagged) rather than written to disk to fail downstream? And is the per-generation
round-trip cost of that gate (generate → splice → typecheck → accept/reject) cheap enough to be
worth paying inside Myriad's actual pipeline, given the pipeline already pays a cold-process-per-
file cost today (`BACKLOG.md`, known gap 2)?

## Why this is the right next spike

Not a capability claim about what typed *input* access can see (Q001/Q002/Q004's question) — this
is the first quartet to ask about typed *verification* of output, using a mechanism Q001 and Q002
already exercised for a different purpose. Both of those quartets typechecked their spliced
generated output as part of validating the spike's own correctness — Q001's results log it
explicitly ("Generated output (typechecked with zero diagnostics, never written to disk)",
`Q001-fcs-typed-codegen/02-results.md:64`) — but always as an experiment-side check on the
harness, never as a decision the generator itself makes about whether to emit. Turning that
already-proven mechanism into a gate inside the actual generation pipeline is the new claim: it
converts Myriad's current "generate first, discover failure at the next `dotnet build`" contract
into "cannot silently emit code that doesn't typecheck." That's a correctness guarantee, not a new
kind of output — a smaller, more defensible claim than most capability-expansion ideas, and
correspondingly cheaper to falsify.

## Novelty gate

Confirmed by reading `src/Myriad/Program.fs` directly: the generate → format → write path
(`:174` → `:235`/`:245`) has no typecheck or diagnostics call anywhere in it, and no file in
`src/Myriad/`, `src/Myriad.Core/`, or `src/Myriad.Plugins/` references `FSharpChecker` or
`CheckFileResults` at all (verified by grep, zero hits). Not a re-tread of Q001/Q002's use of
typechecking as a spike-verification step — this proposes using the same mechanism as a
product-facing gate, which neither prior quartet built or claimed.

## Contradiction gate

Does not contradict Q001–Q004. It depends on Q001's TransparentCompiler mechanism working
correctly (already validated) and inherits Q001's one open caveat directly: TransparentCompiler is
still labeled "experimental" by both FCS and FSAC as of the 43.9.x line
(`Q001-fcs-typed-codegen/02-results.md:53-56`). A gate that gets to decide whether Myriad emits
code at all is a materially higher-stakes use of an experimental compiler mode than Q001's original
read-only spike — this quartet should treat that caveat as load-bearing, not inherited boilerplate,
and its review must explicitly re-assess whether "experimental" is still an acceptable label for a
gate with veto power over the build.

## Validity preconditions

- FCS pinned to `43.9.101`, matching `Myriad/paket.lock`, same as every prior quartet.
- Must measure the round-trip cost (generate → splice → typecheck) against a project of realistic
  size, not a two-file toy — Q001's own Round 3 scaling test was flagged as too light-weight to
  trust (padding files near-free to typecheck; `BACKLOG.md`, spike-shaped item 4), and that flaw
  would understate this quartet's cost claim in exactly the same way if repeated here.
- Must define what "reject" means operationally: hard build failure with the real diagnostic
  surfaced (preferred, since it fails loud and early instead of at the next `dotnet build`), versus
  falling back to emitting anyway with a warning. Pre-register which one is being tested; don't
  decide after seeing how inconvenient a hard failure turns out to be.
- Must test against a generator whose output can plausibly fail to typecheck for a reason worth
  catching (e.g. Q002's nested-dispatch generator on an adversarial input), not a generator like
  Fields where correctness was already shown to be trivial (Q001) — a gate that never catches
  anything proves nothing about the gate.

## Cheapest falsifier

Before building any gating logic: take Q002's already-built nested-dispatch generator, deliberately
feed it an input that produces output Q002's own review already identified as broken (the
unparenthesized nested-call bug named in `BACKLOG.md` spike-shaped item 3), splice that broken
output into the live virtual project, and check whether `FSharpChecker`'s diagnostics for the
spliced file actually flag it. If a known-bad output doesn't produce a typecheck error the gate
can detect, the entire premise — that in-process typechecking would have caught what shipped —
is false, and there's no reason to build the accept/reject machinery around it.

## Pre-registered decision thresholds

- **SHIP:** the gate correctly rejects the known-bad Q002 output, round-trip cost per generation is
  small enough (order of magnitude below the existing cold-process-per-file cost named in
  `BACKLOG.md`) to be worth paying by default, and TransparentCompiler's "experimental" status is
  judged (with a concrete reason, not a shrug) as acceptable for this use.
- **REVISE:** the gate works and catches real errors, but round-trip cost is high enough that it
  should be opt-in (e.g. a CI-only or `--strict` mode) rather than the default path — a real,
  useful, but narrower result than the full claim.
- **KILL:** the known-bad output falsifier fails (diagnostics don't surface the error), or
  TransparentCompiler's experimental status produces a concrete failure mode (crash, hang,
  incorrect diagnostic) during this quartet's own testing — either would mean the gate can't be
  trusted with veto power over the build, which is a materially worse failure mode than the
  underlying feature not existing at all.
