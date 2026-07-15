# Q001-fcs-typed-codegen / Movement 1 — Hypothesis

**Status:** CLOSED. First quartet under Myriad's own `experiments/` methodology (see
`../README.md`). Originally run and drafted as a scratch spike outside this repo (no code
committed anywhere at the time); backfilled into this format and numbering after the fact, then
its artifacts were reconstructed as buildable projects under `artifacts/` and re-verified to
produce the same output recorded in `02-results.md` — see that file's note.
**Date:** 2026-07-14
**Repo under test:** this repo (Moirae Software's F# code-generation tool).

**Question:** Myriad generates F# source by parsing *untyped* syntax, writing generated `.fs`
files to disk, and having a separate `dotnet build` re-parse and re-typecheck them. Can the same
job be done by hosting `FSharp.Compiler.Service` in-process (the way Fable already does for
transpilation), reading *typed* symbols instead of syntax, and splicing generated code back in
purely in-memory — and if so, does the typed access actually buy anything for a generator Myriad
ships today?

## The claim

Two separable claims, deliberately kept apart so a null result on the second doesn't sink the first:

1. **Mechanism claim:** an in-process `FSharpChecker` host, backed by `DocumentSource.Custom`
   (virtual, non-disk files) and `useTransparentCompiler = true`, can (a) extract resolved typed
   field/attribute information from a source file, (b) splice a generated companion file back into
   the *same* logical project with no disk write, (c) typecheck it correctly, and (d) correctly and
   reasonably-cheaply re-detect staleness when a dependency changes — all without the caller
   hand-rolling a dependency graph.
2. **Capability claim:** typed access gives a real generator information a syntax-only tool
   structurally cannot get cheaply or correctly (e.g. seeing through a user type alias to its
   resolved form), and this shows up as a concrete difference when porting one of Myriad's actual
   shipping generators.

## Why this is newly viable (not a re-tread of "just rewrite Myriad")

An earlier pass on this question (same conversation, prior turns) proposed the same shape but
was rightly challenged as possibly over-scoped to "same tool, incrementally nicer" rather than a
genuine rearchitecture. The counter-check: every load-bearing piece of the ambitious version
already exists and runs in production somewhere in the F# ecosystem — Fable hosts FCS in-process
against the *typed* tree today; FSI hosting is a real embeddable API; F# quotations are a
real typed-output mechanism. So the novel part isn't invention, it's whether these pieces actually
compose the way the pitch assumes. That composability claim is what this spike tests, not a new
idea.

## Validity preconditions

- Must run against the FCS version Myriad itself is pinned to (`43.9.101`, per
  `Myriad/paket.lock`), not an aspirational newer one — an architecture argument built on a
  version Myriad doesn't use yet proves nothing about Myriad.
- The capability claim must be tested against an *actual* Myriad generator
  (`src/Myriad.Plugins/FieldsGenerator.fs`), ported with reasonable fidelity to its real output
  shape, not a synthetic strawman built to make typed access look good.
- Every timing/correctness number must come from code that was actually run, not estimated —
  consistent with how the rest of this conversation's spikes were done.

## Cheapest falsifier

Before investing in anything past the mechanism (comptime evaluation, typed output, hygiene,
plugin packaging): does typed access change the generated output for the *simplest* real Myriad
generator (Fields — pure structural echo, no cross-type reasoning)? If it doesn't, that's a
partial falsifier of "typed access is the headline win" for at least one whole class of existing
generators, and the honest scope of what's been proven shrinks accordingly. This is deliberately
the weakest, cheapest test of the capability claim, run first.

## Pre-registered decision thresholds

- **Mechanism SHIP:** in-process splice typechecks correctly; a broken dependency is
  auto-detected without the caller manually tracking and invalidating; incremental recheck cost is
  low relative to cold start, not proportional to full project re-parse.
- **Capability SHIP (for Fields specifically):** typed field access produces output that differs
  meaningfully (correctness or capability, not just cosmetically) from what Myriad's current
  syntax-echo approach already produces for the same input.
- **KILL the whole line:** in-process hosting cannot reliably detect a stale dependency (silent
  wrong output is worse than Myriad's current disk-round-trip model, not better).
- **REVISE:** mechanism ships, capability claim doesn't generalize from this one generator either
  way — narrow the claim to "this is a viable *foundation*," not "this is Myriad, evolved."
