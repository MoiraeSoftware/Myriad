# Q003-fsi-comptime-eval / Movement 1 — Hypothesis

**Status:** RUNNING (falsifier being built now; see `01-design.md`/`02-results.md` as they land).
**Date:** 2026-07-14

**Question:** Type providers get their power from running arbitrary code at compile time against
external data (a schema file, a database, a WSDL) and producing types from it. Q001/Q002 validated
typed, in-process, whole-project FCS hosting but never touched compile-time *evaluation* — every
generator so far has only read syntax/types, never executed anything. Can `FSharp.Compiler.Interactive.Shell`
(`FsiEvaluationSession`) be hosted in the *same* process as the `FSharpChecker` used in Q001/Q002,
and evaluate a real expression, without the two compiler-adjacent subsystems colliding?

## Why this is the right next spike

Named directly in Q002's `03-review.md` as the one pillar from the original pitch still at zero
evidence. Also the direct answer to "I'd love type providers to be more useful": if this works,
the path is real emitted source driven by compile-time evaluation, not an erased runtime shim.

## Novelty / contradiction gate

Not a re-tread of Q001/Q002 (neither touched execution, only parsing/typechecking). Doesn't
contradict either — it's the named gap both left open.

## Cheapest falsifier

Does `FsiEvaluationSession.Create(...)` even instantiate and evaluate a trivial expression
(`1 + 2`) inside a process that has already created an `FSharpChecker`? Both are compiler-adjacent
subsystems from the same package; the concern is shared static state or initialization order, not
a version mismatch this time (unlike the `dotnet fsi` failure in Q001 — this spike uses one
consistently-resolved `FSharp.Compiler.Service` package reference for both, not two different
hosts). Test this before building anything that assumes it works.

## Pre-registered decision thresholds

- **PASS, proceed:** both subsystems coexist and FSI returns a real evaluated value.
- **PASS further:** an FSI-evaluated "schema" value can drive text generation spliced into the
  same `FSharpChecker` project from Q001/Q002 and typecheck — the actual end-to-end claim.
- **KILL:** instantiating both in one process throws, deadlocks, or corrupts either subsystem's
  state — this would be a real, load-bearing wall for the whole comptime-evaluation pillar.
