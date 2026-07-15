# Q013-compile-then-pc-warming / Movement 1 — Hypothesis

**Status:** RUNNING. Pre-registered, execution not yet started.
**Date:** 2026-07-15
**Repo under test:** `FSharp.TypeProviders.SDK` checkout, Thread 2 lineage. Directly targets the
single named follow-up 1 from `Q012-compiler-behavior-probe/03-review.md` (objection 1), which
`FINDINGS.md`'s "Starting the next session" section calls "not a close call" and the clear top
priority over everything else queued.

**Question:** Does a prior `checker.Compile` call for a generative-provider scenario "warm" a
subsequent `checker.ParseAndCheckFileInProject` (PC) call for the **same** scenario, causing PC to
now resolve the generative type where a cold PC call cannot — the exact, specific, reproducible
failure Q012 established (PC never resolves a generative provided type; only `Compile` does)?

## Why this exists

Q012 found, reproducibly and with saved artifacts, that `ParseAndCheckFileInProject` never resolves
a generative provided type — confirmed against three toy shapes and both of Q011's real providers.
`Q008-provenance-closed-loop/02-results.md` claims the opposite: a generative type resolving via
that same diagnostics-only checking path, with real generated members read back, at ~1161ms cold.
Q008 saved no source, so its claim cannot be checked directly. These two results cannot both be
true of the same pinned FCS version (`43.9.101`) unless Q008's actual harness did something its
prose doesn't mention. Q012's own review named the single cheapest, most plausible reconciling
explanation, never tested by any prior quartet: every scenario in Q012's harness called PC *before*
`Compile`, never the reverse — so if Q008's harness happened to compile something first (for an
unrelated reason, e.g. building a dependency), a caching or state effect crossing the PC/Compile API
boundary could explain the discrepancy without either quartet's numbers being wrong.

## The claim

Two separable claims, same discipline as every quartet in this lineage:

1. **Mechanism claim:** running `checker.Compile` on a scenario, then running
   `checker.ParseAndCheckFileInProject` on the *same* scenario afterward (same process, same
   `FSharpChecker` instance) changes PC's outcome for a generative provided type — from Q012's
   established "couldn't find type" failure to a clean, zero-diagnostic resolution with the
   generated type's members readable via `GetSymbolUseAtLocation`/typed-tree inspection.
2. **Reconciliation claim:** if the mechanism claim holds for the toy `ProbeSimple` generative shape,
   it also holds when retrofitted onto Q011's real, unmodified `SchemaTP`/`ClientTP` providers — the
   actual disputed case — meaning the compile-then-PC ordering is a plausible, generalizable
   explanation for Q008/Q09's claimed numbers, not an artifact specific to a minimal toy shape.

Both must hold for this to actually reconcile Q008/Q09 with Q012; the mechanism claim alone (working
only on a toy shape) would be a real but narrower finding.

## Novelty gate

Checked `Q012-compiler-behavior-probe/artifacts/Harness/Program.fs` directly (`runShape`, lines
176–218): every shape in Q012's own harness calls `parseAndCheck` first, then `compile`, for every
scenario, with no reverse-order or same-instance-reuse test anywhere. This ordering is genuinely
untested by any prior quartet — not a re-tread of Q012's Round 1/2/3, which all tested PC and
Compile as independent, unordered probes of the same cold scenario, never as a sequence sharing
state.

## Contradiction gate

Does not contradict any prior verdict — this quartet exists specifically to interrogate an existing,
named contradiction between Q008/Q09 and Q012, not to introduce a new one. If the warming effect is
real, it narrows (does not reverse) Q012's finding: PC still never resolves a *cold* generative type;
it would mean PC's outcome depends on prior same-process state, which Q012 never varied.

## Validity preconditions

- `FSharp.Compiler.Service` pinned `43.9.101`; `ProvidedTypes.fs`/`.fsi` from
  `FSharp.TypeProviders.SDK` commit `0a95768a2247daba80b24a2604f77f89fc88ff1f` — same pins as every
  Thread 2 quartet since Q006, required for a comparable result.
- Reuse Q012's own saved `ProbeSimple.DesignTime`/`ProbeSimple.Runtime` provider source unmodified
  (`experiments/Q012-compiler-behavior-probe/artifacts/ProbeSimple.*`) as the Round 1 scenario —
  this must be the *exact* generative shape Q012 already proved fails cold via PC, so a changed
  outcome is attributable to ordering alone, not a different provider.
- The "same scenario" must mean literally identical: same virtual file path, same source text, same
  project options — construct it via the same helper Q012 used (`parseAndCheck`/`compile` in
  `artifacts/Harness/Program.fs`), not a hand-rebuilt approximation that might silently differ.
- Test same-checker-instance reuse as the primary condition (Compile and PC called on the same
  `FSharpChecker` object, in that order). If that shows an effect, additionally test whether a
  *fresh* `FSharpChecker` instance in the same process also benefits (isolates instance-local state
  from any process-global FCS caching), since Q008/Q09's own harness construction is unknown and
  either shape is plausible.
- Round 3 (retrofit onto Q011's real providers) only proceeds if Round 1 shows a positive effect on
  the toy shape — no point testing the real artifacts against an ordering that doesn't work even in
  the simplest case.
- Repeat each condition at least 3 times to rule out non-determinism before reporting a result either
  way — Q012 flagged this as a KILL-relevant risk category and this quartet inherits the same
  standard.
- Every timing number single-sample per run but collected across repeats; report the spread, not
  just one number, since ordering-dependent caching effects are exactly the kind of thing that could
  be flaky.

## Cheapest falsifier

Before anything else: take Q012's own `ProbeSimple` generative scenario (`Probe<"gen-warm">` or
similar tag, new to avoid any accidental cache collision with Q012's literal prior run), call
`checker.Compile` on it once, then immediately call `checker.ParseAndCheckFileInProject` on the
identical scenario using the same checker instance. Does the PC call now return zero error
diagnostics for a scenario that Q012 established fails cold? One run, cheap, decisive about whether
this entire line of investigation has legs before building anything further.

## Pre-registered decision thresholds

- **SHIP:** the falsifier passes reproducibly (3+ repeats, same-checker-instance condition) for the
  toy `ProbeSimple` shape, **and** Round 3 confirms the same effect when retrofitted onto Q011's real
  `SchemaTP`/`ClientTP` providers — resolving via PC after a prior `Compile` of the same scenario,
  where Q012 established both fail cold via PC alone. This would reconcile Q008/Q09's claimed numbers
  with Q012's finding and be a genuinely new, useful discovery about FCS's caching behavior across
  API boundaries.
- **REVISE:** the falsifier passes for the toy shape but Round 3 does not confirm it against Q011's
  real providers (or only confirms it under extra preconditions unlikely to match how an ordinary
  harness would be written) — meaning a real warming effect exists but it's narrower than what would
  actually explain Q008/Q09, so the contradiction is only partially addressed.
- **NULL:** the falsifier fails — compile-then-PC ordering makes no difference; PC still fails
  identically (same "couldn't find type" diagnostic) after a prior `Compile` of the identical
  scenario, same checker instance, repeated 3+ times. This hardens the Q008/Q09-vs-Q012 contradiction
  and makes retroactive reconstruction of Q008/Q09's actual harness the clear next priority, no
  longer optional.
- **KILL:** the result is non-deterministic across identical repeats (PC sometimes resolves,
  sometimes doesn't, with no identified controlling variable), or the harness crashes/hangs —
  meaning ordering isn't a clean explanatory variable even if some effect is present, and the
  question needs a differently-shaped investigation before it can be answered cleanly.
