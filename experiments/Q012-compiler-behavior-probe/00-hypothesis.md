# Q012-compiler-behavior-probe / Movement 1 — Hypothesis

**Status:** RUNNING. Pre-registered, execution not yet started.
**Date:** 2026-07-15
**Repo under test:** `FSharp.TypeProviders.SDK` checkout, Thread 2 lineage. Directly targets
`Q011-consumer-driven-contracts/03-review.md`'s follow-up 1 — the highest-priority open question in
the whole `experiments/` lineage per `FINDINGS.md`'s "Starting the next session."

**Question:** Can a generative type provider be built whose purpose is instrumenting *the compiler
hosting it*, not modeling external data — exposing its own invocation count, timing, and (if
capturable) call-stack context as real, queryable provided members, rather than ad hoc file logging?
And, pointed at the specific unresolved question Q011 left open: does a controlled, staged comparison
of near-identical providers — one shaped like Q008/Q009's working `ClientTP` (one static parameter,
no instantiation-time I/O), and two shaped like Q011's failing `SchemaTP` (two static parameters;
two static parameters *plus* real file I/O during instantiation) — reveal which specific factor
causes `ParseAndCheckFileInProject`/`ParseAndCheckProject` to fail to resolve a generative type even
on the zero-diagnostic success path?

## The claim

Two separable claims, kept apart the same way every quartet in this lineage has:

1. **Mechanism claim (probe validity):** a provider's instantiation function can capture and expose,
   via real provided members read back by an independent consumer (not internal file logging, Q011's
   `MiniTP` precedent), its own invocation count and a captured `System.Diagnostics.StackTrace` — and
   that captured trace contains recognizable `FSharp.Compiler.*` namespace frames, not just this
   provider's own code, making it useful as a window into FCS's own call pattern rather than only
   confirming "it was called more than once."
2. **Capability claim (diagnostic payoff):** applying this probe to a staged, minimal-difference triple
   of providers isolates which factor — static-parameter count, or instantiation-time I/O, or neither
   — correlates with `ParseAndCheckFileInProject` failing to resolve the type on the success path,
   narrowing Q011's unexplained regression to a specific, named candidate rather than leaving it
   fully open.

## Why this is the right next spike

Named directly, not re-derived: `Q011-consumer-driven-contracts/03-review.md`'s follow-up 1 calls
finding this root cause "the single highest-value unresolved question in the whole quartet," and
`FINDINGS.md`'s "Starting the next session" section already elevated it to the clear top priority
over every other queued item, because every future Thread 2 idea inherits the same uncertainty about
whether the fast, incremental `FSharpChecker` API path Q008/Q009 relied on will work until this is
resolved. This quartet is also a genuinely novel *use* of the type-provider mechanism in its own
right — every prior quartet in this lineage built a provider that models or enforces something about
external data; this is the first to build one that instruments the compiler hosting it, turning the
SDK into a diagnostic instrument aimed at itself.

## Novelty gate

Checked `Q011-consumer-driven-contracts/02-results.md` and its `artifacts/MiniTP-isolation-probe/`
directly: Q011's own `MiniTP` used ad hoc file-based logging (stdout was noted as unreliable to
observe from inside a hosted provider) to confirm *that* the instantiation function fires more than
once, for one specific scenario, as a side investigation — it never captured a stack trace, never
exposed the finding as a queryable provided member, and never built a controlled comparison isolating
*which* structural factor causes `ParseAndCheckFileInProject` to fail. This quartet generalizes that
ad hoc technique into a reusable diagnostic type and points it at a question Q011 explicitly left
unanswered. Not a re-tread.

## Contradiction gate

Does not contradict any prior verdict. Reuses Q008/Q09/Q011's provider-construction patterns
(`ProvidedTypeDefinition`, `DefineStaticParameters`, `AddCustomAttribute`-adjacent member creation)
and Q011's own `MiniTP` isolation technique as a starting point, extended rather than replaced.

## Validity preconditions

- `FSharp.Compiler.Service` pinned `43.9.101`; `ProvidedTypes.fs`/`.fsi` from the same
  `FSharp.TypeProviders.SDK` commit (`0a95768a2247daba80b24a2604f77f89fc88ff1f`) every Thread 2
  quartet since Q006 has used, for comparable results.
- The comparative triple must vary **one factor at a time**, not conflate them: (a) one static
  parameter, no I/O (mirrors Q008/Q09's working shape); (b) two static parameters, no I/O (isolates
  parameter count alone); (c) two static parameters plus real instantiation-time file I/O (isolates
  I/O on top of (b), mirroring Q011's actual `SchemaProvider` shape as closely as a minimal probe can).
  Building (b) is the load-bearing addition beyond what Q011 itself tested — Q011 only ever tested its
  own real, full shape (two params **and** I/O together), never a two-params/no-I/O midpoint, so this
  is genuinely new information, not a re-run.
- Must test all three shapes under the **same** checking API in the same run
  (`ParseAndCheckFileInProject`, since that's the specific API Q011 found failing) so the comparison is
  apples-to-apples, not confounded by a different harness per shape.
- If Round 2's isolation succeeds, the finding should be checked against Q011's **real** provider code
  (not just the toy analogs), per Round 3 below — a candidate cause identified only in a simplified
  stand-in is a weaker result than one confirmed against the actual artifact the mystery originated in.
- Every timing number single-sample, same convention as every prior quartet.
- Report honestly if stack-trace capture yields nothing useful (erased frames, tail-call optimization,
  a delegate/reflection boundary that strips caller context) — that would be a real, valid negative
  finding about the probe technique's own ceiling, not a reason to quietly drop that half of the design.

## Cheapest falsifier

Before building the three-way comparison: does a **minimal** probe provider (one static parameter,
one property, instrumented to capture and expose invocation count plus a `StackTrace` snapshot via
real provided members) actually show more than one invocation for a single logical check — replicating
Q011's `MiniTP` finding through a different, reusable observation channel — **and** does the captured
stack trace contain any recognizable `FSharp.Compiler.*`-namespaced frame, rather than only this
provider's own code? If the stack trace is empty or uninformative, the probe's core value proposition
(revealing *where in FCS*, not just *that*) collapses immediately, cheaply, before building the
comparative isolation matrix — the matrix would still be worth attempting via invocation-count-and-
success/failure alone, but the "why" half of the claim would already be dead.

## Pre-registered decision thresholds

- **SHIP:** the falsifier passes (multiple invocations confirmed via provided members; captured stack
  traces contain recognizable FCS-internal frames), **and** the isolation matrix (Round 2) identifies a
  specific, reproducible factor — parameter count, instantiation-time I/O, or a specific point in
  either's invocation pattern — that correlates with `ParseAndCheckFileInProject` failing on shapes (b)
  and/or (c) but not (a), **and** Round 3 confirms the same factor explains the failure when retrofitted
  onto Q011's actual `SchemaProvider`/`ClientProvider` artifacts, not just the toy analogs.
- **REVISE:** the invocation-count-and-timing half of the probe works reliably, but stack-trace capture
  doesn't yield FCS-internal information (erased/uninformative frames) — the isolation matrix can still
  be attempted using count/success-vs-failure alone, and if it succeeds on that weaker evidence, treat
  it as a real but less-explained finding (correlation without a visible mechanism), narrower than the
  full claim.
- **NULL:** the probe and isolation matrix both work cleanly, but show **no** discernible difference
  between shapes (a), (b), and (c) under `ParseAndCheckFileInProject` — meaning none of them fail, or
  all of them fail identically regardless of shape — which would mean Q011's regression isn't explained
  by parameter count or instantiation-time I/O at all, and must lie elsewhere (e.g. something specific
  to `SchemaProvider`'s own enforcement logic, or FCS's post-instantiation resolution/caching path, not
  the instantiation call itself). A genuinely useful negative result that redirects the next follow-up
  away from the instantiation function.
- **KILL:** the probe's own capture mechanism is unreliable — invocation counts vary non-deterministically
  across repeated runs of the identical scenario, or capturing a stack trace inside a hosted provider
  crashes or hangs the design-time host — meaning this technique isn't viable as a diagnostic tool at
  all, independent of what it might have revealed about Q011's mystery.
