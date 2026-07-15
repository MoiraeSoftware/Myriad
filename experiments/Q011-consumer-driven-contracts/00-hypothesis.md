# Q011-consumer-driven-contracts / Movement 1 — Hypothesis

**Status:** RUNNING. Pre-registered, execution not yet started.
**Date:** 2026-07-15
**Repo under test:** `FSharp.TypeProviders.SDK` checkout, Thread 2 lineage (Q008/Q009's cross-provider
reflection harness), promoted from `BACKLOG.md`'s "More ambitious ideas" section (the reversed-arrow
entry, marked there as highest-priority next spike).

**Question:** Q008 and Q009 both point enforcement the way compilation always points: the *consumer*
reads the *producer*'s declared provenance and refuses to generate on mismatch. Can the arrow be
reversed? A client-side generative provider, when it generates its own accessors for a subset of a
schema's fields, also stamps its own emitted members with what it actually consumed
(`ConsumesFieldAttribute(fieldName, versionSeenAtGenerationTime)`) — the same member-level attribute
mechanism Q009 already proved survives into independently-reflectable IL, just emitted by the
*client* instead of the *schema*. Can a schema-side generative provider then read a set of known,
already-compiled client assemblies, and refuse to generate a schema change that removes or
re-versions a field any of them is recorded as depending on — citing the specific client assembly,
the specific field, and both versions? And, the sharper question that makes this more than "Q008/Q009
with the labels swapped": is the resulting loop well-founded — does a schema author's edit correctly
get blocked by a *stale* (not-yet-recompiled) client, and does recompiling that client to match the
new schema correctly clear the block, with no deadlock when both sides need to move?

## The claim

Two separable claims, kept apart the same way Q008/Q009 split mechanism from selective-precision:

1. **Mechanism claim:** a client-side generative provider's `AddCustomAttribute` call on its own
   emitted members survives into independently-reflectable IL, exactly as Q009 proved for
   schema-side emission — i.e. attribute survival is a property of the generative-provider IL-emission
   path itself, not something specific to which provider in a pair happens to be doing the emitting.
2. **Capability claim:** a schema-side provider can enforce, at generation time, that no known
   compiled client's recorded field dependency is violated by the schema currently being generated —
   correctly blocking on a stale client's recorded expectation, correctly naming the responsible
   client assembly and field, and correctly clearing once that client is recompiled against the new
   schema (proving the loop doesn't wedge a legitimate coordinated upgrade).

## Why this is the right next spike

Named explicitly in `BACKLOG.md`'s "More ambitious ideas" section as the highest-priority queued
item, for reasons worth restating precisely rather than re-deriving: it needs no new SDK mechanism
(pure recombination of Q008/Q009's already-proven `Assembly.LoadFrom` + reflection + diagnostics-only
gating path), it stays entirely inside Q006's structural wall by construction (both directions of the
pair remain cross-compilation — a client reading a schema assembly, and now a schema reading client
assemblies, neither ever reaches into the compilation currently in progress), and it directly answers
Q009's own still-open follow-up 2 (field *removal*, not just version bumps on a fixed field set) as a
side effect of Round 3 below, without a separate quartet. It is also the first idea in this lineage to
actively *exploit* the wall's shape rather than merely tolerate it: already-compiled client assemblies
are exactly the evidence base a consumer-driven-contract check needs, and the wall guarantees they're
always in the right state (compiled, stable) to reflect over.

## Novelty gate

Checked `Q008-provenance-closed-loop/02-results.md` and `Q009-field-level-provenance/02-results.md`
directly: both quartets' enforcement arrow runs exactly one direction, client reads schema. Neither
ever has a schema-side provider read a client assembly, and neither has a client-side provider stamp
its *own* emitted members with a self-declared attribute — every attribute emitted in both prior
quartets describes the *schema's* provenance, never the *client's* declared consumption. This is
genuinely new: the mechanism claim asks whether attribute-emission-and-survival is symmetric across
which of the two paired providers does the emitting (never tested, plausible but unconfirmed), and the
capability claim (the well-foundedness of a bidirectional-checking loop) has no precedent in either
prior quartet, both of which only ever tested one-shot or one-directional live edits.

## Contradiction gate

Does not contradict Q008 or Q009. Depends on Q009's core finding (member-level custom attributes
survive into independently-reflectable IL) holding when the emitting provider is the client rather
than the schema — a live possibility this quartet must actually verify (Round 1), not assume
transfers automatically just because the underlying `AddCustomAttribute` call is API-identical.
Consistent with Q006's wall: this idea was specifically selected, per `BACKLOG.md`'s own framing,
because both directions of the reflection stay cross-compilation and never attempt to reach a
same-project type the way Q006's port did.

## Validity preconditions

- `FSharp.Compiler.Service` pinned `43.9.101`; `ProvidedTypes.fs`/`.fsi` from the same
  `FSharp.TypeProviders.SDK` commit Q006/Q008/Q009 used, for directly comparable timing numbers.
- Client-assembly discovery is via an **explicit static-parameter path**, mirroring exactly how
  Q008/Q009 locate schema assemblies — not realistic MSBuild multi-project-reference discovery.
  `BACKLOG.md`'s own framing names this as a real secondary falsifier ("whether client assemblies are
  even discoverable from the schema project's design-time context in a realistic multi-project
  layout") and flags it as the more likely source of a REVISE than the coordination semantics. This
  quartet deliberately does **not** attempt to test realistic discovery — say so plainly rather than
  quietly assuming a path parameter generalizes to real project layouts, and record realistic
  discovery as an explicit open follow-up, not a settled question.
- Must test the actual well-foundedness claim as a real scenario, not merely "does the check fire
  once": a schema edit that a client's *stale* recorded dependency conflicts with must block, citing
  the client assembly and field; the same client, *recompiled* against the new schema, must clear the
  block on re-check — both directions required for a PASS, exactly the same discipline Q009 applied
  to its own irrelevant-vs-relevant field distinction.
- Must test field *removal* specifically (a field the client depends on no longer exists at all in the
  new schema), not only re-versioning on a fixed field set — this is Q009's own named gap and this
  quartet is explicitly the vehicle for closing it, so it must produce a diagnostic that says
  "removed," not a version-mismatch diagnostic that happens to also fire for the wrong reason.
- Every timing number single-sample, same convention as every prior quartet in this repo.

## Cheapest falsifier

Before building the two-provider mutual-reflection chain: does a **client**-side generative
provider's `AddCustomAttribute` call, made on a member the client itself provides, survive into
independently-reflectable IL the same way Q009 proved for the schema side? One provider, one provided
type, one property stamped with `ConsumesFieldAttribute(fieldName, version)`, built once, read back
via plain `PropertyInfo.GetCustomAttributesData()` from a program with zero SDK involvement —
identically to Q009's own Round 1, just with the roles swapped. If this fails, the whole reversed-arrow
idea dies here, cheaply, before any schema-side cross-reflection or live-edit machinery is built.

## Pre-registered decision thresholds

- **SHIP:** the falsifier passes; the well-foundedness claim holds in both directions (a schema edit
  that conflicts with a stale client's recorded dependency blocks generation with a diagnostic naming
  the client assembly, the field, and both versions; recompiling that client against the new schema
  clears the block on re-check, with no deadlock); field removal produces a diagnostic distinguishable
  from a version-mismatch diagnostic, naming the field as removed.
- **REVISE:** the mechanism and blocking direction both work, but the unwedging move doesn't cleanly
  clear the failure without extra ceremony beyond "recompile the client" (e.g. the schema-side
  checker's own caching prevents the fix from being recognized without a full process restart, or
  without explicitly invalidating something the design didn't anticipate) — real, useful, but with a
  practical ergonomic cost worth naming precisely rather than glossing over.
- **KILL:** the falsifier itself fails (client-emitted member attributes don't survive the same way
  schema-emitted ones did), or the well-foundedness test reveals a genuine deadlock — recompiling the
  client to match the new schema is itself blocked by the same check, or some other circular trap that
  makes the reversed-arrow idea unusable as a live, editable system rather than a one-shot check.
