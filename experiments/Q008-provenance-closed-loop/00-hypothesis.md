# Q008-provenance-closed-loop / Movement 1 — Hypothesis

**Status:** PLANNED. Pre-registered only — not yet built. Awaiting go-ahead.
**Date:** 2026-07-15

**Question:** A generative type provider emits real IL, and `AddCustomAttribute` accepts arbitrary
`CustomAttributeData` (`ProvidedTypes.fs:950-951` for the shared attribute-list helper backing it,
`:1013`/`:1019` for `ProvidedTypeDefinition`) — already recorded in `BACKLOG.md` as a way to stamp
metadata for *other .NET frameworks* to read. Nobody has tested the other half: can a **second**
type provider, resolving a **different** provided type, read that first provider's emitted
attributes back via ordinary reflection and use them to *decide whether to generate at all* —
refusing to produce members, with a real compiler diagnostic naming the conflict, when two
providers' declared provenance disagrees (a schema provider says v3, a client provider was told to
expect v2)? Does compile-time supply-chain verification work end to end, through the type system,
with no external tooling — and does a live edit that introduces a provenance conflict get caught
the same way Q006 proved a source edit gets picked up: no rebuild, just a re-check?

## Why this is the right next spike

This is the strongest candidate to survive two independent adversarial brainstorming passes (an
Opus-model dig and a Fable-model dig, both run against this same SDK) that were explicitly asked to
find something with more teeth than "underused API surface." Fable's own self-critique of the idea
(recorded in `BACKLOG.md`) named the exact reason to build this one first: attribute injection alone
is already found and is "decoration," not a capability — the *consuming* half, one provider enforcing
another's metadata, is what would make it genuinely new, and neither this session nor the two
brainstorming passes tested it. It's also the one idea of the five surfaced in that round that reuses
only mechanisms already proven working in this lineage (Q006's generative-provider harness, the
already-recorded custom-attribute finding) rather than needing new infrastructure (Z3/SMT bindings
for the proof-carrying-types idea, a model checker for the reachability idea, a property-testing
harness for the adversarial-types idea) — the cheapest of the five ambitious ideas to actually spike.

## Novelty gate

Checked `ProvidedTypes.fs` directly: `AddCustomAttribute`/`GetCustomAttributesData` exist per-member
and a full `ILCustomAttrs`/`ILAttribute` IL model backs real metadata emission (`:2777` onward, used
throughout the IL type definitions for methods/types/fields). Confirmed these compose in principle;
**not** confirmed empirically that a custom attribute survives from a generative
`ProvidedTypeDefinition` all the way into an independently loadable, independently reflectable
compiled assembly — no prior quartet tested this, Q006 only ever read *structural* shape
(`FSharpType.GetRecordFields`), never attribute metadata, and used exactly one provider, never two
providers reading each other. Not a re-tread of Q006, Q007, or either brainstorming pass — it is the
specific gap both passes left named and open.

## Contradiction gate

Does not contradict Q006. It deliberately lives *inside* Q006's already-established wall (a
generative provider only resolves types already compiled in a separately referenced assembly)
rather than fighting it — provenance-checking across a schema-provider/client-provider boundary is
inherently a cross-package scenario in real use (a published schema package, a separately-compiled
client), so Q006's boundary isn't a limitation here, it's the natural shape of the problem this idea
targets. Independent of Q007 (no FSI involved); does not depend on Q007's outcome either way.

## Validity preconditions

- `FSharp.Compiler.Service` pinned to `43.9.101`; `ProvidedTypes.fs`/`.fsi` vendored from the same
  `FSharp.TypeProviders.SDK` commit Q006 used (`0a95768a2247daba80b24a2604f77f89fc88ff1f`), for
  direct comparability of timing numbers against Q006's baseline (1137ms cold / 47ms live re-check).
- Must be **two genuinely separate providers** (`SchemaTP` emitting provenance, `ClientTP` reading
  and enforcing it) — one provider reading its own attributes back would only prove round-trip
  fidelity, not the cross-provider enforcement claim under test.
- Must test **both directions** as a real negative control: matching provenance passes and generates
  real members; mismatched provenance fails with a specific, citable compiler diagnostic naming both
  versions — not a generic/swallowed exception. Reuse Q006's established discipline exactly: gate
  every PASS claim on the diagnostic list, never on whether a symbol merely appears to resolve,
  since Q006's Round 3 documented FCS's error-recovery inferring a plausible type for a `let`-bound
  name even when the underlying provider member failed to resolve.
- The mismatch-detection claim is only meaningfully tested live: a provenance conflict introduced by
  editing a static argument in the consumer source, re-checked with the *same* `FSharpChecker`
  instance and no rebuild, mirroring Q006's Round 1 methodology exactly. A result confined to
  "works after a fresh `dotnet build`" would not test the interesting half of the claim.
- Must measure re-check cost with the cross-provider reflection-and-comparison step in the loop,
  against Q006's baseline — this step does genuinely more work per resolution than Q006's
  `LensesByName` (loading a second assembly, reading its custom attributes, comparing), and that
  cost has not been measured anywhere in this lineage yet.

## Cheapest falsifier

Before building the two-provider chain: does a custom attribute attached to a **generative**
`ProvidedTypeDefinition` survive into the compiled assembly's real IL metadata at all, such that an
ordinary, independent consumer program (not the SDK's own in-memory objects) can
`Assembly.LoadFrom` the built DLL and read the attribute back via plain
`System.Reflection.CustomAttributeData`/`GetCustomAttributesData()`? One provider, one provided type,
one attribute carrying a string payload, built once, read back from a wholly separate process. If
attributes on generative types don't survive into independently-reflectable IL, the entire idea is
dead before any second provider gets written — this is the cheapest possible kill switch and must
run first.

## Pre-registered decision thresholds

- **SHIP:** the falsifier passes; the two-provider gate correctly passes on matching provenance and
  fails with a specific, real, citable diagnostic on mismatch (both directions verified); the
  mismatch is caught via a live source edit with no rebuild, at a re-check cost within the same
  order of magnitude as Q006's 47ms baseline (not 10x-100x worse).
- **REVISE:** the mechanism works correctly end to end, but the live-edit re-check cost is
  materially worse than Q006's baseline — meaning this would need to ship as an opt-in or
  explicitly-cached check rather than something that "just works" the way Q006's result did.
- **NULL:** the cross-provider check works, but the specific case tested could be achieved almost as
  easily by a much simpler existing practice (e.g., a plain version-number convention checked by an
  ordinary build-time script or test, no type-level machinery required) — meaning the "compile-time,
  through-the-type-system" framing doesn't clearly earn its complexity over what teams already do.
- **KILL:** the cheapest falsifier itself fails — generative custom attributes don't survive into
  independently-reflectable compiled IL. Would be a significant, general finding: it bounds not just
  this idea but the entire "attribute injection as a metadata bridge" finding already recorded in
  `BACKLOG.md`, which assumed (but never tested) that this round-trip works.
