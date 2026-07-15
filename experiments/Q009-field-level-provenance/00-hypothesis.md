# Q009-field-level-provenance / Movement 1 — Hypothesis

**Status:** PLANNED. Pre-registered only — not yet built. Awaiting go-ahead.
**Date:** 2026-07-15

**Question:** Q008 proved provenance enforcement at whole-type granularity — one version string
stamped on a generative provided type, one client-side expectation, all-or-nothing: any mismatch
anywhere fails the client, even if the client never touches the field that changed. Can provenance
instead be tracked **per field**, stamped on individual provided members rather than the type as a
whole, with a client that declares a dependency on only a *subset* of fields — passing unaffected
when a field it doesn't depend on changes provenance, and failing with a diagnostic naming the
*specific* field when one it does depend on changes? This is the precision Q008's own review named
as untested (`Q008-provenance-closed-loop/03-review.md`, follow-up 1) and it is a genuine behavioral
difference from Q008, not just "more attributes" — Q008's design was deliberately coarse (one tag,
checked once); this tests whether the same mechanism holds up when the enforcement has to be
selective rather than blanket.

## Why this is the right next spike

Directly named as a follow-up in Q008's closed review, not a fresh brainstorm. Reuses every
mechanism Q008 already proved (generative providers, cross-provider `Assembly.LoadFrom` +
reflection, the same-checker live-edit methodology, diagnostics-only gating discipline) rather than
requiring new infrastructure — the cheapest of Q008's three named follow-ups to spike, same reason
Q008 itself was chosen over the reachability/proof-carrying/adversarial-types ideas in the prior
round. It also has real practical motivation independent of novelty for its own sake: whole-type
version tagging (Q008's mechanism, unchanged) forces *every* client of a schema to bump its expected
version whenever *any* field changes, even fields that client never touches — a well-known real
pain point with coarse-grained API versioning. Field-level provenance is the direct answer to that,
if it works.

## Novelty gate

Checked `Q008-provenance-closed-loop/02-results.md` directly: `SchemaTP` stamped exactly one
`SchemaVersionAttribute` on the whole provided type (`AddCustomAttribute` called once, on the
`ProvidedTypeDefinition`); `ClientTP` checked exactly one version string against it. Neither
`AddCustomAttribute` on an individual `ProvidedProperty`/`ProvidedMethod` (member-level, not
type-level) nor a client that selectively checks a declared subset of fields was tested. Whether
member-level custom attributes survive into independently-reflectable IL the same way type-level
ones did is a genuinely open, unverified question — the SDK's IL-emission path for member-level
attributes is not necessarily the same code path as type-level attribute emission, and Q008
explicitly only exercised the latter. Not a re-tread.

## Contradiction gate

Does not contradict Q008 — extends it at finer grain using the same proven primitives. If member-
level attributes turn out *not* to survive the same way (a live possibility, not assumed away here),
that would be a new, useful negative finding scoped specifically to member-level emission, not a
contradiction of Q008's type-level result, which stands on its own regardless.

## Validity preconditions

- `FSharp.Compiler.Service` pinned `43.9.101`; `ProvidedTypes.fs`/`.fsi` from the same SDK commit
  Q006/Q008 used, for comparable timing numbers.
- The **sharpest competing alternative must be named up front, not discovered during review**:
  could the same selective-enforcement outcome be achieved just as easily by splitting one schema
  into N separately-versioned whole types (reusing Q008's exact mechanism, unchanged, one type per
  field or field-group) instead of building member-level attribute machinery inside one type? This
  is the real NULL-threshold competitor. Field-level granularity inside a single provided type only
  earns its complexity if it preserves the ergonomics of "one record type with N properties" that N
  separate type-provider instantiations would lose for any schema with more than a handful of
  fields — record that ergonomic argument explicitly in the results, don't just assert the mechanism
  works and call it a day.
- Must test the actual precision claim as a real scenario, not merely "can an attribute go on a
  property": a schema change to a field the client does **not** depend on must leave the client
  passing with zero diagnostics; a schema change to a field the client **does** depend on must fail
  with a diagnostic naming that specific field — both directions, both required for a PASS.
- Must extend Q008's live-edit-via-same-checker methodology to both cases above (irrelevant-field
  edit → stays clean; relevant-field edit → new failure), since "an edit to an unrelated part of the
  schema doesn't needlessly break this client" live, in the editor, without a rebuild, is the actual
  motivating value of field-level granularity over Q008's blanket mechanism — asserting it statically
  once would understate the claim the same way a single build-only check would have understated
  Q008's live-edit claim.

## Cheapest falsifier

Before building the selective-enforcement chain: does a custom attribute stamped on a single
`ProvidedProperty` via `AddCustomAttribute` (member-level) survive into independently-reflectable
compiled IL, the same way Q008 proved for a type-level attribute? One provider, one provided type,
one property carrying a `FieldProvenanceAttribute(fieldName, version)`, built once, read back via
plain `System.Reflection.PropertyInfo.GetCustomAttributesData()` from a program with zero SDK
involvement — mirroring Q008's own Round 1 exactly, but at member granularity instead of type
granularity. If this fails, the whole approach dies here, cheaply, before any two-provider chain or
selective-dependency logic gets built.

## Pre-registered decision thresholds

- **SHIP:** the falsifier passes; the precision claim holds in both directions (irrelevant field
  change → client stays clean; relevant field change → client fails with a diagnostic naming that
  specific field), both verified live via the same-checker re-check methodology; live-edit re-check
  cost stays in the same order of magnitude as Q008's baseline (19-32ms) as field count grows to a
  realistic size (double digits, not just the two-or-three-field toy case).
- **REVISE:** the mechanism works correctly but re-check cost scales badly with field count (e.g.
  because every recheck re-reflects every declared dependency's attribute individually with no
  batching) — real, narrower than pitched, and would feed directly into Q008's still-open follow-up
  2 (stress-test against a realistic schema size).
- **NULL:** field-level granularity works, but the ergonomic argument in the validity preconditions
  doesn't hold up in practice — e.g. declaring per-field dependencies turns out to be just as
  verbose as instantiating N separate whole-type providers would have been, meaning true field-level
  granularity inside one type doesn't clearly earn its complexity over reusing Q008's mechanism
  unchanged, just applied more finely.
- **KILL:** the cheapest falsifier itself fails — member-level custom attributes don't survive into
  independently-reflectable IL at all. Would sharpen Q008's own finding: attribute survival would be
  known to be specific to type-level emission, not a general property of generative providers.
