# Q009-field-level-provenance / Movement 4 — Adversarial review

## Three strongest objections

1. **The NULL-threshold comparison the design demanded was done honestly, but its conclusion rests
   partly on ergonomics this quartet's own generated code didn't actually exercise.** `02-results.md`
   argues the single-type approach beats N separately-versioned whole types because it "preserves
   `one record type with N properties`" — real data-access ergonomics. But neither this quartet nor
   Q008 ever generated real per-field *data* accessors gated by provenance; both generate
   verification-only marker members (`CheckedFieldCount`, `ProvenanceOk`, Q008's equivalent). The
   N-types alternative's actual, demonstrated weakness is narrower than "shatters your data model" —
   it's "gives the client four disjoint marker types with nothing representing its dependency view as
   one thing," which is real but a smaller claim than the record-ergonomics framing implies. The
   stronger version of this argument — that N-types would make it structurally impossible to
   construct a cohesive value once real per-field accessors exist — is a reasonable inference from
   Q006's lens-generation work, not something Q009 itself measured. Doesn't change the verdict (the
   measured engineering properties below are sufficient on their own), but the ergonomic case should
   be cited as *plausible and consistent with Q006*, not as something this quartet's harness proved.

2. **Flat scaling is real and well-explained, but tested across a 3-to-12-field range, not near
   anything resembling a production-sized schema.** The mechanism explanation (`02-results.md`:
   reflect the schema's properties once into a map, per-instantiation, then do cheap string
   compares) is sound and the reasoning that FCS's own incremental-check cost dominates at this scale
   is plausible given the numbers — both schema widths land inside noise of each other (15–57ms).
   But a real OpenAPI or DB schema can run to hundreds of fields, an order of magnitude past what was
   tested, and the results file's own caveat says this plainly rather than overselling it. The
   REVISE threshold (materially worse scaling) wasn't hit here, but "flat from 3 to 12" is a thinner
   basis for "flat, period" than the write-up's confidence might suggest to a future reader skimming
   only the summary table.

3. **A recurring defect from Q008 reappears here unchanged and still isn't fixed: the provider
   error is reported twice, identical text, same location.** This was already named as a Q008
   follow-up and shows up again in Q009's Round 2/3 diagnostics verbatim. Reporting it a second time
   without addressing it isn't wrong — it's honest replication — but two quartets now carry the same
   known defect forward unpatched. Worth treating as one fix across the whole Q008/Q009 lineage
   rather than a per-quartet footnote that keeps getting re-discovered.

## Verdict

**SHIP.** Every pre-registered threshold was met cleanly, and this quartet answered a question Q008
explicitly left open (does attribute survival generalize from type-level to member-level emission) —
it does, via the identical `defineCustomAttrs`/`assemblyReplacementMap` path, confirmed by source
reading before building and then empirically. The precision claim — the actual point of doing this
at field grain instead of Q008's whole-type grain — was demonstrated in both directions, live, with
diagnostics that name the exact affected field(s), including the multi-field case naming all of
them in one message rather than just the first. Re-check cost stayed inside Q008's own established
band across a 4x growth in schema width, with the honest caveat above about how far that result
should be trusted to extrapolate.

Scope precisely, because three things travel with the ship:

- **The competing "just use N whole types" alternative was measured, not dismissed, and both work.**
  This quartet's real advantage over it is design-time cost (one `Assembly.LoadFrom`/instantiation
  vs N) and a single dependency declaration as data rather than N separate type instantiations —
  both genuinely measured. The stronger ergonomic claim (grouping matters because real per-field
  data access needs it) is consistent with Q006's separate finding but wasn't independently
  re-proven here, per objection 1. Don't cite this quartet alone as having settled that question.
- **Flat scaling is proven for 3-to-12 fields, not asserted for arbitrary width.** Treat "scales
  flat" as bounded by what was tested, same discipline this lineage has applied to every prior
  timing claim since Q001's own scaling-round caveat.
- **Untested against a literal editor session, same secondary-not-central caveat as Q008.** This
  quartet's core claim is about diagnostic content and timing, which `FSharpChecker` owns directly;
  FSAC's own caching behavior across recheck cycles remains unexercised, same residual risk Q008
  carried, not increased or decreased by this quartet.

**Follow-ups, if the frontier keeps moving (recorded in `BACKLOG.md`'s general type-provider
section):**

1. Stress-test at real production schema width (50–200+ fields) to actually test the flat-scaling
   claim past the 3-to-12 range this quartet covered, not just assume it extrapolates.
2. Test field *removal*, not just version bumps on a fixed field set — a client depending on a field
   that no longer exists in a newer schema is a distinct, untested failure mode from "field exists
   but version disagrees," and real schema evolution includes both.
3. Fix the duplicate-diagnostic reporting once, across both Q008 and Q009's provider pattern, rather
   than letting it recur a third time in some future quartet in this lineage.
4. The decisive test of the ergonomic argument in objection 1: combine this quartet's field-level
   provenance gating with Q006's real per-field accessor generation (actual lens-shaped data access,
   not verification-only markers) and see whether the N-separate-types alternative becomes
   structurally untenable, not just less convenient, once real data composition is at stake.
