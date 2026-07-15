# Q009 — Reconstruction and re-verification (2026-07-16)

Companion to `experiments/Q008-provenance-closed-loop/RECONSTRUCTION.md` — read that file first for
the full account of how this source was found (this job's own scratch temp directory,
`$CLAUDE_JOB_DIR/tmp/tp-field-provenance-spike/`, structurally identical to Q008's recovered
`tp-provenance-spike/`) and the isolation experiment (script-vs-real-`FSharpProjectOptions`) that
explains the standing Q008/Q09-vs-Q012 contradiction. This file records Q009's own specific
reconstruction only.

## What was rebuilt and rerun

All projects (`SchemaTP.Runtime`/`.DesignTime`, `ClientTP.Runtime`/`.DesignTime`, `SchemaAsm`,
`AttrCheck`, `Harness`) built clean with `dotnet build -c Release`, no source changes. The recovered
`SchemaTP.Provider.fs` confirms the field-level design exactly as `02-results.md` describes: one
generative type, one `ProvidedProperty` per field, each individually stamped with its own
`FieldProvenanceAttribute(fieldName, version)`. The recovered `Harness/Program.fs` uses the same
real, hand-built `FSharpProjectOptions` construction (`mkOptions`) as Q008's harness — not a `.fsx`
script — consistent with the axis Q008's reconstruction isolated.

## Results (see `artifacts/run-logs/` for full output)

`AttrCheck` (Round 1 — independent member-level reflection):
```
type 'SchemaAsm.Schemas+Tagged'
  property 'Name'
    attribute: SchemaTP.Runtime.FieldProvenanceAttribute
    ctor arg [0] (FieldName) = "Name"
    ctor arg [1] (Version)   = "v1"
ROUND 1 (independent member-level reflection) verdict: PASS
```

`Harness` (Rounds 2-3 — selective enforcement + live-edit precision, scaled 3-field vs 12-field):

- **Round 2, all four scenarios passed exactly as claimed:** baseline match (0 diagnostics), an
  irrelevant field bump (Email v3->v4, client doesn't depend on it — 0 diagnostics), a relevant field
  bump (Age v2->v3 — 4 diagnostics naming `Age` specifically), and a multi-field bump (Name and Age
  both changed — one diagnostic naming *both* mismatched fields in one message, not just the first).
- **Round 3, both schema widths passed:** the 3-field/2-dependency sequence and the 12-field/
  4-dependency sequence both ran cold-match -> live-irrelevant(clean) -> live-relevant(fail, naming the
  field) -> live-cleared, all four steps correct in both widths.
- **Timings:** cold match 23ms (both widths); live re-checks 15-56ms across both widths and all three
  live-edit steps — inside Q009's own originally claimed 15-57ms band, and flat between the 3-field and
  12-field schema as originally claimed (no visible cost growth with field count).

This is a clean, deterministic reproduction of Q009's central claims: member-level custom attributes
survive into independently-reflectable IL exactly as Q008's type-level ones did, selective
(subset-of-fields) enforcement works correctly in both directions including the multi-mismatch case,
and re-check cost stays flat from 3 to 12 fields.

## Net effect

Combined with Q008's reconstruction, both of this repo's previously-undefendable Thread 2 SHIP verdicts
(`FINDINGS.md`'s "actively disputed" framing) are now reproduced on recovered original source, with the
Q012/Q013 contradiction's mechanistic cause identified (script- vs real-project-options-constructed
`FSharpProjectOptions` given to `ParseAndCheckFileInProject`) and confirmed by direct same-checker,
same-provider isolation in Q008's reconstruction. See `FINDINGS.md` for the updated synthesis.
