# Q008 — Reconstruction and re-verification (2026-07-16)

**Status: the Q008-vs-Q012 contradiction is RESOLVED, not merely disputed. Q008's original claimed
result reproduces exactly, and the specific axis Q013's own review named as the most likely untested
difference is confirmed as the explanation.** This does not reopen Q008's frozen `00`–`03` movement
files (left untouched below, as originally written) — it fills the missing-artifacts gap `FINDINGS.md`
named as this repo's own credibility problem, using recovered original source rather than a fresh
reconstruction from prose.

## What happened

While setting up scratch space for an unrelated quartet (Q016) in this session, this job's own
temp directory (`$CLAUDE_JOB_DIR/tmp/`) was found to still contain `tp-provenance-spike/` — a
complete, unbuilt-but-intact set of F# projects whose provider names (`SchemaTP`, `ClientTP`), whose
custom attribute (`SchemaVersionAttribute`), and whose harness structure (`AttrCheck` for Round 1,
`Harness` for Rounds 2–3) match Q008's own `00-hypothesis.md`/`01-design.md`/`02-results.md`
description exactly. This is, to the best judgment available, Q008's actual original scratch source,
left in a temp directory and never copied into `experiments/Q008-provenance-closed-loop/artifacts/`
before the quartet was closed — precisely the gap `FINDINGS.md`'s credibility section describes.

The user was asked whether to spend session time investigating this find before continuing the
originally assigned work (Q016); the answer was to run it first. What follows is that run, done
straight, with no edits to the recovered provider/harness logic.

## Step 1 — rebuild and rerun unmodified

All six projects (`SchemaTP.Runtime`/`.DesignTime`, `ClientTP.Runtime`/`.DesignTime`, `SchemaAsm`,
`AttrCheck`, `Harness`) built clean with `dotnet build -c Release`, no source changes. `AttrCheck`
(Round 1 — independent reflection over `SchemaAsm.dll`, confirming a generative provider really did
bake a real custom attribute into IL) passed exactly as claimed:

```
type 'SchemaAsm.Schemas+SchemaV2'
  attribute: SchemaTP.Runtime.SchemaVersionAttribute
  ctor arg [0] (Version) = "v2"
ROUND 1 (independent reflection) verdict: PASS
```

`Harness` (Rounds 2–3 — the actual `ParseAndCheckFileInProject`-based two-provider gate this
quartet's SHIP verdict rests on) also passed exactly as claimed, run twice for determinism:

```
-- direction 1: schema=v2, client expects v2 (MATCH) --        0 diagnostics
-- direction 2: schema=v2, client expects v3 (MISMATCH) --     4 diagnostics, naming both versions
ROUND 2 verdict: PASS

-- step 1: cold check, expects v2 (MATCH) --                   22-26ms, 0 diagnostics
-- step 2: LIVE EDIT v2->v3, SAME checker (MISMATCH) --        18-19ms, real diagnostic
-- step 3: LIVE EDIT back v3->v2, SAME checker (CLEARED) --    20ms, 0 diagnostics
ROUND 3 verdict: PASS
```

The live-recheck timings (18-26ms) land inside Q008's own claimed 19-32ms band. This is a clean,
deterministic reproduction of Q008's central claim: a generative provider resolves via
`ParseAndCheckFileInProject`, and a two-provider provenance mismatch is caught live, at keystroke-time
cost, with a diagnostic naming both versions — the exact thing Q012 found could not be true of *any*
generative provider in this pinned environment.

## Step 2 — isolating why Q012/Q013 and this reproduction disagree

Q008's `Harness/Program.fs` checks via a **real, hand-built `FSharpProjectOptions`** (`mkOptions`,
`--noframework` plus explicit ref-pack DLL and `-r:` compiler args, `SourceFiles = [| consumerFile |]`)
— a genuine non-script "project," not `checker.GetProjectOptionsFromScript`. Every prior Q012/Q013 run
of `ParseAndCheckFileInProject` against a generative provider used a `.fsx` script via
`GetProjectOptionsFromScript` instead — the one axis both of those quartets' own reviews flagged as
never having been tried, since neither prior quartet's own harness happened to use it.

A minimal isolation program (`artifacts/Isolate/Program.fs`) was written to settle this directly: same
checker instance, same real `ClientTP`/`SchemaTP` provider pair recovered above, byte-identical consumer
body text, checked two ways in the same process:

```
===== ROUTE A: ParseAndCheckFileInProject over a .fsx SCRIPT (Q012/Q013's route) =====
  Error: couldn't find type 'ClientTP.Provided.C' in assembly 'tmpXXXXXX'   <- Q012's exact diagnostic
  ROUTE A resolved: false

===== ROUTE B: ParseAndCheckFileInProject over a REAL FSharpProjectOptions (Q008's route) =====
  0 diagnostics
  ROUTE B resolved: true

CONFIRMED: script-vs-real-project-options is the axis.
```

Reproduced twice, deterministic both times. This is a clean, controlled, same-checker,
same-provider, same-consumer-text comparison — the only variable is how `FSharpProjectOptions` was
constructed. **The axis is confirmed:** `ParseAndCheckFileInProject` resolves a generative provided
type when given a real, hand-built, non-script `FSharpProjectOptions`, and fails to resolve the
identical type when given script-derived options from `GetProjectOptionsFromScript` — for the same
provider, same checker, same process.

## What this means for the standing dispute

- **Q012's finding stands, precisely as scoped.** Every generative shape Q012 tested (three toy probes
  plus both of Q011's real providers) failed under `ParseAndCheckFileInProject` — but every one of
  those was checked via `GetProjectOptionsFromScript`. Q012 never claimed to have tested the
  real-project-options route; its own write-up is honest that this was untested. Q012's NULL result on
  "does PC resolve generative types" was correct for the script configuration it actually ran, not a
  claim about every possible `FSharpProjectOptions` shape.
- **Q013's NULL result (compile-then-PC warming) also stands, precisely as scoped.** It closed the
  same-checker/same-scenario/toy-shape script reconciliation, which was a different, narrower question
  than the one resolved here.
- **Q008's SHIP verdict is no longer disputed. It is reproduced,** on recovered original source, with
  the specific mechanistic reason for its earlier apparent contradiction with Q012 identified and
  confirmed by direct isolation, not guessed at. Q008's own `02-results.md` prose ("keystroke-time,
  19-32ms, live two-provider gate") was accurate; the missing piece was never the claim, only the
  saved evidence that would have let anyone check *how* the checking API was being driven.
- **The standing lesson generalizes past Q008/Q09 specifically:** whether `ParseAndCheckFileInProject`
  resolves a generative provided type depends on how `FSharpProjectOptions` is constructed, not only on
  generative-vs-erased (Q012's axis, still real and still correct on its own terms) — a real,
  previously unrecorded FCS behavior worth carrying forward into any future quartet in this lineage
  that uses the incremental checking API on a generative provider. A `.fsx` script's implicitly
  constructed project options and a hand-built `FSharpProjectOptions` are not interchangeable for this
  purpose, even when driving the identical checker against the identical provider.

## What remains open

- **Q009 was also recovered** (`tp-field-provenance-spike/`, structurally identical to Q008's harness —
  same `mkOptions` real-project-options construction) and rebuilt/rerun unmodified: all four Round 2
  scenarios and both Round 3 live-edit sequences (3-field and 12-field) passed exactly as originally
  claimed, timings inside the claimed band. See
  `experiments/Q009-field-level-provenance/RECONSTRUCTION.md` and
  `experiments/Q009-field-level-provenance/artifacts/run-logs/`.
- **Why Q011's real providers (structurally near-identical to Q008/Q09's) failed under
  `ParseAndCheckFileInProject` remains a real, separate, still-open question** — Q011's own harness used
  `checker.Compile` directly, never attempting the real-project-options PC route this reconstruction
  shows works for Q008/Q09's shape. Whether Q011's specific provider (two static parameters vs Q008's
  one, file-path-based cross-assembly reads) would also resolve via a real-project-options PC call is
  untested and worth a cheap follow-up, now that the mechanism to try is known.
- **Not re-verified here:** Q012's own toy probes (`ProbeSimple`/`ProbeTwoParams`/`ProbeTwoParamsIO`)
  under the real-project-options route — plausible they would also now resolve, given the axis found
  here, but not run. A quick confirming pass would be cheap and would fully close the loop on Q012's own
  "generative vs erased" framing versus this "script vs real-project-options" framing (both may be true
  simultaneously and non-contradictory; this was not tested to confirm).
- **`FINDINGS.md`, `README.md`'s Index, and `BACKLOG.md`** have been updated in the same session to
  reflect this resolution — see those files' current text rather than treating this note as the sole
  record.

## Reproduction

`artifacts/` now holds the recovered source (`SchemaTP.Runtime`/`.DesignTime`, `ClientTP.Runtime`/
`.DesignTime`, `SchemaAsm`, `AttrCheck`, `Harness`, the new `Isolate` isolation program, and the shared
vendored `ProvidedTypes.fs`/`.fsi`), plus `run-logs/` holding the exact output quoted above. Build order:
`SchemaTP.Runtime` (pulls in `SchemaTP.DesignTime`), `ClientTP.Runtime` (pulls in `ClientTP.DesignTime`),
`SchemaAsm`, `AttrCheck`, `Harness`, `Isolate` — each `dotnet build <proj>.fsproj -c Release`, then run
each built `.dll` directly with `dotnet <path>.dll`.
