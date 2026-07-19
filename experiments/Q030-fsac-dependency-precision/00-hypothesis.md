# Q030 — Hypothesis

## Question

When a live `fsautocomplete` (FSAC) session receives a value-only edit to an early file, does FSAC/FCS
re-analyze **only that file's true transitive dependents**, or **the whole compilation-order suffix**
(every file after it, whether or not any of them reference it)?

`Q023`, `Q024`, and `Q029` all measured edit cost against a **linear dependency chain** (`File{i}`
references `File{i-1}`'s value), which makes "compilation-order successors" and "true dependents"
identical by construction. All three quartets' reviews flagged the same unresolved consequence:
their cost curve cannot tell whether FSAC invalidates by *build order* or by *precise dependency*.
`Q029`'s review named this exact test as the single highest-priority next step.

This is the first quartet in the lineage to build a topology where the two regimes make **divergent,
cheaply-distinguishable predictions**.

## Why this matters for Myriad (both outcomes are load-bearing, neither is "the success")

Myriad's attributed domain types tend to sit early in build order. `Q029` (SHIP) established that a
value-only edit to an early file re-checks its entire tail in the linear chain, and its review read
this pessimistically for Myriad ("attributed types sit early = expensive"). But that reading only bites
if FSAC re-checks the whole *order suffix*. If instead FSAC is **dependency-precise**, an early
attributed record with few actual dependents (even though many unrelated files follow it in build
order) would cost far less than the linear-chain worst case implies, softening `Q029`'s caveat. If FSAC
is **compilation-order-conservative**, the pessimistic reading is confirmed and generalizes.

## Pre-registered outcomes (all valid; no result is privileged)

- **H_precise (good news for Myriad):** Editing Hub re-analyzes approximately `{Hub} ∪ {true
  dependents}` only. In the design below (N=30, Hub at index 2, 3 scattered dependents), that is ~4
  files. The ~24 unrelated post-Hub files stay silent.
- **H_conservative (confirms the pessimistic reading):** Editing Hub re-analyzes approximately `{Hub}
  ∪ {whole compilation-order suffix}` — ~28 files — regardless of which reference Hub. Reproduces
  `Q029`'s linear-chain result in a shape where it could have diverged, and confirms FSAC invalidates
  by build order, not dependency.
- **H_minimal (even better for Myriad, considered unlikely):** Editing Hub re-analyzes only Hub itself
  (signature-based skip: dependents' inputs are unchanged because Hub's exported signature is
  byte-identical). `Q029` makes this unlikely — its value-only edit still cascaded the whole tail, so
  FSAC demonstrably invalidates on content, not signature — but it is a distinct, pre-registered
  possibility and would be the strongest positive for Myriad.
- **H_mixed / NULL:** Something in between or noisy (e.g. some but not all unrelated files fire, or the
  set is unstable across reps). A clean NULL that the topology cannot resolve is a valid, reportable
  outcome.

The decisive discriminator is the **set of files whose `documentAnalyzed` fires** after a Hub edit
(≈4 → H_precise; ≈28 → H_conservative; ≈1 → H_minimal), read from the raw LSP transcript, not a single
aggregate time.

## Novelty

Not covered by any existing generator in `src/Myriad.Plugins/`. Not covered by a prior quartet: every
edit-cost quartet in the lineage (`Q010`, `Q021`, `Q023`, `Q024`, `Q029`) used a linear chain or a
2-file toy and therefore *could not* separate order-successors from true dependents — each of their
reviews says so explicitly. This is the first wide/shallow-DAG topology, purpose-built so the two
invalidation regimes predict different observable event sets.

## Contradiction check

No contradiction with a prior verdict. This directly extends `Q029`'s SHIP by resolving a scope
limitation `Q029`'s own review disclosed and could not answer. It does not touch any type-provider
finding (Q006 etc.) — this is Thread-1-shaped (FSAC's real incremental path), not a type provider.
Whatever the outcome, it refines rather than overturns `Q029`: `Q029`'s cost *curve* stands; this only
tells us whether the x-axis for a real Myriad DAG is "order-successors" or "true dependents."

## Validity preconditions

1. **Real FSAC over real LSP**, not `FSharpChecker`-as-a-library — the same literal `fsautocomplete`
   0.83.0 process `Q022`/`Q029` used (pinned in `artifacts/fsac-tool/.config/dotnet-tools.json`), so
   the answer is about the tool F# developers actually run.
2. **The dependency graph must be genuinely wide/shallow**, i.e. the true-dependent set must be a
   strict, small subset of the compilation-order suffix, with unrelated files *interleaved* among the
   dependents (not clustered), so H_precise and H_conservative predict clearly different event sets.
   The unrelated files must genuinely not reference Hub (verifiable in generated source).
3. **Genuinely typecheck-weighted files** (generic records, `Map`/`List` pipelines, recursion — the
   exact `Q023`/`Q029` `mkFileSrc` shape), not `let x = 5` padding, so a re-analysis is real work and
   `BACKLOG.md` item 4's named mistake doesn't invalidate the result.
4. **Value-only edit** to Hub (a string literal inside the records comprehension; Hub's exported
   `total : int` signature byte-identical before/after) — the identical edit shape `Q023`/`Q024`/`Q029`
   used, so this is comparable and isolates content-vs-order/dependency invalidation from
   signature-change effects.
5. **The signal must be the actual set of re-analyzed files**, captured from the raw LSP
   `documentAnalyzed` transcript and counted programmatically, not inferred from aggregate timing —
   `Q029`'s review showed the transcript is where a claim like this is really settled.
6. **First-typecheck JIT/FCS-init tax controlled** (`Q027`/`Q028`): a discarded warm-up edit runs
   before any measured edit, and every measured edit happens after `workspaceLoad` + `didOpen`-all.

## Cheapest falsifier

One Hub edit, N=30, Hub at index 2, dependents at {7,16,25}. Count distinct `documentAnalyzed` events.
If it is ~4 (Hub + the three dependents), H_precise is live and H_conservative is falsified on the
spot. If it is ~28 (Hub + the whole suffix), H_conservative is live and H_precise is falsified. If ~1,
H_minimal. This single edit already separates all three hypotheses; the reps and the unrelated/
dependent-edit controls only harden it.
