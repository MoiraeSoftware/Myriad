# Q030 — Results

## Headline

**H_conservative. FSAC/FCS invalidation is compilation-order-conservative, not dependency-precise.**
Editing an early file re-analyzes the **entire compilation-order suffix** (`{editedFile .. File{N-1}}`),
regardless of whether the later files actually reference the edited file. True-dependent status made no
difference: unrelated files that never mention the edited file re-analyzed exactly like the genuine
dependents, and a file with **zero** dependents still cascaded to all its order-successors.

This resolves the scope limitation `Q023`/`Q024`/`Q029` all inherited and disclosed, and it resolves it
on the **pessimistic** side for Myriad. `Q029`'s "attributed types sit early = expensive" caveat is
confirmed and generalizes: an early attributed type pays the full order-suffix re-check cost even if
almost nothing depends on it.

## Environment / reproduction

- `fsautocomplete` 0.83.0 (`artifacts/fsac-tool/.config/dotnet-tools.json`, same pin as `Q022`/`Q029`),
  .NET SDK 9.0.310, FSharp.Core resolved by the generated `net9.0` project. Windows 11.
- Harness: `artifacts/q030-fsac-precision.fsx` (LSP mechanism reused verbatim from `Q029`; only topology
  and the collect-the-full-set signal changed).
- Falsifier: `dotnet fsi q030-fsac-precision.fsx <dir> 30 2 7,16,25 2 1`
- Full run: `dotnet fsi q030-fsac-precision.fsx <dir> 30 2 7,16,25 2,25,27 3`
- Durable artifacts: `q030-N30.raw.log` (full timestamped LSP transcript, 573 messages),
  `q030-events-N30.csv` (per-file first-arrival offset + classification), `q030-summary-N30.csv`
  (per-edit set sizes), `q030-N30.log` (run log).

## Topology (verified in generated source)

N=30, Hub = `File0002`, dependents = `File0007`/`File0016`/`File0025` (scattered, interleaved with
unrelated files). Confirmed from disk: dependents' `total` line ends `+ File0002.total`; every other
file (including the two pre-Hub files `File0000`/`File0001`) ends `+ 0` — no cross-file reference. So
true-transitive-dependents-of-Hub = {7, 16, 25} (3 files), while compilation-order-successors-of-Hub =
{3..29} (27 files). The two predictions differ by 24 files, far above any noise floor.

## Data

Per-edit distinct re-analyzed set (`q030-summary-N30.csv`), stable across all 3 reps:

| edit position | role | numAnalyzed | dependents hit | unrelated hit | hub hit | = successors+1? |
|---|---|---|---|---|---|---|
| 2 | hub | **28** | 3/3 | **24** | true | 27+1 ✓ |
| 25 | dependent | 5 | 1/3 (itself) | 4 | false | 4+1 ✓ |
| 27 | unrelated | 3 | 0/3 | 3 | false | 2+1 ✓ |

Every edit re-analyzed exactly `{editedFile .. File0029}` — the compilation-order suffix — reproducing
`Q029`'s `successors+1` count in a topology where successors ≠ dependents. Three independent controls
all point the same way:

1. **Hub edit (idx 2):** 28 files, including all 24 unrelated files that never reference Hub. If FSAC
   were dependency-precise this would have been ~4 (Hub + {7,16,25}). It was not.
2. **Dependent edit (idx 25) — the sharpest control:** `File0025` is referenced by *nothing*, yet
   editing it still cascaded to {25,26,27,28,29}. A dependency-precise invalidator would re-analyze only
   `File0025`. Instead it re-analyzed the whole tail after it — pure compilation-order behavior, at a
   position unrelated to Hub.
3. **Unrelated edit (idx 27):** {27,28,29} — same rule.

## Raw-transcript control (the load-bearing evidence)

From `q030-N30.raw.log`, checked directly, not inferred from timing:

- **Compilation order:** the Hub-edit cascade's `documentAnalyzed` events arrive in strict compile
  order with monotonically rising offsets — `File0002` @20ms, `File0003` @97ms, … `File0029` @399ms
  (`q030-events-N30.csv`), ~6-13ms per successor, matching `Q029`'s ~6ms/successor. Unrelated files
  (3,4,5,6,8…) sit in this sequence at full cost, indistinguishable from the dependents (7,16,25).
- **Negative control — prefix stays silent:** `File0000` and `File0001` (the two files *before* Hub in
  compilation order) each emit `documentAnalyzed` **exactly once in the entire 573-message session** —
  their initial `didOpen` analysis — and never again across any of the 10 edit cycles. So the cascade is
  a genuine *suffix*, not a position-blind global refresh (which `Q029` had already ruled out for the
  linear chain; re-confirmed here for free). Edits never propagate to the compilation-order prefix.

Because `Q029`'s review established that `documentAnalyzed` tracks re-analysis 1:1 (successors+1 events,
in order, in a chain where the last file provably cannot be checked without the edited file's slot), the
*absence* of the event for the 24 unrelated files under a Hub edit... does not occur: they all fire.
The event set is the invalidation set, and it is the order-suffix.

## Deviations from design

None material. The collect-the-full-set signal worked as designed; the edited file's `documentAnalyzed`
double-fire (noted in `Q029`) inflates raw event counts but is deduplicated to the distinct-file set, so
the reported `numAnalyzed` is the true count of re-analyzed files. Result was identical across the
falsifier run and all 3 reps of the full run — no instability, no H_mixed ambiguity.

## What this means (scoped)

- **Answers the standing question:** FSAC/FCS re-checks by compilation order, not by precise dependency.
  For a Myriad project where an early attributed record has few real dependents but many unrelated files
  after it in build order, editing that record still pays the full order-suffix re-check cost. The
  softening a dependency-precise invalidator would have offered does not exist in FSAC 0.83.0 / this FCS.
- **Confirms and generalizes `Q029`'s pessimistic caveat** rather than relieving it. `Q029` showed a
  value-only edit re-checks the whole tail in the linear chain; `Q030` shows the "whole tail" is the
  compilation-order tail, not the true-dependent set — so the linear chain was not a special case, it
  was representative of the general rule.
- **Does not overturn any prior verdict.** `Q029`'s cost *curve* stands unchanged; `Q030` only fixes the
  identity of its x-axis: for a real DAG, the cost is driven by *number of order-successors*, not
  *number of true dependents*. `Q023`/`Q024`'s cost model, expressed in order-successors, is therefore
  the correct model for a real Myriad project, not an over-pessimistic proxy.

## Limits (for the reviewer)

- One FSAC/FCS version (0.83.0) and one project generator; N=30, small though genuinely-weighted files.
  This is a qualitative set-membership result (which regime), so small N is adequate — the regimes differ
  by 24 files — but the *absolute* per-successor latency is not the point here (that is `Q029`'s
  measurement) and is not re-litigated.
- Value-only edit only. A signature-changing edit to Hub could in principle behave differently (wider or
  narrower), and H_minimal (signature-based skip) was not observed for a value-only edit — consistent
  with `Q029`, FSAC invalidated on content. Whether a signature-preserving edit could ever skip
  dependents under some FCS caching mode is untested; the observed behavior is that it does not.
- Single machine/session per configuration; reproduced across falsifier + 3 reps, not across machines.
