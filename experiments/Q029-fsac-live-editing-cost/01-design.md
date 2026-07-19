# Q029 — Design

## What gets built

A single re-runnable F# script, `artifacts/q029-fsac-cost.fsx`, that:

1. **Generates a real project on disk** under a given directory:
   - `N` files `File0000.fs .. File{N-1}.fs`, each genuinely typecheck-weighted (generic record,
     generic `Wrapper<'a>`, a `make` builder using a `Map`/`List.mapi` pipeline, an explicitly
     recursive `fold`, a comprehension building 20 records) — the exact weight shape from Q023's
     `mkPrefixSrc`.
   - A real **compilation-order dependency chain**: `File{i}` binds `let total = <local sum> +
     File{i-1}.total` for `i > 0` (`+ 0` for `File0000`). So every later file transitively depends on
     every earlier file's `total`.
   - A **value-only edit knob**: each file's record comprehension includes a string literal `"vK"`
     (initially `"v0"`). Bumping `K` changes file *content* but changes **no** type or signature —
     exactly Q023/Q024's value-only edit shape. The chain still typechecks unchanged.
   - An `.fsproj` (`OutputType=Library`, `net9.0`) listing all `N` files as `<Compile Include>` in
     order — this fixes the compilation order FSAC/FCS see.
   - `dotnet restore` is run once so `obj/project.assets.json` exists (FSAC's project cracker needs
     it). A full `dotnet build` is **not** required and is not run — this measures FSAC's in-memory
     incremental typecheck, not on-disk build output.

2. **Drives a real `fsautocomplete` 0.83.0 process** over hand-rolled LSP stdio (reusing Q022's
   `lsp-client-mtimeonly.fsx` plumbing verbatim: framed `Content-Length` reader, background reader
   thread, `sendRequest`/`sendNotification`, `initialize` -> `initialized` -> `fsharp/workspaceLoad`
   with the `.fsproj` path). The reader thread **timestamps every received message at receipt** so
   inter-message latency is measurable.

3. **Opens all `N` files** via `textDocument/didOpen` and waits for the workspace + initial analysis
   to settle (drain-until-quiet, as Q022 did).

4. **For each edit position `p` in a supplied list, for each repeat `r`:**
   - Rewrite `File{p}.fs` on disk with a bumped marker (`"v{r}"`), then send `textDocument/didChange`
     (full-document content change) followed by `textDocument/didSave` for that file's URI — the
     sequence a real editor sends on save. (Q022's log shows the downstream cascade fires on
     `didSave`.)
   - **Primary signal — downstream re-analysis settle time:** measure wall-clock from the moment the
     `didChange` is sent until FSAC emits `fsharp/documentAnalyzed` for the **last** file
     (`File{N-1}.fs`, matched by URI). FSAC cascades `documentAnalyzed` to all open dependent files
     after an edit (confirmed in Q022's `lsp-client-mtimeonly.scratch-ungated.live.log`, lines
     55-63), so the last file's re-analysis completing is the point at which the whole edited-tail has
     been re-typechecked. This is the cost of re-checking successors `p+1 .. N-1`.
   - **Secondary / freshness cross-check — pull latency:** immediately after, send a
     `textDocument/hover` on the `total` identifier's definition line in the **last** file and record
     request->response latency and whether it resolves. A resolving hover confirms the last file was
     actually re-checked (not serving a stale/aborted result), guarding against an H2 false reading
     where `documentAnalyzed` fired without a real check.

5. **Warm-up discard:** the very first edit cycle after project load (one edit at a mid position) is
   run and its timing **discarded**, never reported, per `FINDINGS.md`'s first-typecheck-tax caveat.

6. **Emits a durable CSV** `artifacts/q029-results-N{N}.csv` with columns
   `N,position,successors,rep,settleMs,hoverMs,hoverResolved`, plus a full `.log` transcript of the
   LSP session, so the run is re-runnable and reviewable.

## What is measured / the derived quantity

For each position `p`, `successors = N - 1 - p`. H1 predicts `settleMs` rises monotonically (roughly
linearly) with `successors`; editing the last file (`successors = 0`) should be cheapest, the first
file (`successors = N-1`) most expensive. H0 predicts `settleMs` is flat across `p`. The headline
artifact is the median `settleMs` per position and whether it discriminates first-vs-last (the
falsifier) and, if so, the shape across the full sweep.

## Run plan / reproduction

Package versions pinned: `fsautocomplete` 0.83.0 (via `artifacts/fsac-tool/.config/dotnet-tools.json`,
copied from Q022), .NET SDK 9.0.310, FSharp.Core as resolved by the generated project (9.0.x).

```
# from artifacts/
dotnet tool restore --tool-manifest fsac-tool/.config/dotnet-tools.json   # once

# cheapest falsifier: N=20, only first and last positions, 2 reps
dotnet fsi q029-fsac-cost.fsx <scratchDir> 20 0,19 2

# full sweep if falsifier passes: N=20 across 5 positions, 3 reps
dotnet fsi q029-fsac-cost.fsx <scratchDir> 20 0,5,10,14,19 3

# mini scale check at a second N
dotnet fsi q029-fsac-cost.fsx <scratchDir2> 40 0,10,20,30,39 3
```

`<scratchDir>` is created fresh (project generated into it) each run, so runs are independent.

## What a reviewer should press on

- **Stale-cache false negative for H0:** if `settleMs` is flat, is it because FSAC genuinely doesn't
  re-check the tail, or because the `documentAnalyzed` for the last file is being emitted from a cache
  without a real check? The hover freshness cross-check partially guards this, but a reviewer should
  check the raw log for whether `fileParsed`/`documentAnalyzed` for downstream files actually fire
  after each edit, and whether the hover latency tracks `settleMs`.
- **Debounce / reload-storm:** if edits are sent faster than FSAC settles, an edit's cost could bleed
  into the next. The harness drains to quiet between cycles; a reviewer should confirm from the log
  that each cycle's `documentAnalyzed` cascade completed before the next `didChange`.
- **Does `documentAnalyzed` for the last file really imply the whole tail was checked?** FSAC may
  analyze open documents independently. The dependency chain (every file references the previous)
  makes the last file's check genuinely require the whole prefix's inferred types, so a real check of
  the last file cannot skip the edited file's slot. A reviewer should confirm the chain is real in the
  generated source.
- **N is small (≤40) vs Q023/Q024's N≤300:** this is inherent — a real FSAC session per edit is far
  more expensive than an in-memory `ParseAndCheckProject`, so fewer positions/smaller N is the honest
  ceiling. The claim is only ever about whether the *position dependence* exists and its sign, not a
  slope value comparable to Q024's fits.
