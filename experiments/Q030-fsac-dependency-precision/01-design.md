# Q030 — Design

## What gets built

A single re-runnable F# script, `artifacts/q030-fsac-precision.fsx`, that **reuses Q029's proven LSP
measurement mechanism verbatim** (framed `Content-Length` stdio reader, timestamped receive queue,
`documentAnalyzed`-leaf extraction, drain-until-quiet, `initialize` → `workspaceLoad` → `didOpen`-all)
and changes only two things: the project **topology** and the per-edit **signal**.

### Topology (the one real change)

`mkFileSrc i marker` produces the exact `Q023`/`Q029` genuinely-weighted file (generic `Record`,
generic `Wrapper<'a>`, a `Map`/`List.mapi` builder, an explicit recursive `fold`, a 20-record
comprehension carrying the value-only string marker). Only the tail term of `let total = (...) + <tail>`
varies by role:

- **Hub** (`i = hubIndex`, default 2): tail `+ 0`. Hub references nobody; it is the edited root and
  exports `total : int`.
- **Dependent** (`i ∈ {7,16,25}`): tail `+ File{hubIndex:04d}.total`. A genuine source reference to
  Hub's exported value — a true transitive dependent.
- **Unrelated** (all other `i`): tail `+ 0`. No reference to Hub or any other file. Self-contained.

`hubIndex = 2` (not 0) so that files `File0000`/`File0001` sit *before* Hub in compilation order and can
serve as a negative control: under both main hypotheses they must stay silent (they are not
order-successors of Hub), so if they ever fire it signals a position-blind global refresh — a failure
mode `Q029` already ruled out, re-checked here for free. Dependents are scattered (7, 16, 25) and
interleaved with unrelated files (3-6, 8-15, 17-24, 26-29), so H_precise (~4 files) and H_conservative
(~28 files) predict clearly separated event sets.

A generated `.fsproj` lists all N files as ordered `<Compile Include>`; `dotnet restore` runs once so
FSAC's cracker has `obj/project.assets.json`. No `dotnet build` — this measures FSAC's in-memory
incremental typecheck, not on-disk output.

### Signal (the other change)

`Q029` waited for the *last* file's `documentAnalyzed` and reported one settle time. That is wrong for a
wide/shallow shape: if invalidation is dependency-precise, the last (unrelated) file may **never**
re-analyze, so a wait-for-last-file would spuriously time out. Instead `editCollect` sends
`didChange` + `didSave` on the edited file, then **collects every `documentAnalyzed` event** received
after the edit until the cascade goes quiet (4s of no traffic, 60s hard cap), recording each file's
leaf name and first-arrival offset. The **set of distinct re-analyzed files** is the decisive output;
the edited file's double-fire (seen in `Q029`) is deduplicated.

Each edited file is classified (`hub`/`dependent`/`unrelated`) and every analyzed index is classified
against the pre-registered dependent set, so the CSV directly answers "how many of the true dependents
fired, how many unrelated files fired, did Hub fire."

## What is measured / derived quantities

Per Hub edit: `numAnalyzed` (distinct files), `numDependentsHit` / `totalDependents`,
`numUnrelatedHit`, `hubHit`. Mapped to hypotheses:

| Observation | Verdict |
|---|---|
| numAnalyzed ≈ 1 (Hub only), unrelated=0, deps=0 | H_minimal |
| numAnalyzed ≈ 4, deps hit = 3/3, unrelated ≈ 0 | H_precise |
| numAnalyzed ≈ 28, unrelated ≈ 24 (whole suffix) | H_conservative |
| anything else / unstable across reps | H_mixed / NULL |

The raw transcript (`q030-N30.raw.log`, always written) lets a reviewer recount the exact
`documentAnalyzed` set by hand, as `Q029`'s reviewer did.

## Controls

- **Warm-up Hub edit, discarded** (first-typecheck JIT/FCS tax; `Q027`/`Q028`/`FINDINGS.md`).
- **Pre-Hub files (0,1) must stay silent** — negative control against global refresh.
- **Control edits**: besides editing Hub, edit one late *unrelated* file (index 27) and one *dependent*
  file (index 25). Editing an unrelated file with no dependents should re-analyze a small set (itself
  plus its own order-successors under H_conservative, or just itself under H_precise); editing a
  dependent (which nothing references) should re-analyze roughly just itself. These sharpen the
  interpretation of the Hub result by showing the same harness produces *different* sets for
  different edit positions — i.e. the harness is not simply always reporting "all" or "one."

## Run plan / reproduction

Package versions pinned: `fsautocomplete` 0.83.0 (`artifacts/fsac-tool/.config/dotnet-tools.json`,
copied from Q029), .NET SDK 9.0.310, FSharp.Core as resolved by the generated project.

```
# from artifacts/
dotnet tool restore --tool-manifest fsac-tool/.config/dotnet-tools.json   # once

# cheapest falsifier: N=30, hub=2, deps=7,16,25, edit Hub only, 1 rep
dotnet fsi q030-fsac-precision.fsx <dir> 30 2 7,16,25 2 1

# full run: edit Hub (2), a dependent (25), an unrelated late file (27); 3 reps each
dotnet fsi q030-fsac-precision.fsx <dir> 30 2 7,16,25 2,25,27 3
```

`<dir>` is created fresh each run, so runs are independent.

## What a reviewer should press on

- **Does an unrelated file's `documentAnalyzed` failing to fire really mean it wasn't re-checked**, or
  could FSAC re-check it silently without emitting the event? `Q029`'s review established that the
  cascade is strictly position-gated in the linear chain (successors+1 events, in order) — i.e. the
  event *is* emitted whenever a file is re-analyzed. If that holds, absence of the event is genuine
  absence of re-analysis. The reviewer should confirm from `Q029`'s transcript that the event tracks
  re-analysis 1:1, and here confirm the dependents that *do* fire arrive in compilation order.
- **Is the dependency real?** The reviewer should confirm from generated source that dependents contain
  `File0002.total` and unrelated files contain no cross-file reference.
- **Signature-vs-content confound** (H_minimal vs the rest): the edit is value-only, so if only Hub
  fires that is signature-based skipping, not dependency precision. The design distinguishes these by
  counting the dependents specifically.
- **N is small (30)**: inherent to a real FSAC session. The claim is only about *which regime* FSAC is
  in (a qualitative set-membership question), not an absolute latency, so small N is sufficient — the
  two hypotheses differ by ~24 files, far above any noise floor.
