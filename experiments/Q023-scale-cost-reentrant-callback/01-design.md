# Q023-scale-cost-reentrant-callback / Movement 2 — Design

**Status:** NOT YET EXECUTED.
**Location:** scratch console project, `dotnet new console -lang F#`, package
`FSharp.Compiler.Service` pinned to `43.9.101` (matches `Myriad/paket.lock`), same convention as
Q001–Q022. Not part of any committed repo; artifacts saved under `artifacts/` in this quartet folder
once built.

This design reuses Q010/Q021's reentrant `DocumentSource.Custom` harness shape, generalized from two
files to N+1 files, and adds per-N timing across three conditions instead of Q021's correctness-only
signal.

## Project shape

`N` independently-typed "prefix" files (no cross-file `open`, no shared types — see Movement 1's
validity preconditions for why), each with genuine typecheck weight:

```fsharp
module PrefixNNNN
type Record = { Id: int; Name: string; Tags: string list; Meta: Map<string,int> }
type Wrapper<'a> = { Value: 'a; Items: 'a list; Count: int }
let make (id: int) (name: string) (tags: string list) : Record =
    { Id = id; Name = name; Tags = tags; Meta = tags |> List.mapi (fun i t -> t, i) |> Map.ofList }
let rec fold (f: 'acc -> 'a -> 'acc) (acc: 'acc) (xs: 'a list) : 'acc =
    match xs with
    | [] -> acc
    | x :: rest -> fold f (f acc x) rest
let wrap (v: 'a) : Wrapper<'a> = { Value = v; Items = [v]; Count = 1 }
let combine (a: Wrapper<'a>) (b: Wrapper<'a>) : Wrapper<'a> =
    { Value = a.Value; Items = a.Items @ b.Items; Count = a.Count + b.Count }
let records = [ for i in 1..20 -> make i (sprintf "item%d-%s" i "NNNN") [ "a"; "b"; string i ] ]
let total = records |> fold (fun acc r -> acc + r.Meta.Count) 0
```

(`NNNN` substituted per file index — module names must be distinct.) Generic records, `Map`/`List`
pipelines, and an explicit recursive function give each file real inference work, unlike Q001 Round
3's near-free padding files.

One additional file, `Observer.fs`, synthesized **reentrantly** via `DocumentSource.Custom` exactly
like Q010/Q021's `Stratified.fs` — when the outer check requests `Observer.fs`'s text, the callback
itself calls `checker.ParseAndCheckFileInProject` on `Prefix0000` (the first file, arbitrarily) and
emits a small generated module reporting `Prefix0000.Record`'s alias-stripped type name. This keeps
the mechanism under test the actual one item 18 cares about (a live reentrant generator), not a bare
"does FCS cache" microbenchmark disconnected from Myriad's use case.

## Conditions, per N

For each `N` in `{10, 50, 150, 300}`:

1. **Cold:** build one fresh `FSharpChecker.Create(keepAssemblyContents = true, documentSource =
   <reentrant callback>, useTransparentCompiler = true)` and one `FSharpProjectOptions` covering all
   `N` prefix files plus `Observer.fs`. Run `checker.ParseAndCheckProject(opts)` once. Record
   elapsed ms. This is the "everything is unknown" baseline.
2. **No-op repeat × 3:** on the **same** checker/opts, call `ParseAndCheckProject` three more times
   with zero edits between calls (Q021 Round 5's condition, repeated for a stable read instead of
   Round 5's single sample). Record each elapsed ms.
3. **Edit-one × 3:** mutate `Prefix0000.fs`'s content in place (bump the record's field list by one
   entry — a real, compiler-visible change, not a no-op edit) and bump its file version each time,
   then call `ParseAndCheckProject`. Repeat three times (three distinct edits, each reverting-then-
   changing to avoid the harness accidentally re-serving a memoized prior edit). Record each elapsed
   ms, and separately record whether `Observer.fs`'s generated text changed (it shouldn't — Observer
   watches file index 0's *type* shape, `Record`, which the edit doesn't change; only the `records`
   *value* list length changes, not the type, so this also verifies the callback isn't
   over-invalidating on unrelated value-level edits).
4. **Diagnostics check:** confirm zero errors on every call, every N — a scale test that silently
   accumulates compile errors as N grows is not measuring what it claims to.

All four conditions run in one process per N (fresh process per N to avoid any single-process warm-up
carrying over between N values and confounding the cross-N comparison).

## Instrumentation

Wrap the reentrant callback to count invocations and log which file path was requested, so the
run log shows whether the callback fires for `Observer.fs` only or for every prefix file too (Q021
never established this at N>2). Print, per N: cold ms, repeat ms ×3, edit-one ms ×3, callback
invocation count per outer call, diagnostic count.

## Analysis

For each N, compute `repeatMedian(N) / cold(N)` and `editOneMedian(N) / cold(N)`. Plot (in prose —
no charting infra needed for four data points) how these ratios move as N grows 10→50→150→300:

- Ratios shrinking (or already small and staying flat) as N grows → **(c)**: real caching, cost is
  dominated by N only on the cold/first build, not on subsequent calls regardless of what changed.
- Ratios staying roughly constant and non-trivial (e.g. repeat cost within 50%+ of cold cost at every
  N) → **(b)**: no effective caching, every call pays close to the full N-file cost regardless of
  what changed.

## Reproduction

```
dotnet new console -lang F# -o q023-spike
cd q023-spike
dotnet add package FSharp.Compiler.Service --version 43.9.101
# Program.fs generates N prefix files programmatically, see 02-results.md for actual output
dotnet run -c Release
```

No MSBuild, no `Myriad.Sdk`, no real `.fsproj` beyond the throwaway host's own — same deliberate
scoping as every prior quartet in this lineage.

## Explicit instruction to whoever executes this (do not silently deviate)

- Do not edit this file or `00-hypothesis.md` once execution starts. If reality diverges (timings
  are too noisy at these N to be meaningful, `ParseAndCheckProject` behaves differently than
  described, N=300 is impractically slow to even cold-build), report the correction honestly in
  `02-results.md`.
- If the cheapest falsifier (N=10 comparison) already shows repeat ≈ cold, still run the full N
  sweep — a single data point cannot establish a trend, and Movement 1's SHIP/REVISE criteria are
  both trend-shaped, not single-N-shaped.
- Report raw per-call timings, not just medians — Q021's review specifically flagged that
  single-sample timing claims at toy scale are noise; do not repeat that mistake here now that the
  design calls for 3 samples per condition.
- If `Release` vs `Debug` build configuration measurably changes the result, say so — prior quartets
  in this lineage ran `Release` (see Q021's reproduction log); match that unless a stated reason
  requires otherwise.
