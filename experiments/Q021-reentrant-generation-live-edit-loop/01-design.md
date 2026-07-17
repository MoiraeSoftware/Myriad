# Q021-reentrant-generation-live-edit-loop / Movement 2 — Design

**Status:** NOT YET EXECUTED.
**Location:** scratch console project, `dotnet new console -lang F#`, package
`FSharp.Compiler.Service` pinned to `43.9.101` (matches `Myriad/paket.lock`), same convention as
Q001-Q010. Not part of any committed repo; artifacts saved under `artifacts/` in this quartet folder
once built.

This design reuses Q010 Round 1's harness shape almost verbatim (same two-file setup, same reentrant
`DocumentSource.Custom` callback, same alias-stripped-type correctness signal) and changes exactly one
thing: instead of building a fresh `FSharpChecker` once and running one outer check, this harness builds
**one** `FSharpChecker`, reuses it across several sequential edit-then-recheck cycles, and asks whether
each cycle's generated text reflects that cycle's current prefix — the load pattern an LSP session
actually produces, which Q010 never tested.

**Explicit note on an uncertain API, stated up front per this repo's own convention (Q010's own
design did the same for its two named candidate mechanisms):** the exact FCS signal needed to make a
persistent checker re-observe changed source text is not verified from a prior run in this repo. The
working hypothesis, based on how `ParseAndCheckFileInProject`'s `fileVersion: int` parameter is
documented to behave, is that bumping the version integer passed for a changed file each cycle is the
primary signal, with `checker.InvalidateConfiguration(opts)` as a secondary mechanism to try if version
bumping alone proves insufficient. If either API's actual behavior or signature differs from this
description once run, `02-results.md` must say so plainly, not silently patch this file.

## Round 1 — cheapest falsifier: two cycles, persistent checker

Reuse Q010 Round 1's exact two-file setup (`A.fs` with `type Id = int; type Person = { Id: Id; Name:
string }`, `Stratified.fs` synthesized reentrantly) and its exact `synthesizeStratified` function
(alias-stripped vs. syntactic type name, printed side by side).

1. Build one `FSharpChecker.Create(keepAssemblyContents = true, documentSource = <reentrant callback>,
   useTransparentCompiler = true)` and one `FSharpProjectOptions` (via
   `GetProjectOptionsFromScript` + override `SourceFiles`, as Q010 did). Keep both bound to outer
   `let mutable` references the callback closes over, exactly as Q010's harness already does.
2. **Cycle 1:** `files.[fileA] <- "...type Id = int..."`. Run the outer check
   (`checker.ParseAndCheckProject(opts)`, Q010's own "forces callback" mode — use this mode
   throughout, not the placeholder-text mode, since it's the one Q010's own review trusted most).
   Record the generated `Stratified.fs` text and its alias-stripped type (`System.Int32` expected).
3. **Edit:** mutate `files.[fileA]` in place to `"...type Id = string..."`. Bump the file-version
   integer passed to the reentrant `ParseAndCheckFileInProject(fileA, <version>, ...)` call inside the
   callback, and bump the version used for the outer `ParseAndCheckProject`/`ParseAndCheckFileInProject`
   call on `Stratified.fs` too — same lockstep-bump discipline a real LSP server applies on `didChange`.
4. **Cycle 2:** re-run the outer check on the **same** `checker`/`opts` objects (no new
   `FSharpChecker.Create`). Record the generated text and its alias-stripped type again.
5. **Pass condition for this round:** cycle 2's alias-stripped type is `System.String`, not a repeat of
   cycle 1's `System.Int32` — i.e., the second check genuinely re-observed the edit rather than serving
   a memoized result from cycle 1.

If this fails outright (cycle 2 repeats cycle 1's value, or the run hangs/throws), stop, do not build
Round 2, and instead spend the remaining time isolating which piece is missing (version bump alone vs.
`InvalidateConfiguration` also required vs. neither sufficing at all under `TransparentCompiler`) —
report that investigation as this quartet's actual result rather than a bare failure.

## Round 2 — full liveness claim: four cycles, three distinct values, a return visit

Same persistent checker and options object as Round 1, continued (or restarted fresh if Round 1's
checker state needs a clean baseline — report which was actually done).

Four cycles, prefix type sequence `int -> string -> int64 -> int` (the fourth cycle deliberately
repeats the first's value, to rule out an off-by-one artifact where the harness is simply always one
cycle behind — a repeat-of-cycle-1 result on cycle 4 would look like a pass under a naive "changed from
last time" check but is actually still wrong if the underlying bug is a fixed one-step lag). Each cycle:
edit `files.[fileA]`, bump both version integers, run the outer check, record the alias-stripped type.

**Pass condition:** all four cycles' recorded alias-stripped types exactly match their cycle's own
prefix (`Int32, String, Int64, Int32`), zero diagnostics on `Stratified.fs` every cycle, no hang, no
exception.

## Round 3 — the omitted-invalidation control (proves the mechanism is load-bearing, not vacuous)

A fifth cycle, same persistent checker: edit `files.[fileA]` to a fourth distinct value
(`type Id = decimal`), but this time **do not** bump either version integer (leave both at the prior
cycle's value) and do not call `InvalidateConfiguration`. Record the result.

**Why this round matters:** if Rounds 1-2 pass, that alone doesn't prove the version-bump discipline is
*necessary* — `TransparentCompiler`'s snapshot model might simply always re-read current text regardless
of version numbers, in which case Rounds 1-2 would have passed for a different reason than the design
assumes. This round exists specifically to falsify that alternative explanation: if the omitted-bump
cycle *also* correctly reflects the new `decimal` value, the version-bump discipline in Rounds 1-2 was
never actually load-bearing under `TransparentCompiler`, and `02-results.md` must say so plainly rather
than credit the wrong mechanism for the pass. If the omitted-bump cycle instead serves the prior cycle's
stale `Int32` value (or errors), that confirms the discipline is real and necessary.

## Round 4 — stretch, optional: `BackgroundCompiler` comparison

Only if time remains: repeat Round 1's two-cycle test with `useTransparentCompiler = false`. Q001 found
`BackgroundCompiler` requires explicit `InvalidateConfiguration` to avoid serving stale cross-file
results in a *non-reentrant* setting; this round checks whether the same requirement holds for the
*reentrant* callback path specifically, which is new ground. Not part of this quartet's SHIP threshold
(only `TransparentCompiler` is, matching Q010's own SHIP configuration) — report as a side note only,
don't let its outcome affect the main verdict.

## Reproduction

```
dotnet new console -lang F# -o q021-spike
cd q021-spike
dotnet add package FSharp.Compiler.Service --version 43.9.101
# Program.fs per round, see 02-results.md for the actual output of each
dotnet run
```

No MSBuild, no `Myriad.Sdk`, no real `.fsproj` beyond the throwaway host's own — same deliberate scoping
as Q001 and Q010.

## Explicit instruction to whoever executes this (do not silently deviate)

- Do not edit this file or `00-hypothesis.md` once execution starts. If reality diverges (an API
  doesn't behave as described, `InvalidateConfiguration`'s real signature differs, version-bumping alone
  turns out sufficient or insufficient in a way this design didn't anticipate), report the correction
  honestly in `02-results.md`.
- If Round 1 fails, still attempt to isolate why (per Round 1's own instructions above) rather than
  stopping at a bare failure with nothing else recorded.
- Round 3's control is not optional if Rounds 1-2 pass — a pass without the control is a materially
  weaker result and the review should treat it as such if it's skipped.
