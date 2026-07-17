# Q022 — Design

## Environment, pinned for reproduction

- `dotnet --version`: 9.0.310 (also present on this machine: 5.0.416, 10.0.302 — all builds below
  pin `-c Debug`/`net9.0` and invoke `dotnet` from `PATH`, which resolves to 9.0.310's `dotnet.exe`
  unless a `global.json` says otherwise; none is present under `experiments/`).
- `fsautocomplete`: **0.83.0**, installed as a local dotnet tool under
  `artifacts/fsac-tool/.config/dotnet-tools.json` (`dotnet tool install fsautocomplete`, no version
  pinned anywhere else in this repo — recorded here per the hypothesis's own validity precondition).
- Fantomas.FCS `7.0.5`, matching `paket.lock` (used indirectly via the real `Myriad.Core`/
  `Myriad.Plugins`/`Myriad` binaries built from this repo's own `src/`, not rebuilt for this quartet).

## Round 0 (already run while writing 00-hypothesis.md, recorded here for reproducibility)

Confirmed empirically, not assumed, which CLI invocation sets `$(DesignTimeBuild)` the way a real IDE
project-system client does. Scratch project `artifacts/dtb-probe/probe.fsproj` with:

```xml
<Target Name="EchoDTB" BeforeTargets="CoreCompile">
  <Message Text="##DTB_PROBE## DesignTimeBuild=[$(DesignTimeBuild)] BuildingProject=[$(BuildingProject)] SkipCompilerExecution=[$(SkipCompilerExecution)]" Importance="high" />
</Target>
```

| Invocation | DesignTimeBuild | BuildingProject | SkipCompilerExecution |
|---|---|---|---|
| `dotnet build probe.fsproj -c Debug` | (empty) | true | (empty) |
| `dotnet build -p:DesignTimeBuild=true probe.fsproj -c Debug` | true | true | (empty) — **compiler still ran** |
| `dotnet msbuild -p:DesignTimeBuild=true -p:SkipCompilerExecution=true -p:ProvideCommandLineArgs=true -p:BuildingProject=false -t:Compile probe.fsproj` | true | false | true |

Only the third form is a genuine DTB: `BuildingProject=false` and `SkipCompilerExecution=true` together
mean the compiler itself never runs — this is the standard MSBuild design-time-build contract real
project-system clients (`Ionide.ProjInfo`, the .NET project system used by VS/Rider) use, and it is the
invocation used for every DTB run below. The middle row is a documented trap worth naming explicitly:
setting `DesignTimeBuild=true` alone, the naive thing to try, does **not** produce a real DTB — the
compiler still runs — so a probe that only checked the property's value without also checking
`SkipCompilerExecution` would have silently validated the wrong thing.

## Scratch fixture

Two scratch console projects under `artifacts/`, sharing the same shape as
`test/Myriad.IntegrationPluginTests` (real `Fields`/`Lenses` generators from `src/Myriad.Plugins`, not
a synthetic strawman):

- `artifacts/scratch-gated/` — imports the real, **unmodified**
  `src/Myriad.Sdk/build/Myriad.Sdk.targets`.
- `artifacts/scratch-ungated/` — imports a **local copy**,
  `artifacts/Myriad.Sdk.NoGate.targets`, identical to the real file except
  `MyriadSdkGenerateCode`'s `Condition=" '$(DesignTimeBuild)' != 'true' "` is removed and
  `MyriadSdkIncludeCodegenOutputDuringDesignTimeBuild` is dropped (its only job was being the empty
  DTB-time no-op the real target now replaces). The real, shared
  `src/Myriad.Sdk/build/Myriad.Sdk.targets` is never edited — per the hypothesis's own validity
  precondition, this stays a scoped copy under `artifacts/`.

Each project:
- `Person.fs` — `[<Generator.Fields "fields"; Generator.Lenses "lens">] type Person = { Name: string }`
  (the attributed source, editable between rounds).
- `Person.Generated.fs` — Myriad's real output, `<MyriadFile>Person.fs</MyriadFile>`.
- `Consumer.fs` — references `TestLens.PersonLenses.Name`, the one generated member that exists before
  any edit.
- A canary: `MyriadSdkGenerateCode`'s `<Exec>` is preceded by a `<Touch Files="$(IntermediateOutputPath)generation.canary" AlwaysCreate="true" />` in the ungated copy only, so a real regeneration attempt is
  independently observable (mtime) without relying on parsing `<Exec>` output.

Both projects build with `dotnet build` first (real build, not DTB) to establish a clean starting
state with `TestLens.PersonLenses.Name` present in `Person.Generated.fs` and no `Age` field anywhere.

## Round 1 — MSBuild/DTB mechanism only, no IDE

For each of `scratch-gated` and `scratch-ungated`:

1. Edit `Person.fs`, adding a field `Age: int` to the `Person` record (on disk, simulating an IDE-buffer
   save).
2. Run the confirmed real-DTB invocation:
   `dotnet msbuild -p:DesignTimeBuild=true -p:SkipCompilerExecution=true -p:ProvideCommandLineArgs=true -p:BuildingProject=false -t:Compile <proj>`.
3. Record: canary file mtime (did `MyriadSdkGenerateCode` attempt to run at all), whether
   `Person.Generated.fs`'s content now contains `Age` (did generation actually complete and get
   written), and whether the DTB invocation's own exit code is 0 (does adding a real `<Exec>` to the
   DTB path break DTB itself, e.g. by exceeding a timeout or violating an assumption a real project
   system makes about DTB being cheap/side-effect-free).
4. Confirm no `.dll` was produced/updated in `bin/` (proof the compiler genuinely never ran — DTB, not
   a real build, did the work).

**Pre-registered pass/fail for Round 1:**
- `scratch-gated`: canary untouched, `Person.Generated.fs` unchanged (still no `Age`), DTB exits 0.
  This is the baseline proving the gate is real (Q022's own cheapest falsifier, already the
  pre-registered premise of the whole quartet).
- `scratch-ungated`: canary touched (fresh mtime), `Person.Generated.fs` now contains `Age`-derived
  members, DTB still exits 0, no `bin/*.dll` change. This is the core mechanism claim.
- KILL condition: if `scratch-ungated`'s DTB invocation fails outright (non-zero exit, or MSBuild
  refuses to run an `<Exec>` task under `BuildingProject=false`), the mechanism is dead regardless of
  Round 2, and Round 2 is not attempted.

## Round 2 — real FSAC/Ionide LSP session

The standing cross-cutting gap this repo has never closed: every prior "works live in the IDE" claim
used `FSharpChecker` as a library, never a real editor-facing process. `fsautocomplete` 0.83.0 is a
plain LSP server over stdio, so it can be driven directly without an actual editor UI, using a minimal
hand-written LSP client (`artifacts/lsp-client.fsx`, `dotnet fsi`) that speaks `Content-Length`-framed
JSON-RPC.

Two sub-questions, tested in sequence, because the first was judged likely to give a materially
different (weaker) answer than a naive read of item 8 implies, and this design commits to reporting
whichever one actually happens rather than only running the version that flatters the hypothesis:

**2a — cold load.** Edit `Person.fs` to add `Age` (on disk, before FSAC ever starts). Start FSAC fresh
against `scratch-ungated`, let it load the workspace (this itself triggers FSAC's own internal DTB via
`Ionide.ProjInfo`), open `Consumer.fs`, and request `textDocument/hover` (or `completion`) at a
reference to `TestLens.PersonLenses.Age`. Does it resolve, with **no `dotnet build` ever invoked**?
Repeat identically against `scratch-gated` as the negative control (expected: does not resolve, `Age`
absent, matching the documented status quo).

**2b — live edit, no restart.** With FSAC already running and the workspace already loaded once
(against `scratch-ungated`, starting from the Round 2a end-state, i.e. `Age` already present), edit
`Person.fs` again to add a second field `Email: string`, send the file change to FSAC the way a real
editor does (`textDocument/didChange` + `textDocument/didSave`, matching Ionide's own notification
sequence on a file save), then re-query `textDocument/hover` on a reference to a not-yet-written
`TestLens.PersonLenses.Email`. Does FSAC re-trigger its own DTB *without being restarted or explicitly
told to reload the project* and pick up the freshly-regenerated member? This is the sub-question 00-
hypothesis.md flagged as genuinely unknown rather than assumed: FSAC's project-reload trigger (via
`Ionide.ProjInfo`'s file watcher) is documented to fire on changes to `.fsproj`/`obj/project.assets.json`
-shaped files, not on an ordinary `.fs` source save, which would mean this half of the claim fails even
if 2a succeeds. If 2b fails, the honest scoping is "helps at project load/reload time, not during live
editing" — a materially weaker result than "closes the IDE-invisibility gap" and one that should be
reported as its own finding, not folded into a pass.

**Pre-registered pass/fail for Round 2:**
- SHIP-worthy only if 2a passes on `scratch-ungated`, fails correctly on `scratch-gated` (proving the
  comparison is fair, not just "FSAC always shows everything"), **and** 2b passes.
- If 2a passes but 2b fails: REVISE-shaped result, reported precisely as "closes the gap at
  project-(re)load time only, not during live editing without an explicit reload trigger" — still a
  real, useful, previously-unproven finding, just narrower than item 8's framing implies.
- If 2a fails: the DTB-level mechanism (Round 1) does not translate into FSAC actually surfacing the
  result even at load time, for a reason to be investigated (timing/ordering between FSAC's own DTB
  invocation and its subsequent file read is the leading suspect) and reported honestly rather than
  silently re-run until it passes.

## What a review should press on

- Round 1's canary proves the `<Exec>` was *attempted*, not that MSBuild's `Inputs`/`Outputs`
  up-to-date check (`DEVNOTES.md`'s rebuild cache) doesn't skip it right back out on a *second*
  consecutive DTB call with no source change — worth checking, since a real editor issues DTB
  repeatedly, and if the cache correctly no-ops on unchanged input, that is good (no wasted work) but
  needs to be distinguished from the cache incorrectly no-oping on an actually-changed input.
- Round 2's timing is measured on a single, mostly-idle developer machine, single sample — consistent
  with this repo's own standing caveat that no quartet has run repeated trials, but worth restating
  here specifically since DTB latency is the one thing that would make this fix impractical even if it
  works.
- The scratch fixture is one record, one field addition, two generators (`Fields`/`Lenses`) — same
  single-shape-tested caveat every quartet in this repo carries; not tested against
  `test/Myriad.IntegrationPluginTests`'s fuller generator surface (`DUCasesGenerator`, inline
  generation, nested/recursive dispatch).
- Whether FSAC's own behavior here is representative of Ionide-in-VS-Code specifically (Ionide is a
  thin client over the same FSAC process, so this should generalize, but that inference itself hasn't
  been checked against Ionide's own source).
- If Round 2b fails, double-check it isn't a *false* negative caused by this design's own script not
  correctly replicating Ionide's real notification sequence (e.g. missing a `workspace/didChangeWatchedFiles`
  notification a real client sends that this hand-rolled script doesn't) before concluding FSAC itself
  doesn't re-trigger DTB on source saves.
