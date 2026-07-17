# Q022 — Results

## Summary

**Round 1 (MSBuild/DTB mechanism): fully confirmed as designed.** Removing
`MyriadSdkGenerateCode`'s `Condition="'$(DesignTimeBuild)' != 'true'"` gate makes Myriad's real CLI
run during a genuine design-time build (`SkipCompilerExecution=true`, `BuildingProject=false`,
confirmed via a real F# compiler that never re-executes), correctly regenerating a changed
attributed type's output on disk, with the pre-existing rebuild cache still correctly no-op'ing a
repeat DTB call with no source change. DTB itself never fails from carrying an `<Exec>`.

**Round 2 (real FSAC/Ionide LSP session, not `FSharpChecker`-as-library): a genuinely mixed result,
landing exactly on the design's own pre-registered REVISE wording — corrected below after independent
review, see "Amendment" at the end of this section.** A fresh FSAC 0.83.0 process, driven over real LSP
(`textDocument/hover`), does show a newly-added field's generated member with **zero `dotnet build`
ever invoked** — the core claim in item 8's own framing, genuinely demonstrated end to end for the
first time in this repo's history against a literal editor-facing process, not a library harness.
Within **one already-running FSAC session**, an ordinary source save, or an LSP notification that the
source file changed, never causes Myriad's codegen to run again — but an actual change to the
`.fsproj` itself (even a bare mtime touch, no content change) followed by a reissued
`fsharp/workspaceLoad` **does** re-trigger it, live, with no process restart. The practical capability
gap is real (a developer editing `Person.fs` alone never touches the `.fsproj`, so an ordinary source
save still never refreshes generated code) but it is a project-reload gap, not a "restart required"
gap.

## Round 1 — detail

Environment: `dotnet` 9.0.310 on `PATH` (also present: 5.0.416, 10.0.302), MSBuild 17.14.37+3393a8ad9,
Fantomas.FCS 7.0.5 (via this repo's own built `Myriad.Core`/`Myriad.Plugins`/`Myriad` binaries, `-c
Debug`).

**Round 0 (cheapest falsifier, confirming the actual DTB-setting invocation before anything else was
built):**

| Invocation | `DesignTimeBuild` | `BuildingProject` | `SkipCompilerExecution` |
|---|---|---|---|
| `dotnet build probe.fsproj -c Debug` | (empty) | true | (empty) |
| `dotnet build -p:DesignTimeBuild=true probe.fsproj -c Debug` | true | true | (empty) — compiler **still ran** |
| `dotnet msbuild -p:DesignTimeBuild=true -p:SkipCompilerExecution=true -p:ProvideCommandLineArgs=true -p:BuildingProject=false -t:Compile probe.fsproj` | true | false | true |

Only the third form is a real DTB. The middle row is a real trap: setting `DesignTimeBuild=true` alone
does not produce a real DTB.

**Round 1 proper**, on `artifacts/scratch-gated` (real, unmodified `Myriad.Sdk.targets`) and
`artifacts/scratch-ungated` (local copy, `artifacts/Myriad.Sdk.NoGate.targets`, gate removed, real
`src/Myriad.Sdk/build/Myriad.Sdk.targets` never touched). Both established from a clean real build
(`dotnet build`, `Person = { Name: string }` only), then `Person.fs` edited to add `Age: int`, then the
confirmed real-DTB invocation run against each:

| Project | Canary before | Canary after DTB | `Person.Generated.fs` after DTB | DTB exit code | `bin/*.dll` mtime |
|---|---|---|---|---|---|
| `scratch-gated` | (untouched from real build) | **unchanged** | still `Name` only, no `Age` | 0 | unchanged |
| `scratch-ungated` | `...173` (from real build) | **`...224`** (touched) | `Name` **and** `Age` lenses present | 0 | **unchanged** — proves the compiler never ran, only DTB did |

A **repeat** DTB call on `scratch-ungated` with no further source change left the canary at the same
mtime (`...224` → `...224`), confirming `MyriadSdkGenerateCode`'s existing `Inputs`/`Outputs`
up-to-date check (`DEVNOTES.md`'s rebuild cache) still works correctly once the `Condition` gate is
gone — this is not "regenerate on every DTB tick," it correctly no-ops on unchanged input.

This closes Round 1 exactly as pre-registered: the gate is real (baseline confirmed), removing it makes
Myriad's own codegen genuinely run during DTB, the existing cache still governs whether it actually
does any work, and DTB itself tolerates the added `<Exec>` without failing or invoking the real
compiler.

## Round 2 — detail (real `fsautocomplete` 0.83.0, driven over LSP by a hand-rolled client,
`artifacts/lsp-client.fsx`)

FSAC does **not** auto-discover projects from `rootUri`/`initialized` alone — a hover request that
early fails with `"Couldn't find ... in LoadedProjects"`. FSAC's own custom LSP extension,
`fsharp/workspaceLoad`, must be called explicitly with the target `.fsproj` path — this is what a real
Ionide client does after its own `fsharp/workspacePeek` discovery step. Recorded because it is the kind
of protocol detail 00-hypothesis.md's own validity precondition said not to assume.

A first version of the hover check had a real bug worth naming: positioning the cursor one character
into `PersonLenses.Name`/`PersonLenses.Age` landed on the **module** name, so hover trivially
"resolved" for both fields regardless of whether the member itself existed — a false-positive that
would have invalidated the whole round if not caught. Fixed by positioning inside the member name after
the dot; re-verified the fix makes the negative control (`scratch-gated`, `Age`) correctly return
`null` before trusting any further result.

**Round 2a — cold load (fresh FSAC process):**

| Project | `Age` field on disk? | FSAC hover on `Name` | FSAC hover on `Age` |
|---|---|---|---|
| `scratch-gated` (negative control) | yes, but generated file stale (gate blocks DTB regen) | resolved, full type | **null** |
| `scratch-ungated` | yes, and DTB regenerates it live | resolved, full type | **resolved, full type**, e.g. `val Age: Input.Person -> int * Input.Person -> int -> Input.Person` |

No `dotnet build` was ever run in this session. The comparison discriminates correctly: the negative
control fails exactly where expected, the positive case resolves with real, correct type information.
**This is the first time in this repo's history that a "works live in the IDE" claim has been tested
against a literal LSP-driven editor process rather than `FSharpChecker`-as-a-library, and it passed.**

**Round 2b — live edit, same FSAC process, no restart.** Starting from `scratch-ungated`'s already-
loaded state (`Age` present), `Person.fs` edited on disk to add `Email: string`, sent to FSAC as a real
editor would (`textDocument/didOpen` for the not-yet-open `Person.fs`, then `didChange` with the new
full text, then `didSave`), `Consumer.fs` correspondingly edited and `didChange`d to reference
`PersonLenses.Email`, generous settle time given, then hover on `Email`:

**Result: `null` — unresolved.** The on-disk `Person.Generated.fs` was confirmed unchanged (still only
`Name`/`Age`, no `Email`) and the canary file's mtime had **not** advanced since the initial
`workspaceLoad` — Myriad's codegen simply never ran again. This is not a stale-checker-cache symptom
(FCS serving old content); it is FSAC never re-invoking the build system at all.

**Round 2c — ruling out a false negative in this design's own script** (per `01-design.md`'s own "what
a review should press on"): sent an explicit `workspace/didChangeWatchedFiles` notification for
`Person.fs` (`FileChangeType.Changed`), the mechanism a real LSP client uses to report on-disk changes
outside the current edit session. **Still `null`, canary still unmoved.**

**Round 2d — does FSAC's own explicit, user-invoked "reload projects" command help?** Re-issued
`fsharp/workspaceLoad` a second time, mid-session, the identical call Ionide's own manual reload command
makes. It returned `{"Status":"finished"}` — reporting success — but **the canary still did not move,
`Email` still resolved to `null`.** FSAC/`Ionide.ProjInfo` appears to cache the project's cracked state
for the lifetime of the process and short-circuits re-invoking MSBuild's DTB target graph on a repeat
`workspaceLoad` call, at least when only a `Compile`-item's file content changed and not the `.fsproj`
itself.

**Round 2e — does telling FSAC the `.fsproj` itself changed unblock the cache?** Sent
`workspace/didChangeWatchedFiles` for the `.fsproj` file **without actually changing the file** — a
notification-only test — then a further `fsharp/workspaceLoad`. **No canary movement, still `null`.**
As independent review below corrects, this was the wrong test: notifying without mutating leaves the
`.fsproj`'s own mtime unchanged, so FSAC's project cache (keyed on that mtime, not on notifications)
correctly sees nothing new.

**Confirming the other end of this finding — a genuine process restart:** with `Email` already present
in `Person.fs`/`Consumer.fs` on disk (left over from the 2b/2c/2d/2e attempts), a **fresh** FSAC process
was started (`cold` mode again). The canary mtime advanced, and `Person.Generated.fs` on disk now
contained the `Email` lens — a real DTB genuinely ran again.

## Amendment, post-review: the in-session reload boundary is `.fsproj` freshness, not process restart

**`03-review.md`'s independent work corrects this section and must be read alongside it.** The original
write-up here concluded "there may currently be no way to tell a running FSAC session to reload for this
class of change at all, short of restarting the language server." **That conclusion is wrong, and the
review falsified it directly:** an already-running FSAC session *does* re-run Myriad's codegen live, no
restart, once the `.fsproj` file's own last-write-time genuinely changes (even a bare
`File.SetLastWriteTimeUtc` touch with zero content change) and `fsharp/workspaceLoad` is reissued. The
root cause of Round 2e's own false result was that it only sent a *notification* claiming the `.fsproj`
changed, without ever actually changing it — Ionide.ProjInfo's project cache is keyed on the `.fsproj`'s
real mtime, not on being told a change happened, so the notification-only test could never have
succeeded. See `03-review.md`'s "The correction" section for the full account, including the reviewer's
own reproduction log (`artifacts/lsp-client-mtimeonly.scratch-ungated.live.log`).

**Corrected finding:** the in-session boundary is exactly the design's own pre-registered REVISE
wording — "closes the gap at project-(re)load time only, not during live editing without an explicit
reload trigger" — with a concrete, demonstrated trigger: any real change to the `.fsproj` (including a
no-op mtime touch), which is exactly what a real editor's own project-file watcher fires on. This does
**not** rescue the everyday live-edit experience: adding a field to `Person.fs` alone never touches the
`.fsproj`, so an ordinary source save still never refreshes generated code without an explicit or
watcher-triggered project reload. The correction changes the *mechanism* story (an in-session reload
path exists and works), not the *capability* ceiling (bare source-file edits still don't trigger it).

## What a review should press on

- **Single machine, single sample, one FSAC version (0.83.0).** No repeated trials, consistent with
  this repo's own standing timing/sample-size caveat (`FINDINGS.md`). The qualitative pass/fail results
  (does `Email` resolve, does the canary move) are binary and reproduced identically across five
  separate live-mode runs in this session (each reset from the same starting state), which is stronger
  evidence than a single run, but still all on one machine, one FSAC build, in one session.
- **Confirmed exactly right, by the review, in the direction this bullet predicted:** this section's
  original draft flagged "the 'no in-session reload works' finding is a negative result across the
  specific signals tried, not a proof no such signal exists," and named `Ionide.ProjInfo`'s own
  cache-invalidation logic as the thing to check rather than take on faith. The review did exactly that
  and found a working signal (an actual `.fsproj` mtime change) — see the Amendment section above. Kept
  here as a record that the caveat was the right one to write down before review, not just in hindsight.
- **One record, one generator (`Fields`... actually `Lenses` only was used here — `Fields` was named in
  the design but the fixture only exercises `Generator.Lenses`), one field addition.** Same
  single-shape-tested caveat every quartet in this repo carries.
- **This spike never touched a real editor UI (VS Code + Ionide), only `fsautocomplete` directly over
  raw LSP.** Ionide is a thin client over the same FSAC process, so the FSAC-level finding should
  transfer, but that inference itself is untested — Ionide's own extension code might, for instance,
  issue a `workspaceLoad` differently than this hand-rolled client did (e.g. on window focus, or via a
  file-system watcher glob this script never triggered because it isn't a real OS-level file write from
  a *different* process — this script did use ordinary `File.WriteAllText`, which should trigger a real
  filesystem-watcher event indistinguishable from an editor's own save, but a literal VS Code save was
  never tested).
- **The `fsharp/workspaceLoad` request/response protocol itself was reverse-engineered from FSAC's
  behavior** (send `{ textDocuments: [{uri}] }`, watch for a `fsharp/notifyWorkspace` notification and
  a matching response both carrying `{"Kind":"workspaceLoad","Data":{"Status":"finished"}}`), not read
  from FSAC's own source or documentation — correct as far as it was exercised (it did work for the
  cold-start case), but an undocumented detail of FSAC's protocol could differ across versions.
- Round 1's own canary and cache-no-op checks are solid and mechanical, low residual risk.
