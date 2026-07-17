# Q022 — Adversarial review

**Verdict: REVISE.** The Round 1 MSBuild/DTB mechanism and the Round 2a cold-load capability both
reproduced independently and cleanly — the genuinely new and valuable part of this quartet. But the
results doc's sharpest negative claim ("there may currently be no way to tell a running FSAC session to
reload for this class of change at all, short of restarting the language server") is **wrong, and I
falsified it directly**: an already-running FSAC session *does* re-run Myriad's codegen mid-session,
no process restart, once the `.fsproj`'s last-write-time actually changes and a `workspaceLoad` is
reissued. The executor's Round 2e only *notified* FSAC that the `.fsproj` changed without ever changing
the file, so Ionide.ProjInfo's mtime-keyed project cache correctly saw nothing new and short-circuited.
Corrected, the result lands exactly on the design's own pre-registered REVISE shape — "closes the gap
at project-(re)load time only, not during live source editing" — which is more accurate and no weaker
than what the executor reported; it is the over-sharpening in `02-results.md`'s "What actually happened
vs. what the design predicted" section that has to be struck, not the underlying finding.

Everything below was independently rebuilt and rerun. I did not trust the results narrative; where I
report a number it is from a command I ran, cited.

## What I reproduced

Rebuilt the three real binaries (`Myriad.Core`, `Myriad.Plugins`, `Myriad`, `-c Debug`) from the
current `src/` — all three `Build succeeded, 0 Errors` — so the scratch projects' `HintPath` references
resolve against freshly-built DLLs. (Note: `src/` has uncommitted changes from a parallel session —
`Diagnostics.fs` new, `Types.fs`/`Program.fs` modified — which build cleanly and are irrelevant to this
quartet, which only exercises `Generator.Lenses`.)

**Fixture fairness (task item 4).** `diff src/Myriad.Sdk/build/Myriad.Sdk.targets
artifacts/Myriad.Sdk.NoGate.targets` shows exactly three deltas and nothing else: (1) the
`Condition=" '$(DesignTimeBuild)' != 'true' "` removed from `MyriadSdkGenerateCode`; (2) a
`<Touch ...generation.canary>` added before the `<Exec>`; (3) the empty
`MyriadSdkIncludeCodegenOutputDuringDesignTimeBuild` DTB no-op dropped. `scratch-gated` imports the
real, shared, unmodified `../../../../src/Myriad.Sdk/build/Myriad.Sdk.targets` (verified in its
`.fsproj`). The comparison is fair.

**Round 1 (MSBuild/DTB mechanism) — fully reproduced.** From a clean `dotnet build` baseline
(`Person = { Name: string }`, generated file `Name`-only on both), I added `Age: int` on disk and ran
the design's DTB invocation
(`dotnet msbuild -p:DesignTimeBuild=true -p:SkipCompilerExecution=true -p:ProvideCommandLineArgs=true -p:BuildingProject=false -t:Compile`):

| Project | DTB exit | `bin/*.dll` mtime | canary | `Person.Generated.fs` |
|---|---|---|---|---|
| `scratch-gated` (real targets) | 0 | unchanged (`23:50:05.556` → same) | absent (gate blocks target) | still `Name` only |
| `scratch-ungated` (gate removed) | 0 | unchanged (`23:50:07.892` → same) | advanced (`…06.319` → `…22.549`) | `Name` **and** `Age` |

The unchanged `.dll` mtime under a moved canary is the load-bearing proof: Myriad's `<Exec>` ran, the
real F# compiler did not. A **repeat** DTB call with no source change left the canary frozen at
`…22.549` (rebuild cache correctly no-ops); a subsequent real change (add `Email`) moved it to
`…36.907` and wrote the `Email` lens (cache re-fires on genuine change). Round 1's claims hold in full.

**Round 2a (cold FSAC over real LSP) — fully reproduced, both directions.** Restored FSAC 0.83.0 as a
local tool (`dotnet tool run fsautocomplete --version` → `0.83.0+96fabed…`), set both fixtures to a
genuinely stale state (`Person.fs` has `Age`, generated file `Name`-only), and drove
`lsp-client.fsx … cold` against each:

- `scratch-gated` (negative control): `HOVER Name -> RESOLVED`, `HOVER Age -> NULL`, canary `<missing>`.
  The gate keeps the generated file stale, so `Age` genuinely does not exist. `Name` resolving proves
  the project loaded and typechecked, so the `Age` NULL is a real discrimination, not a broken fixture.
- `scratch-ungated` (positive): the on-disk generated file went from `Name`-only *before* FSAC started
  to `Name`+`Age` *after* the cold `workspaceLoad` (canary appeared at `22:52:00`), and
  `HOVER Age -> RESOLVED` with real type info
  (`val Age: Input.Person -> int * Input.Person -> int -> Input.Person`).

No `dotnet build` ran in either session: the `bin/*.dll` mtimes stayed at their pre-FSAC clean-build
values (`23:51:22` ungated, `23:51:20` gated) across every subsequent FSAC run through `23:57`. The
"zero real build" claim (task item 5) holds — FSAC's own DTB (SkipCompilerExecution) does the work,
confirmed by the moving canary against the frozen `.dll`.

**LSP-client correctness (task item 2).** I read the script. The hover-position fix is correct:
`col = moduleCol + dotIdx + 1 + (memberName.Length/2)` lands the cursor on the second `m` of `Name`
(offset 15 from the module's first char), inside the member, not on the module — so a NULL genuinely
means the member is unresolved. I found no other position/interpretation bug: `findPosition` keys on
the full `"PersonLenses.<member>"` needle (unique per line), and `hoverAt` treats a `Null`-kind or
absent `result` as unresolved. The negative control returning NULL while `Name` returns a full
signature is the check that this is measuring what it claims.

**Round 2b–2e (live edit, no restart) — reproduced as reported.** In one running session, none of the
four signals the executor tried moved the canary off `22:52:00` or resolved `Email`: `didChange`+
`didSave`, `didChangeWatchedFiles` on the source file, a second `fsharp/workspaceLoad`, and
`didChangeWatchedFiles` on the `.fsproj` (notification only). Every one returned
`HOVER Email -> NULL`. That part of the result is real.

## The correction — the executor's negative claim is over-sharpened (task item 3)

The task asked me to try at least one thing the executor didn't. I tried three; one overturns the
sharp negative claim.

1. **FSAC's experimental server mode `--adaptive-lsp-server-enabled`.** Reran the full live sequence
   against it. Cold load still works (canary `22:53:48`, `Age` resolves); live 2b–2e still fail on all
   four signals. So the adaptive server does not change the conclusion — this *strengthens* the
   executor's negative for the signals they tried, and is a useful second data point.

2. **Actually mutating the `.fsproj` bytes on disk before reload** (the executor's 2e sent the
   notification but never changed the file). Appended a comment line to the `.fsproj`, then
   `didChangeWatchedFiles(.fsproj)` + `workspaceLoad`, same running session: canary **moved** to
   `22:56:16`, `Person.Generated.fs` gained the `Email` lens, and `HOVER Email -> RESOLVED`
   (`val Email: Input.Person -> string * …`). No process restart.

3. **Isolating the trigger:** an **mtime-only** bump of the `.fsproj` (`File.SetLastWriteTimeUtc`, zero
   content change, *and no `didChangeWatchedFiles` at all*) followed by `workspaceLoad` **also**
   re-triggered: canary `22:57:43`, `Email` resolved. Log:
   `artifacts/lsp-client-mtimeonly.scratch-ungated.live.log`,
   `RESULT-2e … emailOk(after fsproj-touch reload)=true`.

Mechanistically (inferred from black-box behavior, not from reading Ionide.ProjInfo's source — same
honesty caveat the executor flagged): FSAC caches the cracked project keyed on the `.fsproj`'s
last-write-time. A bare `workspaceLoad` with an unchanged `.fsproj` is short-circuited (the executor's
2d), which is why their 2e notification-without-file-change also did nothing. The moment the `.fsproj`
appears newer, `workspaceLoad` re-cracks the project, which re-runs the DTB target graph, which runs
Myriad's `<Exec>`, live, in the same process.

So the correct statement of the negative is **not** "only a process restart works." It is: **a bare
source-file save does not re-run codegen, but a project reload does — and a project reload is triggered
by any real change to the `.fsproj` (including a mere mtime touch), which is exactly what Ionide's own
project-file watcher fires on.** This is the design's pre-registered REVISE wording verbatim ("closes
the gap at project-(re)load time only, not during live editing without an explicit reload trigger"),
now with a concrete demonstration of an in-session reload trigger that the executor wrongly concluded
did not exist.

This does **not** rescue the practical capability. A developer adding a field to `Person.fs` does not
change the `.fsproj`, so the everyday live-edit experience (save source, see new member) still does not
work without a reload. The correction changes the *mechanism* story (in-session reload is possible),
not the *capability* ceiling (bare source save still fails). That is the right way to read it, and it
is the mechanism-vs-capability distinction the review is required to police.

## Mechanism vs. capability (task item 3, core)

The results doc is mostly careful about not letting Round 1's mechanism success or Round 2a's
capability success imply Round 2b. That carefulness holds — with one exception. Round 1 (mechanism:
DTB can run Myriad's `<Exec>`) and Round 2a (capability: a cold LSP session surfaces the member) are
both real and both independently confirmed. Neither implies live-edit pickup, and the doc does not
claim it does. The failure is narrower and located precisely: in the "What actually happened" section,
the mechanism finding "no in-session reload signal I tried worked" was generalized into the capability
claim "there may be no way to reload in-session at all short of restart." That generalization is the
one place a negative *mechanism* result (specific signals didn't fire) was mistaken for a negative
*capability* result (in-session reload is impossible) — and it is false. Strike that sentence; keep
everything else.

## Generalization (task item 6)

One record, one generator (`Lenses` only — `Fields` was named in the design but the fixture never
exercised it, as the results doc itself concedes), one field addition, one machine, one FSAC version
(0.83.0), and — a caveat the executor did not flag — the `lsp-client.fsx` log files are written in
overwrite mode (`StreamWriter(logPath, false, …)`), so my reruns overwrote the executor's original
logs. I therefore cannot audit the executor's specific "reproduced identically across five separate
live-mode runs" claim from the original artifacts; I can only corroborate it by my own independent
reruns, which reproduced the cold-pass / live-fail pattern every time plus the two new variants. The
qualitative binary results (does `Age` resolve cold, does `Email` resolve live, does the canary move)
are robust; nothing here supports timing generalization, and no timing claim is being made.

The `.fsproj`-touch finding was demonstrated by a hand-rolled client issuing `workspaceLoad` manually.
In a real VS Code + Ionide session the client registers a file-system watcher on the `.fsproj` and
issues the reload automatically, so the finding should transfer, but that inference (like the
executor's own FSAC-transfers-to-Ionide inference) is untested against the literal editor UI.

## Standing cross-cutting caveat (task item 7)

`FINDINGS.md`: *"Every 'works live in the IDE' claim has only ever been tested through `FSharpChecker`
as a library, never a literal Ionide/FSAC/VS session."* This quartet **partially** closes that gap and
should be cited as partial, not full:

- **Closed:** for the first time in this repo's lineage, a "works live" claim was driven against a real
  `fsautocomplete` process over real `Content-Length`-framed LSP, not `FSharpChecker`-as-library. The
  cold-load capability (2a) genuinely holds against that process, both directions, and I reproduced it.
- **Not closed:** (a) it is `fsautocomplete` directly, not the literal VS Code + Ionide UI, so "a
  literal Ionide/VS session" is still one inference-hop away; (b) the half that held (cold/reload) is
  the weaker half — the live-source-edit half (2b) fails; (c) Visual Studio and Rider (different
  project systems entirely) are untouched. Cite it as: "first real-LSP-process test in this repo;
  proves the DTB-hook surfaces members at project load/reload against a real FSAC process; does not
  prove live source editing, and does not use the editor UI."

## Contradiction check against prior quartets (task item 7)

No contradiction. This is Thread-1-shaped (retiming Myriad's own MSBuild/`<Exec>` step), not a type
provider. Q006's wall ("a type provider can never see a type from the compilation currently in
progress") is about `FSharp.TypeProviders.SDK` providers running inside FCS's type-check protocol.
Here, Myriad writes a real `Person.Generated.fs` to disk *before* `CoreCompile`, and FCS/FSAC then
compiles that file as ordinary source — no provider resolves an in-progress type at all. I verified
this reasoning rather than taking `FINDINGS.md`'s correction on faith: the generated member resolves
via a normal `Compile`-item source file (`*Assembly: scratch-ungated*` in the hover payload), exactly
the path Q006's wall does not touch. Consistent with Q021's finding that Thread-1 mechanisms are not
Q006-bound.

## Verdict

**REVISE.** Reasoning against the pre-registration: the SHIP bar required 2a to pass *and* 2b to pass;
2b fails, so SHIP is off the table by the quartet's own rule. The design pre-registered precisely this
outcome — "If 2a passes but 2b fails: REVISE-shaped result, reported as 'closes the gap at
project-(re)load time only, not during live editing without an explicit reload trigger'." That is the
honest verdict, and my independent work confirms both halves of it: 2a passes against a real LSP
process (the genuinely new contribution), and the live-edit gap is real. The one required amendment is
to `02-results.md`'s over-sharpened "only a restart works" framing, which I falsified directly — an
in-session reload triggered by a real `.fsproj` change (even a bare mtime touch) re-runs codegen live,
which is the ordinary project-reload path, not a restart. Unlike Q012, the pre-registered categories
fit cleanly here; this is a textbook REVISE, not an "found something more specific than asked" case —
the specific thing found (`.fsproj`-touch reload works in-session) tightens the result *back onto* the
design's own REVISE wording rather than opening new territory.

Net, for the digest: **item 8's mechanism works and its cold/reload capability is real and now proven
against a literal FSAC process — but its headline framing ("gets FSAC to show generated members without
a real `dotnet build`") is true only at project load/reload time, not during live source editing; a
bare source save never re-runs codegen, and the only in-session refresh is a project reload forced by a
real `.fsproj` change.** The IDE-invisibility gap is narrowed at the load/reload boundary, not closed
for live editing.
