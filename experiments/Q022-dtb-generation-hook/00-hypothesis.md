# Q022 — Hooking Myriad's codegen into the design-time-build path

## Question

`MyriadSdkGenerateCode` (`src/Myriad.Sdk/build/Myriad.Sdk.targets:162`), the target that actually
runs the Myriad CLI (`<Exec>`, line 227), is gated `Condition="'$(DesignTimeBuild)' != 'true'"`. When
an IDE (Ionide/FSAC, Visual Studio, Rider) triggers a design-time build (DTB) — which happens far more
often than a real `dotnet build`, typically on file save/open and project load — this target, and
therefore all Myriad code generation, is skipped outright.
`MyriadSdkIncludeCodegenOutputDuringDesignTimeBuild` (line 230), the target that runs *instead* during
DTB, is empty.

**Does removing this gate — running the real Myriad CLI during DTB too, so a freshly-edited
attributed type's generated members exist on disk before the IDE's own project-options resolution
reads the file list — make FSAC show those newly generated members without a real `dotnet build`?**

This is `BACKLOG.md` item 8, named across three prior sessions ("the more direct candidate") but never
spiked. It supersedes trying to route around Q006's wall from the type-provider side: DTB runs the
project's own build system (MSBuild) inside the *same* compilation the attributed type belongs to, so
Q006's wall (a type provider can never see a type from the compilation currently in progress) never
applies here at all — this isn't a type provider, it's Myriad's own existing generate-before-build
step, just retimed.

## Novelty

Not covered by any existing generator or closed quartet. Every quartet that has attacked Myriad's
IDE-invisibility gap so far (Q006, Q016-18, Q019, Q020, Q021) did so from the *type-provider* side —
building something that runs *inside* FCS's own type-checking protocol, standing next to Myriad's
real pipeline rather than modifying it. This is the first quartet in either thread to touch Myriad's
own MSBuild target directly. It is also the first quartet in this repo to require exercising real
`dotnet msbuild`/DTB behavior rather than `FSharpChecker`-as-a-library — see Validity preconditions
below for why that changes what "cheapest falsifier" even means here.

## Contradiction check

- Does not contradict Q006's verdict (type providers structurally cannot reach a same-project type) —
  this mechanism is not a type provider and was never subject to that wall in the first place, per
  `FINDINGS.md`'s own correction: "Thread 1 (Myriad-CLI-hosted `FSharpChecker`) is not bound by the
  Q006 same-compilation wall at all." This item is Thread-1-shaped (modifying Myriad's own host/build
  step), even though it lives in the DTB/MSBuild layer rather than an in-process `FSharpChecker`.
- Does not contradict `DEVNOTES.md`'s rebuild-cache design (file hash of Myriad version + refs +
  `--inputfile`/`--outputfile`/`--configkey`) — that cache determines whether `MyriadSdkGenerateCode`'s
  `Inputs`/`Outputs` mark the target up-to-date; it says nothing about *when* (DTB vs real build) the
  target is allowed to run at all, which is what the `Condition` gate controls and what this quartet
  changes.
- Does not contradict any Thread 2 (type-provider) finding — this quartet touches none of that
  machinery.
- **Directly engages** this repo's own standing cross-cutting caveat (`FINDINGS.md`, "Cross-cutting
  limitations"): *"Every 'works live in the IDE' claim has only ever been tested through
  `FSharpChecker` as a library, never a literal Ionide/FSAC/VS session."* This item was named in
  `BACKLOG.md` item 17 as worth closing and Q021 closed a *different*, narrower piece of it (the
  reentrant-`DocumentSource.Custom` mechanism under repeated LSP-shaped load, still `FSharpChecker`-
  as-library). Nothing so far has driven a real `fsautocomplete` (FSAC) process over LSP. This quartet
  is scoped explicitly to attempt that, not to repeat the FSharpChecker-as-library pattern by default.

## Validity preconditions

- Must run against a real MSBuild invocation of a real `.fsproj`, using the version of MSBuild bundled
  with the installed .NET SDK on this machine (checked at execution time, recorded in `01-design.md`)
  — not `FSharpChecker`'s own project-options parsing, which does not exercise MSBuild's actual DTB
  code path (target `Condition`s, `BeforeTargets`/`AfterTargets` ordering, `<Exec>` task behavior) at
  all.
- Must confirm, empirically and not by assumption, which CLI invocation actually sets
  `$(DesignTimeBuild)` = `true` the way a real IDE's project-system client does. This is not something
  to take on faith from general knowledge of the "Roslyn DTB protocol" — it must be verified by adding
  a throwaway `<Message>` echoing the property's value under the candidate invocation before anything
  else is built on top of it.
- The change under test (removing the `Condition` gate) must be scoped to a *copy* of
  `Myriad.Sdk.targets` used only by this quartet's scratch project, not the real, shared
  `src/Myriad.Sdk/build/Myriad.Sdk.targets` — that file is imported by every Myriad consumer in this
  repo (including `test/Myriad.IntegrationPluginTests`) and by any real downstream user of the NuGet
  package; editing it in place would be a production change disguised as a spike, and is exactly the
  kind of scope creep the quartet discipline exists to prevent.
- Must test against a real attributed type and a real generator (`Fields` or `Lenses` from
  `src/Myriad.Plugins`), not a synthetic strawman generator built to flatter the hypothesis — reusing
  the same fixture shape `test/Myriad.IntegrationPluginTests` already uses is preferable to inventing a
  new one.
- If Round 2 (real FSAC/Ionide session) is attempted: must record the exact `fsautocomplete` version
  installed (no version is pinned anywhere in this repo, unlike FCS/Fantomas.FCS in `paket.lock`), and
  must disclose plainly, not paper over, any tooling friction encountered — consistent with how Q016's
  review handled its own build-tooling outage.

## Cheapest falsifier

Before building anything else: confirm which `dotnet msbuild`/`dotnet build` invocation actually
causes `$(DesignTimeBuild)` to evaluate `true` inside the target graph, using nothing but a one-line
`<Message Text="DesignTimeBuild=$(DesignTimeBuild)" Importance="high" />` added to a trivial scratch
`.fsproj`. If no invocation available from this machine's installed SDK actually sets the property the
way a real IDE does, the rest of this quartet's design needs to change before anything else is built —
this is cheap (minutes), and every other round depends on it being right.

Second-cheapest, gating Round 1 proper: with the property-setting invocation confirmed, run it against
the **unmodified**, real `Myriad.Sdk.targets` on a scratch project with a stale on-disk generated file
(edited source, not yet regenerated) and confirm directly — not assumed from reading the `.targets`
file — that `MyriadSdkGenerateCode`'s `<Exec>` genuinely does not run (via a canary file the `<Exec>`
would otherwise touch) and the stale generated file is left untouched. This is the baseline the rest of
the quartet is measured against, and confirms this quartet's own premise (the gate is real and matters)
before spending effort proving anything more elaborate.
