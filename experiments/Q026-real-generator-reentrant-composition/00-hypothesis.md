# Q026 — Movement 1: Hypothesize

## Question

Promotes `BACKLOG.md` item 22's own cheapest falsifier (the narrower precursor to its full
live-watcher vision, deliberately scoped down before attempting that): can **Myriad's actual,
unmodified `LensesGenerator`** (`Myriad.Plugins.dll`, invoked via the real `IMyriadGenerator`
interface against a real on-disk `[<Lenses>]`-attributed file, formatted by the real Fantomas
pipeline `src/Myriad/Program.fs` uses) be **reentrant-queried by a second, real, unmodified
`IMyriadGenerator` implementation** — inside one in-process `FSharpChecker`, using Q010's
proven-correct reentrant `DocumentSource.Custom` mechanism — the way Q010 proved works only for a
hand-rolled two-file toy stand-in that never touched Myriad's real generator code at all?

## Novelty

Every prior quartet that exercised the reentrant-`DocumentSource.Custom` mechanism (Q010, Q021,
Q023, Q024) used **fabricated stand-ins**, not real Myriad generators:

- Q010's "B.fs" (the file standing in for `LensesGenerator`'s output) is a hand-typed F# string
  literal in `Q010-.../artifacts/round2-cross-generator/Program.fs` (line 37) that the quartet's
  own comment calls "faithful to `src/Myriad.Plugins/LensesGenerator.fs`" — a manual transcription,
  never `LensesGenerator.Generate` actually being called.
- Q010's "third generator" (the JSON serializer) is a bare function (`synthesizePersonJson`)
  inlined directly into the callback closure — it never implements `IMyriadGenerator`, is never
  instantiated via `Activator.CreateInstance`, and is never invoked the way Myriad's real CLI
  (`src/Myriad/Program.fs`'s `runGenerator`) invokes a generator.
- Q023/Q024 used synthetic, genuinely-typecheck-weighted *padding* files (generic records,
  `Map`/`List` pipelines) for their scale sweep — real typecheck cost, but still not Myriad's own
  generator code.

No quartet in this lineage has run a real, compiled Myriad generator (from `Myriad.Plugins.dll`)
inside an in-process `FSharpChecker`, let alone two of them composing via reentrant typed query.
`BACKLOG.md` item 22 names this exact gap plainly: "none of the three [pieces] has ever run
together... every mechanism this needs already has its own narrow, closed proof, but... Q010
showed [reentrant query] works... only inside a throwaway two/three-file toy project... never
against Myriad's own real MSBuild-invoked codegen path." This quartet is item 22's own named
"cheapest falsifier," not its full three-piece live-watcher vision (which also needs item 19's
save-watcher and Q022's `.fsproj`-touch/reload trigger — explicitly out of scope here, see
Validity preconditions).

## Contradiction check

- **Does not contradict Q010's SHIP.** Q010 proved the reentrant mechanism is *sometimes* correct
  (no hang, zero diagnostics, symbol-verified resolution) for a hand-rolled two-generator toy. This
  quartet asks whether the identical mechanism still holds when the virtual files' content is
  produced by *real* generator invocations instead of hand-typed stand-ins — a stronger precondition
  on the same claim, not a different claim. A KILL/REVISE here would not undo Q010's own verdict
  (which is scoped to what it actually tested); it would say the toy-to-real generalization fails,
  which is new information Q010 never claimed to cover.
- **Does not contradict Q022's REVISE** (DTB/MSBuild-hook route closes the IDE-invisibility gap at
  project-reload time, not live-edit time). Q022 retimes Myriad's *existing* per-file, no-FCS CLI
  invocation inside the MSBuild DTB pipeline; this quartet asks a different-axis question — can
  Myriad's generator *code* be hosted inside a persistent, reentrant, in-process `FSharpChecker` at
  all. The two are orthogonal mechanisms aimed at the same named gap from different directions, per
  `FINDINGS.md`'s own framing of item 8/Q022 vs. item 18/Q021 as "a second, non-type-provider
  route."
- **Does not contradict Q006's structural wall** (a type provider can never see a type from the
  compilation currently in progress). This quartet uses no type provider — it is Thread 1's
  in-process-hosting line (Q001/Q002/Q010's family), which Q006's own review already noted never
  hits that wall in the first place.

## Validity preconditions

State every one of these plainly before any code exists, per this repo's own "no unstated
assumptions" rule:

1. **Must reference the actual compiled DLLs**, not reimplement anything: `Myriad.Core.dll` and
   `Myriad.Plugins.dll`, built at `src/Myriad.Core/bin/Release/net9.0/` and
   `src/Myriad.Plugins/bin/Release/net9.0/` respectively (both confirmed present in this session).
   `LensesGenerator` must be found and instantiated the way `src/Myriad/Program.fs`'s
   `Implementation.findPlugins` does — scanning `assembly.GetTypes()` for the
   `MyriadGeneratorAttribute`, then `Activator.CreateInstance` and an `IMyriadGenerator` cast — not
   a direct `LensesGenerator()` constructor call, so the discovery mechanism is faithful to the real
   CLI, not simplified away. One disclosed, deliberate simplification: this quartet does **not**
   route through `McMaster.NETCore.Plugins.PluginLoader`'s `AssemblyLoadContext` isolation — it
   loads `Myriad.Plugins.dll` via an ordinary project/assembly reference. ALC isolation is
   `BACKLOG.md` item 2's own separate, still-open question (untested since Q003) and is explicitly
   not this quartet's concern; naming it here so the simplification isn't silently smuggled in.
2. **Must invoke `LensesGenerator` against a real on-disk `.fs` file** carrying the real
   `[<Lenses>]` attribute (`Myriad.Plugins.LensesAttribute`), not a hand-typed stand-in of its
   input or output. `GeneratorHelpers.parseInputAst` (confirmed by reading
   `src/Myriad.Plugins/GeneratorHelpers.fs:30-33`) calls `Ast.fromFilename context.InputFilename`,
   which reads from disk — so the domain record must exist as a real file, not only as an in-memory
   string.
3. **Must format the real `Output.Ast` result through the same pipeline `Program.fs` uses**
   (confirmed by reading `src/Myriad/Program.fs:300-315`): `ParsedInput.ImplFile(
   ParsedImplFileInput.CreateFs(...))` then `CodeFormatter.FormatASTAsync` (Fantomas.Core 7.0.5,
   matching `paket.lock`) then `Generation.getHeaderedCode`. Skipping straight from `Output.Ast` to
   hand-written text would reintroduce exactly the kind of stand-in this quartet exists to remove.
4. **The second generator must also be a real, compiled type implementing `IMyriadGenerator`**,
   discovered and invoked the identical way (reflection over `MyriadGeneratorAttribute`,
   `Activator.CreateInstance`, `.Generate(context)` call) — not a bare function. It is new code
   (there is no existing second Myriad generator that reentrant-queries another one), but it must
   be real, buildable, `[<MyriadGenerator("...")>]`-attributed code, not inlined into the harness's
   `DocumentSource.Custom` callback the way Q010's stand-in was.
5. **A real, load-bearing gap must be named upfront, not discovered mid-build and smoothed over**:
   `IMyriadGenerator.Generate(context: GeneratorContext)` (confirmed from
   `src/Myriad.Core/Types.fs:40-42`) carries no parameter or field through which a generator could
   receive a live `FSharpChecker`/`FSharpProjectOptions` handle. There is no existing mechanism for
   a generator to ask for one. This quartet's second generator therefore needs a side-channel to
   reach the in-flight checker/options — the design (Movement 2) must specify this concretely
   (a harness-owned static mutable module the generator reads from) and this must be reported as
   exactly what it is: a real gap in `IMyriadGenerator`'s current shape that any future real
   integration would have to close for real (e.g. by widening `GeneratorContext`), not a detail to
   bury in the harness's plumbing.
6. **FCS pinned to `43.9.101`**, matching `paket.lock` and every prior Thread-1 quartet.
7. **`src/Myriad/Program.fs`, `src/Myriad.Core`, and `src/Myriad.Plugins` must not be modified.**
   Both generators exercised must be real and unmodified; the harness hosting them is new,
   standalone code under this quartet's own `artifacts/`, not a change to Myriad's shipped CLI.
8. **Out of scope, deliberately** (per item 22's own "narrower falsifier" framing, not a full
   live-watcher): no `FileSystemWatcher`/live-save trigger (item 19), no `.fsproj`
   touch-and-reload (Q022's mechanism), no FSAC/Ionide session. This quartet only asks whether the
   two-real-generator composition works inside one `FSharpChecker.ParseAndCheckProject` call, once.

## The deepest open question this quartet cannot resolve by construction — named upfront

`src/Myriad/Program.fs` (read directly this session) is a single `[<EntryPoint>]`: parse args,
load plugins via `McMaster.NETCore.Plugins.PluginLoader.CreateFromAssemblyFile`, run each
generator once per codegen unit, write output, exit. There is no `FSharpChecker` anywhere in it,
no persistent state across invocations — it is a short-lived, per-invocation process today,
confirmed directly rather than assumed. Whatever this quartet finds, it finds it inside a **new,
standalone harness** that plays the role a future, differently-architected Myriad host would need
to play — it does not and cannot show that Myriad's *actual shipping CLI* can host this today. A
positive result here answers "does the composition mechanism work when hosted," not "does
Myriad's real CLI already support this" — those are different questions, and conflating them would
be exactly the kind of overclaiming this repo's own digest (`FINDINGS.md`) has caught five times
already. If the composition succeeds, the honest framing is: the mechanism works, hosted; turning
Myriad's actual CLI into that kind of host is a distinct, unattempted architecture change.

## Cheapest falsifier

Before building any `FSharpChecker`/reentrancy wiring at all: can the real, unmodified
`LensesGenerator` even be invoked standalone, outside `Program.fs`'s own process, against a real
on-disk `[<Lenses>]`-attributed file, and produce correctly-formatted lens code via the real
Fantomas pipeline? This is cheap (no FCS involvement, no reentrancy, just "load the DLL, find the
type, call the interface method, format the result") and could fail for reasons that have nothing
to do with reentrant typed hosting — e.g. `EditorConfig.readConfiguration` behaving differently
outside a real project context, or a hidden dependency on something `McMaster.NETCore.Plugins`
supplies that an ordinary assembly reference doesn't. If this fails, that is itself a valuable,
cheap KILL signal for the larger composition question, before any FCS/reentrancy code is written.
Only once this passes does Round 2 (the actual reentrant composition, the quartet's real question)
get attempted.
