# Q026 — Movement 2: Design

## Overview

Four pieces, built in dependency order. Rounds 1 and 2 below correspond to the two-round structure
named in `00-hypothesis.md`'s cheapest falsifier.

```
artifacts/
  Domain/Person.fs                     <- real on-disk [<Lenses("person")>] record (file A)
  Q026.Bridge/                         <- shared side-channel (the disclosed IMyriadGenerator gap)
  ReentrantJsonGenerator/               <- second real, unmodified IMyriadGenerator
  Harness/                             <- new standalone host process (not Program.fs)
```

### `Domain/Person.fs` (file A, real, on disk)

```fsharp
namespace Domain

open Myriad.Plugins

[<Generator.LensesAttribute("person")>]
type Person = { Name: string; Age: int }
```

The `"person"` config-group argument is `Generator.LensesAttribute`'s real, required constructor
parameter (confirmed by reading `src/Myriad.Plugins/Attribute.fs`) — real Myriad usage requires a
config-group name here, looked up via `GeneratorContext.ConfigGetter`. The harness supplies this
getter directly (see Bridge below) rather than parsing a `myriad.toml` file — a legitimate,
real value for `ConfigGetter: string -> (string*obj) seq`, not a generator-logic stand-in; real
`Program.fs` derives the identical function from parsed TOML, and `IMyriadGenerator` only ever sees
the function, never the file.

### `Q026.Bridge` (Library, net9.0)

The concrete answer to precondition 5's named gap: `IMyriadGenerator.Generate(context:
GeneratorContext)` has no field for a live `FSharpChecker`/`FSharpProjectOptions` handle, so the
second generator needs a side-channel. `Bridge.fs`:

```fsharp
module Q026Bridge

open System.Collections.Concurrent
open FSharp.Compiler.CodeAnalysis

/// Populated by the harness before invoking the reentrant generator's Generate method.
let mutable Checker : FSharpChecker option = None
let mutable Opts : FSharpProjectOptions option = None
let mutable PersonFilePath = ""
let mutable PersonLensesFilePath = ""

/// Populated by the harness's DocumentSource.Custom callback as each virtual file's real
/// content is computed - mirrors Q010 Round 2's own `files: ConcurrentDictionary` cache.
let Files = ConcurrentDictionary<string, string>()
```

References: `FSharp.Compiler.Service 43.9.101` only.

### `ReentrantJsonGenerator` (Library, net9.0) — the second real, unmodified generator

New code (no such generator exists today), but real: implements `IMyriadGenerator`, attributed with
`MyriadGeneratorAttribute`, discovered and invoked the identical way `LensesGenerator` is (see
Harness below). Its `Generate` method body performs Q010 Round 2's own proven reentrant-inspection
logic (`collectEntities`/`collectMfvs`/`getterReturnTypeName`, copied from
`Q010-.../artifacts/round2-cross-generator/Program.fs:48-73` since that logic is already
independently reviewed and SHIPped — reusing a proven mechanism, not re-deriving it) but pointed at
the **real** `PersonLenses` module text (produced by the real `LensesGenerator` in Round 2, not
Q010's hand-typed stand-in) and reading the checker/opts/file paths from `Q026Bridge` instead of
module-level `let mutable`s:

```fsharp
[<MyriadGenerator("reentrantjson")>]
type ReentrantJsonGenerator() =
    interface IMyriadGenerator with
        member _.ValidInputExtensions = seq { ".fs" }
        member _.Generate(_context: GeneratorContext) : Output =
            let checker = Q026Bridge.Checker.Value
            let opts = Q026Bridge.Opts.Value
            let fileA = Q026Bridge.PersonFilePath
            let fileB = Q026Bridge.PersonLensesFilePath
            let run =
                async {
                    let! _, ansA =
                        checker.ParseAndCheckFileInProject(
                            fileA, 0, SourceText.ofString Q026Bridge.Files.[fileA], opts)
                    match ansA with
                    | FSharpCheckFileAnswer.Aborted -> failwith "reentrant check of Person.fs aborted"
                    | FSharpCheckFileAnswer.Succeeded _ -> ()
                    let! _, ansB =
                        checker.ParseAndCheckFileInProject(
                            fileB, 0, SourceText.ofString Q026Bridge.Files.[fileB], opts)
                    let checkedB =
                        match ansB with
                        | FSharpCheckFileAnswer.Succeeded r -> r
                        | FSharpCheckFileAnswer.Aborted -> failwith "reentrant check of PersonLenses.fs aborted"
                    // typed-inspect checkedB.ImplementationFile for the PersonLenses module
                    // and its lens bindings — Q010's proven mechanism, real symbols this time.
                    ... (module lookup by DisplayName = "PersonLenses", binding discovery via
                         collectMfvs, getter-return-type sniffing, exactly Q010 Round 2)
                    return Output.Source jsonSourceText
                }
            Async.RunSynchronously(run, timeout = 30_000)
```

The exact generated text (module/namespace name the real `LensesGenerator` wraps its output in,
the binding names/order) depends on `Q026Bridge`'s supplied config and is **not guessed here** —
per this repo's own "no unstated assumptions" rule, Round 1 (below) runs first and its real,
printed output is what this generator's typed-inspection code is written against. If Round 1's
real output differs structurally from Q010's hand-typed stand-in in some way that breaks the
lookup-by-`DisplayName` approach, that is itself relevant Round 2 evidence, reported honestly in
`02-results.md`, not silently patched around.

References: `Myriad.Core.dll` (HintPath), `Q026.Bridge` (ProjectReference), `FSharp.Compiler.Service
43.9.101`.

### `Harness` (Exe, net9.0) — the new standalone host

References: `Myriad.Core.dll` (HintPath), `Q026.Bridge` (ProjectReference),
`FSharp.Compiler.Service 43.9.101`, `Fantomas.Core 7.0.5`. **Deliberately does not reference
`Myriad.Plugins.dll` or `ReentrantJsonGenerator.dll` at compile time** — both are discovered at
runtime via a `findPlugins`-equivalent helper, copied from and faithful to `src/Myriad/
Program.fs:16-35`'s real `Implementation.findPlugins`, with one disclosed simplification named in
precondition 1: plain `Assembly.LoadFrom(path)` instead of `McMaster.NETCore.Plugins.PluginLoader`
(ALC isolation is `BACKLOG.md` item 2's own separate open question, not this quartet's concern).

```fsharp
let findGenerators (assembly: Reflection.Assembly) : Type list =
    assembly.GetTypes()
    |> Array.filter (fun t -> t.GetCustomAttributes(typeof<MyriadGeneratorAttribute>, true).Length > 0)
    |> List.ofArray

let instantiate (t: Type) : IMyriadGenerator =
    Activator.CreateInstance(t) :?> IMyriadGenerator
```

**Round 1 (cheapest falsifier, no FCS involved):**

1. `Assembly.LoadFrom(".../src/Myriad.Plugins/bin/Release/net9.0/Myriad.Plugins.dll")`.
2. `findGenerators` → locate the type named `LensesGenerator`, `instantiate` it.
3. Build a real `GeneratorContext`:
   `GeneratorContext.Create(Some "person", (fun key -> if key = "person" then seq { "namespace",
   box "Domain" } else Seq.empty), <path to Domain/Person.fs>, None, dict [])`.
4. Call `.Generate(context)` → real `Output.Ast modules`.
5. Format exactly as `Program.fs:300-315` does: `ParsedInput.ImplFile(ParsedImplFileInput.CreateFs(
   filename, modules = ast))` → `CodeFormatter.FormatASTAsync(parseTree, EditorConfig.
   readConfiguration filename)` → `Generation.getHeaderedCode`.
6. Print the result and write it to `artifacts/round1-output/PersonLenses.fs` as a durable,
   checked-in artifact. **Pass/fail for Round 1**: the call succeeds with no exception and produces
   text containing a `PersonLenses` module with `Name`/`Age` bindings recognizably shaped like
   `LensesGenerator.fs`'s own tuple-of-lambdas construction (visual/structural confirmation — this
   round doesn't need typed verification, only "did the real generator run standalone at all,"
   per the cheapest-falsifier framing in `00-hypothesis.md`).

**Round 2 (the actual composition, only attempted if Round 1 passes):**

1. Three file paths: `fileA` = the real `Domain/Person.fs` path; `fileB` = a virtual path (e.g.
   `C:\virt\PersonLenses.fs`, never written to disk except as the Round 1 durable copy); `fileC` =
   a virtual path (e.g. `C:\virt\PersonJson.fs`).
2. `DocumentSource.Custom` callback:
   - `path = fileA` → `Q026Bridge.Files.[fileA] <- File.ReadAllText fileA` (real on-disk content;
     unlike Q010, no hand-copied string literal for this file since it already exists for real);
     return it.
   - `path = fileB` → run Round 1's exact steps 1-5 (real `LensesGenerator.Generate` +
     real Fantomas formatting) to compute the text, cache it in `Q026Bridge.Files.[fileB]`, return
     it. Set a `reentrantLensesInvoked <- true` sentinel (mirroring Q010's own `reentrantEntered`
     footgun-detection pattern, itself named in Q010's review objection 3) to detect the silent
     `DocumentSource.Custom`-bypass failure mode if it recurs here.
   - `path = fileC` → `Assembly.LoadFrom` the compiled `ReentrantJsonGenerator.dll`, `findGenerators`
     + `instantiate` it (same reflection path as fileB's Lenses invocation — both generators
     discovered identically, no special-casing), set `Q026Bridge.Checker/Opts/PersonFilePath/
     PersonLensesFilePath`, call `.Generate(genericContext)`, cache and return the resulting
     `Output.Source` text.
   - else → `None`.
3. `checker = FSharpChecker.Create(keepAssemblyContents = true, documentSource = docSource,
   useTransparentCompiler = true)` — same configuration as Q010/Q021/Q023/Q024, for direct
   comparability.
4. `GetProjectOptionsFromScript(fileA, SourceText.ofString (File.ReadAllText fileA))`, then override
   `SourceFiles = [| fileA; fileB; fileC |]` and `ProjectFileName`, exactly Q010's own pattern.
5. `checker.ParseAndCheckProject(opts)` — the outer drive.
6. Verification, matching Q010's own capability bar exactly (not a weaker one):
   - Zero `Error`-severity diagnostics on `fileB` and `fileC`.
   - `reentrantLensesInvoked = true` and (a second sentinel) `reentrantJsonInvoked = true` — both
     callbacks actually fired, not bypassed.
   - Re-check `fileC` standalone (`ParseAndCheckFileInProject`) to get `FSharpCheckFileResults`, then
     `GetAllUsesOfAllSymbolsInFile` + `GetSymbolUseAtLocation` — same two independent checks Q010
     used — confirming every reference in the generated JSON serializer to a `PersonLenses` binding
     resolves to a symbol whose `DeclarationLocation.FileName = fileB`, i.e. the **real**,
     Fantomas-formatted, `LensesGenerator`-produced file, not a hand-typed stand-in and not a
     textual coincidence.

**Explicitly out of scope** (per `00-hypothesis.md` precondition 8): no watcher, no `.fsproj`
touch, no FSAC/Ionide session — this is one `ParseAndCheckProject` call, once, in a standalone
process.

## Reproduction

```
dotnet build src/Myriad.Core/Myriad.Core.fsproj -c Release
dotnet build src/Myriad.Plugins/Myriad.Plugins.fsproj -c Release
dotnet build experiments/Q026-real-generator-reentrant-composition/artifacts/Q026.Bridge -c Release
dotnet build experiments/Q026-real-generator-reentrant-composition/artifacts/ReentrantJsonGenerator -c Release
dotnet run --project experiments/Q026-real-generator-reentrant-composition/artifacts/Harness -c Release
```

Package versions pinned throughout: `FSharp.Compiler.Service 43.9.101`, `Fantomas.Core 7.0.5`
(both read directly from `paket.lock` this session, matching every prior Thread-1 quartet).

## What would make this NULL / KILL / REVISE, stated before running

- **KILL** if Round 1 itself fails — the real `LensesGenerator`, invoked standalone outside
  `Program.fs`'s process, throws, hangs, or cannot locate/parse `Person.fs` for a reason connected
  to running outside the real CLI (e.g. `EditorConfig.readConfiguration` needing something an
  ordinary process doesn't have, or a hidden static/AppDomain dependency). This would be a cheap,
  valuable, real finding about Myriad's generator code's portability, independent of anything to do
  with FCS or reentrancy.
- **REVISE** if Round 1 passes but Round 2 fails only for reasons specific to composing two real
  generators through the side-channel (e.g. assembly-identity mismatch between the `Q026.Bridge.dll`
  the Harness loaded and the one `ReentrantJsonGenerator.dll` was built against, since the latter is
  loaded via `Assembly.LoadFrom` rather than a shared in-process reference) — a real, actionable
  plumbing problem distinct from Q010's already-proven mechanism.
- **REVISE**, differently, if Round 2 succeeds mechanically but the generated `PersonLenses` module
  the real `LensesGenerator` produces doesn't shape the way the typed-inspection code (copied from
  Q010) expects, forcing a hand-adjustment — worth reporting honestly rather than silently patching,
  since it would show Q010's toy stand-in wasn't fully faithful to the real generator's actual
  output shape.
- **SHIP, scoped** if both rounds pass with the same evidentiary bar Q010 used (sentinel-confirmed
  non-bypass, zero diagnostics, two independent symbol-resolution checks) — scoped explicitly, per
  `00-hypothesis.md`'s own named "deepest open question," to "the composition mechanism works when
  hosted in a new, standalone process," not "Myriad's actual CLI already supports this."
