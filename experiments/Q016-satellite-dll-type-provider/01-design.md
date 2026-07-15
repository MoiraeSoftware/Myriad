# Q016-satellite-dll-type-provider / Movement 2 — Design

**Status:** RUNNING.

## What gets built

Six pieces, mirroring the shape every Thread 2 quartet since Q008 has used (runtime/design-time
provider pair + harness), plus a new element none of them needed: a real Myriad-produced satellite.

1. **`SampleLib/`** — the "project A" input. `Person.fs`:
   ```fsharp
   namespace SampleNs
   open Myriad.Plugins
   [<Generator.Fields "fields">]
   type Person = { name: string; age: int }
   ```
   and `myriad.toml`:
   ```toml
   [fields]
   namespace = "SampleFields"
   ```
   Lowercase field identifiers, matching this repo's own `README.md` Fields example exactly, so the
   generated getter function names (`name`, `age`) and `create`'s camelCase parameters
   (`Ast.Ident.asCamelCase`, `src/Myriad.Plugins/FieldsGenerator.fs:43`) don't collide in casing.

2. **Real Myriad CLI run, unmodified.** `src/Myriad.Plugins/Myriad.Plugins.fsproj` and
   `src/Myriad/Myriad.fsproj` built once (`dotnet build -c Release`, no source edits — confirmed
   already buildable in this repo via `dotnet tool restore` then `dotnet build`). The built
   `Myriad.dll` is invoked exactly as `README.md`'s own MSBuild-equivalent CLI usage describes:
   ```
   dotnet <repo>/src/Myriad/bin/Release/net9.0/Myriad.dll \
     --inputfile Person.fs --outputfile Generated.fs \
     --plugin <repo>/src/Myriad.Plugins/bin/Release/net9.0/Myriad.Plugins.dll \
     --configfile myriad.toml
   ```
   `Generated.fs` is inspected for Myriad's own header comment (`Generation.getHeaderedCode`) before
   being trusted as real output, per this quartet's own validity precondition.

3. **Satellite compile.** A small F# program (matching Q011/Q012/Q014/Q015's own `checker.Compile`
   pattern) compiles `Person.fs` + `Generated.fs` together — mirroring how a real consuming project's
   `<Compile>` list actually looks (source file, then its Myriad-generated sibling) — into
   `Satellite.dll`, referencing the already-built `Myriad.Plugins.dll`/`Myriad.Core.dll` (needed only
   for the `Generator.Fields` attribute on `Person.fs`; `Generated.fs` itself has no Myriad
   dependency). `Person`'s CLR type and `SampleFields.Person`'s `create`/`name`/`age` static methods
   are therefore real, fully-resolved compiled members the moment `Satellite.dll` exists — nothing
   about them is provider-specific until Round 1 wraps them.

4. **`SatelliteTP.Runtime` / `SatelliteTP.DesignTime`** — the generative provider pair itself,
   structured exactly like every real Thread 2 provider (`TypeProviderAssembly` attribute in Runtime,
   `assemblyReplacementMap` in DesignTime, vendored `ProvidedTypes.fs`/`.fsi` copied forward from
   `Q012-compiler-behavior-probe/artifacts/vendor/` for exact reproducibility). Static parameters:
   `SatelliteDllPath: string`, `ModuleTypeName: string` (e.g. `"SampleFields.Person"`),
   `RecordTypeName: string` (e.g. `"SampleNs.Person"`). `ApplyStaticArguments`:
   - `Assembly.Load(File.ReadAllBytes satelliteDllPath)` — **`Load(bytes)`, not `LoadFrom(path)`,
     chosen from the start per Q011's own already-learned lesson** (`LoadFrom` was observed there to
     hold a Windows file lock for the process's lifetime, blocking a later rebuild of the same path;
     `Load(bytes)` reads and closes). An `AppDomain.AssemblyResolve` hook (same pattern as
     `Q011-consumer-driven-contracts/artifacts/SchemaTP.DesignTime/SchemaProvider.fs`'s
     `AssemblyProbing` module) resolves `Satellite.dll`'s own co-located dependency
     (`Myriad.Plugins.dll`/`Myriad.Core.dll`, needed at runtime only if `Person`'s attribute metadata
     is touched — checked, not assumed).
   - Reflect `moduleType.GetMethod("create")`, `.GetMethod("name")`, `.GetMethod("age")` and the real
     `recordType` via `Assembly.GetType`.
   - Build `ProvidedMethod("Create", [name: string; age: int], recordType, isStatic = true,
     invokeCode = fun args -> Expr.Call(realCreateMi, args))` and
     `ProvidedMethod("GetName", [person: recordType], typeof<string>, isStatic = true, invokeCode =
     fun args -> Expr.Call(realNameMi, args))` — genuine reflection-forwarding: the emitted IL calls
     Myriad's real compiled methods directly, the provider contributes no logic of its own.
   - A `FileSystemWatcher` on `satelliteDllPath`'s directory, filtered to the DLL's filename, calls
     `this.Invalidate()` on `Changed`/`Created` (the mechanism `BACKLOG.md`'s "External-signal
     `Invalidate()`" idea names, never previously built in this repo) — set up once, memoized the same
     defensive way Q011 found necessary for `DefineStaticParameters` in general.

5. **`Harness/`** — `checker.Compile` (not `ParseAndCheckFileInProject`; Q012/Q013 already settled
   that generative types need a real compile, and Q008/Q09's 2026-07-16 reconstruction found real
   `FSharpProjectOptions`-based PC also works, but re-litigating which checking API to use is not this
   quartet's question) of a consumer instantiating
   `SatelliteTP.Provided.MyriadSatellite<Satellite.dll path, "SampleFields.Person", "SampleNs.Person">`
   and calling `.Create("Ada", 42)` / `.GetName(p)`. Two independent correctness checks, matching this
   quartet's own pre-registered falsifier: (a) call `Satellite.dll`'s real `create`/`name` directly via
   reflection from the harness, outside the provider entirely; (b) load the harness's *compiled
   consumer output* and read back the provided members' return values via reflection (the Q011/Q012
   readback pattern). Both must agree with each other and with a plain call to the generated function.

6. **Regeneration round.** `Person.fs` gets a real edit (`email: string` added to the record), the
   *same* Myriad CLI command reruns unmodified, `Generated.fs` is confirmed to now contain a `create`
   with three parameters and an `email` getter, `Satellite.dll` is recompiled to the *same path*
   (overwrite), and — on the *same* long-lived harness process/checker instance that already loaded
   the v1 satellite — a new consumer referencing `GetEmail` is checked. Confirms: (a) no file-lock
   exception on the DLL overwrite (the named Windows risk from `BACKLOG.md` item 15, real and
   previously untested); (b) after the `FileSystemWatcher`-driven `Invalidate()` fires, the new
   `GetEmail` member resolves with zero diagnostics on a fresh `checker.Compile` of the new consumer,
   without restarting the process.

## What's measured

- Round 1 (falsifier): pass/fail on the two-independent-check agreement; wall-clock for satellite
  compile, provider resolution, and readback, single-sample per this file's own cross-cutting caveat.
- Round 2 (regeneration): pass/fail on (a) no file-lock exception, (b) `GetEmail` resolves post-
  `Invalidate()` with zero diagnostics, (c) time from DLL overwrite to `Invalidate()` firing (the
  `FileSystemWatcher` event is async — measured, not assumed instantaneous).
- Round 3 (cost, explicitly single-sample): end-to-end wall-clock for the whole pipeline — Myriad CLI
  run, satellite compile, provider resolution, consumer compile — compared to Q006's 47ms/1137ms and
  Q008's 19-32ms/1161ms bands for scale, not as a claim of equivalence (this quartet's payload does
  more real work than either).

## How it's run / reproduced

Scratch build under `$CLAUDE_JOB_DIR/tmp/q016-satellite-dll-spike/`, durable source and run logs copied
to `artifacts/` on completion, same convention as every prior quartet. Build order: `src/Myriad.Plugins`
and `src/Myriad` in this repo (already built for this session), `SatelliteTP.Runtime` (pulls in
`SatelliteTP.DesignTime`), then the `SampleLib`-to-`Satellite.dll` pipeline (a small F# console program,
not a manual script, so Round 2's regeneration is a real rerun of the same code path, not hand-edited),
then `Harness`. FCS pinned to `43.9.101` throughout, matching every FCS-hosting quartet in this repo.

## Deviations expected, to report honestly if they occur

- Whether `Expr.Call(mi, args)` against a `MethodInfo` obtained via `Assembly.Load(bytes)` (not a
  compile-time-known one) is accepted by the SDK's IL-emission path the same way a compile-time
  `MethodInfo` would be — not verified by reading `ProvidedTypes.fs`, only by running it. If it isn't,
  the fallback is emitting the call by a lower-level `ILGenerator`-style thunk, which would itself be a
  reportable finding, not silently worked around.
- Whether `FileSystemWatcher` fires reliably and promptly enough inside a design-time provider host
  (as opposed to a normal process) to make Round 2's timing meaningful, or whether a short explicit wait
  is needed before the re-check — if so, the wait duration used will be reported, not hidden.
