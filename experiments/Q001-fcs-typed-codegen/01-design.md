# myriad-fcs-typed-codegen / Movement 2 — Design

**Status:** DONE (all four rounds executed; see 02-results.md).
**Location:** scratch console project, `dotnet new console -lang F#`, package
`FSharp.Compiler.Service` pinned to `43.9.101` (matches `Myriad/paket.lock`). Not part of any
committed repo.

## Round 1 — in-process hosting + typed alias resolution

Host `FSharpChecker` with `DocumentSource.Custom` backing two virtual files (`A.fs`, `Generated.fs`,
neither ever written to disk). `A.fs` declares `type Id = int` and a record `Person` with a field
typed as the alias. Extract `Person`'s fields via the *typed* tree
(`FSharpCheckFileResults.ImplementationFile.Declarations`, `FSharpEntity.FSharpFields`,
`FSharpField.FieldType`), generate a companion module referencing `Person`, splice it back via the
same `DocumentSource.Custom` callback, and typecheck it in the same `FSharpProjectOptions`.

Key API points used: `FSharpChecker.Create(keepAssemblyContents = true, documentSource = ...)`,
`GetProjectOptionsFromScript` (single-file, zero MSBuild), `ParseAndCheckFileInProject`,
`FSharpType.StripAbbreviations().Format(ctx)` to resolve the alias.

## Round 2 — incrementality and staleness

Same two-file setup. Sequence: cold check both files; re-check unchanged (same/bumped version);
mutate the downstream file only; then the real test — introduce a *breaking* change in the
upstream file (remove a field the downstream file reads) and re-check the downstream file under
three conditions: (a) unchanged version, (b) bumped version, (c) after an explicit
`checker.InvalidateConfiguration(options)` call. Timed every check with `System.Diagnostics.Stopwatch`.

## Round 3 — scaling comparison, BackgroundCompiler vs TransparentCompiler

Extended the harness with N synthetic "padding" files (`module PadI ... let xI = I`) inserted into
`FSharpProjectOptions.SourceFiles` ahead of the real dependency pair, at N=2 and N=60. Ran the
Round 2 "break the dependency, recheck downstream" scenario under two checker configurations:
default (`useTransparentCompiler = false`, i.e. `BackgroundCompiler`) and
`useTransparentCompiler = true`. Recorded initial full-project warm time and the post-break
recheck time for each.

Caveat carried forward honestly: padding files are near-free to typecheck (`let x = 5`-level), so
this round stress-tests correctness/API behavior more than it stress-tests raw per-file typecheck
cost at scale — a rigorous scaling number needs padding files with real typecheck weight (opens,
generics, inference), which this round did not build.

## Round 4 — port a real Myriad generator (Fields)

Read `Myriad/src/Myriad.Plugins/FieldsGenerator.fs` (the actual shipping generator: for a record,
emits one accessor per field, a `create` taking every field as a camelCased parameter, and a `map`
taking one mapper function per field). Reimplemented its output shape driven by typed data instead
of `Fantomas.FCS.Syntax.SynField`:

- Attribute discovery via the typed attribute list (`FSharpEntity.Attributes`,
  `FSharpAttribute.AttributeType.DisplayName`) instead of a syntax-level attribute-name match.
- Field types rendered via `checkedA.GetDisplayContextForPos(entity.DeclarationLocation.Start)` —
  the alias-preserving, opens-aware display context (what Ionide tooltips use), not the
  alias-stripped one from Round 1 — since idiomatic generated output should say `Id`, not
  `System.Int32`, when `Id` is in scope.
- Spliced the generated module back exactly as in Rounds 1–3 and typechecked it.
- Separately printed the same `Age` field's type both alias-preserving and alias-stripped, to make
  the "does typed access actually change anything for this generator" comparison explicit and
  inspectable rather than assumed.

## Reproduction

```
dotnet new console -lang F# -o fcs-spike
cd fcs-spike
dotnet add package FSharp.Compiler.Service --version 43.9.101
# Program.fs per round, see 02-results.md for the actual output of each
dotnet run
```

No MSBuild, no `Myriad.Sdk`, no real `.fsproj` beyond the throwaway host's own — deliberate, since
the question under test is what the compiler-hosting layer can do, not the MSBuild integration
layer Myriad already has working.
