# Q002-typed-nested-dispatch / Movement 2 — Design

**Status:** DONE.
**Location:** `artifacts/nested-dispatch/`, one console project, `FSharp.Compiler.Service`
pinned to `43.9.101` (Myriad's own pin), same pattern as Q001.

## Setup

Three virtual files (`DocumentSource.Custom`, `useTransparentCompiler = true`, per Q001's
findings — no reason to retest `BackgroundCompiler` here, Q001 already settled that choice):

- `Address.fs`: declares `FieldsAttribute` and `[<Fields("fields")>] type Address = { Street: string; City: string }`.
- `Person.fs`: `open Address`, declares `[<Fields("fields")>] type Person = { Name: string; HomeAddress: Address }`.
- `Generated.fs`: the spliced output, written by the generator logic below.

## Round A — cheapest falsifier

Typecheck `Person.fs`, get `Person`'s `FSharpEntity`, get the `HomeAddress` field's
`FSharpField.FieldType`. Check: does `.HasTypeDefinition` hold, does `.TypeDefinition` resolve to
an entity whose `DeclarationLocation.FileName` is the *other* virtual file (`Address.fs`, not
`Person.fs`), and does that resolved entity's `.Attributes` show the `Fields` attribute? If any of
these fail, stop — the capability claim is dead before any generator code is written.

## Round B — the actual generator

If Round A passes: a `describe`-function generator. For each field of an attributed record:
- if `FieldType.HasTypeDefinition` and that type definition itself carries the `Fields` attribute
  → emit a recursive call into the nested type's own generated `describe` function
  (`Address.describe x.HomeAddress`)
- else → emit a direct format (`sprintf "%O" x.Field`)

Run this for both `Address` (all-primitive) and `Person` (one nested field), emit both modules
into one `namespace rec TestDescribe` block in `Generated.fs`, splice in-memory, typecheck against
both source files in the same project.

## Round C — syntax-only comparison

Not built as running code. Grounded instead in two verified facts about Myriad's actual current
plugin API (read directly, not assumed):

1. `GeneratorContext` (`src/Myriad.Core/Types.fs`) exposes exactly one parsed file:
   `InputFilename: string` — a path, and generators receive that *one* file's `ParsedInput`.
   `ProjectContext.compile: string array` gives sibling file *paths*, not their parsed ASTs.
2. `GeneratorHelpers.generateModules` (`src/Myriad.Plugins/GeneratorHelpers.fs:47`), the shared
   helper all three built-in generators (`Fields`, `Lenses`, `DUCases`) call into, takes a single
   `ParsedInput` parameter — confirmed by reading its signature, not inferred from the generators
   built on top of it.

Building a working syntax-only cross-file resolver (parse every path in `ProjectContext.compile`
via `Fantomas.FCS.Parse`, walk each AST, string-match type names, read attribute lists) was judged
not worth doing as running code for this round: the technical *possibility* was never in question
(Fantomas.FCS is public, nothing stops a plugin author from calling it), so a toy resolver would
mostly restate that. The actual claim under test is engineering cost — no caching, no incremental
reuse, reimplemented per plugin — and that claim rests on the two facts above, not on a benchmark.
This is a real limitation of this round: the comparison is reasoned from source, not measured
against a build. See `03-review.md`.

## Reproduction

```
cd artifacts/nested-dispatch
dotnet run
```
