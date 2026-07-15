---
name: new-generator
description: Scaffold a new built-in Myriad plugin generator (attribute, AST-builder module, fsproj wiring, test fixture, myriad.toml section, Tests.fs assertions). Use when adding a new code-generation plugin to Myriad.Plugins, e.g. "add a generator that produces X".
---

Scaffold a new Myriad generator for: $ARGUMENTS

Follow the pattern already used by `DUCasesGenerator.fs`, `FieldsGenerator.fs`, and
`LensesGenerator.fs` in `src/Myriad.Plugins/`. Read at least one of those (DUCasesGenerator.fs
is the shortest) before writing anything, to match naming and structure exactly.

## Steps

1. **Add the attribute** in `src/Myriad.Plugins/Attribute.fs`, inside the `Generator` module:
   ```fsharp
   type <Name>Attribute(configGroup: string) =
       inherit Attribute()
   ```
   The `configGroup` string is what users pass as `[<Generator.<Name> "sectionName">]`, and it
   maps to a `[sectionName]` table in `myriad.toml`.

2. **Create `src/Myriad.Plugins/<Name>Generator.fs`** with two parts:
   - An `internal module Create<Name>Module` that builds the output AST using
     `Fantomas.FCS.Syntax` types (`SynModuleOrNamespace`, `SynTypeDefn`, `SynExpr`, `SynPat`, …).
     Reuse `GeneratorHelpers` for shared idioms (see [[myriad-ast-conventions]]) instead of
     re-deriving them — e.g. `GeneratorHelpers.getCaseIdent`, `resolveCaseIdent`,
     `createTypedNamedParen`, `generateModules`.
   - A public type registered with the generator pipeline:
     ```fsharp
     [<MyriadGenerator("<configKey>")>]
     type <Name>Generator() =
         interface IMyriadGenerator with
             member _.ValidInputExtensions = seq {".fs"}
             member _.Generate(context: GeneratorContext) =
                 GeneratorHelpers.generateModules<Generator.<Name>Attribute> context Ast.extract<Whatever> Create<Name>Module.create<Whatever>Module
     ```
     Use `GeneratorHelpers.generateModules` when the generator follows the standard
     "parse → extract typed decls → filter by attribute → build a module per type" shape (this
     is what DUCasesGenerator and FieldsGenerator do). Write a bespoke `Generate` body only if
     the shape genuinely differs (LensesGenerator does, because of wrapper-type handling).

3. **Register the file** in `src/Myriad.Plugins/Myriad.Plugins.fsproj`, in the `<ItemGroup>`
   `<Compile>` list. It must come after `Attribute.fs`, `GeneratorConfig.fs`, and
   `GeneratorHelpers.fs` (F# compiles top-to-bottom and these are shared dependencies); order
   relative to the other `*Generator.fs` files doesn't matter.

4. **Add a myriad.toml section** in `test/Myriad.IntegrationPluginTests/myriad.toml` matching
   the config-group name you'll use in the test fixture, e.g.:
   ```toml
   [<sectionName>]
   namespace = "Test<Name>"
   ```

5. **Decorate a test type** in `test/Myriad.IntegrationPluginTests/Input.fs` with the new
   attribute, e.g. `[<Generator.<Name> "<sectionName>">]`. If you want to exercise the
   inline-generation path too, add an equivalent type to `InputSelfGenerate.fs` and list the
   new generator in its `<Generators>` MSBuild item in
   `test/Myriad.IntegrationPluginTests/Myriad.IntegrationPluginTests.fsproj`.

6. **Add assertions** to `test/Myriad.IntegrationPluginTests/Tests.fs` under the `tests`
   `testList`, following the `Expect.equal` style already used for `TestFields`/`TestDus`/
   `TestLens` namespaces (the namespace comes from the `namespace = "..."` you set in step 4).

7. **Build and run** to verify generation actually happens and the fixture compiles:
   ```
   dotnet build src/Myriad/Myriad.fsproj -c Debug
   dotnet run --framework net9.0 --project ./test/Myriad.IntegrationPluginTests/Myriad.IntegrationPluginTests.fsproj
   ```
   This mirrors the `Test` target in `build.proj` and is what CI runs (`-t:Test`).

Report which files you touched and the test output at the end — don't claim the generator
works without having run step 7.
