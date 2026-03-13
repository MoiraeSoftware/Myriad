# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.8.6] - 2026-03-13

### Fixed
- Publish workflow now supports manual dispatch via `workflow_dispatch` input, uses `$GITHUB_OUTPUT` instead of deprecated `::set-output`, and replaces archived GitHub Actions with `softprops/action-gh-release@v2`. A missing `dotnet paket restore` step was also added before build and test steps. (#234)
- `Myriad.Sdk.proj` now uses `PackageDownload` instead of `PackageReference` for the `Myriad` tool dependency, resolving **NU1102** and **NU1212** errors that caused every publish attempt to fail since v0.8.4. Consumers of `Myriad.Sdk` also get the matching `Myriad` package placed in the NuGet cache automatically via a `PackageDownload` item in `Myriad.Sdk.props`. (#236)
- `build.proj` Pack target now normalises 2-segment version numbers (e.g. `0.85`) to 3-segment (`0.85.0`) before passing them to `dotnet pack`/`dotnet restore`, preventing a Paket nuspec filename mismatch that caused pack to fail when using versions like `/p:Version=0.85`. (#238)
- Publish workflow now requires 3-segment version tags (e.g. `v0.8.5`, not `v0.85`) and validates that the tag version matches `VersionPrefix` in `Directory.Build.props`, preventing accidental releases under the wrong version number (e.g. `0.85.0` instead of `0.8.5`). (#239, #252)

### Changed
- `Ast.extractTypeDefn` and `Ast.ModuleOrNamespace.getTypeDefns` now share a single private `extractTypesFromDecls` helper, eliminating an identical nested `extractTypes` definition that was duplicated in both functions. (#262)
- `resolveCaseIdent` helper extracted from `DUCasesGenerator` into `GeneratorHelpers`, eliminating duplication with `LensesGenerator`. (#253)
- `generateModules` helper extracted into `GeneratorHelpers`, eliminating the duplicate `Generate` pipeline boilerplate in `DUCasesGenerator` and `FieldsGenerator`. (#241, #255)
- `filterTypes` helper extracted into `Ast`, eliminating five identical inline filter expressions. (#250)
- Expecto test framework updated from 10.2.1 to 10.2.3 (patch). (#243)

## [0.8.5]
### Fixed
- Custom plugins built against a different target framework than the Myriad tool no longer fail with `System.Reflection.ReflectionTypeLoadException`. The plugin loader now sets `PreferSharedTypes = true` so that assemblies already loaded by the host (such as `FSharp.Core` and `Fantomas.FCS`) are shared with the plugin instead of being loaded again from the plugin's output directory. Type load failures are also handled gracefully so that generators that do load are not lost.

### Added
- `SynType.CreateFromLongIdent` helper in `Myriad.Core.AstExtensions`. Plugin authors can now convert a `LongIdent` to a `SynType` using this single convenience method instead of manually chaining `SynLongIdent.Create` and `SynType.CreateLongIdent`.
- Generated code is now formatted using `.editorconfig` settings applicable to the output file. Myriad previously formatted all generated code with `FormatConfig.Default`, ignoring any Fantomas settings in `.editorconfig`. The new `Myriad.Core.EditorConfig.readConfiguration` function reads the `.editorconfig` file for the output path and maps `indent_size`, `max_line_length`, `end_of_line`, `insert_final_newline`, and all `fsharp_*` Fantomas properties to the corresponding `FormatConfig` values. If no `.editorconfig` is present, `FormatConfig.Default` is used as before.
- `Ast.extractLiteralBindings` and `Ast.getAttributeConstantsWithBindings` helpers in `Myriad.Core`. Plugin authors can now resolve `[<Literal>]`-attributed identifier references in attribute arguments to their actual string values — for example when a type is annotated with `[<MyAttr(MyConst)>]` where `let [<Literal>] MyConst = "Hello"` is defined elsewhere in the same file.

### Fixed
- Myriad now correctly processes input files that contain F# 8 dot-lambda (`_.property`) shorthand syntax. Previously, types using `_.` in `with member` bodies would cause a parse error:

  ```
  Fantomas.Core.ParseException: Unexpected symbol '_' in expression
  ```

  This was resolved by upgrading `Fantomas.Core` and `Fantomas.FCS` from `7.0.0` to `7.0.5`, which includes full support for `SynExpr.DotLambda`. For example, the following now works correctly:

  ```fsharp
  type MyType = {
      items: MyItem list
  } with
      member self.itemIds = self.items |> List.map _.id
  ```

## [0.8.4]
## Changed
- Upgraded to net9.0
  - Updated Nuget packages
    Fantomas.Core to 7.0.0
    - FSharp.Core 9.0.101
    - Argu 6.2.5
    - FSharp.Compiler.Service 43.9.101
    - Tomlyn 0.18.0
    - Expecto 10.2.1
    - Aether 8.3.1
    - McMaster.NETCore.Plugins 2.0.0
- Removed `dotnet-mergenupkg` (broken in .NET 9 due to expired signature certificates)
- `Myriad` and `Myriad.Sdk` are now published as separate NuGet packages; `Myriad.Sdk` declares `Myriad` as a transitive dependency so no manual tool installation is required
- CI publish workflow now runs on `ubuntu-latest`
## [0.8.3]
## Fixed
Nuget package now has correct link to changelog
## Added
Readme added to Nuget package
## Changed
- Upgraded Fantomas.Core to 6.1.1.
    - Changed namespace of Fantomas.FCS. (Open `Fantomas.FCS` instead of `open FSharp.Compiler`.)
    - Changed type signatures in Fantomas.FCS. (Update code where affected.)
    - Fantomas.FCS is more sensitive to missing ranges. (Include a `Some range0` where one is needed, instead of `None`.)

## [0.8.2]
### Changed
Updated FSharp.Compiler.Service to 41.0.6 Thanks @AlexeyRaga
### Fixed
MyriadSdk path is now quoted which fixes #159 and allows the path to have spaces.  Thanks @lucasteles

## [0.8.1]
### Changed
- Added support for generator filters with msbuild property `<Generators>...</Generators>` only the named generators are run, all others are excluded, the Generators element is added under the Compile element as follows:
    ```
        <Compile Include="Input.fs">
            <MyriadParams>
                <MyriadParam1>1</MyriadParam1>
                <MyriadParam2>2</MyriadParam2>
            </MyriadParams>
            <Generators>LensesGenerator;FieldsGenerator;DUCasesGenerator</Generators>
        </Compile>
    ```

- If `MyriadConfigFile` is not specified assume it would be: `$(ProjectDirectory)\myriad.toml`.
- If there is no config file present an empty one is created.


## [0.8.0]
### Changed
- Updated to dotnet 6.0.202
- Various improvements to lens generation code

## [0.7.4]
### Changed
- Exceptions are no longer written to the generated files but are instead reported to the command line
With the advent on in line generation, errors out to the source code can be quite disruptive


## [0.7.3]
### Added
- Added ProjectPath to the project context so the full name of the project is know by the plugin rather than just its project name
- Added contextual loading of each plugin so transient assemblies are loaded relative to the plugin location.

## [0.7.2]
### Added
- Added netcoreapp3.0 target for the Myriad CLI as theres no CLI version of netstandard2.0

## [0.7.1]
### Added
- Added netstandard2.0 target

## [0.7.0]
### Added
- Added a project context to Myriad context that comprises all of the data 
available at pre compile time that is useful to code generation, especially 
when generating a typed Ast.  
- Added more extensions to Ast nodes so they can be constructed easier.  
### Changed
- Altered the generation code to use plain Ast nodes rather then wrapper records, in 
the long term depreciating Rcd prefixed types from FsAst, simple extensions are preferred 
and less verbose.  
### Breaking
- Altered the plugin API so that both Ast and string based output are possible via a 
discriminated union return type.  

## [0.6.4]
### Added
- More Ast extensions to make creating Ast nodes easier.  

## [0.6.3]
### Fixed
- Added stack trace to failing code generation.  

## [0.6.2]
### Added
- Cache invalidation has been improved for inline generation, both file hash and time stamp of 
the input file are now considered.

## [0.6.1]
### Fixed
- Targets file not using correct Using references.  
- Inline generation not using the input file prior to myriad generation being appended.  

## [0.6.0]
### Added
- Add Support for inline code generation, generated code appears at the end of the input file.
### Changed
- Removed Fornax
- Removed Fake
- Updated to dotnet5.0
- Rewrote the build script as msbuild

## [0.5.4]
- Add support from config arguments to be specified in the msbuild config via the sub elements 
using: <MyriadParams><Param>test</Param></MyriadParams>
- Add support for custom arguments via the --additionalparam myparam=test;myotherparam=test2
- Update FSAst to 0.9.0
- Add support for Aether via pipedsetters=true in myriad config for lens

## [0.5.2]
- Update FSAst to 0.8.0 - Thanks! @theangrybyrd

## [0.5.1]
### Added
- A global config file can now be specified in fsproj: `<MyriadConfigFile>someglobalconfig.toml</MyriadConfigFile>`
- The DU plugin can now use the `alwaysFullyQualify = true` setting to force qualification of DU's
### Fixed
- RequireQualifiedAccess is now respected by the DU plugin - Thanks! @matthewcrews

## [0.5.0]
###Added
- Allow toml config to be used in plugins
- Allow arbitrary inputs to be used
- Added a config key that can be added per MyriadFile iteration or as a CLI parameter
### Changed
- Depreciated usage of MyriadNamespace in MSBuild

## [0.4.1]
- Update to use netstandard 2.0 for maximum compatibility

## [0.4.0]
- Update FSAst 0.6, Fantomas 4.0.0-beta-001, FSharp.Compiler.Service 36.0.3

## [0.3.1]
- Added lens generation

## [0.2.8]
### Changed
- Updated to latest version of FsAst 0.5.0

## [0.2.7]
### Added
- Verbose output flag added to CLI tool for diagnostics
- PDB info is embedded in plugins for better debugging experience
- Updated compiler and fantomas assemblies

## [0.2.6]
### Added
- Update CI infrastructure and release pipeline

## [0.2.5]
### Added
- New feature - DuCases generator added
- New feature - Added map function to Fields plugin

## [0.2.4]
### Changed
- Fixed Example via direct reference to Myriad.Core

### Removed
- Remove duplicate ItemGroup reference

## [0.2.3]
### Changed
- Corrected issue with plugin targets using PropertyGroup rather than ItemGroup

## [0.2.2]
### Changed
- Fixed typo in msbuild cache

### Added
- Added core plugin to target props as this look to be not included in the nuget package

## [0.2.1]
### Added
- Added diagnostics

## [0.2.0]
### Changed
- Updated to dotnet 3.0

### Added
- Added Plugin API

## [0.1.0] - 2019-04-19
### Changed
- Stopped using Fields as a nested module.
- Opened namespace of enclosing record so references are found.
