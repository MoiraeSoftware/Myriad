---
title: External Plugins
category: tutorial
menu_order: 3
pre: "<i class='fas fa-external-link-alt'></i> "

---

# Using external plugins

To consume external plugins that aren't included in the `Myriad.Plugins` package, you must register them with Myriad. If you are using the CLI tool then the way to do this is by passing in the `--plugin <path to dll>` command-line argument. If you are using MSBuild then this can be done by adding to the `MyriadSdkGenerator` property to your project file:

```xml
<ItemGroup>
    <MyriadSdkGenerator Include="<path to plugin dll>" />
</ItemGroup>
```

For example, if you had a project layout like this:

```
\src
-\GeneratorLib
 - Generator.fs
 - Generator.fsproj
-\GeneratorTests
 - Tests.fs
 - GeneratorTests.fsproj
```

You would add the following to Generator.fsproj:
```xml
  <ItemGroup>
    <Content Include="build\Generator.props">
      <Pack>true</Pack>
      <PackagePath>%(Identity)</PackagePath>
      <Visible>true</Visible>
    </Content>
  </ItemGroup>
```

Then add a new folder `build` with the `Generator.props` file within:
```xml
<Project>
    <ItemGroup>
        <MyriadSdkGenerator Include="$(MSBuildThisFileDirectory)/../lib/net9.0/Generator.dll" />
    </ItemGroup>
</Project>
```

Often an additional props file (In this smaple the file would be `Generator.InTest.props`) is used to make testing easier.  The matching element for the tests fsproj would be something like this:

```xml
<Project>
    <ItemGroup>
        <MyriadSdkGenerator Include="$(MSBuildThisFileDirectory)/../bin/$(Configuration)/net9.0/Generator.dll" />
    </ItemGroup>
</Project>
```

Notice the Include path is pointing locally rather than within the packaged nuget folder structure.

In your testing `fsproj` you would add the following to allow the plugin to be used locally rather that having to consume a nuget package:

```xml
<!-- include plugin -->
<Import Project="<Path to Generator plugin location>\build\Myriad.Plugins.InTest.props" />
```

## AST Helper Utilities

Myriad provides a number of convenience helpers in `Myriad.Core.AstExtensions` to simplify building F# AST nodes in plugins.

### `SynType.CreateFromLongIdent`

Converts a `LongIdent` directly to a `SynType`. This is equivalent to chaining `SynLongIdent.Create` and `SynType.CreateLongIdent`, but expressed as a single call:

```fsharp
open Myriad.Core

// Instead of:
let synType =
    SynLongIdent.Create (ident |> List.map (fun i -> i.idText))
    |> SynType.CreateLongIdent

// You can now write:
let synType = SynType.CreateFromLongIdent ident
```

This is useful when your plugin receives a `LongIdent` (e.g. from the namespace or type name in the parsed input) and needs to reference it as a type annotation in the generated AST.

## Framework Compatibility

Myriad supports loading plugins built against a **different target framework** than the Myriad tool itself. For example, a plugin targeting `net8.0` can be loaded by the `net9.0` Myriad tool without errors.

The plugin loader uses `PreferSharedTypes = true`, which means assemblies already loaded by the Myriad host — such as `FSharp.Core` and `Fantomas.FCS` — are shared with the plugin rather than loaded again from the plugin's output directory. This prevents `System.Reflection.ReflectionTypeLoadException` that would otherwise occur due to type identity mismatches across `AssemblyLoadContext` boundaries.

> **Note:** While cross-framework plugin loading is supported, it is still recommended to target the same framework version as the Myriad tool for the most predictable behaviour.

## Resolving Literal Constant References in Attribute Arguments

When a type uses a `[<Literal>]`-attributed identifier as an attribute argument, the standard `Ast.getAttributeConstants` helper returns the identifier name rather than the constant value. For example:

```fsharp
module A =
    let [<Literal>] MyConst = "Hello"

    [<MyAttribute(MyConst)>]
    type Thing = { Foo: string }
```

To resolve `MyConst` to `"Hello"` in your plugin, use the two-step API in `Myriad.Core.Ast`:

1. **`Ast.extractLiteralBindings`** — scans the entire parsed AST and returns a `Map<string, SynConst>` of all `[<Literal>]` bindings.
2. **`Ast.getAttributeConstantsWithBindings`** — works like `getAttributeConstants` but also looks up any identifier arguments in the bindings map.

```fsharp
open Myriad.Core

// In your Generate implementation:
let bindings = Ast.extractLiteralBindings ast  // ast : ParsedInput from GeneratorContext

let constants =
    someAttribute
    |> Ast.getAttributeConstantsWithBindings bindings
// constants : string list — identifier references are now resolved to their literal values
```

This avoids the need for type-checking (which would be significantly slower) and works entirely from the parsed AST available to every Myriad plugin.
