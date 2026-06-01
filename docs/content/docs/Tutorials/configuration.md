---
title: Configuration And Plugins
category: tutorial
menu_order: 5
pre: "<i class='fas fa-cog'></i> "

---

# Configuration

Myriad uses a configuration file called `myriad.toml` which uses the [toml format](https://toml.io/en/).  Myriad looks in the current directory fro this file but it can be configured by placing `MyriadConfigFile` in your project file like this:

```msbuild
<?xml version="1.0" encoding="utf-8"?>
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net9.0</TargetFramework>
    <OutputType>Exe</OutputType>
    <MyriadConfigFile>myConfig.toml</MyriadConfigFile>
  </PropertyGroup>
...
```

The configuration section for the built in plugins is very simple, the configuration name or key is passed in by either the plugins generator attribute, in the case of the fields plugin:

```
[<Generator.Fields "fields">]
type Test1 = { one: int; two: string; three: float; four: float32 }

```

The configuration key is "fields", if we now look at the `myriad.toml` file it looks like this:

```toml
[fields]
namespace = "TestFields"
```

You can see that there is a `namespace` key that is set to "TestFields".  Using an attribute like this you can configure your plugin to take further information on how to generate the code.  

There is another mechanism of specifying what configuration a plugin will use and that is by using an MSBuild property `MyriadConfigKey` as follows:

```xml
<Compile Include="ArbitaryFile.fs">
    <MyriadFile>Test.txt</MyriadFile>
    <MyriadConfigKey>example1</MyriadConfigKey>
</Compile>
```

The configuration key `example1` would be passed to the plugin so that it could be consumed.  This is used in the `Myriad.Plugin.Example1` plugin and use uses configuration to specify the `namespace` used.  

# Writing plugins to consume configuration

The plugin interface receives this key via: `GeneratorContext` . `ConfigKey`:

```fsharp
type GeneratorContext = {
    ConfigKey: string option
    ConfigGetter: string -> (string * obj) seq
    InputFilename: string
    ProjectContext: ProjectContext option
    AdditionalParameters: IDictionary<string, string>
}

type Output =
    | Ast of SynModuleOrNamespace list
    | Source of string

type IMyriadGenerator =
    abstract member ValidInputExtensions: string seq
    abstract member Generate: GeneratorContext -> Output
```

ValidInputExtensions is used so a plugin is tied to certain file types as input, if you plugin generating ast fragments from java files then you would add `.java` to the `ValidInputExtensions` implementation for example.  

```fsharp
    interface IMyriadGenerator with
        member __.ValidInputExtensions = seq {".java"}
        //...
```

While we are discussing the `IMyriadGenerator` interface lets discuss the `Generate` member:

Generate takes a `GeneratorContext` which contains the `ConfigKey` as mentioned above, the `ConfigGetter` : `string -> (string * obj) seq` which is a mean to allow access to the `myriad.toml` file, you give the function the configuration ket you wish to receive and it returns the configuration.  Finally a `InputFilename` is also passed in so you can load or parse your input files ready to generate an AST.  If you look at the included plugins you can see the mechanism for extracting and building AST fragments for reference.  

# Code Formatting and .editorconfig

Myriad formats generated code using the [Fantomas](https://fsprojects.github.io/fantomas/) code formatter. By default it uses `FormatConfig.Default`, but if an `.editorconfig` file exists in or above the output file's directory, Myriad will automatically apply those settings.

Any standard Fantomas `.editorconfig` properties are supported, for example:

```ini
[*.fs]
indent_size = 2
max_line_length = 100
fsharp_space_before_colon = true
```

This means generated files will match your project's formatting conventions without requiring them to be listed in `.fantomasignore`. The `.editorconfig` lookup is scoped to the output file path, so different output files in different directories can use different formatting settings.
