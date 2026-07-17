namespace Myriad.Core

open System
open System.Collections.Generic
open Fantomas.FCS.Syntax
open Fantomas.FCS.Text.Range

type MyriadGeneratorAttribute(name: string) =
    inherit Attribute()
    member _.Name = name

type ProjectContext =
    { project: string
      projectPath: string
      refs: string array
      compileBefore: string array
      compile: string array
      compileAfter: string array
      defineConstants: string array }

type GeneratorContext =
    { ConfigKey: string option
      ConfigGetter: string -> (string * obj) seq
      InputFilename: string
      ProjectContext: ProjectContext option
      AdditionalParameters: IDictionary<string, string> }

    static member Create(configKey, configHandler, inputFile, projectContext, additionalParams) =
        { ConfigKey = configKey
          ConfigGetter = configHandler
          InputFilename = inputFile
          ProjectContext = projectContext
          AdditionalParameters = additionalParams }

[<RequireQualifiedAccess>]
type Output =
    | Ast of SynModuleOrNamespace list
    | Source of string

type IMyriadGenerator =
    abstract member ValidInputExtensions : string seq
    abstract member Generate : GeneratorContext -> Output

[<RequireQualifiedAccess>]
type DiagnosticSeverity =
    | Error
    | Warning
    | Info

/// A non-fatal generator diagnostic, rendered as a canonical MSBuild line
/// (`path(line,col,line,col): severity CODE: message`) so it surfaces the same way a
/// compiler warning/error does, without requiring the generator to throw and abort the build.
type MyriadDiagnostic =
    { Code: string
      Message: string
      Severity: DiagnosticSeverity
      Range: Fantomas.FCS.Text.Range option }

/// Opt-in extension of IMyriadGenerator for generators that want to report diagnostics
/// (warnings, info) alongside their output, rather than only being able to fail the whole
/// build by throwing. Existing IMyriadGenerator implementations are unaffected.
type IMyriadGeneratorWithDiagnostics =
    inherit IMyriadGenerator
    abstract member GenerateWithDiagnostics : GeneratorContext -> Output option * MyriadDiagnostic list
