namespace Myriad.Plugins.Example1

open System.IO
open Myriad.Core
open Myriad.Core.Ast
open Fantomas.FCS.Syntax

[<MyriadGenerator("example1")>]
type Example1Gen() =
    interface IMyriadGenerator with
        member _.ValidInputExtensions = seq {".txt"}
        member _.Generate(context: GeneratorContext) =

            let example1Namespace =
                context.ConfigKey
                |> Option.map context.ConfigGetter
                |> Option.bind (Seq.tryPick (fun (n,v) -> if n = "namespace" then Some (v :?> string) else None ))
                |> Option.defaultValue "UnknownNamespace"

            let let42 =
                SynModuleDecl.CreateLet
                    [ SynBinding.Let(pattern = SynPat.CreateNamed(Ident.Create "fourtyTwo"), expr = SynExpr.CreateConst(SynConst.Int32 42)) ]

            let allModules =
                File.ReadAllLines context.InputFilename
                |> Seq.map (fun moduleName ->
                                    let componentInfo = SynComponentInfo.Create [ Ident.Create moduleName ]
                                    let module' = SynModuleDecl.CreateNestedModule(componentInfo,  [ let42 ])
                                    module')
                |> Seq.toList

            Output.Ast [SynModuleOrNamespace.CreateNamespace(Ident.CreateLong example1Namespace, decls = allModules)]

/// Exercises IMyriadGeneratorWithDiagnostics: still produces output, and reports one
/// non-fatal Warning diagnostic alongside it. Otherwise identical to Example1Gen.
[<MyriadGenerator("diagnosticswarning")>]
type DiagnosticsWarningGen() =
    interface IMyriadGeneratorWithDiagnostics with
        member _.ValidInputExtensions = seq { ".txt" }

        member this.Generate(context: GeneratorContext) =
            (this :> IMyriadGeneratorWithDiagnostics).GenerateWithDiagnostics(context)
            |> fst
            |> Option.defaultValue (Output.Source "")

        member _.GenerateWithDiagnostics(context: GeneratorContext) =
            let let42 =
                SynModuleDecl.CreateLet
                    [ SynBinding.Let(pattern = SynPat.CreateNamed(Ident.Create "fourtyTwo"), expr = SynExpr.CreateConst(SynConst.Int32 42)) ]

            let allModules =
                File.ReadAllLines context.InputFilename
                |> Seq.map (fun moduleName ->
                                    let componentInfo = SynComponentInfo.Create [ Ident.Create moduleName ]
                                    let module' = SynModuleDecl.CreateNestedModule(componentInfo,  [ let42 ])
                                    module')
                |> Seq.toList

            let output =
                Some(Output.Ast [ SynModuleOrNamespace.CreateNamespace(Ident.CreateLong "TestDiagnosticsWarning", decls = allModules) ])

            let diagnostic =
                { Code = "MYR001"
                  Message = "test warning from DiagnosticsWarningGen"
                  Severity = DiagnosticSeverity.Warning
                  Range = None }

            output, [ diagnostic ]

/// Exercises IMyriadGeneratorWithDiagnostics' failure path: always reports one Error
/// diagnostic and no output, so the CLI must fail the build the same way a thrown
/// exception does today.
[<MyriadGenerator("diagnosticserror")>]
type DiagnosticsErrorGen() =
    interface IMyriadGeneratorWithDiagnostics with
        member _.ValidInputExtensions = seq { ".txt" }

        member this.Generate(context: GeneratorContext) =
            (this :> IMyriadGeneratorWithDiagnostics).GenerateWithDiagnostics(context)
            |> fst
            |> Option.defaultValue (Output.Source "")

        member _.GenerateWithDiagnostics(_context: GeneratorContext) =
            let diagnostic =
                { Code = "MYR002"
                  Message = "test error from DiagnosticsErrorGen"
                  Severity = DiagnosticSeverity.Error
                  Range = None }

            None, [ diagnostic ]
