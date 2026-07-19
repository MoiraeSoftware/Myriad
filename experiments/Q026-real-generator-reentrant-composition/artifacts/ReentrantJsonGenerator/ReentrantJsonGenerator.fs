namespace Q026

open System
open Myriad.Core
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text

/// The second real, unmodified IMyriadGenerator this quartet composes with the real
/// LensesGenerator. New code (no such generator exists in Myriad today), but real: discovered and
/// invoked exactly the way Myriad's own CLI (src/Myriad/Program.fs) discovers and invokes any
/// generator - via MyriadGeneratorAttribute + Activator.CreateInstance + the IMyriadGenerator
/// interface, never a direct constructor call from the harness.
///
/// Its Generate method reentrant-queries a second file's already-typechecked content (the real,
/// Fantomas-formatted PersonLenses module the real LensesGenerator produced) via Q010's proven
/// mechanism (DocumentSource.Custom made reentrant), reusing Q010's own typed-inspection logic
/// (collectEntities/collectMfvs/getterReturnTypeName, from
/// Q010-prefix-stratified-generation/artifacts/round2-cross-generator/Program.fs) against real
/// symbols this time instead of a hand-typed stand-in.
[<MyriadGenerator("reentrantjson")>]
type ReentrantJsonGenerator() =

    let rec collectEntities decls = seq {
        for d in decls do
            match d with
            | FSharpImplementationFileDeclaration.Entity(e, sub) ->
                yield e
                yield! collectEntities sub
            | _ -> () }

    let rec collectMfvs decls = seq {
        for d in decls do
            match d with
            | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(v, _, _) -> yield v
            | FSharpImplementationFileDeclaration.Entity(_, sub) -> yield! collectMfvs sub
            | _ -> () }

    // Extract getter return-type name from a lens binding's type: ((P -> ret) * (P -> v -> P)).
    let getterReturnTypeName (v: FSharpMemberOrFunctionOrValue) =
        try
            let t = v.FullType
            if t.IsTupleType && t.GenericArguments.Count >= 1 then
                let getterFn = t.GenericArguments.[0]
                if getterFn.IsFunctionType && getterFn.GenericArguments.Count >= 2 then
                    Some(getterFn.GenericArguments.[getterFn.GenericArguments.Count - 1].Format FSharpDisplayContext.Empty)
                else None
            else None
        with _ -> None

    interface IMyriadGenerator with
        member _.ValidInputExtensions = seq { ".fs" }

        member _.Generate(_context: GeneratorContext) : Output =
            let checker =
                match Q026Bridge.Checker with
                | Some c -> c
                | None -> failwith "Q026Bridge.Checker not set - harness must populate it before invoking this generator"
            let opts =
                match Q026Bridge.Opts with
                | Some o -> o
                | None -> failwith "Q026Bridge.Opts not set - harness must populate it before invoking this generator"
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

                    let implB =
                        match checkedB.ImplementationFile with
                        | Some impl -> impl
                        | None -> failwith "PersonLenses.fs has no ImplementationFile - keepAssemblyContents must be true"

                    let lensModule =
                        collectEntities implB.Declarations
                        |> Seq.tryFind (fun e -> e.DisplayName = "PersonLenses")
                        |> function
                           | Some m -> m
                           | None -> failwith "Real LensesGenerator output has no 'PersonLenses' module - see 02-results.md for the actual shape found"

                    let bindings =
                        collectMfvs implB.Declarations
                        |> Seq.filter (fun v ->
                            not v.IsMember
                            && v.DeclaringEntity.IsSome
                            && v.DeclaringEntity.Value.DisplayName = "PersonLenses")
                        |> Seq.map (fun v ->
                            let retTy = getterReturnTypeName v |> Option.defaultValue "?"
                            v.DisplayName, retTy)
                        |> List.ofSeq

                    if bindings.IsEmpty then
                        failwith "No lens bindings discovered in the real PersonLenses module"

                    let letLines =
                        bindings
                        |> List.map (fun (n, _) -> sprintf "    let get%s, _ = PersonLenses.%s" n n)
                        |> String.concat "\n"
                    let jsonParts =
                        bindings
                        |> List.map (fun (n, ret) ->
                            let spec = if ret.EndsWith "string" then "\\\"%s\\\"" else "%d"
                            sprintf "\\\"%s\\\":%s" (n.ToLowerInvariant()) spec)
                        |> String.concat ","
                    let jsonArgs =
                        bindings
                        |> List.map (fun (n, _) -> sprintf "(get%s p)" n)
                        |> String.concat " "

                    let source =
                        sprintf
                            "module Domain.PersonJson\n\nopen Domain\n\nlet serialize (p: Person) =\n%s\n    sprintf \"{%s}\" %s\n"
                            letLines jsonParts jsonArgs

                    return Output.Source source
                }
            Async.RunSynchronously(run, timeout = 30_000)
