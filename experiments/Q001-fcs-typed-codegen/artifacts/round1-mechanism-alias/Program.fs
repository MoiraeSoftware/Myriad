// Round 1: in-process FCS hosting, zero disk writes, typed alias resolution.
// Proves the base mechanism - see ../../02-results.md for the run this produced.

open System.Collections.Concurrent
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text

let files = ConcurrentDictionary<string, string>()
let fileA = @"C:\virt\A.fs"
let fileB = @"C:\virt\Generated.fs"

files.[fileA] <-
    """module A

type MyAttribute() =
    inherit System.Attribute()

/// Type alias - the point of this spike is that a typed macro sees straight
/// through this to `int`, where a syntax-only tool just sees the string "Id".
type Id = int

[<My>]
type Person = { Name: string; Age: Id; Nick: string option }
"""

let docSource =
    DocumentSource.Custom(fun path ->
        async {
            match files.TryGetValue path with
            | true, txt -> return Some(SourceText.ofString txt :> ISourceText)
            | _ -> return None
        })

let checker = FSharpChecker.Create(keepAssemblyContents = true, documentSource = docSource)

let rec collectEntities decls = seq {
    for d in decls do
        match d with
        | FSharpImplementationFileDeclaration.Entity(e, sub) ->
            yield e
            yield! collectEntities sub
        | _ -> () }

let run =
    async {
        let srcA = SourceText.ofString files.[fileA]
        let! baseOpts, scriptDiags = checker.GetProjectOptionsFromScript(fileA, srcA)

        if not scriptDiags.IsEmpty then
            printfn "Script option diagnostics: %A" scriptDiags

        let opts =
            { baseOpts with
                ProjectFileName = @"C:\virt\spike.fsproj"
                SourceFiles = [| fileA; fileB |] }

        let! _, ansA = checker.ParseAndCheckFileInProject(fileA, 0, srcA, opts)

        let checkedA =
            match ansA with
            | FSharpCheckFileAnswer.Succeeded r -> r
            | FSharpCheckFileAnswer.Aborted -> failwith "check of A aborted"

        printfn "A.fs diagnostics: %A" checkedA.Diagnostics

        let implA = checkedA.ImplementationFile |> Option.get
        let ctx = FSharpDisplayContext.Empty

        let personFields =
            collectEntities implA.Declarations
            |> Seq.filter (fun e -> e.IsFSharpRecord)
            |> Seq.collect (fun e -> e.FSharpFields |> Seq.map (fun f -> f.Name, f.FieldType.StripAbbreviations().Format ctx))
            |> List.ofSeq

        printfn ""
        printfn "Resolved fields of Person (via TYPED tree, not syntax):"
        for name, ty in personFields do
            printfn "  %s : %s" name ty
        // Age prints as System.Int32, not "Id" - the alias is resolved via StripAbbreviations().
        // Plain .Format(ctx) WITHOUT StripAbbreviations prints "A.Id" verbatim - it does not
        // resolve automatically. That correction is the point of this round.

        let paramList = personFields |> List.map (fun (n, _) -> n.ToLowerInvariant()) |> String.concat " "
        let assignments = personFields |> List.map (fun (n, _) -> sprintf "%s = %s" n (n.ToLowerInvariant())) |> String.concat "; "

        let generated =
            sprintf
                "module Generated\nopen A\n\nlet create %s : Person =\n    { %s }\n\nlet describe (p: Person) = sprintf \"%%s/%%d\" p.Name p.Age\n"
                paramList assignments

        printfn ""
        printfn "Generated source (never written to disk):"
        printfn "%s" generated

        files.[fileB] <- generated

        let! _, ansB = checker.ParseAndCheckFileInProject(fileB, 0, SourceText.ofString generated, opts)

        let checkedB =
            match ansB with
            | FSharpCheckFileAnswer.Succeeded r -> r
            | FSharpCheckFileAnswer.Aborted -> failwith "check of Generated aborted"

        printfn ""
        printfn "Generated.fs diagnostics (empty == it typechecked, in-process, zero disk writes):"
        printfn "%A" checkedB.Diagnostics

        return checkedB.Diagnostics.Length
    }

[<EntryPoint>]
let main _ =
    let errorCount = Async.RunSynchronously run
    if errorCount = 0 then
        printfn ""
        printfn "PASSED: typed field info extracted, generated code typechecked, zero disk writes for generated code."
        0
    else
        1
