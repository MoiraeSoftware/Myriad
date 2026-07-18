// Round 4: port Myriad's real Fields generator (src/Myriad.Plugins/FieldsGenerator.fs)
// onto the typed, in-process, TransparentCompiler model validated in rounds 1-3.
// Question: does typed access change anything for THIS generator?
// Headline finding: NO. The alias-preserving format ("Id") the generator correctly
// chooses is exactly what Myriad's existing syntax-echo approach already produces.
// Typed access was available and unused by the better choice. See ../../02-results.md
// and ../../03-review.md for what this does and doesn't prove.

open System.Collections.Concurrent
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text

let files = ConcurrentDictionary<string, string>()
let fileA = @"C:\virt\Example.fs"
let fileB = @"C:\virt\Generated.fs"

let sourceV1 =
    """module Example

type FieldsAttribute(configKey: string) =
    inherit System.Attribute()

/// Type alias, deliberately, to see whether it matters for THIS generator.
type Id = int

[<Fields("fields")>]
type Person = { Name: string; Age: Id; Nick: string option }
"""

files.[fileA] <- sourceV1

let docSource =
    DocumentSource.Custom(fun path ->
        async {
            match files.TryGetValue path with
            | true, txt -> return Some(SourceText.ofString txt :> ISourceText)
            | _ -> return None
        })

let checker = FSharpChecker.Create(keepAssemblyContents = true, documentSource = docSource, useTransparentCompiler = true)

let rec collectEntities decls = seq {
    for d in decls do
        match d with
        | FSharpImplementationFileDeclaration.Entity(e, sub) ->
            yield e
            yield! collectEntities sub
        | _ -> () }

let camelCase (s: string) =
    if s.Length = 0 then s else string (System.Char.ToLowerInvariant s.[0]) + s.Substring(1)

/// The actual generator logic - typed-tree port of Myriad's Create.createRecordModule.
/// Given a record entity, produce the same shape Myriad's Fields plugin produces:
/// one accessor per field, a `create`, and a `map`.
let generateFieldsModule (ctx: FSharpDisplayContext) (openModule: string) (targetNamespace: string) (entity: FSharpEntity) =
    let recordName = entity.DisplayName
    let fields = entity.FSharpFields |> List.ofSeq

    let accessors =
        fields
        |> List.map (fun f -> sprintf "    let %s (x : %s) = x.%s" f.Name recordName f.Name)
        |> String.concat "\n"

    let createParams =
        fields
        |> List.map (fun f -> sprintf "(%s : %s)" (camelCase f.Name) (f.FieldType.Format ctx))
        |> String.concat " "

    let createBody =
        fields
        |> List.map (fun f -> sprintf "%s = %s" f.Name (camelCase f.Name))
        |> String.concat "; "

    let mapParams =
        fields
        |> List.map (fun f -> sprintf "(map%s : %s -> %s)" f.Name (f.FieldType.Format ctx) (f.FieldType.Format ctx))
        |> String.concat " "

    let mapBody =
        fields
        |> List.map (fun f -> sprintf "          %s = map%s record'.%s" f.Name f.Name f.Name)
        |> String.concat "\n"

    sprintf
        "namespace rec %s\n\nmodule %s =\n    open %s\n\n%s\n\n    let create %s : %s =\n        { %s }\n\n    let map %s (record': %s) =\n      { record' with\n%s }\n"
        targetNamespace recordName openModule accessors createParams recordName createBody mapParams recordName mapBody

let run =
    async {
        let! baseOpts, _ = checker.GetProjectOptionsFromScript(fileA, SourceText.ofString sourceV1)
        let opts = { baseOpts with ProjectFileName = @"C:\virt\spike.fsproj"; SourceFiles = [| fileA; fileB |] }

        let! _, ansA = checker.ParseAndCheckFileInProject(fileA, 0, SourceText.ofString sourceV1, opts)
        let checkedA = match ansA with FSharpCheckFileAnswer.Succeeded r -> r | _ -> failwith "aborted"
        printfn "Example.fs diagnostics: %A" checkedA.Diagnostics

        let implA = checkedA.ImplementationFile |> Option.get

        let personEntity =
            collectEntities implA.Declarations
            |> Seq.find (fun e ->
                e.IsFSharpRecord
                && e.Attributes |> Seq.exists (fun a -> a.AttributeType.DisplayName.StartsWith "Fields"))

        printfn ""
        printfn "Found record '%s' with %d fields, tagged with a Fields-style attribute (found via TYPED attribute list, not a syntax attribute-name string match)." personEntity.DisplayName personEntity.FSharpFields.Count

        let ctx = checkedA.GetDisplayContextForPos(personEntity.DeclarationLocation.Start) |> Option.defaultValue FSharpDisplayContext.Empty

        let generated = generateFieldsModule ctx "Example" "TestFields" personEntity

        printfn ""
        printfn "Generated Fields module (typed-tree port of Myriad's real generator, never written to disk):"
        printfn "%s" generated

        files.[fileB] <- generated

        let! _, ansB = checker.ParseAndCheckFileInProject(fileB, 0, SourceText.ofString generated, opts)
        let checkedB = match ansB with FSharpCheckFileAnswer.Succeeded r -> r | _ -> failwith "aborted"

        printfn ""
        printfn "Generated.fs diagnostics (empty == typechecked, zero disk writes): %A" checkedB.Diagnostics

        let ageField = personEntity.FSharpFields |> Seq.find (fun f -> f.Name = "Age")
        printfn ""
        printfn "Age field type via typed access, alias-preserving format: %s" (ageField.FieldType.Format ctx)
        printfn "Age field type via typed access, alias-STRIPPED format:   %s" (ageField.FieldType.StripAbbreviations().Format ctx)

        return checkedB.Diagnostics.Length
    }

[<EntryPoint>]
let main _ =
    let errCount = Async.RunSynchronously run
    printfn ""
    if errCount = 0 then printfn "PROTOTYPE PASSED: real Fields-generator port typechecked end to end." else printfn "PROTOTYPE FAILED."
    errCount
