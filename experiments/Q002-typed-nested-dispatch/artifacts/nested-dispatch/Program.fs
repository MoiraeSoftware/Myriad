// Q002: can typed access detect that one field's type is itself another Myriad-attributed
// type, ACROSS virtual files, and use that to drive nested/recursive generation - the
// capability class Q001 left untested. See ../../02-results.md for the run this produced.

open System.Collections.Concurrent
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text

let files = ConcurrentDictionary<string, string>()
let fileAddress = @"C:\virt\Address.fs"
let filePerson = @"C:\virt\Person.fs"
let fileGenerated = @"C:\virt\Generated.fs"

let addressSrc =
    """module Address

type FieldsAttribute(configKey: string) =
    inherit System.Attribute()

[<Fields("fields")>]
type Address = { Street: string; City: string }
"""

let personSrc =
    """module Person
open Address

[<Fields("fields")>]
type Person = { Name: string; HomeAddress: Address }
"""

files.[fileAddress] <- addressSrc
files.[filePerson] <- personSrc

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

let isFieldsAttributed (e: FSharpEntity) =
    e.Attributes |> Seq.exists (fun a -> a.AttributeType.DisplayName.StartsWith "Fields")

/// Round B generator: for each field, either format it directly (primitive) or recurse into
/// the nested type's own describe function (if that type is itself Fields-attributed).
let generateDescribeModule (ctx: FSharpDisplayContext) (openModule: string) (entity: FSharpEntity) =
    let typeName = entity.DisplayName
    let fields = entity.FSharpFields |> List.ofSeq

    let fieldExprs =
        fields
        |> List.map (fun f ->
            if f.FieldType.HasTypeDefinition && isFieldsAttributed f.FieldType.TypeDefinition then
                sprintf "(%s.describe x.%s)" f.FieldType.TypeDefinition.DisplayName f.Name
            else
                sprintf "(sprintf \"%%O\" x.%s)" f.Name)

    let fmtString =
        fields
        |> List.map (fun f -> sprintf "%s=%%s" f.Name)
        |> String.concat "; "

    sprintf "module %s =\n    open %s\n    let describe (x: %s) : string =\n        sprintf \"%s\" %s\n"
        typeName openModule typeName fmtString (String.concat " " fieldExprs)

let run =
    async {
        let! baseOpts, _ = checker.GetProjectOptionsFromScript(fileAddress, SourceText.ofString addressSrc)
        let opts = { baseOpts with ProjectFileName = @"C:\virt\spike.fsproj"; SourceFiles = [| fileAddress; filePerson; fileGenerated |] }

        let! _, ansAddr = checker.ParseAndCheckFileInProject(fileAddress, 0, SourceText.ofString addressSrc, opts)
        let checkedAddr = match ansAddr with FSharpCheckFileAnswer.Succeeded r -> r | _ -> failwith "Address.fs check aborted"
        printfn "Address.fs diagnostics: %A" checkedAddr.Diagnostics

        let! _, ansPerson = checker.ParseAndCheckFileInProject(filePerson, 0, SourceText.ofString personSrc, opts)
        let checkedPerson = match ansPerson with FSharpCheckFileAnswer.Succeeded r -> r | _ -> failwith "Person.fs check aborted"
        printfn "Person.fs diagnostics: %A" checkedPerson.Diagnostics

        let implAddr = checkedAddr.ImplementationFile |> Option.get
        let implPerson = checkedPerson.ImplementationFile |> Option.get

        let addressEntity = collectEntities implAddr.Declarations |> Seq.find (fun e -> e.IsFSharpRecord && e.DisplayName = "Address")
        let personEntity = collectEntities implPerson.Declarations |> Seq.find (fun e -> e.IsFSharpRecord && e.DisplayName = "Person")

        // ---------------- Round A: cheapest falsifier ----------------
        printfn ""
        printfn "=== Round A: cross-file entity resolution (the falsifier) ==="
        let homeAddressField = personEntity.FSharpFields |> Seq.find (fun f -> f.Name = "HomeAddress")
        let ctxEmpty = FSharpDisplayContext.Empty
        printfn "HomeAddress field type (as seen from Person.fs): %s" (homeAddressField.FieldType.Format ctxEmpty)
        printfn "HasTypeDefinition: %b" homeAddressField.FieldType.HasTypeDefinition

        if not homeAddressField.FieldType.HasTypeDefinition then
            printfn "FALSIFIED: field type has no resolvable type definition. Stopping."
            return 0
        else
            let resolvedAddr = homeAddressField.FieldType.TypeDefinition
            let resolvedFromDifferentFile = resolvedAddr.DeclarationLocation.FileName = fileAddress
            printfn "Resolved TypeDefinition.DisplayName: %s" resolvedAddr.DisplayName
            printfn "Resolved TypeDefinition.DeclarationLocation.FileName: %s" resolvedAddr.DeclarationLocation.FileName
            printfn "Resolution reached a DIFFERENT virtual file than Person.fs: %b (expected true)" resolvedFromDifferentFile
            let resolvedHasFieldsAttr = isFieldsAttributed resolvedAddr
            printfn "Resolved cross-file entity carries the Fields attribute: %b (expected true)" resolvedHasFieldsAttr

            if not (resolvedFromDifferentFile && resolvedHasFieldsAttr) then
                printfn "FALSIFIED: cross-file resolution did not reach the real, attributed Address entity."
                return 1
            else
                printfn "Round A PASS: cross-file typed resolution works."

                // ---------------- Round B: build + splice the nested-dispatch generator ----------------
                printfn ""
                printfn "=== Round B: generate nested-dispatch describe functions, splice, typecheck ==="

                let addressModule = generateDescribeModule ctxEmpty "Address" addressEntity
                let personModule = generateDescribeModule ctxEmpty "Person" personEntity

                let generated = sprintf "namespace rec TestDescribe\n\n%s\n%s" addressModule personModule

                printfn "Generated source (never written to disk):"
                printfn "%s" generated

                files.[fileGenerated] <- generated

                let! _, ansGen = checker.ParseAndCheckFileInProject(fileGenerated, 0, SourceText.ofString generated, opts)
                let checkedGen = match ansGen with FSharpCheckFileAnswer.Succeeded r -> r | _ -> failwith "Generated.fs check aborted"

                printfn ""
                printfn "Generated.fs diagnostics (empty == typechecked, zero disk writes): %A" checkedGen.Diagnostics

                return checkedGen.Diagnostics.Length
    }

[<EntryPoint>]
let main _ =
    let errCount = Async.RunSynchronously run
    printfn ""
    if errCount = 0 then printfn "Q002 PASSED end to end." else printfn "Q002 FAILED (%d)." errCount
    errCount
