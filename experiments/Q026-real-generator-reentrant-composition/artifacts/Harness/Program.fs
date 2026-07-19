// Q026 - real-generator reentrant composition.
//
// Round 1 (cheapest falsifier): invoke the REAL, unmodified Myriad.Plugins.LensesGenerator,
// standalone, outside src/Myriad/Program.fs's own process, against a real on-disk
// [<Lenses("person")>]-attributed file, through the same discovery (MyriadGeneratorAttribute scan
// + Activator.CreateInstance + IMyriadGenerator cast) and formatting (Fantomas CodeFormatter,
// Generation.getHeaderedCode) pipeline Program.fs itself uses.
//
// Round 2 (the actual composition, only run if Round 1 passes): host an in-process FSharpChecker
// with a reentrant DocumentSource.Custom callback (Q010's proven mechanism) over three files -
// the real Person.fs, a virtual PersonLenses.fs computed ON DEMAND by re-running Round 1's exact
// steps, and a virtual PersonJson.fs computed by a SECOND real, unmodified IMyriadGenerator
// (ReentrantJsonGenerator) that reentrantly typed-queries PersonLenses.fs's already-checked
// content via a disclosed side-channel (Q026Bridge), the same way Q010's own hand-rolled stand-in
// did, but against real generator output this time.
//
// See ../../02-results.md for the run this produced.

open System
open System.IO
open System.Reflection
open Myriad.Core
open Myriad.Core.Ast
open Fantomas.FCS.Syntax
open Fantomas.Core
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text

// ---- Faithful copy of src/Myriad/Program.fs's own generator-discovery mechanism -----------------
// One disclosed simplification (00-hypothesis.md precondition 1): plain Assembly.LoadFrom instead
// of McMaster.NETCore.Plugins.PluginLoader's AssemblyLoadContext isolation. ALC isolation is
// BACKLOG.md item 2's own separate, still-open question, not this quartet's concern.

let findGenerators (assembly: Assembly) : Type list =
    let types =
        try assembly.GetTypes()
        with :? ReflectionTypeLoadException as ex -> ex.Types |> Array.filter (fun t -> t <> null)
    [ for t in types do
        if t.GetCustomAttributes(typeof<MyriadGeneratorAttribute>, true).Length > 0 then
            yield t ]

let instantiate (t: Type) : IMyriadGenerator =
    Activator.CreateInstance(t) :?> IMyriadGenerator

// ---- Round 1: real LensesGenerator invocation + real Fantomas formatting pipeline ---------------
// Mirrors src/Myriad/Program.fs:226-335's own runGenerator + formattedCode steps exactly.

let repoRoot = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "..", ".."))
let pluginsDllPath = Path.Combine(repoRoot, "src", "Myriad.Plugins", "bin", "Release", "net9.0", "Myriad.Plugins.dll")
let personFilePath = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", "Domain", "Person.fs"))

let personConfigHandler (key: string) : (string * obj) seq =
    if key = "person" then seq { "namespace", box "Domain" } else Seq.empty

/// Runs the real LensesGenerator against the real on-disk Person.fs and returns the real,
/// Fantomas-formatted, headered source text - identical steps to Program.fs's own pipeline.
let runLensesGeneratorForReal () : string =
    let pluginsAsm = Assembly.LoadFrom(pluginsDllPath)
    let lensesType =
        findGenerators pluginsAsm
        |> List.tryFind (fun t -> t.Name = "LensesGenerator")
        |> function
           | Some t -> t
           | None -> failwithf "No type named LensesGenerator found with MyriadGeneratorAttribute in %s" pluginsDllPath
    let instance = instantiate lensesType

    let context =
        GeneratorContext.Create(Some "person", personConfigHandler, personFilePath, None, dict [])

    let output = instance.Generate(context)

    let ast =
        match output with
        | Output.Ast modules -> modules
        | Output.Source _ -> failwith "Expected LensesGenerator to return Output.Ast"

    let parseTree = ParsedInput.ImplFile(ParsedImplFileInput.CreateFs(personFilePath, modules = ast))
    let cfg = Myriad.Core.EditorConfig.readConfiguration personFilePath
    let formatted = CodeFormatter.FormatASTAsync(parseTree, cfg) |> Async.RunSynchronously
    let headered = Myriad.Core.Generation.getHeaderedCode formatted |> String.concat Environment.NewLine
    headered

// ---- Round 2: reentrant in-process composition --------------------------------------------------

let reentrantJsonDllPath =
    Path.Combine(__SOURCE_DIRECTORY__, "..", "ReentrantJsonGenerator", "bin", "Release", "net9.0", "ReentrantJsonGenerator.dll")
    |> Path.GetFullPath

let fileA = personFilePath
let fileB = @"C:\virt-q026\PersonLenses.fs"
let fileC = @"C:\virt-q026\PersonJson.fs"

let mutable lensesCallbackFired = false
let mutable jsonCallbackFired = false

let mutable checkerRef : FSharpChecker = Unchecked.defaultof<_>
let mutable optsRef : FSharpProjectOptions = Unchecked.defaultof<_>

let runReentrantJsonGeneratorForReal () : string =
    let jsonAsm = Assembly.LoadFrom(reentrantJsonDllPath)
    let jsonType =
        findGenerators jsonAsm
        |> List.tryFind (fun t -> t.Name = "ReentrantJsonGenerator")
        |> function
           | Some t -> t
           | None -> failwithf "No type named ReentrantJsonGenerator found with MyriadGeneratorAttribute in %s" reentrantJsonDllPath
    let instance = instantiate jsonType

    Q026Bridge.Checker <- Some checkerRef
    Q026Bridge.Opts <- Some optsRef
    Q026Bridge.PersonFilePath <- fileA
    Q026Bridge.PersonLensesFilePath <- fileB

    let context = GeneratorContext.Create(None, (fun _ -> Seq.empty), fileC, None, dict [])
    match instance.Generate(context) with
    | Output.Source src -> src
    | Output.Ast _ -> failwith "Expected ReentrantJsonGenerator to return Output.Source"

let makeReentrantDocSource () =
    DocumentSource.Custom(fun path ->
        async {
            if path = fileA then
                let text = File.ReadAllText fileA
                Q026Bridge.Files.[fileA] <- text
                return Some(SourceText.ofString text :> ISourceText)
            elif path = fileB then
                lensesCallbackFired <- true
                let text = runLensesGeneratorForReal ()
                Q026Bridge.Files.[fileB] <- text
                return Some(SourceText.ofString text :> ISourceText)
            elif path = fileC then
                jsonCallbackFired <- true
                let text = runReentrantJsonGeneratorForReal ()
                Q026Bridge.Files.[fileC] <- text
                return Some(SourceText.ofString text :> ISourceText)
            else
                return None
        })

let runRound2 () =
    printfn ""
    printfn "=== Round 2: reentrant composition of two real, unmodified IMyriadGenerators ==="
    let docSource = makeReentrantDocSource ()
    let checker = FSharpChecker.Create(keepAssemblyContents = true, documentSource = docSource, useTransparentCompiler = true)
    checkerRef <- checker

    let run =
        async {
            let! baseOpts, _ = checker.GetProjectOptionsFromScript(fileA, SourceText.ofString (File.ReadAllText fileA))
            let opts =
                { baseOpts with
                    ProjectFileName = @"C:\virt-q026\q026.fsproj"
                    SourceFiles = [| fileA; fileB; fileC |]
                    OtherOptions = Array.append baseOpts.OtherOptions [| sprintf "-r:%s" pluginsDllPath |] }
            optsRef <- opts

            let! projResults = checker.ParseAndCheckProject(opts)
            printfn "  ParseAndCheckProject returned."
            printfn "  Lenses callback fired : %b" lensesCallbackFired
            printfn "  Json callback fired   : %b" jsonCallbackFired

            printfn ""
            printfn "=== Generated PersonLenses.fs (real LensesGenerator output, never written except as artifact) ==="
            printfn "%s" Q026Bridge.Files.[fileB]
            printfn "=== Generated PersonJson.fs (real ReentrantJsonGenerator output) ==="
            printfn "%s" Q026Bridge.Files.[fileC]

            let errors = projResults.Diagnostics |> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)
            let errorsB = errors |> Array.filter (fun d -> d.FileName = fileB)
            let errorsC = errors |> Array.filter (fun d -> d.FileName = fileC)
            printfn ""
            printfn "=== Diagnostics ==="
            printfn "  project-wide errors: %d" errors.Length
            for d in errors do printfn "    %s (%s @ %A)" d.Message d.FileName d.Range
            printfn "  errors on PersonLenses.fs: %d" errorsB.Length
            printfn "  errors on PersonJson.fs  : %d" errorsC.Length

            let! _, ansC = checker.ParseAndCheckFileInProject(fileC, 0, SourceText.ofString Q026Bridge.Files.[fileC], opts)
            let checkedC =
                match ansC with
                | FSharpCheckFileAnswer.Succeeded r -> r
                | FSharpCheckFileAnswer.Aborted -> failwith "PersonJson.fs re-check aborted"

            printfn ""
            printfn "=== Typed-resolution proof: does PersonJson's reference to PersonLenses.Name"
            printfn "    resolve to a REAL symbol declared in the real, Fantomas-formatted"
            printfn "    PersonLenses.fs, or is it a textual coincidence? ==="

            let allUses = checkedC.GetAllUsesOfAllSymbolsInFile() |> Array.ofSeq
            let lensRefUses =
                allUses
                |> Array.choose (fun u ->
                    match u.Symbol with
                    | :? FSharpMemberOrFunctionOrValue as m
                        when m.DeclaringEntity.IsSome && m.DeclaringEntity.Value.DisplayName = "PersonLenses" ->
                        Some(m, u)
                    | _ -> None)
            printfn ""
            printfn "  Uses in PersonJson.fs that resolved to a PersonLenses member: %d" lensRefUses.Length
            let mutable allFromB = lensRefUses.Length > 0
            for (m, u) in lensRefUses do
                let declFile = m.DeclarationLocation.FileName
                let fromB = declFile = fileB
                if not fromB then allFromB <- false
                printfn "    ref '%s' at %A -> symbol declared at %s (in real PersonLenses.fs: %b)"
                    m.DisplayName u.Range declFile fromB

            printfn ""
            printfn "  GetSymbolUseAtLocation on a generated 'PersonLenses.Name'-shaped reference:"
            let cText = Q026Bridge.Files.[fileC]
            let lines = cText.Replace("\r\n", "\n").Split('\n')
            let mutable located = false
            for i in 0 .. lines.Length - 1 do
                let line = lines.[i]
                let marker = "PersonLenses.Name"
                let idx = line.IndexOf(marker)
                if idx >= 0 && not located then
                    located <- true
                    let endCol = idx + marker.Length
                    let lineNo = i + 1
                    let symUse = checkedC.GetSymbolUseAtLocation(lineNo, endCol, line, [ "PersonLenses"; "Name" ])
                    match symUse with
                    | Some su ->
                        let decl = su.Symbol.DeclarationLocation
                        printfn "    resolved to symbol '%s', declared at file=%s range=%A"
                            su.Symbol.DisplayName (decl |> Option.map (fun r -> r.FileName) |> Option.defaultValue "<none>") decl
                        match decl with
                        | Some r when r.FileName = fileB ->
                            printfn "    CONFIRMED: 'PersonLenses.Name' round-trips to the real generated file's actual binding."
                        | _ -> printfn "    NOT confirmed as declared in PersonLenses.fs."
                    | None -> printfn "    GetSymbolUseAtLocation returned None at (%d,%d)." lineNo endCol

            let capabilityPass =
                errorsB.Length = 0 && errorsC.Length = 0 && allFromB
                && lensesCallbackFired && jsonCallbackFired
            printfn ""
            printfn "=== Result ==="
            printfn "  lenses callback fired (no silent DocumentSource.Custom bypass) : %b" lensesCallbackFired
            printfn "  json callback fired (no silent DocumentSource.Custom bypass)   : %b" jsonCallbackFired
            printfn "  zero errors on real-generated PersonLenses.fs                  : %b" (errorsB.Length = 0)
            printfn "  zero errors on real-generated PersonJson.fs                    : %b" (errorsC.Length = 0)
            printfn "  every PersonLenses ref resolved to a typed symbol declared in the real file : %b" allFromB
            if capabilityPass then
                printfn "  CAPABILITY PASS: two REAL, unmodified IMyriadGenerator implementations composed"
                printfn "  via reentrant typed query, inside one in-process FSharpChecker, invoked the same"
                printfn "  way Myriad's real CLI invokes a generator."
            return (if capabilityPass then 0 else 1)
        }
    Async.RunSynchronously(run, timeout = 60_000)

[<EntryPoint>]
let main _ =
    printfn "Q026 - real-generator reentrant composition"
    printfn ""
    printfn "=== Round 1: standalone invocation of the real, unmodified LensesGenerator ==="
    printfn "  Myriad.Plugins.dll: %s" pluginsDllPath
    printfn "  Person.fs         : %s" personFilePath

    let round1Result =
        try
            let text = runLensesGeneratorForReal ()
            let outPath = Path.Combine(__SOURCE_DIRECTORY__, "..", "round1-output", "PersonLenses.fs") |> Path.GetFullPath
            File.WriteAllText(outPath, text)
            printfn ""
            printfn "=== Round 1 output (also written to %s) ===" outPath
            printfn "%s" text
            Ok text
        with ex ->
            Error ex

    match round1Result with
    | Error ex ->
        printfn ""
        printfn "Round 1 FAILED: %s" (ex.ToString())
        printfn "Per 00-hypothesis.md's cheapest falsifier, this alone is a valuable KILL signal -"
        printfn "not attempting Round 2."
        1
    | Ok _ ->
        printfn ""
        printfn "Round 1 PASSED."
        runRound2 ()
