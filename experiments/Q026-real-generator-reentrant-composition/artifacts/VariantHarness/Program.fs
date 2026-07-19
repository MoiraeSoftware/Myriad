// Q026 Movement 4 (review) - variant harness. Reuses the SAME built ReentrantJsonGenerator.dll and
// Myriad.Plugins.dll as the executor's Harness, discovered via the SAME MyriadGeneratorAttribute
// reflection path, but drives them over record shapes the executor never tried, and instruments the
// ALC/static-sharing question 02-results.md only speculates about.

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

let findGenerators (assembly: Assembly) : Type list =
    let types =
        try assembly.GetTypes()
        with :? ReflectionTypeLoadException as ex -> ex.Types |> Array.filter (fun t -> t <> null)
    [ for t in types do
        if t.GetCustomAttributes(typeof<MyriadGeneratorAttribute>, true).Length > 0 then yield t ]

let instantiate (t: Type) : IMyriadGenerator = Activator.CreateInstance(t) :?> IMyriadGenerator

let repoRoot = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "..", ".."))
let pluginsDllPath = Path.Combine(repoRoot, "src", "Myriad.Plugins", "bin", "Release", "net9.0", "Myriad.Plugins.dll")
let reentrantJsonDllPath =
    Path.Combine(__SOURCE_DIRECTORY__, "..", "ReentrantJsonGenerator", "bin", "Release", "net9.0", "ReentrantJsonGenerator.dll")
    |> Path.GetFullPath

let personConfigHandler (key: string) : (string * obj) seq =
    if key = "person" then seq { "namespace", box "Domain" } else Seq.empty

let mutable checkerRef : FSharpChecker = Unchecked.defaultof<_>
let mutable optsRef : FSharpProjectOptions = Unchecked.defaultof<_>

let runLenses (personFilePath: string) : string =
    let pluginsAsm = Assembly.LoadFrom(pluginsDllPath)
    let lensesType = findGenerators pluginsAsm |> List.find (fun t -> t.Name = "LensesGenerator")
    let instance = instantiate lensesType
    let context = GeneratorContext.Create(Some "person", personConfigHandler, personFilePath, None, dict [])
    let ast =
        match instance.Generate(context) with
        | Output.Ast m -> m
        | Output.Source _ -> failwith "expected Ast"
    let parseTree = ParsedInput.ImplFile(ParsedImplFileInput.CreateFs(personFilePath, modules = ast))
    let cfg = Myriad.Core.EditorConfig.readConfiguration personFilePath
    let formatted = CodeFormatter.FormatASTAsync(parseTree, cfg) |> Async.RunSynchronously
    Myriad.Core.Generation.getHeaderedCode formatted |> String.concat Environment.NewLine

let runJson (fileA: string) (fileB: string) (fileC: string) : string =
    let jsonAsm = Assembly.LoadFrom(reentrantJsonDllPath)
    let jsonType = findGenerators jsonAsm |> List.find (fun t -> t.Name = "ReentrantJsonGenerator")
    let instance = instantiate jsonType
    Q026Bridge.Checker <- Some checkerRef
    Q026Bridge.Opts <- Some optsRef
    Q026Bridge.PersonFilePath <- fileA
    Q026Bridge.PersonLensesFilePath <- fileB
    let context = GeneratorContext.Create(None, (fun _ -> Seq.empty), fileC, None, dict [])
    match instance.Generate(context) with
    | Output.Source src -> src
    | Output.Ast _ -> failwith "expected Source"

let runVariant (label: string) (personFilePath: string) : bool =
    printfn ""
    printfn "################ VARIANT: %s ################" label
    printfn "  Person.fs: %s" personFilePath
    let fileA = personFilePath
    let fileB = @"C:\virt-q026v\PersonLenses.fs"
    let fileC = @"C:\virt-q026v\PersonJson.fs"
    let mutable lensesFired = false
    let mutable jsonFired = false
    let docSource =
        DocumentSource.Custom(fun path ->
            async {
                if path = fileA then
                    let t = File.ReadAllText fileA
                    Q026Bridge.Files.[fileA] <- t
                    return Some(SourceText.ofString t :> ISourceText)
                elif path = fileB then
                    lensesFired <- true
                    let t = runLenses fileA
                    Q026Bridge.Files.[fileB] <- t
                    return Some(SourceText.ofString t :> ISourceText)
                elif path = fileC then
                    jsonFired <- true
                    let t = runJson fileA fileB fileC
                    Q026Bridge.Files.[fileC] <- t
                    return Some(SourceText.ofString t :> ISourceText)
                else return None
            })
    let checker = FSharpChecker.Create(keepAssemblyContents = true, documentSource = docSource, useTransparentCompiler = true)
    checkerRef <- checker
    let run =
        async {
            let! baseOpts, _ = checker.GetProjectOptionsFromScript(fileA, SourceText.ofString (File.ReadAllText fileA))
            let opts =
                { baseOpts with
                    ProjectFileName = @"C:\virt-q026v\q026v.fsproj"
                    SourceFiles = [| fileA; fileB; fileC |]
                    OtherOptions = Array.append baseOpts.OtherOptions [| sprintf "-r:%s" pluginsDllPath |] }
            optsRef <- opts
            let! projResults = checker.ParseAndCheckProject(opts)
            let errors = projResults.Diagnostics |> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)
            let errorsB = errors |> Array.filter (fun d -> d.FileName = fileB)
            let errorsC = errors |> Array.filter (fun d -> d.FileName = fileC)
            printfn ""
            printfn "--- generated PersonLenses.fs ---"
            printfn "%s" Q026Bridge.Files.[fileB]
            printfn "--- generated PersonJson.fs ---"
            printfn "%s" Q026Bridge.Files.[fileC]
            printfn "  lenses callback fired: %b | json callback fired: %b" lensesFired jsonFired
            printfn "  project-wide errors: %d (on PersonLenses.fs: %d, on PersonJson.fs: %d)" errors.Length errorsB.Length errorsC.Length
            for d in errors do printfn "    ERROR: %s (%s @ %A)" d.Message d.FileName d.Range
            let! _, ansC = checker.ParseAndCheckFileInProject(fileC, 0, SourceText.ofString Q026Bridge.Files.[fileC], opts)
            let checkedC = match ansC with | FSharpCheckFileAnswer.Succeeded r -> r | _ -> failwith "aborted"
            let allUses = checkedC.GetAllUsesOfAllSymbolsInFile() |> Array.ofSeq
            let lensRefUses =
                allUses |> Array.choose (fun u ->
                    match u.Symbol with
                    | :? FSharpMemberOrFunctionOrValue as m when m.DeclaringEntity.IsSome && m.DeclaringEntity.Value.DisplayName = "PersonLenses" -> Some(m, u)
                    | _ -> None)
            let mutable allFromB = lensRefUses.Length > 0
            printfn "  PersonLenses refs resolved in PersonJson.fs: %d" lensRefUses.Length
            for (m, u) in lensRefUses do
                let fromB = m.DeclarationLocation.FileName = fileB
                if not fromB then allFromB <- false
                printfn "    ref '%s' at %A -> declared in real PersonLenses.fs: %b" m.DisplayName u.Range fromB
            let pass = lensesFired && jsonFired && errorsB.Length = 0 && errorsC.Length = 0 && allFromB
            printfn "  VARIANT RESULT: composition+symbol-resolution clean = %b" pass
            return pass
        }
    Async.RunSynchronously(run, timeout = 60_000)

[<EntryPoint>]
let main argv =
    printfn "Q026 review variant harness (args: %A)" argv
    let runA = argv |> Array.isEmpty || argv |> Array.contains "A"
    let runB = argv |> Array.isEmpty || argv |> Array.contains "B"

    // ---- ALC / static-sharing evidence (02-results.md's untested caveat) --------------------------
    printfn ""
    printfn "################ ALC / static side-channel sharing ################"
    // Force the harness's OWN compiled-against copy of Q026.Bridge to load (F# loads it lazily on
    // first static access), so the assembly count is meaningful rather than reflecting lazy loading.
    Q026Bridge.PersonFilePath <- "touch-to-force-load"
    let harnessBridgeAsm =
        AppDomain.CurrentDomain.GetAssemblies() |> Array.find (fun a -> a.GetName().Name = "Q026.Bridge")
    let bridgeBefore =
        AppDomain.CurrentDomain.GetAssemblies() |> Array.filter (fun a -> a.GetName().Name = "Q026.Bridge")
    printfn "  Q026.Bridge copies loaded after harness touches its own copy: %d" bridgeBefore.Length
    let genAsm = Assembly.LoadFrom(reentrantJsonDllPath)
    let genBridgeName = genAsm.GetReferencedAssemblies() |> Array.find (fun n -> n.Name = "Q026.Bridge")
    let genBridgeAsm = Assembly.Load(genBridgeName)
    let bridgeAfter =
        AppDomain.CurrentDomain.GetAssemblies() |> Array.filter (fun a -> a.GetName().Name = "Q026.Bridge")
    printfn "  Q026.Bridge copies loaded after generator DLL + its Q026.Bridge dep resolved: %d" bridgeAfter.Length
    let sameInstance = obj.ReferenceEquals(genBridgeAsm, harnessBridgeAsm)
    printfn "  generator's Q026.Bridge IS the SAME assembly instance as the harness's: %b" sameInstance

    let a = if runA then runVariant "A: three primitive fields Name Age Email" (Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", "variant-Domain", "Person3.fs"))) else true
    let b = if runB then runVariant "B: nested non-primitive field Home Address" (Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", "variant-Domain", "PersonNested.fs"))) else true
    printfn ""
    printfn "################ SUMMARY ################"
    if runA then printfn "  variant A (3 primitives) clean: %b" a
    if runB then printfn "  variant B (nested type)  clean: %b" b
    printfn "  single shared Q026.Bridge instance across Assembly.LoadFrom: %b" sameInstance
    0
