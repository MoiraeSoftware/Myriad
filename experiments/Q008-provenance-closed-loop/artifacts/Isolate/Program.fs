// Isolation addendum, 2026-07-16: given the SAME checker instance and the SAME real
// ClientTP/SchemaTP generative providers Q008 actually used (recovered from this job's own scratch,
// C:\Users\Dave\.claude\jobs\f0b85ddf\tmp\tp-provenance-spike\), does ParseAndCheckFileInProject
// resolve the generative type via a .fsx SCRIPT (GetProjectOptionsFromScript, Q012/Q013's tested
// route) versus a real, hand-built FSharpProjectOptions (a non-script .fs "project", Q008's own
// harness's actual route, confirmed reproducing above)? Isolates the exact axis Q013's own review
// named as untested.

open System
open System.IO
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open FSharp.Compiler.Diagnostics

let spikeDir =
    let baseDir = AppContext.BaseDirectory
    let mutable d = DirectoryInfo(baseDir)
    while d <> null && d.Name <> "tp-provenance-spike" do d <- d.Parent
    d.FullName

let clientRuntimeDll =
    Path.Combine(spikeDir, "ClientTP.Runtime", "bin", "Release", "net8.0", "ClientTP.Runtime.dll")

let schemaAsmPath =
    Path.Combine(spikeDir, "SchemaAsm", "bin", "Release", "net8.0", "SchemaAsm.dll").Replace("\\", "/")

let schemaTypeName = "SchemaAsm.Schemas+SchemaV2"

let clientConsumerBody =
    sprintf "type C = ClientTP.Provided.Client<SchemaAssemblyPath = \"%s\", SchemaTypeName = \"%s\", ExpectedVersion = \"v2\">\nlet v : string = C.VerifiedVersion\nlet ok : bool = C.ProvenanceOk\n"
        schemaAsmPath schemaTypeName

let checker = FSharpChecker.Create(keepAssemblyContents = true)

let hasErrors (diags: FSharpDiagnostic[]) =
    diags |> Array.exists (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)

let printDiags label (diags: FSharpDiagnostic[]) =
    printfn "  [%s] %d diagnostic(s)" label diags.Length
    for d in diags do
        printfn "    %s (%d,%d): %s" (string d.Severity) d.StartLine d.StartColumn d.Message

// ROUTE A: .fsx script via GetProjectOptionsFromScript, Q012/Q013's tested route.
let checkViaScript () =
    let dir = Path.Combine(spikeDir, "Isolate", "scratch")
    Directory.CreateDirectory dir |> ignore
    let scriptPath = Path.Combine(dir, "ScriptRoute.fsx")
    // No "module X" line: matches Q012/Q013's own working script convention exactly
    // (Q012-compiler-behavior-probe/artifacts/Harness/Program.fs's parseAndCheck/pcScript never use one).
    let text = sprintf "#r @\"%s\"\n%s" clientRuntimeDll clientConsumerBody
    File.WriteAllText(scriptPath, text)
    let source = SourceText.ofString text
    let opts, optDiags =
        checker.GetProjectOptionsFromScript(scriptPath, source, assumeDotNetFramework = false)
        |> Async.RunSynchronously
    let _parse, answer = checker.ParseAndCheckFileInProject(scriptPath, 0, source, opts) |> Async.RunSynchronously
    match answer with
    | FSharpCheckFileAnswer.Succeeded res -> res.Diagnostics
    | FSharpCheckFileAnswer.Aborted -> [||]

// ROUTE B: real, hand-built FSharpProjectOptions -- Q008's own harness's actual route (mkOptions,
// copied verbatim from the recovered Harness/Program.fs), reused here for a byte-identical consumer.
let refPackDir =
    let root = @"C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref"
    Directory.GetDirectories(root)
    |> Array.filter (fun d -> Path.GetFileName(d).StartsWith "8.")
    |> Array.sort
    |> Array.last
    |> fun v -> Path.Combine(v, "ref", "net8.0")

let fsharpCore = typeof<int list>.Assembly.Location

let mkOptions (consumerFile: string) =
    let sysRefs =
        Directory.GetFiles(refPackDir, "*.dll")
        |> Array.map (fun r -> "-r:" + r)
        |> Array.toList
    let otherOptions =
        [ "--targetprofile:netcore"
          "--noframework"
          "-r:" + fsharpCore
          "-r:" + clientRuntimeDll ]
        @ sysRefs
    { ProjectFileName = Path.Combine(Path.GetDirectoryName consumerFile, "Consumer.fsproj")
      ProjectId = None
      SourceFiles = [| consumerFile |]
      OtherOptions = Array.ofList otherOptions
      ReferencedProjects = [||]
      IsIncompleteTypeCheckEnvironment = false
      UseScriptResolutionRules = false
      LoadTime = DateTime.Now
      UnresolvedReferences = None
      OriginalLoadReferences = []
      Stamp = None }

let checkViaRealProject () =
    let dir = Path.Combine(spikeDir, "Isolate", "scratch")
    Directory.CreateDirectory dir |> ignore
    let fsPath = Path.Combine(dir, "ProjectRoute.fs")
    File.WriteAllText(fsPath, "module ProjectRoute\n" + clientConsumerBody)
    let source = SourceText.ofString (File.ReadAllText fsPath)
    let opts = mkOptions fsPath
    let _parse, answer = checker.ParseAndCheckFileInProject(fsPath, 0, source, opts) |> Async.RunSynchronously
    match answer with
    | FSharpCheckFileAnswer.Succeeded res -> res.Diagnostics
    | FSharpCheckFileAnswer.Aborted -> [||]

[<EntryPoint>]
let main _ =
    printfn "Same checker instance, same real ClientTP/SchemaTP providers, identical consumer body."
    printfn "clientConsumerBody:\n%s" clientConsumerBody
    printfn ""

    printfn "===== ROUTE A: ParseAndCheckFileInProject over a .fsx SCRIPT (Q012/Q013's route) ====="
    let diagsA = checkViaScript ()
    printDiags "script-route" diagsA
    let aResolved = not (hasErrors diagsA)
    printfn "  ROUTE A resolved: %b" aResolved
    printfn ""

    printfn "===== ROUTE B: ParseAndCheckFileInProject over a REAL FSharpProjectOptions (Q008's route) ====="
    let diagsB = checkViaRealProject ()
    printDiags "project-route" diagsB
    let bResolved = not (hasErrors diagsB)
    printfn "  ROUTE B resolved: %b" bResolved
    printfn ""

    printfn "===== ISOLATION VERDICT ====="
    printfn "  Route A (.fsx script)              resolved=%b" aResolved
    printfn "  Route B (real FSharpProjectOptions) resolved=%b" bResolved
    if (not aResolved) && bResolved then
        printfn "  CONFIRMED: script-vs-real-project-options is the axis. Same checker, same provider,"
        printfn "  only the project-options shape differs, and only Route B resolves."
        0
    elif aResolved && bResolved then
        printfn "  BOTH resolved -- the axis is not script-vs-project after all; something else (session"
        printfn "  state, checker construction, provider differences) explains Q012/Q013's divergence."
        1
    elif (not aResolved) && (not bResolved) then
        printfn "  NEITHER resolved -- this reproduction run diverges from the earlier Harness run above;"
        printfn "  investigate before trusting either result."
        1
    else
        printfn "  UNEXPECTED: script route resolved but real-project route did not."
        1
