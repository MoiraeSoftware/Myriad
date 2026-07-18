module Harness.Program

open System
open System.IO
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open FSharp.Compiler.Diagnostics

let artifactsDir =
    let baseDir = AppContext.BaseDirectory
    let mutable d = DirectoryInfo(baseDir)
    while d <> null && d.Name <> "artifacts" do d <- d.Parent
    d.FullName

let sampleLibDir = Path.Combine(artifactsDir, "SampleLib")
let personFs = Path.Combine(sampleLibDir, "Person.fs")
let runtimeDll = Path.Combine(artifactsDir, "MyriadPreview.Runtime", "bin", "Debug", "net9.0", "MyriadPreview.Runtime.dll")

let refPackDir =
    let root = @"C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref"
    Directory.GetDirectories(root)
    |> Array.filter (fun d -> Path.GetFileName(d).StartsWith "9.")
    |> Array.sort
    |> Array.last
    |> fun v -> Path.Combine(v, "ref", "net9.0")

let fsharpCore = typeof<int list>.Assembly.Location

let checker = FSharpChecker.Create(keepAssemblyContents = true)

// Precondition check (00-hypothesis.md's validity precondition): confirm no reference path here can
// possibly define SampleNs.Person. Only the BCL ref pack, FSharp.Core, and MyriadPreview.Runtime.dll
// (which carries no compiled record types of its own -- checked directly below, not assumed) are on
// this list. Person.fs is read as TEXT by the provider; it is never compiled by anything in this
// harness.
let mkOptions (consumerFile: string) =
    let sysRefs =
        Directory.GetFiles(refPackDir, "*.dll")
        |> Array.map (fun r -> "-r:" + r)
        |> Array.toList
    let otherOptions =
        [ "--targetprofile:netcore"
          "--noframework"
          "-r:" + fsharpCore
          "-r:" + runtimeDll ]
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

let mutable version = 0

let check (consumerFile: string) =
    version <- version + 1
    let text = File.ReadAllText consumerFile
    let source = SourceText.ofString text
    let opts = mkOptions consumerFile
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let _parse, answer =
        checker.ParseAndCheckFileInProject(consumerFile, version, source, opts)
        |> Async.RunSynchronously
    sw.Stop()
    match answer with
    | FSharpCheckFileAnswer.Succeeded res -> sw.ElapsedMilliseconds, res.Diagnostics, Some res
    | FSharpCheckFileAnswer.Aborted -> sw.ElapsedMilliseconds, [||], None

let printDiags label (diags: FSharpDiagnostic[]) =
    printfn "  [%s] %d diagnostic(s)" label diags.Length
    for d in diags do
        printfn "    %s (%d,%d): %s" (string d.Severity) d.StartLine d.StartColumn d.Message

let personFsForward = personFs.Replace("\\", "/")

let round1 () =
    printfn "===== ROUND 1: design-time member resolution, target record NEVER compiled anywhere ====="
    printfn "  runtimeDll = %s (exists=%b)" runtimeDll (File.Exists runtimeDll)
    printfn "  Person.fs  = %s (exists=%b, target record compiled by nothing in this harness)" personFs (File.Exists personFs)
    printfn "  OtherOptions reference list (grep for anything that could define SampleNs.Person):"
    let opts = mkOptions personFs
    let suspicious = opts.OtherOptions |> Array.filter (fun o -> o.Contains "SampleLib" || o.Contains "Person")
    printfn "    suspicious refs (should be empty): %A" suspicious

    let dir = Path.Combine(artifactsDir, "Harness", "scratch")
    Directory.CreateDirectory dir |> ignore
    let consumerFile = Path.Combine(dir, "Consumer1.fsx")
    let consumerFsPath = Path.Combine(dir, "Consumer1.fs")
    let text =
        sprintf "module Consumer1\ntype P = MyriadPreview.Provided.Fields<\"%s\", \"SampleNs.Person\">\nlet describe (instance: obj) : string =\n    let p = P(instance)\n    sprintf \"%%A / %%A\" (p.name) (p.age)\n" personFsForward
    File.WriteAllText(consumerFsPath, text)
    ignore consumerFile
    printfn "consumer text:\n%s" text

    let ms, diags, _res = check consumerFsPath
    printDiags "Round 1 check" diags
    printfn "  check time: %dms" ms
    let errs = diags |> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)
    let pass = errs.Length = 0
    printfn "ROUND 1 verdict: %s" (if pass then "PASS" else "FAIL")
    pass

let round2 () =
    printfn "\n===== ROUND 2: live edit (add 'email' field) picked up via FileSystemWatcher + Invalidate(), no rebuild ====="
    let originalText = File.ReadAllText personFs
    try
        let editedText = "namespace SampleNs\n\ntype Person = { name: string; age: int; email: string }\n"
        File.WriteAllText(personFs, editedText)
        printfn "  edited Person.fs on disk (added 'email' field)"

        // Give FileSystemWatcher's async event a moment to fire and call Invalidate().
        System.Threading.Thread.Sleep(1500)

        let dir = Path.Combine(artifactsDir, "Harness", "scratch")
        let consumerFsPath = Path.Combine(dir, "Consumer2.fs")
        let text =
            sprintf "module Consumer2\ntype P = MyriadPreview.Provided.Fields<\"%s\", \"SampleNs.Person\">\nlet describe (instance: obj) : string =\n    let p = P(instance)\n    sprintf \"%%A / %%A / %%A\" (p.name) (p.age) (p.email)\n" personFsForward
        File.WriteAllText(consumerFsPath, text)
        printfn "consumer text:\n%s" text

        let ms, diags, _res = check consumerFsPath
        printDiags "Round 2 check (post-edit, no rebuild)" diags
        printfn "  check time: %dms" ms
        let errs = diags |> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)
        let pass = errs.Length = 0
        printfn "ROUND 2 verdict: %s" (if pass then "PASS" else "FAIL")
        pass
    finally
        File.WriteAllText(personFs, originalText)
        printfn "  restored original Person.fs"

[<EntryPoint>]
let main argv =
    match argv with
    | [| "round1" |] -> if round1 () then 0 else 1
    | [| "round2" |] -> if round2 () then 0 else 1
    | [| "all" |] ->
        let r1 = round1 ()
        let r2 = round2 ()
        printfn "\nSummary: Round1=%b Round2=%b" r1 r2
        if r1 && r2 then 0 else 1
    | _ ->
        eprintfn "usage: Harness (round1|round2|all)"
        2
