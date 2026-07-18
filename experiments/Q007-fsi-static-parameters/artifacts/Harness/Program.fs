module Harness.Program

open System
open System.IO
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open FSharp.Compiler.Symbols
open FSharp.Compiler.Diagnostics

// ---------------------------------------------------------------------------
// Round 3 -- live-edit re-check cost, same cold-vs-live-edit methodology as Q006/Q008:
// a real, hand-built (non-script) FSharpProjectOptions, one persistent FSharpChecker
// instance, ParseAndCheckFileInProject timed with Stopwatch, single samples.
// ---------------------------------------------------------------------------

let artifactsDir =
    let baseDir = AppContext.BaseDirectory
    let mutable d = DirectoryInfo(baseDir)
    while d <> null && d.Name <> "artifacts" do d <- d.Parent
    d.FullName

let refPackDir =
    let root = @"C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref"
    Directory.GetDirectories(root)
    |> Array.filter (fun d -> Path.GetFileName(d).StartsWith "8.")
    |> Array.sort
    |> Array.last
    |> fun v -> Path.Combine(v, "ref", "net8.0")

let fsharpCore = typeof<int list>.Assembly.Location

let runtimeDll =
    Path.Combine(artifactsDir, "FsiParamTP.Runtime", "bin", "Release", "net8.0", "FsiParamTP.Runtime.dll")

let checker = FSharpChecker.Create(keepAssemblyContents = true)

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

/// Check a consumer file's on-disk text with the SHARED checker; returns (ms, diagnostics, results).
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

let hasErrors (diags: FSharpDiagnostic[]) =
    diags |> Array.exists (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)

let findSymbolType (res: FSharpCheckFileResults) (name: string) =
    res.GetAllUsesOfAllSymbolsInFile()
    |> Seq.tryPick (fun su ->
        match su.Symbol with
        | :? FSharpMemberOrFunctionOrValue as m when m.DisplayName = name -> Some (m.FullType.ToString())
        | _ -> None)

// consumer text: Container<Expr> with a fixed set of member references (Round 1 shape).
let intConsumerText (expr: string) =
    "module Consumer\n" +
    sprintf "type T = FsiParamTP.Provided.Container<\"%s\">\n" expr +
    "let p0 : int = T.P0\n" +
    "let p1 : int = T.P1\n" +
    "let p2 : int = T.P2\n"

// consumer text: Container<Expr> for a string-list Expr (Round 2 shape); `items` names both
// the list literal AND the properties referenced, so a live edit that renames one element is
// exercised as a genuine content change, not just a cosmetic one.
let listConsumerText (items: string list) =
    // Each item must appear as an ESCAPED string literal inside the outer static-argument string
    // literal that Container<"..."> itself is -- i.e. the generated .fs file's actual bytes need
    // `\"Name\"`, not a bare `"Name"` (which would end the outer literal early). Found by running:
    // the first version of this generator produced invalid syntax
    // (`Container<"["Name"; ...`), caught by real diagnostics from the harness's own check.
    let escapedItem (s: string) = sprintf "\\\"%s\\\"" s
    let exprLiteral = items |> List.map escapedItem |> String.concat "; "
    let refLines = items |> List.map (fun s -> sprintf "let v_%s : string = T.%s\n" s s) |> String.concat ""
    "module Consumer2\n" +
    sprintf "type T = FsiParamTP.Provided.Container<\"[%s]\">\n" exprLiteral +
    refLines

let round3Int () =
    printfn "===== ROUND 3a: int-expr (\"1+2\") cold vs live-edit ====="
    let dir = Path.Combine(AppContext.BaseDirectory, "consumers")
    Directory.CreateDirectory dir |> ignore
    let consumer = Path.Combine(dir, "R3IntConsumer.fs")

    printfn "-- step 1: cold check, Expr=\"1+2\" --"
    File.WriteAllText(consumer, intConsumerText "1+2")
    let msCold, diagsCold, resCold = check consumer
    printfn "  (check took %dms)" msCold
    printDiags "cold" diagsCold
    match resCold with
    | Some r ->
        match findSymbolType r "p0" with
        | Some t -> printfn "  T.P0 binding type: %s" t
        | None -> printfn "  p0 not resolved"
    | None -> ()
    let coldOk = not (hasErrors diagsCold)

    printfn "-- step 2: LIVE EDIT Expr \"1+2\" -> \"2+2\", SAME checker, no rebuild --"
    File.WriteAllText(consumer, intConsumerText "2+2")
    let msLive, diagsLive, _ = check consumer
    printfn "  (re-check took %dms)" msLive
    printDiags "live-edit" diagsLive
    let liveOk = not (hasErrors diagsLive)

    printfn ""
    printfn "  TIMING (compare vs Q006: 1137ms cold / 47ms live re-check):"
    printfn "    cold check        : %dms" msCold
    printfn "    live-edit re-check: %dms" msLive
    (coldOk && liveOk, msCold, msLive)

let round3List () =
    printfn "\n===== ROUND 3b: string-list expr cold vs live-edit ====="
    let dir = Path.Combine(AppContext.BaseDirectory, "consumers")
    Directory.CreateDirectory dir |> ignore
    let consumer = Path.Combine(dir, "R3ListConsumer.fs")

    let coldItems = [ "Name"; "Age"; "Email" ]
    let editedItems = [ "Name"; "Age"; "Phone" ]

    printfn "-- step 1: cold check, Expr=[\"Name\"; \"Age\"; \"Email\"] --"
    File.WriteAllText(consumer, listConsumerText coldItems)
    let msCold, diagsCold, _ = check consumer
    printfn "  (check took %dms)" msCold
    printDiags "cold" diagsCold
    let coldOk = not (hasErrors diagsCold)

    printfn "-- step 2: LIVE EDIT last element \"Email\" -> \"Phone\", SAME checker, no rebuild --"
    File.WriteAllText(consumer, listConsumerText editedItems)
    let msLive, diagsLive, _ = check consumer
    printfn "  (re-check took %dms)" msLive
    printDiags "live-edit" diagsLive
    let liveOk = not (hasErrors diagsLive)

    printfn ""
    printfn "  TIMING (compare vs Q006: 1137ms cold / 47ms live re-check):"
    printfn "    cold check        : %dms" msCold
    printfn "    live-edit re-check: %dms" msLive
    (coldOk && liveOk, msCold, msLive)

[<EntryPoint>]
let main argv =
    printfn "runtimeDll = %s" runtimeDll
    printfn "runtimeDll exists = %b\n" (File.Exists runtimeDll)
    let ok1, cold1, live1 = round3Int ()
    let ok2, cold2, live2 = round3List ()
    printfn "\n===== SUMMARY ====="
    printfn "Round 3a (int expr)  : ok=%b cold=%dms live-edit=%dms" ok1 cold1 live1
    printfn "Round 3b (list expr) : ok=%b cold=%dms live-edit=%dms" ok2 cold2 live2
    printfn "Q006 baseline        : cold=1137ms live-edit=47ms"
    if ok1 && ok2 then 0 else 1
