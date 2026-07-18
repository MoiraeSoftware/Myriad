module Harness.Program

open System
open System.IO
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open FSharp.Compiler.Symbols
open FSharp.Compiler.Diagnostics

// ---------------------------------------------------------------------------
// Reference set: net8.0 ref pack + FSharp.Core (as loaded by the harness) +
// the built ClientTP.Runtime.dll (the TPRTC for the client provider). Its
// design-time DLL sits next to it and is discovered by FCS via the
// TypeProviderAssembly attribute. SchemaAsm.dll is NOT referenced by the
// consumer; ClientTP loads it by path (a static argument) at instantiation.
// ---------------------------------------------------------------------------

let spikeDir =
    let baseDir = AppContext.BaseDirectory
    let mutable d = DirectoryInfo(baseDir)
    while d <> null && d.Name <> "tp-provenance-spike" do d <- d.Parent
    d.FullName

let refPackDir =
    let root = @"C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref"
    Directory.GetDirectories(root)
    |> Array.filter (fun d -> Path.GetFileName(d).StartsWith "8.")
    |> Array.sort
    |> Array.last
    |> fun v -> Path.Combine(v, "ref", "net8.0")

let fsharpCore = typeof<int list>.Assembly.Location

let clientRuntimeDll =
    Path.Combine(spikeDir, "ClientTP.Runtime", "bin", "Release", "net8.0", "ClientTP.Runtime.dll")

// The independently-built schema assembly ClientTP reflects into. Forward slashes so the
// path embeds cleanly into an F# string literal.
let schemaAsmPath =
    Path.Combine(spikeDir, "SchemaAsm", "bin", "Release", "net8.0", "SchemaAsm.dll").Replace("\\", "/")

// The emitted generative type's full name, as observed by AttrCheck in Round 1.
let schemaTypeName = "SchemaAsm.Schemas+SchemaV2"

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

let findSymbolType (res: FSharpCheckFileResults) (name: string) =
    res.GetAllUsesOfAllSymbolsInFile()
    |> Seq.tryPick (fun su ->
        match su.Symbol with
        | :? FSharpMemberOrFunctionOrValue as m when m.DisplayName = name -> Some (m.FullType.ToString())
        | _ -> None)

let hasErrors (diags: FSharpDiagnostic[]) =
    diags |> Array.exists (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)

// consumer text: ClientTP.Client<schemaAsm, schemaTypeName, expected>
let clientConsumerText (expected: string) =
    "module Consumer\n" +
    sprintf "type C = ClientTP.Provided.Client<SchemaAssemblyPath = \"%s\", SchemaTypeName = \"%s\", ExpectedVersion = \"%s\">\n"
        schemaAsmPath schemaTypeName expected +
    "let v : string = C.VerifiedVersion\n" +
    "let ok : bool = C.ProvenanceOk\n"

// ---------------------------------------------------------------------------

let round2 () =
    printfn "===== ROUND 2: two-provider provenance gate, both directions ====="
    let dir = Path.Combine(AppContext.BaseDirectory, "consumers")
    Directory.CreateDirectory dir |> ignore
    let consumer = Path.Combine(dir, "R2Consumer.fs")

    printfn "-- direction 1: schema=v2, client expects v2 (MATCH, expect 0 diagnostics) --"
    File.WriteAllText(consumer, clientConsumerText "v2")
    let ms1, diags1, res1 = check consumer
    printfn "  (check took %dms)" ms1
    printDiags "match" diags1
    match res1 with
    | Some r ->
        match findSymbolType r "v" with
        | Some t -> printfn "  C.VerifiedVersion binding type: %s" t
        | None -> printfn "  v not resolved"
    | None -> ()
    let matchPass = not (hasErrors diags1)

    printfn "-- direction 2: schema=v2, client expects v3 (MISMATCH, expect real diagnostic) --"
    File.WriteAllText(consumer, clientConsumerText "v3")
    let ms2, diags2, _ = check consumer
    printfn "  (check took %dms)" ms2
    printDiags "mismatch" diags2
    printfn "  --- verbatim diagnostic messages on mismatch ---"
    for d in diags2 do printfn "  >>> %s" d.Message
    let mismatchPass = hasErrors diags2

    let pass = matchPass && mismatchPass
    printfn "ROUND 2 verdict: %s (match=%b mismatch-errors=%b)" (if pass then "PASS" else "FAIL") matchPass mismatchPass
    pass

let round3 () =
    printfn "\n===== ROUND 3: live-edit provenance violation, SAME checker, no rebuild ====="
    let dir = Path.Combine(AppContext.BaseDirectory, "consumers")
    Directory.CreateDirectory dir |> ignore
    let consumer = Path.Combine(dir, "R3Consumer.fs")

    printfn "-- step 1: cold check, expects v2 (MATCH) --"
    File.WriteAllText(consumer, clientConsumerText "v2")
    let ms1, diags1, _ = check consumer
    printfn "  (check took %dms)" ms1
    printDiags "cold-match" diags1
    let ok1 = not (hasErrors diags1)

    printfn "-- step 2: LIVE EDIT ExpectedVersion v2 -> v3, SAME checker, no rebuild (MISMATCH) --"
    File.WriteAllText(consumer, clientConsumerText "v3")
    let ms2, diags2, _ = check consumer
    printfn "  (re-check took %dms)" ms2
    printDiags "live-mismatch" diags2
    let ok2 = hasErrors diags2

    printfn "-- step 3: LIVE EDIT back v3 -> v2, SAME checker (diagnostics should clear) --"
    File.WriteAllText(consumer, clientConsumerText "v2")
    let ms3, diags3, _ = check consumer
    printfn "  (re-check took %dms)" ms3
    printDiags "live-cleared" diags3
    let ok3 = not (hasErrors diags3)

    printfn ""
    printfn "  TIMING TABLE (compare vs Q006: 1137ms cold / 47ms live re-check):"
    printfn "    step 1 cold match           : %dms" ms1
    printfn "    step 2 live-edit mismatch   : %dms" ms2
    printfn "    step 3 live-edit cleared    : %dms" ms3

    let pass = ok1 && ok2 && ok3
    printfn "ROUND 3 verdict: %s (cold=%b mismatch-caught=%b cleared=%b)" (if pass then "PASS" else "FAIL") ok1 ok2 ok3
    pass

[<EntryPoint>]
let main argv =
    printfn "Round 1 (independent-reflection falsifier) validated separately by AttrCheck: PASS."
    printfn "schemaAsmPath = %s" schemaAsmPath
    printfn "schemaTypeName = %s\n" schemaTypeName
    let r2 = round2 ()
    let r3 = round3 ()
    printfn "\nSummary: R2=%b R3=%b" r2 r3
    if r2 && r3 then 0 else 1
