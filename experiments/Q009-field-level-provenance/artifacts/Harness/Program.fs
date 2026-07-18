module Harness.Program

open System
open System.IO
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open FSharp.Compiler.Symbols
open FSharp.Compiler.Diagnostics

// ---------------------------------------------------------------------------
// Reference set: net8.0 ref pack + FSharp.Core + the built ClientTP.Runtime.dll (the client
// provider's TPRTC; its design-time DLL sits next to it and is discovered via the
// TypeProviderAssembly attribute). SchemaAsm.dll is NOT referenced by the consumer; ClientTP
// loads it by path (a static argument) at instantiation and reads the per-field attributes.
// ---------------------------------------------------------------------------

let spikeDir =
    let baseDir = AppContext.BaseDirectory
    let mutable d = DirectoryInfo(baseDir)
    while d <> null && d.Name <> "tp-field-provenance-spike" do d <- d.Parent
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

let schemaAsmPath =
    Path.Combine(spikeDir, "SchemaAsm", "bin", "Release", "net8.0", "SchemaAsm.dll").Replace("\\", "/")

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

// consumer text: ClientTP.Client<schemaAsm, schemaTypeName, dependsOn>
let clientConsumerText (schemaTypeName: string) (dependsOn: string) =
    "module Consumer\n" +
    sprintf "type C = ClientTP.Provided.Client<SchemaAssemblyPath = \"%s\", SchemaTypeName = \"%s\", DependsOn = \"%s\">\n"
        schemaAsmPath schemaTypeName dependsOn +
    "let n : int = C.CheckedFieldCount\n" +
    "let ok : bool = C.ProvenanceOk\n"

let tn (name: string) = "SchemaAsm.Schemas+" + name

// ---------------------------------------------------------------------------

let round2 () =
    printfn "===== ROUND 2: selective-enforcement gate, four scenarios ====="
    let dir = Path.Combine(AppContext.BaseDirectory, "consumers")
    Directory.CreateDirectory dir |> ignore
    let consumer = Path.Combine(dir, "R2Consumer.fs")
    let mutable allPass = true

    // Scenario 1 — baseline match
    printfn "-- scenario 1: schema Name:v1;Age:v2;Email:v3, client DependsOn Name:v1;Age:v2 (MATCH) --"
    File.WriteAllText(consumer, clientConsumerText (tn "SchemaBaseline") "Name:v1;Age:v2")
    let ms1, d1, res1 = check consumer
    printfn "  (check took %dms)" ms1
    printDiags "baseline" d1
    match res1 with
    | Some r ->
        match findSymbolType r "n" with
        | Some t -> printfn "  C.CheckedFieldCount binding type: %s" t
        | None -> printfn "  n not resolved"
    | None -> ()
    let s1 = not (hasErrors d1)
    allPass <- allPass && s1

    // Scenario 2 — irrelevant-field change (Email v3->v4), client never mentions Email
    printfn "\n-- scenario 2: schema Email v3->v4 (IRRELEVANT), client DependsOn Name:v1;Age:v2 (expect CLEAN) --"
    File.WriteAllText(consumer, clientConsumerText (tn "SchemaEmailBumped") "Name:v1;Age:v2")
    let ms2, d2, _ = check consumer
    printfn "  (check took %dms)" ms2
    printDiags "irrelevant-change" d2
    let s2 = not (hasErrors d2)
    allPass <- allPass && s2

    // Scenario 3 — relevant-field change (Age v2->v3), client depends on Age
    printfn "\n-- scenario 3: schema Age v2->v3 (RELEVANT), client DependsOn Name:v1;Age:v2 (expect FAIL naming Age) --"
    File.WriteAllText(consumer, clientConsumerText (tn "SchemaAgeBumped") "Name:v1;Age:v2")
    let ms3, d3, _ = check consumer
    printfn "  (check took %dms)" ms3
    printDiags "relevant-change" d3
    printfn "  --- VERBATIM diagnostic messages (relevant-field change) ---"
    for d in d3 do printfn "  >>> %s" d.Message
    let s3 = hasErrors d3
    allPass <- allPass && s3

    // Scenario 4 — multiple simultaneous relevant changes (Name AND Age)
    printfn "\n-- scenario 4: schema Name v1->v2 AND Age v2->v3, client DependsOn Name:v1;Age:v2 (expect FAIL naming BOTH) --"
    File.WriteAllText(consumer, clientConsumerText (tn "SchemaNameAgeBumped") "Name:v1;Age:v2")
    let ms4, d4, _ = check consumer
    printfn "  (check took %dms)" ms4
    printDiags "multi-change" d4
    printfn "  --- VERBATIM diagnostic messages (multiple relevant changes) ---"
    for d in d4 do printfn "  >>> %s" d.Message
    let s4 = hasErrors d4
    allPass <- allPass && s4

    printfn "\nROUND 2 verdict: %s (s1-match=%b s2-irrelevant-clean=%b s3-relevant-fail=%b s4-multi-fail=%b)"
        (if allPass then "PASS" else "FAIL") s1 s2 s3 s4
    allPass

// Generic 4-step live-edit sequence for a given schema size, on the SAME checker.
let round3Sequence label (consumerName: string)
        (baselineType, baselineDeps)
        (irrelevantType)
        (relevantType)
        =
    printfn "\n----- ROUND 3 (%s): live-edit sequence, SAME checker, no rebuild -----" label
    let dir = Path.Combine(AppContext.BaseDirectory, "consumers")
    Directory.CreateDirectory dir |> ignore
    let consumer = Path.Combine(dir, consumerName)

    printfn "-- step 1: cold check, baseline (MATCH) --"
    File.WriteAllText(consumer, clientConsumerText (tn baselineType) baselineDeps)
    let ms1, d1, _ = check consumer
    printfn "  (check took %dms)" ms1
    printDiags "cold-match" d1
    let ok1 = not (hasErrors d1)

    printfn "-- step 2: LIVE EDIT to schema variant with an IRRELEVANT field bumped, SAME checker (expect CLEAN) --"
    File.WriteAllText(consumer, clientConsumerText (tn irrelevantType) baselineDeps)
    let ms2, d2, _ = check consumer
    printfn "  (re-check took %dms)" ms2
    printDiags "live-irrelevant" d2
    let ok2 = not (hasErrors d2)

    printfn "-- step 3: LIVE EDIT to schema variant with a RELEVANT field bumped, SAME checker (expect FIELD-SPECIFIC FAIL) --"
    File.WriteAllText(consumer, clientConsumerText (tn relevantType) baselineDeps)
    let ms3, d3, _ = check consumer
    printfn "  (re-check took %dms)" ms3
    printDiags "live-relevant" d3
    for d in d3 do printfn "  >>> %s" d.Message
    let ok3 = hasErrors d3

    printfn "-- step 4: LIVE EDIT back to baseline, SAME checker (diagnostics should clear) --"
    File.WriteAllText(consumer, clientConsumerText (tn baselineType) baselineDeps)
    let ms4, d4, _ = check consumer
    printfn "  (re-check took %dms)" ms4
    printDiags "live-cleared" d4
    let ok4 = not (hasErrors d4)

    printfn ""
    printfn "  TIMING TABLE [%s] (compare vs Q008: 1161ms cold / 19-32ms live re-check):" label
    printfn "    step 1 cold match          : %dms" ms1
    printfn "    step 2 live irrelevant     : %dms" ms2
    printfn "    step 3 live relevant fail  : %dms" ms3
    printfn "    step 4 live cleared        : %dms" ms4

    let pass = ok1 && ok2 && ok3 && ok4
    printfn "  ROUND 3 [%s] verdict: %s (cold=%b irrelevant-clean=%b relevant-fail=%b cleared=%b)"
        label (if pass then "PASS" else "FAIL") ok1 ok2 ok3 ok4
    pass, (ms1, ms2, ms3, ms4)

let round3 () =
    printfn "\n===== ROUND 3: live-edit precision, same FSharpChecker, growing field count ====="
    // Small schema (3 fields, client depends on 2)
    let smallPass, smallT =
        round3Sequence "SMALL 3-field, client deps 2" "R3SmallConsumer.fs"
            ("SchemaBaseline", "Name:v1;Age:v2")
            "SchemaEmailBumped"   // Email irrelevant to client
            "SchemaAgeBumped"     // Age relevant to client
    // Wide schema (12 fields, client depends on 4)
    let widePass, wideT =
        round3Sequence "WIDE 12-field, client deps 4" "R3WideConsumer.fs"
            ("WideBaseline", "F0:v1;F3:v1;F7:v1;F9:v1")
            "WideIrrelevantBump"  // F5 bumped, not a dependency
            "WideRelevantBump"    // F3 bumped, a dependency

    let (s1,s2,s3,s4) = smallT
    let (w1,w2,w3,w4) = wideT
    printfn "\n  SCALING COMPARISON (small 3-field vs wide 12-field, live re-check ms):"
    printfn "    cold match        : small %dms | wide %dms" s1 w1
    printfn "    live irrelevant   : small %dms | wide %dms" s2 w2
    printfn "    live relevant fail: small %dms | wide %dms" s3 w3
    printfn "    live cleared      : small %dms | wide %dms" s4 w4
    smallPass && widePass

[<EntryPoint>]
let main argv =
    printfn "Round 1 (independent member-level reflection) validated separately by AttrCheck."
    printfn "schemaAsmPath = %s\n" schemaAsmPath
    let r2 = round2 ()
    let r3 = round3 ()
    printfn "\nSummary: R2=%b R3=%b" r2 r3
    if r2 && r3 then 0 else 1
