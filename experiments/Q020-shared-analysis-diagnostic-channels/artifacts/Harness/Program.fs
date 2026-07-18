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

let runtimeDll = Path.Combine(artifactsDir, "DiagTP.Runtime", "bin", "Debug", "net9.0", "DiagTP.Runtime.dll")
let companyFs = Path.Combine(artifactsDir, "SampleLib", "Company.fs").Replace("\\", "/")

let refPackDir =
    let root = @"C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref"
    Directory.GetDirectories(root)
    |> Array.filter (fun d -> Path.GetFileName(d).StartsWith "9.")
    |> Array.sort
    |> Array.last
    |> fun v -> Path.Combine(v, "ref", "net9.0")

let fsharpCore = typeof<int list>.Assembly.Location
let checker = FSharpChecker.Create(keepAssemblyContents = true)

let mkOptions (consumerFile: string) =
    let sysRefs =
        Directory.GetFiles(refPackDir, "*.dll")
        |> Array.map (fun r -> "-r:" + r)
        |> Array.toList
    let otherOptions =
        [ "--targetprofile:netcore"; "--noframework"; "-r:" + fsharpCore; "-r:" + runtimeDll ] @ sysRefs
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
    let _parse, answer =
        checker.ParseAndCheckFileInProject(consumerFile, version, source, opts)
        |> Async.RunSynchronously
    match answer with
    | FSharpCheckFileAnswer.Succeeded res -> res.Diagnostics, Some res
    | FSharpCheckFileAnswer.Aborted -> [||], None

/// Independently computes the (line, startCol, endCol) of a named token's occurrence in `text`,
/// WITHOUT reading anything back from a diagnostic -- so a range assertion can't trivially pass by
/// construction. 1-based line, 0-based columns, matching FSharpDiagnostic's own convention.
let locate (text: string) (token: string) =
    let idx = text.IndexOf(token: string)
    if idx < 0 then failwithf "token '%s' not found in text" token
    let before = text.Substring(0, idx)
    let line = (before |> Seq.filter ((=) '\n') |> Seq.length) + 1
    let lastNewline = before.LastIndexOf '\n'
    let col = idx - lastNewline - 1
    (line, col, col + token.Length)

let printDiags label (diags: FSharpDiagnostic[]) =
    printfn "  [%s] %d diagnostic(s)" label diags.Length
    for d in diags do
        printfn "    %s (%d,%d)-(%d,%d) %s: %s" (string d.Severity) d.StartLine d.StartColumn d.EndLine d.EndColumn (d.ErrorNumberText) d.Message

let dir = Path.Combine(artifactsDir, "Harness", "scratch")

let round1 () =
    printfn "===== ROUND 1: AddObsoleteAttribute use-site anchoring + severity ====="
    Directory.CreateDirectory dir |> ignore
    let text =
        "module Consumer1\n"
        + "type S = DiagTP.Provided.Sample\n"
        + "let s = S()\n"
        + "let g = s.Good\n"
        + "let b   =   s.Bad\n"
        + "let e   =   s.BadError\n"
    let path = Path.Combine(dir, "Consumer1.fs")
    File.WriteAllText(path, text)
    printfn "consumer text:\n%s" text
    let diags, _res = check path
    printDiags "Round 1" diags

    let goodOk = diags |> Array.filter (fun d -> d.Message.Contains "Good") |> Array.isEmpty
    printfn "  Good produced no diagnostic (negative control): %b" goodOk

    // Found by running: FCS anchors the obsolete diagnostic at the whole member-access expression
    // ('s.Bad'), not the narrower 'Bad' property-name token alone. Still genuine use-site anchoring
    // (the right line, the right call), just coarser than the design's first assumption.
    let (bLine, bStart, bEnd) = locate text "s.Bad"
    let badDiag =
        diags |> Array.tryFind (fun d -> d.Message.Contains "Q020 test message" && d.Severity = FSharpDiagnosticSeverity.Warning)
    let badRangeOk =
        match badDiag with
        | Some d -> d.StartLine = bLine && d.StartColumn = bStart && d.EndLine = bLine && d.EndColumn = bEnd
        | None -> false
    printfn "  Bad: found=%b rangeOk=%b (expected (%d,%d)-(%d,%d), got %s)"
        badDiag.IsSome badRangeOk bLine bStart bLine bEnd
        (match badDiag with Some d -> sprintf "(%d,%d)-(%d,%d)" d.StartLine d.StartColumn d.EndLine d.EndColumn | None -> "<none>")

    let (eLine, eStart, eEnd) = locate text "s.BadError"
    let errorDiag =
        diags |> Array.tryFind (fun d -> d.Message.Contains "Q020 test message" && d.Severity = FSharpDiagnosticSeverity.Error)
    let errorRangeOk =
        match errorDiag with
        | Some d -> d.StartLine = eLine && d.StartColumn = eStart && d.EndLine = eLine && d.EndColumn = eEnd
        | None -> false
    printfn "  BadError: found=%b rangeOk=%b severity=Error confirmed=%b" errorDiag.IsSome errorRangeOk (errorDiag |> Option.map (fun d -> d.Severity = FSharpDiagnosticSeverity.Error) |> Option.defaultValue false)

    let pass = goodOk && badDiag.IsSome && badRangeOk && errorDiag.IsSome && errorRangeOk
    printfn "ROUND 1 verdict: %s" (if pass then "PASS" else "FAIL")
    pass

let round2 () =
    printfn "\n===== ROUND 2: synthetic backtick-named member -- legal AND discoverable ====="
    let text =
        "module Consumer2\n"
        + "type S = DiagTP.Provided.Sample\n"
        + "let s = S()\n"
        + "let x = s.``warning MYR099: synthetic diagnostic with no natural member``\n"
    let path = Path.Combine(dir, "Consumer2.fs")
    File.WriteAllText(path, text)
    printfn "consumer text:\n%s" text
    let diags, resOpt = check path
    printDiags "Round 2 (accessibility)" diags
    let accessible = diags |> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error) |> Array.isEmpty
    printfn "  accessible via double-backtick reference (0 errors): %b" accessible

    // Discoverability: ask for completions right after "s." on a separate line.
    let complText = "module Consumer2b\ntype S = DiagTP.Provided.Sample\nlet s = S()\nlet y = s.\n"
    let complPath = Path.Combine(dir, "Consumer2b.fs")
    File.WriteAllText(complPath, complText)
    let _diags2, res2Opt = check complPath
    let discoverable =
        match res2Opt with
        | None -> false
        | Some res ->
            let lineText = "let y = s."
            let lineNum = 4 // 1-based line of "let y = s."
            let colNum = lineText.Length // position right after the dot
            let partialName = FSharp.Compiler.EditorServices.QuickParse.GetPartialLongNameEx(lineText, colNum - 1)
            let decls : FSharp.Compiler.EditorServices.DeclarationListInfo =
                res.GetDeclarationListInfo(None, lineNum, lineText, partialName, (fun () -> []))
            decls.Items |> Array.exists (fun i -> i.NameInList.Contains "MYR099")
    printfn "  synthetic member name appears in GetDeclarationListInfo completions: %b" discoverable

    let pass = accessible && discoverable
    printfn "ROUND 2 verdict: %s" (if pass then "PASS" else "FAIL")
    pass

let round3 () =
    printfn "\n===== ROUND 3: one shared analysis, two consistent emitters ====="
    let text =
        sprintf
            "module Consumer3\ntype C = DiagTP.Provided.Fields<\"%s\", \"SampleNs.Company\">\nlet describe (o: obj) : obj =\n    let c = C(o)\n    c.meta\n"
            companyFs
    let path = Path.Combine(dir, "Consumer3.fs")
    File.WriteAllText(path, text)
    printfn "consumer text:\n%s" text
    let diags, _res = check path
    printDiags "Round 3 (live channel)" diags

    let liveDiag = diags |> Array.tryFind (fun d -> d.Message.Contains "cannot be resolved without a real build")
    printfn "  live (use-site) diagnostic found: %b" liveDiag.IsSome
    match liveDiag with
    | Some d -> printfn "  live message: %s" d.Message
    | None -> ()

    printfn "  --- Emitter A (declaration-site, Myriad-CLI-style) ---"
    let emitterDiags = SharedAnalysis.Analyze.analyze companyFs
    for d in emitterDiags do
        let (sl, sc, el, ec) = d.Range
        printfn "  %s(%d,%d,%d,%d): warning %s: %s" companyFs sl (sc + 1) el (ec + 1) d.Code d.Message

    let sameMessage =
        match liveDiag, emitterDiags |> List.tryHead with
        | Some live, Some emitted -> live.Message.Contains emitted.Message || emitted.Message = (live.Message.Replace("This construct is deprecated. ", "").Trim())
        | _ -> false
    printfn "  live channel message matches Emitter A's message (same underlying finding): %b" sameMessage

    let pass = liveDiag.IsSome && not emitterDiags.IsEmpty && sameMessage
    printfn "ROUND 3 verdict: %s" (if pass then "PASS" else "FAIL")
    pass

[<EntryPoint>]
let main argv =
    match argv with
    | [| "round1" |] -> if round1 () then 0 else 1
    | [| "round2" |] -> if round2 () then 0 else 1
    | [| "round3" |] -> if round3 () then 0 else 1
    | [| "all" |] ->
        let r1 = round1 ()
        let r2 = round2 ()
        let r3 = round3 ()
        printfn "\nSummary: Round1=%b Round2=%b Round3=%b" r1 r2 r3
        if r1 && r2 && r3 then 0 else 1
    | _ ->
        eprintfn "usage: Harness (round1|round2|round3|all)"
        2
