// Q012 Harness -- compiler-behavior probe.
//
// For each of three probe shapes (differing by exactly one factor: param count, then instantiation
// I/O), this runs the SAME two FCS entry points against a consumer that instantiates the probe type:
//   1. checker.ParseAndCheckFileInProject  -- the diagnostics-only API Q011 correction 2 found FAILING
//      to resolve its generative type on the success path. This is the API under test.
//   2. checker.Compile                     -- the full-compile API Q011 fell back to. Run as a control:
//      if a shape fails (1) but passes (2), that reproduces Q011's exact divergence for that shape.
//
// Instrumentation: each provider writes a per-invocation file log to $PROBE_LOG_DIR (cleared before
// each scenario), giving the true design-time invocation count + captured stack frames independent of
// the baked provided-member value. For a shape that resolves, the provided members InvocationCount /
// CallLog / Tag are also read back from compiled output (checker.Compile -> load -> reflect) to test
// the "expose via provided members" channel end to end.

module Harness.Program

open System
open System.IO
open System.Diagnostics
open System.Reflection
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Text

let ROOT = @"C:\Users\Dave\tp-compiler-behavior-probe-spike"
let logDir = Path.Combine(ROOT, "Harness", "logs")
let scratch = Path.Combine(ROOT, "Harness", "scratch")

let net8 (proj: string) name = Path.Combine(ROOT, proj, "bin", "Release", "net8.0", name)
let probeSimpleRt   = net8 "ProbeSimple.Runtime"   "ProbeSimple.Runtime.dll"
let probeTwoRt      = net8 "ProbeTwoParams.Runtime" "ProbeTwoParams.Runtime.dll"
let probeTwoIORt    = net8 "ProbeTwoParamsIO.Runtime" "ProbeTwoParamsIO.Runtime.dll"
let dummyDll        = Path.Combine(ROOT, "DummyAsm", "bin", "Release", "netstandard2.0", "DummyAsm.dll")
let schemaRt        = net8 "SchemaTP.Runtime" "SchemaTP.Runtime.dll"
let clientRt        = net8 "ClientTP.Runtime" "ClientTP.Runtime.dll"
let schemaRtDir     = Path.GetDirectoryName schemaRt

let checker = FSharpChecker.Create(keepAssemblyContents = true)

type Shape =
    { Name: string
      RuntimeDll: string
      LogStem: string
      // the "type T = <providerType>" instantiation text, given the runtime dll already referenced
      Instantiation: string }

let shapes =
    [ { Name = "ProbeSimple (1 param, no I/O)"
        RuntimeDll = probeSimpleRt
        LogStem = "ProbeSimple"
        Instantiation = "ProbeSimple.Provided.Probe<\"A\">" }
      { Name = "ProbeTwoParams (2 params, no I/O)"
        RuntimeDll = probeTwoRt
        LogStem = "ProbeTwoParams"
        Instantiation = sprintf "ProbeTwoParams.Provided.Probe<\"A\", \"X\">" }
      { Name = "ProbeTwoParamsIO (2 params, + I/O)"
        RuntimeDll = probeTwoIORt
        LogStem = "ProbeTwoParamsIO"
        Instantiation = sprintf "ProbeTwoParamsIO.Provided.Probe<\"A\", @\"%s\">" dummyDll } ]

let clearLogs (stem: string) =
    for suffix in [ ".log"; ".stack.txt" ] do
        let p = Path.Combine(logDir, stem + suffix)
        if File.Exists p then File.Delete p

let readCompactLog (stem: string) =
    let p = Path.Combine(logDir, stem + ".log")
    if File.Exists p then File.ReadAllLines p else [||]

let firstFewStack (stem: string) (maxLines: int) =
    let p = Path.Combine(logDir, stem + ".stack.txt")
    if File.Exists p then
        let all = File.ReadAllLines p
        all |> Array.truncate maxLines
    else [||]

// --- API 1: ParseAndCheckFileInProject (a script, so refs come from GetProjectOptionsFromScript) ---
let parseAndCheck (shape: Shape) =
    let scriptPath = Path.Combine(scratch, shape.LogStem + "_pc.fsx")
    let text =
        sprintf "#r @\"%s\"\ntype T = %s\nlet _t = T()\nprintfn \"%%s\" (_t.Tag)\n" shape.RuntimeDll shape.Instantiation
    File.WriteAllText(scriptPath, text)
    let source = SourceText.ofString text
    let sw = Stopwatch.StartNew()
    let opts, optDiags =
        checker.GetProjectOptionsFromScript(scriptPath, source, assumeDotNetFramework = false)
        |> Async.RunSynchronously
    let _parse, answer =
        checker.ParseAndCheckFileInProject(scriptPath, 0, source, opts) |> Async.RunSynchronously
    sw.Stop()
    let diags =
        match answer with
        | FSharpCheckFileAnswer.Succeeded res -> res.Diagnostics
        | FSharpCheckFileAnswer.Aborted -> [||]
    let errs = diags |> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)
    let aborted = match answer with FSharpCheckFileAnswer.Aborted -> true | _ -> false
    (sw.ElapsedMilliseconds, aborted, errs, optDiags)

// --- API 2: checker.Compile (a real .fs -> library) ---
let compile (shape: Shape) =
    let consumerFs = Path.Combine(scratch, shape.LogStem + "_c.fs")
    let text =
        sprintf "module ProbeConsumer\ntype T = %s\nlet _t = T()\nlet tag : string = _t.Tag\nlet invocationCount : int = _t.InvocationCount\nlet callLog : string = _t.CallLog\n" shape.Instantiation
    File.WriteAllText(consumerFs, text)
    let outDll = Path.Combine(scratch, shape.LogStem + "_out.dll")
    if File.Exists outDll then File.Delete outDll
    let args =
        [| "fsc.exe"; "-o"; outDll; "--target:library"; "-r:" + shape.RuntimeDll; "--nowarn:57"; consumerFs |]
    let sw = Stopwatch.StartNew()
    let diags, exitCode = checker.Compile(args) |> Async.RunSynchronously
    sw.Stop()
    let errs = diags |> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)
    (sw.ElapsedMilliseconds, exitCode, errs, outDll)

// Read the baked provided members back from a compiled library, in-process via reflection.
let readBack (shape: Shape) (outDll: string) =
    try
        let dir = Path.GetDirectoryName(shape.RuntimeDll)
        let resolver =
            ResolveEventHandler(fun _ args ->
                let name = AssemblyName(args.Name).Name
                let candidate = Path.Combine(dir, name + ".dll")
                if File.Exists candidate then Assembly.LoadFrom candidate else null)
        AppDomain.CurrentDomain.add_AssemblyResolve resolver
        let a = Assembly.LoadFrom outDll
        let t = a.GetType("ProbeConsumer")
        let getProp (n: string) =
            match t.GetProperty(n, BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static) with
            | null -> box "<no prop>"
            | p -> p.GetValue(null)
        Some (getProp "tag", getProp "invocationCount", getProp "callLog")
    with ex ->
        printfn "    readBack FAILED: %s" ex.Message
        None

// Run an explicit script body under ParseAndCheckFileInProject only, report resolved/fail.
let pcScript (label: string) (refDll: string) (body: string) =
    let scriptPath = Path.Combine(scratch, "sharp_" + label + ".fsx")
    let text = sprintf "#r @\"%s\"\n%s\n" refDll body
    File.WriteAllText(scriptPath, text)
    let source = SourceText.ofString text
    let opts, _ = checker.GetProjectOptionsFromScript(scriptPath, source, assumeDotNetFramework = false) |> Async.RunSynchronously
    let _p, answer = checker.ParseAndCheckFileInProject(scriptPath, 0, source, opts) |> Async.RunSynchronously
    let diags = match answer with FSharpCheckFileAnswer.Succeeded r -> r.Diagnostics | _ -> [||]
    let errs = diags |> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)
    printfn "  [PC] %-14s -> %d err  %s" label errs.Length (if errs.Length = 0 then "RESOLVED" else "FAIL")
    for m in (errs |> Array.map (fun d -> d.Message) |> Array.distinct |> Array.truncate 2) do
        printfn "        %s" (m.Replace("\r"," ").Replace("\n"," "))

// Compile explicit .fs text against a set of references; returns (exitCode, errs, outDll).
let compileText (label: string) (refDlls: string list) (fsText: string) (outDll: string) =
    let consumerFs = Path.Combine(scratch, "r3_" + label + ".fs")
    File.WriteAllText(consumerFs, fsText)
    if File.Exists outDll then (try File.Delete outDll with _ -> ())
    let args =
        [ yield "fsc.exe"; yield "-o"; yield outDll; yield "--target:library"
          for r in refDlls do yield "-r:" + r
          yield "--nowarn:57"; yield consumerFs ]
        |> List.toArray
    let sw = Stopwatch.StartNew()
    let diags, exitCode = checker.Compile(args) |> Async.RunSynchronously
    sw.Stop()
    let errs = diags |> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)
    printfn "  [Compile] %-12s -> %dms exit=%d %d err  outExists=%b" label sw.ElapsedMilliseconds exitCode errs.Length (File.Exists outDll)
    for m in (errs |> Array.map (fun d -> d.Message) |> Array.distinct |> Array.truncate 3) do
        printfn "        %s" (m.Replace("\r"," ").Replace("\n"," "))
    (exitCode, errs, outDll)

let printErrs (label: string) (errs: FSharpDiagnostic[]) =
    let unique = errs |> Array.map (fun d -> d.Message) |> Array.distinct
    printfn "    %s: %d error diagnostic(s)%s" label errs.Length (if errs.Length = 0 then " (CLEAN/RESOLVED)" else "")
    for m in unique |> Array.truncate 6 do
        printfn "      DIAG: %s" (m.Replace("\r"," ").Replace("\n"," "))

let runShape (round1: bool) (shape: Shape) =
    printfn "======================================================================"
    printfn "SHAPE: %s" shape.Name
    printfn "======================================================================"

    // --- API 1 under test ---
    clearLogs shape.LogStem
    let pcMs, aborted, pcErrs, optDiags = parseAndCheck shape
    let optErrs = optDiags |> List.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)
    printfn "[ParseAndCheckFileInProject] %dms  aborted=%b" pcMs aborted
    if not (List.isEmpty optErrs) then
        printfn "    (GetProjectOptionsFromScript reported %d error(s))" optErrs.Length
    printErrs "ParseAndCheck" pcErrs
    let pcLog = readCompactLog shape.LogStem
    printfn "    design-time invocations during ParseAndCheck (file log): %d" pcLog.Length
    for l in pcLog do printfn "      LOG: %s" l
    // capture the full stack of the ParseAndCheck run now, before the log is cleared for Compile
    let pcFullStack = firstFewStack shape.LogStem 40

    // --- API 2 control ---
    clearLogs shape.LogStem
    let cMs, exitCode, cErrs, outDll = compile shape
    printfn "[checker.Compile] %dms  exitCode=%d  outDllExists=%b" cMs exitCode (File.Exists outDll)
    printErrs "Compile" cErrs
    let cLog = readCompactLog shape.LogStem
    printfn "    design-time invocations during Compile (file log): %d" cLog.Length
    for l in cLog do printfn "      LOG: %s" l

    // --- read back provided members from the compiled output (the novel channel) ---
    if exitCode = 0 && File.Exists outDll then
        match readBack shape outDll with
        | Some (tag, invCount, callLog) ->
            printfn "    READBACK from compiled output: Tag=%A InvocationCount=%A" tag invCount
            printfn "    READBACK CallLog:"
            for l in (string callLog).Split('\n') do printfn "      %s" l
        | None -> ()

    if round1 then
        printfn ""
        printfn "[Round 1 falsifier] full stack captured inside the provider during the"
        printfn "ParseAndCheckFileInProject run (the API under test):"
        for l in pcFullStack do printfn "    %s" l
    printfn ""

[<EntryPoint>]
let main argv =
    Directory.CreateDirectory logDir |> ignore
    Directory.CreateDirectory scratch |> ignore
    Environment.SetEnvironmentVariable("PROBE_LOG_DIR", logDir)
    printfn "PROBE_LOG_DIR=%s" logDir
    printfn "checker: keepAssemblyContents=true, default (TransparentCompiler off unless specified)"
    printfn ""
    match argv with
    | [| "round1" |] ->
        runShape true (shapes |> List.head)
        0
    | [| "matrix" |] ->
        for s in shapes do runShape false s
        0
    | [| "all" |] ->
        runShape true (shapes |> List.head)
        for s in List.tail shapes do runShape false s
        0
    | [| "sharpen" |] ->
        printfn "Sharpening under ParseAndCheckFileInProject only (generative vs erased; defn vs expr):"
        let rt = probeSimpleRt
        pcScript "gen-defn"    rt "type T = ProbeSimple.Provided.Probe<\"A\">\nlet _t = T()\nprintfn \"%s\" _t.Tag"
        pcScript "gen-expr"    rt "let _t = ProbeSimple.Provided.Probe<\"A\">()\nprintfn \"%s\" _t.Tag"
        pcScript "erased-defn" rt "type T = ProbeSimple.Provided.ProbeErased<\"A\">\nlet _t = T()\nprintfn \"%s\" _t.Tag"
        pcScript "erased-expr" rt "let _t = ProbeSimple.Provided.ProbeErased<\"A\">()\nprintfn \"%s\" _t.Tag"
        0
    | [| "round3" |] ->
        printfn "ROUND 3 - confirm against Q011's REAL providers (instrumented, source else verbatim)."
        printfn ""
        // --- SchemaTP (real): empty known-clients = guaranteed success path, exactly Q011 step 1 shape ---
        printfn "=== SchemaTP.Provided.Schema<\"Name:v1;Age:v2\", \"\"> (real Q011 provider) ==="
        let schemaLog = Path.Combine(logDir, "SchemaTP.log")
        if File.Exists schemaLog then File.Delete schemaLog
        pcScript "schema-PC" schemaRt "type S = SchemaTP.Provided.Schema<\"Name:v1;Age:v2\", \"\">\nlet _s = S()\nprintfn \"%s\" _s.Name"
        let scLog1 = if File.Exists schemaLog then File.ReadAllLines schemaLog else [||]
        printfn "    design-time firings during ParseAndCheck: %d" scLog1.Length
        for l in scLog1 do printfn "      LOG: %s" l
        if File.Exists schemaLog then File.Delete schemaLog
        let schemaCarrier = Path.Combine(schemaRtDir, "schemaCarrier.dll")
        let ec, _, _ =
            compileText "schema-Comp" [ schemaRt ]
                "module SchemaCarrier\ntype S = SchemaTP.Provided.Schema<\"Name:v1;Age:v2\", \"\">\nlet _s = S()\nlet name : string = _s.Name\n"
                schemaCarrier
        let scLog2 = if File.Exists schemaLog then File.ReadAllLines schemaLog else [||]
        printfn "    design-time firings during Compile: %d" scLog2.Length
        for l in scLog2 do printfn "      LOG: %s" l
        printfn ""
        // --- ClientTP (real): needs a schema carrier assembly to read FieldProvenanceAttribute from ---
        printfn "=== ClientTP.Provided.Client<schemaCarrier, \"Name;Age\"> (real Q011 provider) ==="
        printfn "    (schemaCarrier.dll built above via Compile, exit=%d, colocated with SchemaTP.Runtime.dll)" ec
        let clientLog = Path.Combine(logDir, "ClientTP.log")
        if File.Exists clientLog then File.Delete clientLog
        let carrierFwd = schemaCarrier.Replace("\\","/")
        pcScript "client-PC" clientRt (sprintf "type Dep = ClientTP.Provided.Client<\"%s\", \"Name;Age\">\nlet _d = Dep()\nprintfn \"%%s\" _d.Name" carrierFwd)
        let clLog1 = if File.Exists clientLog then File.ReadAllLines clientLog else [||]
        printfn "    design-time firings during ParseAndCheck: %d" clLog1.Length
        for l in clLog1 do printfn "      LOG: %s" l
        if File.Exists clientLog then File.Delete clientLog
        let clientOut = Path.Combine(scratch, "clientOut.dll")
        compileText "client-Comp" [ clientRt ]
            (sprintf "module ClientConsumer\ntype Dep = ClientTP.Provided.Client<\"%s\", \"Name;Age\">\nlet _d = Dep()\nlet name : string = _d.Name\n" carrierFwd)
            clientOut |> ignore
        let clLog2 = if File.Exists clientLog then File.ReadAllLines clientLog else [||]
        printfn "    design-time firings during Compile: %d" clLog2.Length
        for l in clLog2 do printfn "      LOG: %s" l
        0
    | _ ->
        eprintfn "usage: Harness (round1|matrix|all)"
        2
