// Q013 Harness -- compile-then-PC warming probe.
//
// Q012 established, reproducibly, that checker.ParseAndCheckFileInProject (PC) NEVER resolves a
// generative provided type in this environment -- it always fails with "couldn't find type ... in
// assembly tmpXXXXXX" -- while checker.Compile does resolve it. Crucially, every scenario in Q012's
// harness ran PC BEFORE Compile, or ran them as independent probes. This harness tests the single
// untested ordering named by Q012's review and Q013's hypothesis:
//
//   Does a prior checker.Compile of a generative scenario "warm" a SUBSEQUENT
//   checker.ParseAndCheckFileInProject of the SAME scenario (same process, same checker instance),
//   causing PC to now resolve where a cold PC fails?
//
// The compile / parseAndCheck helpers are reused verbatim in structure from Q012's Harness
// (Q012-compiler-behavior-probe/artifacts/Harness/Program.fs), only parameterized to accept the
// FSharpChecker instance and the static-argument tag, so "same scenario" is guaranteed identical to
// what Q012 already measured -- the ordering and the checker instance are the ONLY things varied.
//
// Rounds:
//   Round 1 (falsifier): per repeat, fresh checker. (i) cold PC of a never-compiled tag -> expect
//            FAIL (reproduce Q012 cold-fail in-process). (ii) same-checker Compile(warmTag) then
//            PC(warmTag) -> the test. 3 repeats, fresh tags + fresh checker each, for determinism.
//   Round 2 (only if Round 1 positive): Compile on checker A, PC on a fresh checker B, same process.
//   Round 3 (only if Round 1 positive): retrofit onto Q011's real SchemaTP/ClientTP providers.

module Harness.Program

open System
open System.IO
open System.Diagnostics
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Text

// ROOT is the scratch copy of this artifacts tree, OUTSIDE the Myriad repo, so the repo's
// Directory.Build.props / global.json do not leak into the provider builds (Q012 used the same
// out-of-repo scratch approach). Overridable via env var Q013_ROOT.
let ROOT =
    match Environment.GetEnvironmentVariable "Q013_ROOT" with
    | null | "" -> @"C:\Users\Dave\q013-warm-spike"
    | d -> d

let logDir = Path.Combine(ROOT, "Harness", "logs")
let scratch = Path.Combine(ROOT, "Harness", "scratch")

let net8 (proj: string) name = Path.Combine(ROOT, proj, "bin", "Release", "net8.0", name)
let probeSimpleRt = net8 "ProbeSimple.Runtime" "ProbeSimple.Runtime.dll"
let schemaRt      = net8 "SchemaTP.Runtime" "SchemaTP.Runtime.dll"
let clientRt      = net8 "ClientTP.Runtime" "ClientTP.Runtime.dll"
let schemaRtDir   = Path.GetDirectoryName schemaRt

let mkChecker () = FSharpChecker.Create(keepAssemblyContents = true)

let clearLog (stem: string) =
    let p = Path.Combine(logDir, stem + ".log")
    if File.Exists p then File.Delete p
let readLog (stem: string) =
    let p = Path.Combine(logDir, stem + ".log")
    if File.Exists p then File.ReadAllLines p else [||]

let msgOneLine (m: string) = m.Replace("\r", " ").Replace("\n", " ")

// --- API 1: ParseAndCheckFileInProject (a script; refs from GetProjectOptionsFromScript) ---
// Structure copied verbatim from Q012 Harness `parseAndCheck`, parameterized by checker + refDll +
// instantiation text so the same generative scenario can be checked on any checker instance.
let parseAndCheck (checker: FSharpChecker) (label: string) (refDll: string) (instantiation: string) =
    let scriptPath = Path.Combine(scratch, label + "_pc.fsx")
    let text =
        sprintf "#r @\"%s\"\ntype T = %s\nlet _t = T()\nprintfn \"%%s\" (_t.Tag)\n" refDll instantiation
    File.WriteAllText(scriptPath, text)
    let source = SourceText.ofString text
    let sw = Stopwatch.StartNew()
    let opts, _optDiags =
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
    (sw.ElapsedMilliseconds, aborted, errs)

// --- API 2: checker.Compile (a real .fs -> library) ---
// Structure copied verbatim from Q012 Harness `compile`, parameterized by checker + refDll +
// consumer text so an arbitrary generative scenario can be compiled on any checker instance.
let compile (checker: FSharpChecker) (label: string) (refDll: string) (consumerText: string) =
    let consumerFs = Path.Combine(scratch, label + "_c.fs")
    File.WriteAllText(consumerFs, consumerText)
    let outDll = Path.Combine(scratch, label + "_out.dll")
    if File.Exists outDll then File.Delete outDll
    let args =
        [| "fsc.exe"; "-o"; outDll; "--target:library"; "-r:" + refDll; "--nowarn:57"; consumerFs |]
    let sw = Stopwatch.StartNew()
    let diags, exitCode = checker.Compile(args) |> Async.RunSynchronously
    sw.Stop()
    let errs = diags |> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)
    (sw.ElapsedMilliseconds, exitCode, errs, outDll)

let probeInstantiation (tag: string) = sprintf "ProbeSimple.Provided.Probe<\"%s\">" tag
// Compile consumer for ProbeSimple: identical shape to Q012's `compile` consumer text.
let probeConsumer (tag: string) =
    sprintf "module ProbeConsumer\ntype T = %s\nlet _t = T()\nlet tag : string = _t.Tag\nlet invocationCount : int = _t.InvocationCount\nlet callLog : string = _t.CallLog\n" (probeInstantiation tag)

let printErrs (indent: string) (label: string) (errs: FSharpDiagnostic[]) =
    let unique = errs |> Array.map (fun d -> d.Message) |> Array.distinct
    printfn "%s%s: %d error(s)%s" indent label errs.Length (if errs.Length = 0 then " (CLEAN/RESOLVED)" else " (FAIL)")
    for m in unique |> Array.truncate 4 do
        printfn "%s  DIAG: %s" indent (msgOneLine m)

// A single same-checker compile-then-PC trial on the ProbeSimple generative shape.
// Returns (compileExit, warmPcResolved).
let warmTrial (tag: string) =
    let checker = mkChecker ()
    printfn "  -- warm trial tag=%s (same checker instance for Compile then PC) --" tag
    clearLog "ProbeSimple"
    let cMs, exitCode, cErrs, outDll = compile checker ("warm_" + tag) probeSimpleRt (probeConsumer tag)
    printfn "    [1] checker.Compile               %dms exit=%d outExists=%b" cMs exitCode (File.Exists outDll)
    printErrs "        " "Compile" cErrs
    let cLog = readLog "ProbeSimple"
    printfn "        design-time firings during Compile (file log): %d" cLog.Length
    clearLog "ProbeSimple"
    let pcMs, aborted, pcErrs = parseAndCheck checker ("warm_" + tag) probeSimpleRt (probeInstantiation tag)
    let resolved = pcErrs.Length = 0 && not aborted
    printfn "    [2] ParseAndCheckFileInProject    %dms aborted=%b -> %s" pcMs aborted (if resolved then "RESOLVED" else "FAIL")
    printErrs "        " "PC" pcErrs
    let pcLog = readLog "ProbeSimple"
    printfn "        design-time firings during PC (file log): %d" pcLog.Length
    (exitCode, resolved)

// Cold PC control: never-compiled tag, fresh checker, PC only. Expected to reproduce Q012's cold-fail.
let coldTrial (tag: string) =
    let checker = mkChecker ()
    clearLog "ProbeSimple"
    let pcMs, aborted, pcErrs = parseAndCheck checker ("cold_" + tag) probeSimpleRt (probeInstantiation tag)
    let resolved = pcErrs.Length = 0 && not aborted
    printfn "  -- cold control tag=%s (fresh checker, PC only, no prior Compile) --" tag
    printfn "    ParseAndCheckFileInProject        %dms aborted=%b -> %s" pcMs aborted (if resolved then "RESOLVED" else "FAIL")
    printErrs "        " "PC" pcErrs
    resolved

let round1 () =
    printfn "======================================================================"
    printfn "ROUND 1 -- falsifier: same-checker compile-then-PC on ProbeSimple (generative)"
    printfn "======================================================================"
    let repeats = [ "r1a"; "r1b"; "r1c" ]
    let mutable coldResults = []
    let mutable warmResults = []
    for r in repeats do
        printfn ""
        printfn "### repeat %s ###" r
        let coldResolved = coldTrial ("cold-" + r)
        coldResults <- coldResults @ [ coldResolved ]
        let _exit, warmResolved = warmTrial ("warm-" + r)
        warmResults <- warmResults @ [ warmResolved ]
    printfn ""
    printfn "----------------------------------------------------------------------"
    printfn "ROUND 1 SUMMARY"
    printfn "  cold PC (no prior Compile):  %A  (all FAIL expected, reproducing Q012)" coldResults
    printfn "  warm PC (Compile then PC):   %A" warmResults
    let allWarmResolve = warmResults |> List.forall id
    let noneWarmResolve = warmResults |> List.forall not
    let verdict =
        if noneWarmResolve then "NULL -- compile-then-PC makes no difference; PC still fails after prior Compile"
        elif allWarmResolve then "POSITIVE -- warm PC resolves in every repeat; proceed to Round 2/3"
        else "KILL -- non-deterministic: warm PC sometimes resolves, sometimes not, no controlling variable"
    printfn "  VERDICT: %s" verdict
    printfn "----------------------------------------------------------------------"
    allWarmResolve

// ---- Round 2: does the effect need the SAME checker instance? ----
let round2 () =
    printfn ""
    printfn "======================================================================"
    printfn "ROUND 2 -- cross-instance: Compile on checker A, PC on fresh checker B (same process)"
    printfn "======================================================================"
    let repeats = [ "r2a"; "r2b"; "r2c" ]
    let mutable results = []
    for r in repeats do
        let tag = "cross-" + r
        let checkerA = mkChecker ()
        clearLog "ProbeSimple"
        let cMs, exitCode, cErrs, outDll = compile checkerA ("cross_" + tag) probeSimpleRt (probeConsumer tag)
        printfn "  [A] Compile tag=%s  %dms exit=%d outExists=%b errs=%d" tag cMs exitCode (File.Exists outDll) cErrs.Length
        let checkerB = mkChecker ()
        clearLog "ProbeSimple"
        let pcMs, aborted, pcErrs = parseAndCheck checkerB ("cross_" + tag) probeSimpleRt (probeInstantiation tag)
        let resolved = pcErrs.Length = 0 && not aborted
        printfn "  [B] PC (fresh checker) tag=%s  %dms -> %s" tag pcMs (if resolved then "RESOLVED" else "FAIL")
        printErrs "      " "PC" pcErrs
        results <- results @ [ resolved ]
    printfn "  ROUND 2 SUMMARY: cross-instance warm PC results = %A" results

// ---- Round 3: retrofit onto Q011's real providers ----
let round3 () =
    printfn ""
    printfn "======================================================================"
    printfn "ROUND 3 -- retrofit onto Q011's REAL SchemaTP / ClientTP providers"
    printfn "======================================================================"

    // SchemaTP: empty known-clients = Q011 step-1 guaranteed success path (same scenario as Q012 round3).
    printfn ""
    printfn "=== SchemaTP.Provided.Schema<\"Name:v1;Age:v2\", \"\"> (real Q011 provider) ==="
    let schemaInst = "SchemaTP.Provided.Schema<\"Name:v1;Age:v2\", \"\">"
    let schemaLog = "SchemaTP"
    // cold PC control (fresh checker, PC only)
    let cChecker = mkChecker ()
    clearLog schemaLog
    let scPcMs0, _ab0, scErrs0 = parseAndCheck cChecker "schema_cold" schemaRt schemaInst
    printfn "  cold  PC (fresh checker)        %dms -> %s" scPcMs0 (if scErrs0.Length = 0 then "RESOLVED" else "FAIL")
    printErrs "      " "PC-cold" scErrs0
    // warm: same checker Compile then PC
    let checker = mkChecker ()
    clearLog schemaLog
    let schemaCarrier = Path.Combine(schemaRtDir, "schemaCarrier.dll")
    let scCText = sprintf "module SchemaCarrier\ntype S = %s\nlet _s = S()\nlet name : string = _s.Name\n" schemaInst
    // Compile emits to the carrier location (Q012 round3 used this too); reuse compile helper's outDll otherwise.
    let scCMs, scExit, scCErrs, scOut = compile checker "schema_warm" schemaRt scCText
    // also produce the carrier dll ClientTP needs
    File.Copy(scOut, schemaCarrier, true)
    printfn "  [1]  Compile                     %dms exit=%d errs=%d outExists=%b" scCMs scExit scCErrs.Length (File.Exists scOut)
    clearLog schemaLog
    let scPcMs, scAb, scPcErrs = parseAndCheck checker "schema_warm" schemaRt schemaInst
    let scResolved = scPcErrs.Length = 0 && not scAb
    printfn "  [2]  PC (same checker as Compile) %dms -> %s" scPcMs (if scResolved then "RESOLVED" else "FAIL")
    printErrs "      " "PC-warm" scPcErrs

    // ClientTP: needs a schema carrier assembly to read FieldProvenanceAttribute from.
    printfn ""
    printfn "=== ClientTP.Provided.Client<schemaCarrier, \"Name;Age\"> (real Q011 provider) ==="
    let carrierFwd = schemaCarrier.Replace("\\", "/")
    let clientInst = sprintf "ClientTP.Provided.Client<\"%s\", \"Name;Age\">" carrierFwd
    let clientLog = "ClientTP"
    // cold PC control
    let cChecker2 = mkChecker ()
    clearLog clientLog
    let clPcMs0, _cab0, clErrs0 = parseAndCheck cChecker2 "client_cold" clientRt clientInst
    printfn "  cold  PC (fresh checker)        %dms -> %s" clPcMs0 (if clErrs0.Length = 0 then "RESOLVED" else "FAIL")
    printErrs "      " "PC-cold" clErrs0
    // warm: same checker Compile then PC
    let checker2 = mkChecker ()
    clearLog clientLog
    let clCText = sprintf "module ClientConsumer\ntype Dep = %s\nlet _d = Dep()\nlet name : string = _d.Name\n" clientInst
    let clCMs, clExit, clCErrs, clOut = compile checker2 "client_warm" clientRt clCText
    printfn "  [1]  Compile                     %dms exit=%d errs=%d outExists=%b" clCMs clExit clCErrs.Length (File.Exists clOut)
    for m in (clCErrs |> Array.map (fun d -> d.Message) |> Array.distinct |> Array.truncate 3) do
        printfn "        Compile DIAG: %s" (msgOneLine m)
    clearLog clientLog
    let clPcMs, clAb, clPcErrs = parseAndCheck checker2 "client_warm" clientRt clientInst
    let clResolved = clPcErrs.Length = 0 && not clAb
    printfn "  [2]  PC (same checker as Compile) %dms -> %s" clPcMs (if clResolved then "RESOLVED" else "FAIL")
    printErrs "      " "PC-warm" clPcErrs

[<EntryPoint>]
let main argv =
    Directory.CreateDirectory logDir |> ignore
    Directory.CreateDirectory scratch |> ignore
    Environment.SetEnvironmentVariable("PROBE_LOG_DIR", logDir)
    printfn "Q013 compile-then-PC warming harness"
    printfn "ROOT=%s" ROOT
    printfn "PROBE_LOG_DIR=%s" logDir
    printfn "checker: keepAssemblyContents=true, default compiler (TransparentCompiler off), fresh per condition"
    printfn ""
    match argv with
    | [| "round1" |] ->
        round1 () |> ignore
        0
    | [| "all" |] ->
        let positive = round1 ()
        if positive then
            round2 ()
            round3 ()
        else
            printfn ""
            printfn "Round 1 was NOT positive -> stopping per design (no Round 2/3 for a NULL effect)."
        0
    | [| "round2" |] -> round2 (); 0
    | [| "round3" |] -> round3 (); 0
    | _ ->
        eprintfn "usage: Harness (round1|all|round2|round3)"
        2
