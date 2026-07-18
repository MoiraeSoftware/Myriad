// Q011 Harness - reversed-arrow consumer-driven-contract enforcement (Rounds 2 + 3).
//
// METHODOLOGY CORRECTION (reported in full in 02-results.md, not silently substituted): the design
// calls for Q008/Q009's ParseAndCheckFileInProject/ParseAndCheckProject diagnostics-only live-edit
// harness. Isolated testing (see 02-results.md) showed that path fails to resolve THIS quartet's
// generative type on its own SUCCESS path ("couldn't find type", even with zero possibility of an
// exception) under FSharpChecker's diagnostics-only checking, while the identical provider succeeds
// via a real `dotnet fsi` run and via `checker.Compile`. This harness therefore uses `checker.Compile`
// (a real, full compile via FCS) for every scenario check below -- still driven by FCS, still re-run
// after each live edit of the on-disk consumer file, just not the sub-50ms incremental-typecheck-only
// path Q008/Q009 used. Timings below are COMPILE times, not incremental-recheck times -- reported and
// compared as such, not conflated with Q008/Q009's numbers.
//
// mode "phase1": steps 1,3,4,5 (ClientCo.dll = v1, records Age:v2), a same-process identical-recheck
//   probe, THEN THE UNWEDGE: rebuilds ClientCo from source against SchemaAsmV2 (Age:v3) via a real
//   `dotnet build` subprocess while this process is still alive, then re-checks step 5's exact
//   scenario to see whether the SAME process picks up the rebuilt DLL cleanly.
// mode "phase2": fresh-process re-check of step 5's exact scenario, step 7 (re-confirm not-stale-pass)
//   and Round 3 (field removal).

module Harness.Program

open System
open System.IO
open System.Diagnostics
open System.Reflection
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics

let WIN = @"C:\Users\Dave\tp-consumer-driven-contracts-spike"
let fwd (p: string) = p.Replace('\\', '/')

let schemaRuntimeDll  = Path.Combine(WIN, @"SchemaTP.Runtime\bin\Release\netstandard2.0\SchemaTP.Runtime.dll")
let clientCoDll        = Path.Combine(WIN, @"ClientCo\bin\Release\net8.0\ClientCo.dll")
let clientCoFsproj     = Path.Combine(WIN, @"ClientCo\ClientCo.fsproj")
let clientCoProgramFs  = Path.Combine(WIN, @"ClientCo\Program.fs")
let schemaV1Dll = fwd (Path.Combine(WIN, @"SchemaAsmV1\bin\Release\net8.0\SchemaAsmV1.dll"))
let schemaV2Dll = fwd (Path.Combine(WIN, @"SchemaAsmV2\bin\Release\net8.0\SchemaAsmV2.dll"))
let consumerFsPath = Path.Combine(WIN, @"Harness\SchemaConsumer.fs")
let outDllTemplate = Path.Combine(WIN, @"Harness\out_")

let clientCoProgramV1 =
    sprintf "module ClientCo.Main\ntype Dep = ClientTP.Provided.Client<\"%s\", \"Name;Age\">\n\n[<EntryPoint>]\nlet main _ =\n    let d = Dep()\n    printfn \"ClientCo generated. Name=%%s Age=%%s\" (d.Name) (d.Age)\n    0\n" schemaV1Dll

let clientCoProgramV2 =
    sprintf "module ClientCo.Main\ntype Dep = ClientTP.Provided.Client<\"%s\", \"Name;Age\">\n\n[<EntryPoint>]\nlet main _ =\n    let d = Dep()\n    printfn \"ClientCo generated. Name=%%s Age=%%s\" (d.Name) (d.Age)\n    0\n" schemaV2Dll

let mutable resolverHooked = false
let registerProbeDir (path: string) =
    if not resolverHooked then
        resolverHooked <- true
        let dir = Path.GetDirectoryName(path: string)
        AppDomain.CurrentDomain.add_AssemblyResolve(ResolveEventHandler(fun _ args ->
            let name = AssemblyName(args.Name).Name
            let candidate = Path.Combine(dir, name + ".dll")
            if File.Exists candidate then
                try Assembly.LoadFrom candidate with _ -> null
            else null))

let dumpClientConsumes (path: string) =
    registerProbeDir path
    let bytes = File.ReadAllBytes(path)
    let a = Assembly.Load(bytes)
    let flags = BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static ||| BindingFlags.Instance
    [ for t in a.GetTypes() do
        for p in t.GetProperties(flags) do
            for cad in p.GetCustomAttributesData() do
                if cad.AttributeType.FullName = "ClientTP.Runtime.ConsumesFieldAttribute" then
                    yield (cad.ConstructorArguments.[0].Value :?> string, cad.ConstructorArguments.[1].Value :?> string) ]
    |> List.distinct

let consumerText (fieldSpec: string) (clientPaths: string) =
    sprintf "module SchemaConsumer\ntype S = SchemaTP.Provided.Schema<\"%s\", \"%s\">\nlet _s = S()\n" fieldSpec clientPaths

let checker = FSharpChecker.Create()
let mutable seq = 0

// One real `checker.Compile` call per scenario. Returns (elapsedMs, errorDiagnostics, distinctMessages).
let check (label: string) (fieldSpec: string) (clientPaths: string) =
    seq <- seq + 1
    let text = consumerText fieldSpec clientPaths
    File.WriteAllText(consumerFsPath, text)
    let outDll = outDllTemplate + string seq + ".dll"
    let args =
        [| "fsc.exe"; "-o"; outDll; "--target:library"; "-r:" + schemaRuntimeDll
           "--nowarn:57"; consumerFsPath |]
    let sw = Stopwatch.StartNew()
    let diags, exitCode = checker.Compile(args) |> Async.RunSynchronously
    sw.Stop()
    let errs = diags |> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)
    printfn "--- %s" label
    printfn "    FieldSpec=%A  KnownClients=%s" fieldSpec (if clientPaths = "" then "<none>" else "ClientCo.dll")
    printfn "    compile: %dms, exitCode=%d, %d error diagnostic(s)%s" sw.ElapsedMilliseconds exitCode errs.Length (if errs.Length = 0 then " (CLEAN)" else "")
    let unique = errs |> Array.map (fun d -> d.Message) |> Array.distinct
    for m in unique do printfn "    DIAG: %s" (m.Replace("\r"," ").Replace("\n"," "))
    (sw.ElapsedMilliseconds, errs, unique)

let runDotnetBuild (fsproj: string) =
    let psi = ProcessStartInfo("dotnet", sprintf "build \"%s\" -c Release" fsproj)
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.UseShellExecute <- false
    let p = Process.Start(psi)
    let out = p.StandardOutput.ReadToEnd()
    let err = p.StandardError.ReadToEnd()
    p.WaitForExit()
    (p.ExitCode, out, err)

let phase1 () =
    printfn "=== PHASE 1 (single process; ClientCo.dll starts as v1, records Age:v2) ==="
    File.WriteAllText(clientCoProgramFs, clientCoProgramV1)
    let code0, out0, err0 = runDotnetBuild clientCoFsproj
    printfn "  (setup) rebuilt ClientCo against SchemaAsmV1 to guarantee known-good v1 start state: exit=%d" code0
    if code0 <> 0 then printfn "STDOUT:\n%s\nSTDERR:\n%s" out0 err0
    printfn "ClientCo recorded dependencies (independent reflection, v1 state): %A" (dumpClientConsumes clientCoDll)
    printfn ""
    let cc = fwd clientCoDll
    check "step1: schema v1, ZERO known clients" "Name:v1;Age:v2;Email:v3" "" |> ignore
    check "step3: schema v1 (Age:v2), ClientCo known -> expect MATCH" "Name:v1;Age:v2;Email:v3" cc |> ignore
    check "step4: bump Email v3->v4 (irrelevant to ClientCo) -> expect CLEAN" "Name:v1;Age:v2;Email:v4" cc |> ignore
    let _, errs5, diags5 = check "step5: bump Age v2->v3 (ClientCo recorded v2) -> expect STALE BLOCK" "Name:v1;Age:v3;Email:v4" cc
    printfn ""
    printfn "=== probe: re-check IDENTICAL step5 scenario, same process, no external change ==="
    check "step5-repeat (identical text, same process)" "Name:v1;Age:v3;Email:v4" cc |> ignore
    printfn ""

    printfn "=== THE UNWEDGE: recompile ClientCo from source against SchemaAsmV2 (Age:v3) ==="
    File.WriteAllText(clientCoProgramFs, clientCoProgramV2)
    let swBuild = Stopwatch.StartNew()
    let code, out, err = runDotnetBuild clientCoFsproj
    swBuild.Stop()
    printfn "  dotnet build ClientCo (repointed at SchemaAsmV2) exit=%d in %dms" code swBuild.ElapsedMilliseconds
    if code <> 0 then printfn "STDOUT:\n%s\nSTDERR:\n%s" out err
    let fi = FileInfo(clientCoDll)
    printfn "  ClientCo.dll on disk after rebuild: length=%d bytes, lastWriteUtc=%O" fi.Length fi.LastWriteTimeUtc

    printfn ""
    printfn "=== post-rebuild re-check: SAME process (no restart) ==="
    let _, errsA, diagsA = check "step5-scenario after rebuild, SAME process" "Name:v1;Age:v3;Email:v4" cc

    printfn ""
    printfn "phase1 summary:"
    printfn "  step5 (pre-rebuild, expected BLOCK):                    errors=%d" errs5.Length
    printfn "  post-rebuild same-process re-check (expected CLEAR):    errors=%d -> %s" errsA.Length (if errsA.Length = 0 then "CLEARED" else "STILL BLOCKED")
    printfn "  ClientCo build (repointed at SchemaAsmV2) exit code:    %d (0=success)" code
    0

let phase2 () =
    printfn "=== PHASE 2 (FRESH PROCESS; ClientCo.dll on disk is whatever phase1 left) ==="
    printfn "ClientCo recorded dependencies (independent reflection): %A" (dumpClientConsumes clientCoDll)
    printfn ""
    let cc = fwd clientCoDll
    let _, errsC, diagsC = check "step5-scenario, FRESH PROCESS (expected CLEAR if a restart is what's needed)" "Name:v1;Age:v3;Email:v4" cc
    printfn ""
    let _, errs7, diags7 = check "step7 (re-confirm not-stale-pass): schema Age BACK to v2, ClientCo now records v3 -> expect BLOCK" "Name:v1;Age:v2;Email:v4" cc
    printfn ""
    let _, errsR3, diagsR3 = check "Round3: REMOVE Age entirely from schema, ClientCo depends on Age -> expect REMOVED failure" "Name:v1;Email:v4" cc
    printfn ""
    printfn "phase2 summary:"
    printfn "  fresh-process re-check (expected CLEAR):  errors=%d -> %s" errsC.Length (if errsC.Length = 0 then "CLEARED" else "STILL BLOCKED")
    printfn "  step7 re-block (expected BLOCK):           errors=%d -> %s" errs7.Length (if errs7.Length > 0 then "BLOCKED (correct)" else "CLEAN (WRONG)")
    printfn "  round3 field-removal (expected FAIL):      errors=%d -> %s" errsR3.Length (if errsR3.Length > 0 then "FAILED (correct)" else "CLEAN (WRONG)")
    printfn ""
    printfn "step7 (stale) diag: %s" (if diags7.Length > 0 then diags7.[0] else "<none>")
    printfn "round3 (removed) diag: %s" (if diagsR3.Length > 0 then diagsR3.[0] else "<none>")
    (if errsC.Length = 0 && errs7.Length > 0 && errsR3.Length > 0 then 0 else 1)

[<EntryPoint>]
let main argv =
    match argv with
    | [| "phase1" |] -> phase1 ()
    | [| "phase2" |] -> phase2 ()
    | _ -> eprintfn "usage: Harness (phase1|phase2)"; 2
