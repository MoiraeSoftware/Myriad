module Harness.Program

open System
open System.IO
open System.Diagnostics
open System.Reflection
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics

let SPIKE = @"C:\Users\Dave\.claude\jobs\f0b85ddf\tmp\q016-satellite-dll-spike"
let MYRIAD = @"C:\Users\Dave\Documents\GitHub\Myriad"
let fwd (p: string) = p.Replace('\\', '/')

let sampleLibDir = Path.Combine(SPIKE, "SampleLib")
let personFs = Path.Combine(sampleLibDir, "Person.fs")
let satelliteDll = fwd (Path.Combine(sampleLibDir, "Satellite.dll"))
let satelliteRuntimeDll = Path.Combine(SPIKE, "SatelliteTP.Runtime", "bin", "Release", "net8.0", "SatelliteTP.Runtime.dll")
let myriadDll = Path.Combine(MYRIAD, "src", "Myriad", "bin", "Release", "net9.0", "Myriad.dll")
let myriadPluginsDll = Path.Combine(MYRIAD, "src", "Myriad.Plugins", "bin", "Release", "net9.0", "Myriad.Plugins.dll")
let myriadCoreDll = Path.Combine(MYRIAD, "src", "Myriad.Core", "bin", "Release", "net9.0", "Myriad.Core.dll")
let satelliteBuilderDll = Path.Combine(SPIKE, "SatelliteBuilder", "bin", "Release", "net8.0", "SatelliteBuilder.dll")

let checker = FSharpChecker.Create()

let runProcess (exe: string) (args: string) (workDir: string) =
    let psi = ProcessStartInfo(exe, args)
    psi.WorkingDirectory <- workDir
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.UseShellExecute <- false
    let sw = Stopwatch.StartNew()
    use p = Process.Start(psi)
    let out = p.StandardOutput.ReadToEnd()
    let err = p.StandardError.ReadToEnd()
    p.WaitForExit()
    sw.Stop()
    (p.ExitCode, out, err, sw.ElapsedMilliseconds)

let runMyriadCli () =
    runProcess "dotnet"
        (sprintf "\"%s\" --inputfile Person.fs --outputfile Generated.fs --plugin \"%s\" --configfile myriad.toml" myriadDll myriadPluginsDll)
        sampleLibDir

let runSatelliteBuilder () =
    runProcess "dotnet"
        (sprintf "\"%s\" \"%s\" \"%s\" \"%s\" \"%s\"" satelliteBuilderDll sampleLibDir myriadPluginsDll myriadCoreDll (Path.Combine(sampleLibDir, "Satellite.dll")))
        SPIKE

let consumerText (moduleName: string) (withEmail: bool) =
    let createArgs = if withEmail then "\"Ada\", 42, \"ada@example.com\"" else "\"Ada\", 42"
    let emailLine = if withEmail then "let readEmail : string = P.GetEmail(person)\n" else ""
    sprintf "module %s\ntype P = SatelliteTP.Provided.MyriadSatellite<\"%s\", \"SampleFields.Person\", \"SampleNs.Person\">\nlet person = P.Create(%s)\nlet readName : string = P.GetName(person)\nlet readAge : int = P.GetAge(person)\n%s"
        moduleName satelliteDll createArgs emailLine

let compileConsumer (label: string) (moduleName: string) (text: string) =
    let dir = Path.Combine(SPIKE, "Harness", "scratch")
    Directory.CreateDirectory dir |> ignore
    let fsPath = Path.Combine(dir, moduleName + ".fs")
    File.WriteAllText(fsPath, text)
    let outDll = Path.Combine(dir, moduleName + "_out.dll")
    if File.Exists outDll then (try File.Delete outDll with _ -> ())
    // The consumer must reference Satellite.dll directly, not only SatelliteTP.Runtime.dll: FCS's
    // type-provider host validates that any "design-time type" a provided member's signature exposes
    // (here, the real SampleNs.Person from the satellite, used as GetName/GetAge's parameter type and
    // Create's return type) is present in the consumer's own target reference assembly set -- found by
    // running (see 02-results.md), not anticipated by the design. This mirrors the real cross-project
    // shape anyway: project B would reference project A's compiled output directly too.
    let args = [| "fsc.exe"; "-o"; outDll; "--target:library"; "-r:" + satelliteRuntimeDll; "-r:" + satelliteDll; "--nowarn:57"; fsPath |]
    let sw = Stopwatch.StartNew()
    let diags, exitCode = checker.Compile(args) |> Async.RunSynchronously
    sw.Stop()
    let errs = diags |> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)
    printfn "--- %s ---" label
    printfn "  compile: %dms exitCode=%d errors=%d outExists=%b" sw.ElapsedMilliseconds exitCode errs.Length (File.Exists outDll)
    for d in diags do printfn "    %s: %s" (string d.Severity) (d.Message.Replace("\r"," ").Replace("\n"," "))
    (sw.ElapsedMilliseconds, errs, outDll)

let mutable resolverHooked = false
let registerProbeDir (dir: string) =
    if not resolverHooked then
        resolverHooked <- true
        AppDomain.CurrentDomain.add_AssemblyResolve(ResolveEventHandler(fun _ args ->
            let name = AssemblyName(args.Name).Name
            let candidate = Path.Combine(dir, name + ".dll")
            if File.Exists candidate then (try Assembly.LoadFrom candidate with _ -> null) else null))

let readBack (outDll: string) (moduleName: string) =
    registerProbeDir (Path.GetDirectoryName satelliteRuntimeDll)
    let asm = Assembly.LoadFrom outDll
    let t = asm.GetType(moduleName)
    let getVal (n: string) =
        match t.GetProperty(n, BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static) with
        | null -> box "<no prop>"
        | p -> p.GetValue(null)
    (getVal "readName", getVal "readAge")

// Independent check: call Satellite.dll's real create/name/age DIRECTLY via reflection, outside the
// provider entirely.
let directSatelliteCall (satPath: string) (withEmail: bool) =
    let bytes = File.ReadAllBytes satPath
    let asm = Assembly.Load bytes
    let modType = asm.GetType("SampleFields.Person")
    let flags = BindingFlags.Public ||| BindingFlags.Static
    if withEmail then
        let createMi = modType.GetMethod("create", flags)
        let person = createMi.Invoke(null, [| box "Ada"; box 42; box "ada@example.com" |])
        let nameMi = modType.GetMethod("name", flags)
        let emailMi = modType.GetMethod("email", flags)
        (nameMi.Invoke(null, [| person |]) :?> string), (emailMi.Invoke(null, [| person |]) :?> string)
    else
        let createMi = modType.GetMethod("create", flags)
        let person = createMi.Invoke(null, [| box "Ada"; box 42 |])
        let nameMi = modType.GetMethod("name", flags)
        let ageMi = modType.GetMethod("age", flags)
        (nameMi.Invoke(null, [| person |]) :?> string), string (ageMi.Invoke(null, [| person |]) :?> int)

// Q017: independent check for the Map round -- call Satellite.dll's real `map` DIRECTLY via reflection,
// outside the provider entirely, with the SAME lambda values the provider-mediated consumer uses.
let directSatelliteMapCall (satPath: string) (name: string) (age: int) =
    let bytes = File.ReadAllBytes satPath
    let asm = Assembly.Load bytes
    let modType = asm.GetType("SampleFields.Person")
    let flags = BindingFlags.Public ||| BindingFlags.Static
    let createMi = modType.GetMethod("create", flags)
    let person = createMi.Invoke(null, [| box name; box age |])
    let mapMi = modType.GetMethod("map", flags)
    let upper = FSharpFunc<string, string>.FromConverter(fun (s: string) -> s.ToUpperInvariant())
    let incr = FSharpFunc<int, int>.FromConverter(fun (i: int) -> i + 1)
    let mapped = mapMi.Invoke(null, [| box upper; box incr; person |])
    let nameMi = modType.GetMethod("name", flags)
    let ageMi = modType.GetMethod("age", flags)
    (nameMi.Invoke(null, [| mapped |]) :?> string), (ageMi.Invoke(null, [| mapped |]) :?> int)

let consumerTextMap (moduleName: string) =
    sprintf "module %s\ntype P = SatelliteTP.Provided.MyriadSatellite<\"%s\", \"SampleFields.Person\", \"SampleNs.Person\">\nlet person = P.Create(\"Ada\", 42)\nlet mapped = P.Map((fun (s: string) -> s.ToUpperInvariant()), (fun (i: int) -> i + 1), person)\nlet readMappedName : string = P.GetName(mapped)\nlet readMappedAge : int = P.GetAge(mapped)\n"
        moduleName satelliteDll

let round3Generalization () =
    printfn "\n===== ROUND 3 (Q017): function-typed-parameter generalization -- Map(mapname, mapage, person) ====="
    let text = consumerTextMap "Consumer3Map"
    printfn "consumer text:\n%s" text
    let ms, errs, outDll = compileConsumer "Round 3 (Q017) compile" "Consumer3Map" text
    if errs.Length > 0 then
        printfn "ROUND 3 (Q017): FAIL (compile errors -- see diagnostics above)"
        false
    else
        let asm = Assembly.LoadFrom outDll
        let t = asm.GetType("Consumer3Map")
        let getVal (n: string) =
            t.GetProperty(n, BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static).GetValue(null)
        let rbName = getVal "readMappedName" :?> string
        let rbAge = getVal "readMappedAge" :?> int
        printfn "  readback via provider-mediated Map: name=%A age=%A" rbName rbAge
        let (directName, directAge) = directSatelliteMapCall satelliteDll "Ada" 42
        printfn "  direct reflection call to real map (outside provider): name=%A age=%A" directName directAge
        let nameOk = rbName = directName
        let ageOk = rbAge = directAge
        printfn "  agreement: name %b, age %b" nameOk ageOk
        printfn "  compile time: %dms" ms
        let pass = nameOk && ageOk
        printfn "ROUND 3 (Q017) verdict: %s" (if pass then "PASS" else "FAIL")
        pass

let round1 () =
    printfn "===== ROUND 1: cheapest falsifier - reflection-forwarding to Myriad's real compiled output ====="
    let text = consumerText "Consumer1" false
    printfn "consumer text:\n%s" text
    let ms, errs, outDll = compileConsumer "Round 1 compile" "Consumer1" text
    if errs.Length > 0 then
        printfn "ROUND 1: FAIL (compile errors)"
        false
    else
        let (rbName, rbAge) = readBack outDll "Consumer1"
        printfn "  readback via compiled consumer: name=%A age=%A" rbName rbAge
        let (directName, directAge) = directSatelliteCall satelliteDll false
        printfn "  direct reflection on Satellite.dll (outside provider): name=%A age=%s" directName directAge
        let nameOk = (string rbName) = directName
        let ageOk = (string rbAge) = directAge
        printfn "  agreement: name %b, age %b" nameOk ageOk
        printfn "  compile time: %dms" ms
        let pass = errs.Length = 0 && nameOk && ageOk
        printfn "ROUND 1 verdict: %s" (if pass then "PASS" else "FAIL")
        pass

let round2 () =
    printfn "\n===== ROUND 2: regeneration + Invalidate() + Windows file-lock check ====="
    printfn "-- step 1: cold check, v1 satellite (2 fields) already loaded from Round 1 in this process --"
    // Force the provider to actually load v1 in THIS run too (fresh checker state per consumer name).
    let (_, errsV1, _) = compileConsumer "Round 2 step1 (v1 sanity)" "Consumer2Pre" (consumerText "Consumer2Pre" false)
    printfn "  v1 sanity compile errors=%d" errsV1.Length

    printfn "-- step 2: REAL EDIT to Person.fs (add email field), rerun Myriad's real CLI unmodified --"
    let originalPersonFs = File.ReadAllText personFs
    let editedPersonFs = "namespace SampleNs\n\nopen Myriad.Plugins\n\n[<Generator.Fields \"fields\">]\ntype Person = { name: string; age: int; email: string }\n"
    File.WriteAllText(personFs, editedPersonFs)
    let (myriadExit, myriadOut, myriadErr, myriadMs) = runMyriadCli ()
    printfn "  Myriad CLI rerun: exit=%d in %dms" myriadExit myriadMs
    if myriadExit <> 0 then printfn "  MYRIAD STDOUT:\n%s\n  MYRIAD STDERR:\n%s" myriadOut myriadErr
    let generatedFs = File.ReadAllText(Path.Combine(sampleLibDir, "Generated.fs"))
    let hasEmailGetter = generatedFs.Contains("let email")
    printfn "  Generated.fs now contains an email getter: %b" hasEmailGetter

    printfn "-- step 3: recompile satellite to the SAME path (overwrite), while this process still holds v1 loaded --"
    let mutable lockException = false
    let (satExit, satOut, satErr, satMs) = runSatelliteBuilder ()
    printfn "  satellite rebuild subprocess: exit=%d in %dms" satExit satMs
    if satExit <> 0 then
        printfn "  SATELLITEBUILDER STDOUT:\n%s\n  SATELLITEBUILDER STDERR:\n%s" satOut satErr
        let lockPhrases = [ "being used by another process"; "Could not open file for writing"; "problem occurred writing the binary" ]
        if lockPhrases |> List.exists (fun p -> satOut.Contains(p) || satErr.Contains(p)) then
            lockException <- true
    printfn "  file-lock exception observed: %b" lockException

    printfn "-- step 4: wait briefly for FileSystemWatcher's async Invalidate() to fire --"
    System.Threading.Thread.Sleep(1500)

    printfn "-- step 5: SAME checker instance, check a NEW consumer referencing GetEmail (expect 0 diagnostics) --"
    let (ms5, errs5, outDll5) = compileConsumer "Round 2 step5 (post-regen, GetEmail)" "Consumer2Post" (consumerText "Consumer2Post" true)
    let step5Ok = errs5.Length = 0
    if step5Ok then
        let asm = Assembly.LoadFrom outDll5
        let t = asm.GetType("Consumer2Post")
        let readName = t.GetProperty("readName", BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static).GetValue(null)
        let readEmailProp = t.GetProperty("readEmail", BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static)
        let readEmail = if readEmailProp = null then box "<prop not found>" else readEmailProp.GetValue(null)
        printfn "  readback: name=%A email=%A" readName readEmail
        let (directName, directEmail) = directSatelliteCall satelliteDll true
        printfn "  direct reflection (outside provider): name=%A email=%A" directName directEmail
        printfn "  agreement: name %b, email %b" ((string readName) = directName) ((string readEmail) = directEmail)

    // restore original Person.fs so reruns of this harness start from a known state
    File.WriteAllText(personFs, originalPersonFs)
    runMyriadCli () |> ignore

    let pass = hasEmailGetter && (not lockException) && step5Ok
    printfn "ROUND 2 verdict: %s (email-getter-generated=%b no-lock-exception=%b post-regen-resolves=%b)"
        (if pass then "PASS" else "FAIL") hasEmailGetter (not lockException) step5Ok
    pass

[<EntryPoint>]
let main argv =
    match argv with
    | [| "round1" |] -> if round1 () then 0 else 1
    | [| "round2" |] -> if round2 () then 0 else 1
    | [| "round3" |] -> if round3Generalization () then 0 else 1
    | [| "all" |] ->
        let r1 = round1 ()
        let r2 = round2 ()
        let r3 = round3Generalization ()
        printfn "\nSummary: Round1=%b Round2=%b Round3(Q017)=%b" r1 r2 r3
        if r1 && r2 && r3 then 0 else 1
    | _ ->
        eprintfn "usage: Harness (round1|round2|round3|all)"
        2
