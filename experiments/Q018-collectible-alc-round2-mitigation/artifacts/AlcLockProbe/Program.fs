module AlcLockProbe.Program

open System
open System.IO
open System.Runtime.CompilerServices
open System.Runtime.Loader

// NoInlining: prevents the JIT from extending the local variables' (alc/asm) live range into the
// caller's frame, which would keep them rooted past this method's return and defeat the whole test.
[<MethodImpl(MethodImplOptions.NoInlining)>]
let loadIntoCollectibleAlc (dllPath: string) : WeakReference<AssemblyLoadContext> =
    let alc = AssemblyLoadContext("AlcLockProbe", isCollectible = true)
    let asm = alc.LoadFromAssemblyPath(dllPath)
    printfn "  loaded: %s" asm.FullName
    WeakReference<AssemblyLoadContext>(alc)

let probe (dllPath: string) =
    let weakAlc = loadIntoCollectibleAlc dllPath

    match weakAlc.TryGetTarget() with
    | true, alc -> alc.Unload()
    | false, _ -> printfn "  ALC already collected before Unload() was even called (unexpected)"

    let mutable iterations = 0
    let mutable alive = true
    let sw = Diagnostics.Stopwatch.StartNew()
    while alive && iterations < 30 do
        GC.Collect(2, GCCollectionMode.Forced, true, true)
        GC.WaitForPendingFinalizers()
        GC.Collect(2, GCCollectionMode.Forced, true, true)
        Threading.Thread.Sleep(50)
        iterations <- iterations + 1
        match weakAlc.TryGetTarget() with
        | true, _ -> alive <- true
        | false, _ -> alive <- false
    sw.Stop()
    printfn "  collected=%b after %d GC iterations, %dms" (not alive) iterations sw.ElapsedMilliseconds

    if alive then
        printfn "  RESULT: ALC did not collect within the bound -- cannot test file-lock release"
        false
    else
        try
            use fs = File.Open(dllPath, FileMode.Open, FileAccess.Write, FileShare.None)
            printfn "  write-open SUCCEEDED -- file lock released"
            true
        with ex ->
            printfn "  write-open FAILED: %s: %s" (ex.GetType().Name) ex.Message
            false

[<EntryPoint>]
let main argv =
    match argv with
    | [| dllPath |] ->
        printfn "===== ROUND A (Q018): collectible ALC unload -- does it release the file lock? ====="
        let dllPath = Path.GetFullPath dllPath
        // Work on a copy so Satellite.dll itself doesn't need to stay writable for later rounds.
        let copyPath = Path.Combine(Path.GetDirectoryName dllPath, "SatelliteProbeCopy.dll")
        File.Copy(dllPath, copyPath, true)
        let pass = probe copyPath
        printfn "ROUND A verdict: %s" (if pass then "PASS" else "FAIL")
        if pass then 0 else 1
    | _ ->
        eprintfn "usage: AlcLockProbe <path-to-dll>"
        2
