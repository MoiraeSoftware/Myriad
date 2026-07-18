// Compiles SampleLib's Person.fs + Generated.fs (Myriad's real, unmodified output) together into
// Satellite.dll, mirroring how a real consuming project's <Compile> list looks: source file, then its
// Myriad-generated sibling. Reused unchanged for both the v1 build (Round 1) and the v2 rebuild after
// a real edit + Myriad rerun (Round 2) -- same code path both times, not a hand-edited one-off.
module SatelliteBuilder.Program

open System
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics

[<EntryPoint>]
let main argv =
    match argv with
    | [| sampleLibDir; myriadPluginsDll; myriadCoreDll; outDll |] ->
        let personFs = IO.Path.Combine(sampleLibDir, "Person.fs")
        let generatedFs = IO.Path.Combine(sampleLibDir, "Generated.fs")
        let checker = FSharpChecker.Create()
        let args =
            [| "fsc.exe"; "-o"; outDll; "--target:library"
               "-r:" + myriadPluginsDll; "-r:" + myriadCoreDll
               "--nowarn:57"
               personFs; generatedFs |]
        printfn "Compiling: %s" (String.Join(" ", args))
        let diags, exitCode = checker.Compile(args) |> Async.RunSynchronously
        for d in diags do printfn "  %s: %s" (string d.Severity) d.Message
        printfn "exitCode=%d outExists=%b" exitCode (IO.File.Exists outDll)
        let errs = diags |> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)
        if exitCode = 0 && errs.Length = 0 && IO.File.Exists outDll then 0 else 1
    | _ ->
        eprintfn "usage: SatelliteBuilder <sampleLibDir> <MyriadPluginsDll> <MyriadCoreDll> <outDll>"
        2
