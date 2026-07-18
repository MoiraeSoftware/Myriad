// Round 3: does invalidation cost scale with total project size, or with just the
// changed file + its dependents? Compares the default BackgroundCompiler against the
// opt-in TransparentCompiler at two project sizes. See ../../02-results.md for the run.
// Headline finding: TransparentCompiler auto-detects a broken dependency with NO
// explicit InvalidateConfiguration call, and stayed fast (single-digit ms) at both
// N=2 and N=60 padding files, where BackgroundCompiler needed the explicit call.
// Caveat: padding files here are trivially cheap to typecheck, so this round did not
// decisively stress-test BackgroundCompiler's O(n) prediction - see 02-results.md.

open System.Diagnostics
open System.Collections.Concurrent
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text

let files = ConcurrentDictionary<string, string>()
let fileA = @"C:\virt\A.fs"
let fileB = @"C:\virt\Generated.fs"

let personSrcV1 = "module A\ntype Id = int\ntype Person = { Name: string; Age: Id; Nick: string option }\n"
let personSrcV2 = "module A\ntype Id = int\ntype Person = { Name: string; Nick: string option }\n" // breaking: Age removed
let generatedSrc = "module Generated\nopen A\n\nlet describe (p: Person) = sprintf \"%s/%d\" p.Name p.Age\n"

let paddingFile i = sprintf @"C:\virt\Pad%d.fs" i
let paddingSrc i = sprintf "module Pad%d\nlet x%d = %d\nlet y%d s = sprintf \"%%s-%d\" s\n" i i i i i

let docSource =
    DocumentSource.Custom(fun path ->
        async {
            match files.TryGetValue path with
            | true, txt -> return Some(SourceText.ofString txt :> ISourceText)
            | _ -> return None
        })

let time (label: string) (f: unit -> Async<FSharpCheckFileAnswer>) =
    async {
        let sw = Stopwatch.StartNew()
        let! ans = f ()
        sw.Stop()
        let outcome =
            match ans with
            | FSharpCheckFileAnswer.Succeeded r ->
                if r.Diagnostics.Length = 0 then "OK" else sprintf "ERR(%d): %s" r.Diagnostics.Length r.Diagnostics.[0].Message
            | FSharpCheckFileAnswer.Aborted -> "ABORTED"
        printfn "    %-60s %6dms  %s" label sw.ElapsedMilliseconds outcome
        return ans
    }

/// Runs the full break/invalidate/recheck cycle for one checker configuration at N padding files.
let runScenario (label: string) (useTransparent: bool) (padCount: int) (explicitInvalidate: bool) =
    async {
        printfn "--- %s | padCount=%d | explicitInvalidate=%b ---" label padCount explicitInvalidate
        files.Clear()
        files.[fileA] <- personSrcV1
        files.[fileB] <- generatedSrc
        for i in 1 .. padCount do
            files.[paddingFile i] <- paddingSrc i

        let checker = FSharpChecker.Create(keepAssemblyContents = true, documentSource = docSource, useTransparentCompiler = useTransparent)

        let! baseOpts, _ = checker.GetProjectOptionsFromScript(fileA, SourceText.ofString personSrcV1)
        let padFiles = [| for i in 1 .. padCount -> paddingFile i |]
        let opts = { baseOpts with ProjectFileName = @"C:\virt\spike.fsproj"; SourceFiles = Array.append padFiles [| fileA; fileB |] }

        let sw0 = Stopwatch.StartNew()
        for i in 1 .. padCount do
            checker.ParseAndCheckFileInProject(paddingFile i, 0, SourceText.ofString files.[paddingFile i], opts)
            |> Async.Ignore
            |> Async.RunSynchronously
        let! _, _ = checker.ParseAndCheckFileInProject(fileA, 0, SourceText.ofString files.[fileA], opts)
        let! _, ansB0 = checker.ParseAndCheckFileInProject(fileB, 0, SourceText.ofString files.[fileB], opts)
        sw0.Stop()
        printfn "    %-60s %6dms" "full initial warm (all pad files + A + B, v0)" sw0.ElapsedMilliseconds
        match ansB0 with
        | FSharpCheckFileAnswer.Succeeded r -> printfn "    initial B check: %s" (if r.Diagnostics.Length = 0 then "OK" else "ERR")
        | FSharpCheckFileAnswer.Aborted -> ()

        files.[fileA] <- personSrcV2
        let! _, _ = checker.ParseAndCheckFileInProject(fileA, 1, SourceText.ofString files.[fileA], opts)

        if explicitInvalidate then
            checker.InvalidateConfiguration(opts)

        let! ansB1 =
            time "recheck B after A's breaking change (this is the number that matters)"
                 (fun () -> async { let! _, ans = checker.ParseAndCheckFileInProject(fileB, 1, SourceText.ofString files.[fileB], opts) in return ans })

        let caughtIt =
            match ansB1 with
            | FSharpCheckFileAnswer.Succeeded r -> r.Diagnostics.Length > 0
            | FSharpCheckFileAnswer.Aborted -> false
        printfn "    correctness: %s" (if caughtIt then "caught the break" else "STALE - missed it")
        printfn ""
    }

let run =
    async {
        do! runScenario "BackgroundCompiler" false 2 true
        do! runScenario "BackgroundCompiler" false 60 true
        do! runScenario "TransparentCompiler, NO explicit invalidate" true 2 false
        do! runScenario "TransparentCompiler" true 2 true
        do! runScenario "TransparentCompiler" true 60 true
        return ()
    }

[<EntryPoint>]
let main _ =
    Async.RunSynchronously run
    0
