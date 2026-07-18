// Q010 Round 1 - reentrancy baseline (the cheapest falsifier).
//
// The load-bearing mechanism claim of the whole quartet: can a DocumentSource.Custom
// callback that supplies file N's (Stratified.fs) source text ITSELF call back into the
// same checker to ParseAndCheckFileInProject on an earlier file (A.fs) of the SAME
// in-flight project - and return correct, complete typed results, zero diagnostics on the
// generated file, within a timeout, no hang / exception / stack overflow?
//
// See ../../02-results.md for the run this produced.

open System
open System.Diagnostics
open System.Collections.Concurrent
open System.Threading.Tasks
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text

let files = ConcurrentDictionary<string, string>()
let fileA = @"C:\virt\A.fs"
let fileStratified = @"C:\virt\Stratified.fs"

let aSrc =
    """module Domain
type Id = int
type Person = { Id: Id; Name: string }
"""
files.[fileA] <- aSrc

// Set before the outer check runs; referenced reentrantly from inside the callback.
let mutable checkerRef : FSharpChecker = Unchecked.defaultof<_>
let mutable optsRef : FSharpProjectOptions = Unchecked.defaultof<_>

// Instrumentation.
let mutable reentrantEntered = false
let mutable reentrantReturned = false
let mutable strippedForm = "<unset>"
let mutable syntacticForm = "<unset>"
let mutable generatedText = "<unset>"

let rec collectEntities decls = seq {
    for d in decls do
        match d with
        | FSharpImplementationFileDeclaration.Entity(e, sub) ->
            yield e
            yield! collectEntities sub
        | _ -> () }

// Compute Stratified.fs's text FROM the typed check results of the A.fs prefix.
let synthesizeStratified (checkedA: FSharpCheckFileResults) =
    let implA = checkedA.ImplementationFile |> Option.get
    let ctx = FSharpDisplayContext.Empty
    let person =
        collectEntities implA.Declarations
        |> Seq.find (fun e -> e.IsFSharpRecord && e.DisplayName = "Person")
    let idField = person.FSharpFields |> Seq.find (fun f -> f.Name = "Id")
    let stripped = idField.FieldType.StripAbbreviations().Format ctx   // e.g. "System.Int32"
    let syntactic = idField.FieldType.Format ctx                        // e.g. "Id"
    strippedForm <- stripped
    syntacticForm <- syntactic
    sprintf
        "module Domain.Stratified\n\n/// Text computed from the typed check of the A.fs prefix.\n/// A syntax-only reader would only ever see the token \"Id\"; this file names the\n/// alias-STRIPPED resolved type, so it could not have been produced syntactically.\nlet personIdResolvedType = \"%s\"\nlet personIdSyntacticType = \"%s\"\n"
        stripped syntactic

let makeReentrantDocSource () =
    DocumentSource.Custom(fun path ->
        async {
            if path = fileA then
                return Some(SourceText.ofString files.[fileA] :> ISourceText)
            elif path = fileStratified then
                // THE REENTRANT STEP: before returning Stratified.fs's text, check the
                // A.fs prefix on the same checker/opts that the outer check is using.
                reentrantEntered <- true
                let! _, ansA = checkerRef.ParseAndCheckFileInProject(fileA, 0, SourceText.ofString files.[fileA], optsRef)
                let checkedA =
                    match ansA with
                    | FSharpCheckFileAnswer.Succeeded r -> r
                    | FSharpCheckFileAnswer.Aborted -> failwith "reentrant check of A.fs aborted"
                let text = synthesizeStratified checkedA
                generatedText <- text
                files.[fileStratified] <- text
                reentrantReturned <- true
                return Some(SourceText.ofString text :> ISourceText)
            else
                return None
        })

let diagLines (diags: FSharpDiagnostic[]) =
    diags
    |> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)
    |> Array.map (fun d -> sprintf "    [%A] %s (%s)" d.Severity d.Message d.FileName)

// Runs one outer-check scenario end to end and returns error count on Stratified.fs (-1 = never produced).
let runScenario (useTransparent: bool) (mode: string) =
    async {
        reentrantEntered <- false
        reentrantReturned <- false
        files.TryRemove(fileStratified) |> ignore

        let docSource = makeReentrantDocSource ()
        let checker = FSharpChecker.Create(keepAssemblyContents = true, documentSource = docSource, useTransparentCompiler = useTransparent)
        checkerRef <- checker

        let! baseOpts, scriptDiags = checker.GetProjectOptionsFromScript(fileA, SourceText.ofString aSrc)
        if not scriptDiags.IsEmpty then printfn "  script diags: %A" scriptDiags
        let opts =
            { baseOpts with
                ProjectFileName = @"C:\virt\spike.fsproj"
                SourceFiles = [| fileA; fileStratified |] }
        optsRef <- opts

        let sw = Stopwatch.StartNew()
        match mode with
        | "ParseAndCheckFileInProject-placeholder" ->
            // The design's literal Round 1 form: check Stratified.fs, passing PLACEHOLDER
            // text as the source arg, expecting the callback to override with the real text.
            let placeholder = "module Domain.Stratified\n// placeholder\n"
            let! _, ans = checker.ParseAndCheckFileInProject(fileStratified, 0, SourceText.ofString placeholder, opts)
            sw.Stop()
            match ans with
            | FSharpCheckFileAnswer.Succeeded r ->
                printfn "  outer check returned in %dms; reentrant callback fired: %b" sw.ElapsedMilliseconds reentrantEntered
                let errs = r.Diagnostics |> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)
                return (reentrantEntered, errs.Length, sw.ElapsedMilliseconds)
            | FSharpCheckFileAnswer.Aborted ->
                printfn "  outer check ABORTED after %dms" sw.ElapsedMilliseconds
                return (reentrantEntered, -1, sw.ElapsedMilliseconds)
        | "ParseAndCheckProject" ->
            // Forces every file's source (including Stratified.fs) through DocumentSource,
            // guaranteeing the reentrant callback is exercised.
            let! projResults = checker.ParseAndCheckProject(opts)
            sw.Stop()
            printfn "  ParseAndCheckProject returned in %dms; reentrant callback fired: %b" sw.ElapsedMilliseconds reentrantEntered
            let stratDiags =
                projResults.Diagnostics
                |> Array.filter (fun d -> d.FileName = fileStratified && d.Severity = FSharpDiagnosticSeverity.Error)
            return (reentrantEntered, stratDiags.Length, sw.ElapsedMilliseconds)
        | _ -> return failwithf "unknown mode %s" mode
    }

// Race the scenario against a 30s timeout so a genuine deadlock is a clean, reportable failure.
let runWithTimeout (label: string) (useTransparent: bool) (mode: string) =
    printfn "=== %s (useTransparentCompiler=%b, mode=%s) ===" label useTransparent mode
    let work = runScenario useTransparent mode |> Async.StartAsTask
    let timeout = Task.Delay(30_000)
    let winner = Task.WhenAny(work, timeout).Result
    if obj.ReferenceEquals(winner, timeout) then
        printfn "  *** TIMEOUT/HANG: outer check did not return within 30s. Reentrant callback"
        printfn "      entered=%b returned=%b - a genuine deadlock, caught by the timeout. ***" reentrantEntered reentrantReturned
        None
    else
        try Some work.Result
        with ex ->
            printfn "  *** EXCEPTION from outer check: %s: %s ***" (ex.GetType().Name) ex.Message
            None

[<EntryPoint>]
let main _ =
    printfn "Q010 Round 1 - reentrant DocumentSource.Custom callback"
    printfn ""

    // Primary SHIP-relevant config: TransparentCompiler (Q001's recommended default).
    let tcLiteral = runWithTimeout "Attempt 1: TransparentCompiler, design's literal form" true "ParseAndCheckFileInProject-placeholder"
    printfn ""
    let tcProject = runWithTimeout "Attempt 2: TransparentCompiler, ParseAndCheckProject (forces callback)" true "ParseAndCheckProject"
    printfn ""
    // Architecturally different path the design flagged as a distinct reentrancy risk.
    let bgProject = runWithTimeout "Attempt 3: BackgroundCompiler, ParseAndCheckProject (forces callback)" false "ParseAndCheckProject"
    printfn ""

    printfn "=== Alias-resolution proof (the 'typed, not syntactic' check) ==="
    printfn "  Person.Id via typed tree, alias-STRIPPED : %s" strippedForm
    printfn "  Person.Id via typed tree, alias-preserving: %s" syntacticForm
    printfn ""
    printfn "=== Generated Stratified.fs text (never written to disk) ==="
    printfn "%s" generatedText
    printfn ""

    let report label = function
        | Some (entered, errs, ms) ->
            printfn "  %-55s entered=%b  strat-errors=%d  %dms" label entered errs ms
        | None -> printfn "  %-55s HANG/EXCEPTION (see above)" label
    printfn "=== Summary ==="
    report "TransparentCompiler / placeholder form" tcLiteral
    report "TransparentCompiler / ParseAndCheckProject" tcProject
    report "BackgroundCompiler / ParseAndCheckProject" bgProject

    // Round 1 mechanism SHIP requires: reentrant callback returned correct results and the
    // spliced Stratified.fs typechecks with zero diagnostics under TransparentCompiler.
    match tcProject with
    | Some (true, 0, _) ->
        printfn ""
        printfn "PASSED (TransparentCompiler): reentrant callback fired, prefix typed-resolved, zero diagnostics on generated file."
        0
    | _ ->
        printfn ""
        printfn "NOT a clean pass under TransparentCompiler/ParseAndCheckProject - see summary above."
        1
