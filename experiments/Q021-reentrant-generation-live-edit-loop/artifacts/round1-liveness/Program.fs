// Q021 - reentrant generation under a persistent-checker, multi-edit ("LSP-shaped") loop.
//
// Extends Q010 Round 1's reentrant DocumentSource.Custom mechanism (one checker, one check) to
// the load pattern an actual language server produces: one long-lived checker instance serving
// many sequential checks, with the watched prefix file's content changing between them.
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

let mkASrc (idType: string) =
    sprintf "module Domain\ntype Id = %s\ntype Person = { Id: Id; Name: string }\n" idType

// Mutable state the reentrant callback closes over - set before each outer check.
let mutable checkerRef : FSharpChecker = Unchecked.defaultof<_>
let mutable optsRef : FSharpProjectOptions = Unchecked.defaultof<_>
let mutable aVersion = 0
let mutable stratVersion = 0

let mutable reentrantEntered = false
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

let synthesizeStratified (checkedA: FSharpCheckFileResults) =
    let implA = checkedA.ImplementationFile |> Option.get
    let ctx = FSharpDisplayContext.Empty
    let person =
        collectEntities implA.Declarations
        |> Seq.find (fun e -> e.IsFSharpRecord && e.DisplayName = "Person")
    let idField = person.FSharpFields |> Seq.find (fun f -> f.Name = "Id")
    let stripped = idField.FieldType.StripAbbreviations().Format ctx
    let syntactic = idField.FieldType.Format ctx
    strippedForm <- stripped
    syntacticForm <- syntactic
    sprintf
        "module Domain.Stratified\n\n/// Text computed from the typed check of the A.fs prefix, cycle-fresh.\nlet personIdResolvedType = \"%s\"\nlet personIdSyntacticType = \"%s\"\n"
        stripped syntactic

// THE REENTRANT CALLBACK. Version-bumping is done by the caller before invoking the outer check;
// this callback just reads whatever aVersion/files.[fileA] currently hold.
let makeReentrantDocSource () =
    DocumentSource.Custom(fun path ->
        async {
            if path = fileA then
                return Some(SourceText.ofString files.[fileA] :> ISourceText)
            elif path = fileStratified then
                reentrantEntered <- true
                let! _, ansA = checkerRef.ParseAndCheckFileInProject(fileA, aVersion, SourceText.ofString files.[fileA], optsRef)
                let checkedA =
                    match ansA with
                    | FSharpCheckFileAnswer.Succeeded r -> r
                    | FSharpCheckFileAnswer.Aborted -> failwith "reentrant check of A.fs aborted"
                let text = synthesizeStratified checkedA
                generatedText <- text
                files.[fileStratified] <- text
                return Some(SourceText.ofString text :> ISourceText)
            else
                return None
        })

// One edit-then-recheck cycle on the SAME checker/opts. bumpVersions controls whether this
// cycle plays by the LSP-shaped rules (bump both file versions) or deliberately doesn't
// (Round 3's control).
let runCycle (label: string) (idType: string) (bumpVersions: bool) =
    async {
        files.[fileA] <- mkASrc idType
        if bumpVersions then
            aVersion <- aVersion + 1
            stratVersion <- stratVersion + 1
        reentrantEntered <- false
        files.TryRemove(fileStratified) |> ignore

        let sw = Stopwatch.StartNew()
        let! projResults = checkerRef.ParseAndCheckProject(optsRef)
        sw.Stop()
        let stratErrs =
            projResults.Diagnostics
            |> Array.filter (fun d -> d.FileName = fileStratified && d.Severity = FSharpDiagnosticSeverity.Error)
        printfn "  [%s] idType=%-8s aVer=%d stratVer=%d bumped=%b entered=%b errs=%d %dms"
            label idType aVersion stratVersion bumpVersions reentrantEntered stratErrs.Length sw.ElapsedMilliseconds
        printfn "        stripped=%-15s syntactic=%-15s" strippedForm syntacticForm
        return (strippedForm, stratErrs.Length, reentrantEntered)
    }

// DEVIATION FROM DESIGN, discovered while running Round 3: it passed even with versions not
// bumped, which could mean either (a) TransparentCompiler genuinely doesn't need version bumps
// under ParseAndCheckProject, or (b) ParseAndCheckProject always re-checks every file regardless
// of caching and the version-bump question was never really exercised. This variant uses
// ParseAndCheckFileInProject directly on fileStratified instead (Q010's "literal form", closer to
// what an LSP host actually calls per-completion-request) to check whether THAT path is sensitive
// to the version number, which the design did not originally call for but the result above makes
// necessary to distinguish (a) from (b).
let runCycleViaFileCheck (label: string) (idType: string) (bumpVersions: bool) =
    async {
        files.[fileA] <- mkASrc idType
        if bumpVersions then
            aVersion <- aVersion + 1
            stratVersion <- stratVersion + 1
        reentrantEntered <- false
        files.TryRemove(fileStratified) |> ignore

        let sw = Stopwatch.StartNew()
        let placeholder = "module Domain.Stratified\n// placeholder\n"
        let! _, ans = checkerRef.ParseAndCheckFileInProject(fileStratified, stratVersion, SourceText.ofString placeholder, optsRef)
        sw.Stop()
        match ans with
        | FSharpCheckFileAnswer.Succeeded r ->
            let errs = r.Diagnostics |> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)
            printfn "  [%s] idType=%-8s aVer=%d stratVer=%d bumped=%b entered=%b errs=%d %dms"
                label idType aVersion stratVersion bumpVersions reentrantEntered errs.Length sw.ElapsedMilliseconds
            printfn "        stripped=%-15s syntactic=%-15s" strippedForm syntacticForm
            return (strippedForm, errs.Length, reentrantEntered)
        | FSharpCheckFileAnswer.Aborted ->
            printfn "  [%s] ABORTED after %dms" label sw.ElapsedMilliseconds
            return (strippedForm, -1, reentrantEntered)
    }

let runWithTimeout (label: string) (work: Async<'a>) : 'a option =
    let task = work |> Async.StartAsTask
    let timeout = Task.Delay(30_000)
    let winner = Task.WhenAny(task :> Task, timeout).Result
    if obj.ReferenceEquals(winner, timeout) then
        printfn "  *** TIMEOUT/HANG on %s ***" label
        None
    else
        try Some task.Result
        with ex ->
            printfn "  *** EXCEPTION on %s: %s: %s ***" label (ex.GetType().Name) ex.Message
            None

[<EntryPoint>]
let main _ =
    printfn "Q021 - reentrant generation under a persistent checker, multi-edit loop"
    printfn ""

    // One checker, one options object, built ONCE and reused for every cycle below -
    // the whole point of this quartet versus Q010's one-shot harness.
    let docSource = makeReentrantDocSource ()
    let checker = FSharpChecker.Create(keepAssemblyContents = true, documentSource = docSource, useTransparentCompiler = true)
    checkerRef <- checker

    files.[fileA] <- mkASrc "int"
    let baseOptsResult =
        checker.GetProjectOptionsFromScript(fileA, SourceText.ofString files.[fileA])
        |> Async.RunSynchronously
    let baseOpts, scriptDiags = baseOptsResult
    if not scriptDiags.IsEmpty then printfn "  script diags: %A" scriptDiags
    let opts =
        { baseOpts with
            ProjectFileName = @"C:\virt\spike.fsproj"
            SourceFiles = [| fileA; fileStratified |] }
    optsRef <- opts

    printfn "=== Round 1: cheapest falsifier - two cycles, persistent checker, versions bumped ==="
    let r1c1 = runWithTimeout "R1C1" (runCycle "R1C1" "int" true)
    let r1c2 = runWithTimeout "R1C2" (runCycle "R1C2" "string" true)
    let round1Pass =
        match r1c1, r1c2 with
        | Some ("System.Int32", 0, true), Some ("System.String", 0, true) -> true
        | _ -> false
    printfn "  Round 1 pass (cycle 2 reflects its OWN edit, not cycle 1's stale value): %b" round1Pass
    printfn ""

    printfn "=== Round 2: full liveness - four cycles, int -> string -> int64 -> int (repeat) ==="
    let r2c1 = runWithTimeout "R2C1" (runCycle "R2C1" "int" true)
    let r2c2 = runWithTimeout "R2C2" (runCycle "R2C2" "string" true)
    let r2c3 = runWithTimeout "R2C3" (runCycle "R2C3" "int64" true)
    let r2c4 = runWithTimeout "R2C4" (runCycle "R2C4" "int" true)
    let expectStripped = [ "System.Int32"; "System.String"; "System.Int64"; "System.Int32" ]
    let actualStripped =
        [ r2c1; r2c2; r2c3; r2c4 ]
        |> List.map (function Some (s, 0, true) -> s | Some (s, _, _) -> sprintf "FAIL(%s)" s | None -> "HANG/EXN")
    let round2Pass = actualStripped = expectStripped
    printfn "  expected: %A" expectStripped
    printfn "  actual:   %A" actualStripped
    printfn "  Round 2 pass: %b" round2Pass
    printfn ""

    printfn "=== Round 3: omitted-invalidation control - edit WITHOUT bumping either version ==="
    let r3 = runWithTimeout "R3" (runCycle "R3" "decimal" false)
    let round3Result =
        match r3 with
        | Some ("System.Decimal", 0, _) -> "FRESH (decimal observed - version-bump discipline was NOT load-bearing here)"
        | Some ("System.Int32", _, _) -> "STALE (still Int32 from R2C4 - version-bump discipline IS load-bearing)"
        | Some (s, _, _) -> sprintf "OTHER (%s)" s
        | None -> "HANG/EXN"
    printfn "  Round 3 result: %s" round3Result
    printfn ""

    printfn "=== Round 4 (unplanned, added after Round 3's surprise): ParseAndCheckFileInProject directly on Stratified.fs, same-version repeat then bumped ==="
    printfn "  (distinguishes 'TransparentCompiler genuinely doesn't need version bumps' from 'ParseAndCheckProject always fully re-checks regardless of caching')"
    let r4c1 = runWithTimeout "R4C1" (runCycleViaFileCheck "R4C1" "single" true)
    let r4c2same = runWithTimeout "R4C2-same-version" (runCycleViaFileCheck "R4C2-same-version" "byte" false)
    printfn "  R4C1 (bumped, single) -> %A" (r4c1 |> Option.map (fun (s,e,_) -> s,e))
    printfn "  R4C2 (version NOT bumped, byte) -> %A" (r4c2same |> Option.map (fun (s,e,_) -> s,e))
    let round4Finding =
        match r4c1, r4c2same with
        | Some ("System.Single", 0, _), Some ("System.Byte", 0, _) ->
            "FRESH even via ParseAndCheckFileInProject without a version bump - confirms (a): this checker configuration doesn't key caching off the fileVersion int for this callback-driven path."
        | Some ("System.Single", 0, _), Some ("System.Single", _, _) ->
            "STALE via ParseAndCheckFileInProject without a version bump - confirms (b): ParseAndCheckProject's Round 3 pass was an artifact of that API always fully re-checking, not evidence version-bumping is unnecessary in general."
        | _ -> sprintf "INCONCLUSIVE: r4c1=%A r4c2=%A" r4c1 r4c2same
    printfn "  Round 4 finding: %s" round4Finding
    printfn ""

    printfn "=== Round 5 (unplanned, added because Round 4 reproduced Q010's known footgun instead of testing anything new): two back-to-back ParseAndCheckProject calls with ZERO edit between them ==="
    printfn "  (does the callback fire and fully recompute even when nothing changed, or is there real content-keyed caching underneath?)"
    reentrantEntered <- false
    let sw5a = Stopwatch.StartNew()
    let proj5a = checkerRef.ParseAndCheckProject(optsRef) |> Async.RunSynchronously
    sw5a.Stop()
    let entered5a = reentrantEntered
    reentrantEntered <- false
    let sw5b = Stopwatch.StartNew()
    let proj5b = checkerRef.ParseAndCheckProject(optsRef) |> Async.RunSynchronously
    sw5b.Stop()
    let entered5b = reentrantEntered
    printfn "  call A (no edit): entered=%b %dms" entered5a sw5a.ElapsedMilliseconds
    printfn "  call B (no edit, repeat): entered=%b %dms" entered5b sw5b.ElapsedMilliseconds
    let round5Finding =
        if entered5b then sprintf "Callback re-fires every ParseAndCheckProject call regardless of content change (B took %dms vs A's %dms)." sw5b.ElapsedMilliseconds sw5a.ElapsedMilliseconds
        else "Callback did NOT re-fire on the repeat call - ParseAndCheckProject has real caching that skips DocumentSource when nothing changed."
    printfn "  Round 5 finding: %s" round5Finding
    printfn ""

    printfn "=== Summary ==="
    printfn "  Round 1 (2-cycle falsifier):        %b" round1Pass
    printfn "  Round 2 (4-cycle full liveness):    %b" round2Pass
    printfn "  Round 3 (omitted-bump control):     %s" round3Result
    printfn "  Round 4 (ParseAndCheckFileInProject cross-check): %s" round4Finding
    printfn "  Round 5 (no-edit repeat call):       %s" round5Finding

    if round1Pass && round2Pass then 0 else 1
