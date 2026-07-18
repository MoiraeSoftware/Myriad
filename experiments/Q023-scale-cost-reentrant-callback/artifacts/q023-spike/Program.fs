// Q023 - scale-cost test for the reentrant DocumentSource.Custom callback.
//
// Resolves Q021's own review Objection 1 / Follow-up 1: does ParseAndCheckProject cost on an
// UNCHANGED remainder stay flat as project size N grows (explanation (c), real caching) or scale
// with N regardless of what changed (explanation (b), unconditional full recompute)?
//
// See ../../02-results.md for the run this produced.

open System
open System.Diagnostics
open System.Collections.Concurrent
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text

let files = ConcurrentDictionary<string, string>()
let observerPath = @"C:\virt\Observer.fs"
let prefixPath i = sprintf @"C:\virt\Prefix%04d.fs" i

// Genuine typecheck weight per file: generic record, Map/List pipeline, explicit recursion -
// deliberately NOT a `let x = 5` padding file (BACKLOG.md item 4's named weakness in Q001 Round 3).
// extraTags controls a real, compiler-visible edit that does NOT change the exported type shape.
let mkPrefixSrc (i: int) (extraTags: int) =
    let tagExtras = [ for t in 1 .. extraTags -> sprintf "\"extra%d\"" t ] |> String.concat "; "
    let tagList = if extraTags = 0 then "\"a\"; \"b\"; string k" else sprintf "\"a\"; \"b\"; string k; %s" tagExtras
    sprintf
        "module Prefix%04d\n\
         type Record = { Id: int; Name: string; Tags: string list; Meta: Map<string,int> }\n\
         type Wrapper<'a> = { Value: 'a; Items: 'a list; Count: int }\n\
         let make (id: int) (name: string) (tags: string list) : Record =\n\
         \x20   { Id = id; Name = name; Tags = tags; Meta = tags |> List.mapi (fun i t -> t, i) |> Map.ofList }\n\
         let rec fold (f: 'acc -> 'a -> 'acc) (acc: 'acc) (xs: 'a list) : 'acc =\n\
         \x20   match xs with\n\
         \x20   | [] -> acc\n\
         \x20   | x :: rest -> fold f (f acc x) rest\n\
         let wrap (v: 'a) : Wrapper<'a> = { Value = v; Items = [v]; Count = 1 }\n\
         let combine (a: Wrapper<'a>) (b: Wrapper<'a>) : Wrapper<'a> =\n\
         \x20   { Value = a.Value; Items = a.Items @ b.Items; Count = a.Count + b.Count }\n\
         let records = [ for k in 1..20 -> make k (sprintf \"item%%d-%d\" k) [ %s ] ]\n\
         let total = records |> fold (fun acc r -> acc + r.Meta.Count) 0\n"
        i i tagList

let mutable checkerRef : FSharpChecker = Unchecked.defaultof<_>
let mutable optsRef : FSharpProjectOptions = Unchecked.defaultof<_>
let mutable fileVersions = ConcurrentDictionary<string, int>()

let callbackInvocations = ConcurrentDictionary<string, int>()
let resetInvocationLog () = callbackInvocations.Clear()

let rec collectEntities decls = seq {
    for d in decls do
        match d with
        | FSharpImplementationFileDeclaration.Entity(e, sub) ->
            yield e
            yield! collectEntities sub
        | _ -> () }

let synthesizeObserver (checked0: FSharpCheckFileResults) =
    let impl0 = checked0.ImplementationFile |> Option.get
    let rec' =
        collectEntities impl0.Declarations
        |> Seq.find (fun e -> e.IsFSharpRecord && e.DisplayName = "Record")
    let qualifiedName = rec'.QualifiedName
    sprintf "module Observer\n\nlet watchedType = \"%s\"\n" qualifiedName

let makeReentrantDocSource () =
    DocumentSource.Custom(fun path ->
        async {
            callbackInvocations.AddOrUpdate(path, 1, fun _ n -> n + 1) |> ignore
            if path = observerPath then
                let p0 = prefixPath 0
                let v0 = fileVersions.GetOrAdd(p0, 0)
                let! _, ans0 = checkerRef.ParseAndCheckFileInProject(p0, v0, SourceText.ofString files.[p0], optsRef)
                match ans0 with
                | FSharpCheckFileAnswer.Succeeded r ->
                    let text = synthesizeObserver r
                    files.[observerPath] <- text
                    return Some(SourceText.ofString text :> ISourceText)
                | FSharpCheckFileAnswer.Aborted -> return failwith "reentrant check of Prefix0000 aborted"
            elif files.ContainsKey(path) then
                return Some(SourceText.ofString files.[path] :> ISourceText)
            else
                return None
        })

let buildProject (n: int) =
    files.Clear()
    fileVersions.Clear()
    for i in 0 .. n - 1 do
        let p = prefixPath i
        files.[p] <- mkPrefixSrc i 0
        fileVersions.[p] <- 0
    let docSource = makeReentrantDocSource ()
    let checker = FSharpChecker.Create(keepAssemblyContents = true, documentSource = docSource, useTransparentCompiler = true)
    checkerRef <- checker
    let baseOptsResult =
        checker.GetProjectOptionsFromScript(prefixPath 0, SourceText.ofString files.[prefixPath 0])
        |> Async.RunSynchronously
    let baseOpts, scriptDiags = baseOptsResult
    if not scriptDiags.IsEmpty then printfn "  script diags: %A" scriptDiags
    let sourceFiles = Array.append [| for i in 0 .. n - 1 -> prefixPath i |] [| observerPath |]
    let opts =
        { baseOpts with
            ProjectFileName = @"C:\virt\spike.fsproj"
            SourceFiles = sourceFiles }
    optsRef <- opts
    checker, opts

let runCheck (label: string) : int64 * int * int =
    resetInvocationLog ()
    let sw = Stopwatch.StartNew()
    let result = checkerRef.ParseAndCheckProject(optsRef) |> Async.RunSynchronously
    sw.Stop()
    let errs = result.Diagnostics |> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)
    let totalInvocations = callbackInvocations.Values |> Seq.sum
    if errs.Length > 0 then
        printfn "  [%s] *** %d ERRORS: %A" label errs.Length (errs |> Array.map (fun d -> d.Message))
    (sw.ElapsedMilliseconds, errs.Length, totalInvocations)

let median (xs: int64 list) =
    let sorted = List.sort xs
    sorted.[sorted.Length / 2]

// editIdx: which prefix file the editOne condition edits. Defaults to 0 (the original design's
// fixed choice). Follow-up 1 from 03-review.md: the review found editing idx 0 (the FIRST file,
// with N-1 compilation-order successors) is the worst-case edit position, not a representative
// "unrelated file" - added here as a variable so that claim can be checked at other positions
// without touching the primary run's own numbers (editIdx=0 reproduces them exactly).
let runForN (editIdx: int) (n: int) =
    printfn "=== N = %d, editIdx = %d ===" n editIdx
    let _checker, _opts = buildProject n

    let coldMs, coldErrs, coldInv = runCheck (sprintf "N=%d cold" n)
    printfn "  cold:        %5dms  errs=%d  callbackInvocations=%d" coldMs coldErrs coldInv

    let repeats = [ for r in 1..3 -> runCheck (sprintf "N=%d repeat%d" n r) ]
    repeats |> List.iteri (fun i (ms, errs, inv) -> printfn "  repeat%d:     %5dms  errs=%d  callbackInvocations=%d" (i+1) ms errs inv)
    let repeatMedian = repeats |> List.map (fun (ms,_,_) -> ms) |> median

    let observerBefore = match files.TryGetValue(observerPath) with | true, v -> v | _ -> "<none>"

    let editOnes =
        [ for r in 1..3 ->
            let pEdit = prefixPath editIdx
            files.[pEdit] <- mkPrefixSrc editIdx r
            fileVersions.[pEdit] <- fileVersions.[pEdit] + 1
            runCheck (sprintf "N=%d editIdx=%d editOne%d" n editIdx r) ]
    editOnes |> List.iteri (fun i (ms, errs, inv) -> printfn "  editOne%d:    %5dms  errs=%d  callbackInvocations=%d" (i+1) ms errs inv)
    let editOneMedian = editOnes |> List.map (fun (ms,_,_) -> ms) |> median

    let observerAfter = files.[observerPath]
    let observerTypeUnchanged = observerBefore = observerAfter
    printfn "  Observer.fs generated text unchanged across value-only edit (type shape untouched): %b" observerTypeUnchanged

    let repeatRatio = float repeatMedian / float coldMs
    let editOneRatio = float editOneMedian / float coldMs
    printfn "  cold=%dms  repeatMedian=%dms (ratio=%.3f)  editOneMedian=%dms (ratio=%.3f)"
        coldMs repeatMedian repeatRatio editOneMedian editOneRatio
    printfn ""
    {| N = n; Cold = coldMs; RepeatMedian = repeatMedian; EditOneMedian = editOneMedian
       RepeatRatio = repeatRatio; EditOneRatio = editOneRatio; ObserverTypeUnchanged = observerTypeUnchanged |}

[<EntryPoint>]
let main argv =
    // DEVIATION FROM DESIGN, corrected before results were written: the design calls for "fresh
    // process per N to avoid any single-process warm-up carrying over between N values" (01-design.md,
    // "Conditions, per N"). The first implementation ran all four N in one process/one main loop,
    // which let one-time JIT/assembly-load cost at the first checker creation (N=10) inflate that
    // N's cold measurement relative to later N's in the same warm process. Fixed here: N is now
    // taken from argv, one N per process invocation, run four times externally.
    // Second, optional argv: which prefix file index editOne edits (default 0, matching the
    // primary run this file originally produced). Added per 03-review.md Follow-up 1.
    let n, editIdx =
        match argv with
        | [| nStr |] -> int nStr, 0
        | [| nStr; editIdxStr |] -> int nStr, int editIdxStr
        | _ -> failwith "usage: q023-spike <N> [editIdx]"

    printfn "Q023 - scale cost of ParseAndCheckProject under a reentrant DocumentSource.Custom callback"
    printfn "Testing whether unchanged-remainder cost stays flat (c) or tracks cold cost (b) as N grows."
    printfn "Single-N-per-process run: N=%d editIdx=%d" n editIdx
    printfn ""

    let r = runForN editIdx n

    printfn "=== Result (N=%d, editIdx=%d) ===" n editIdx
    printfn "cold=%dms repeatMedian=%dms (ratio=%.4f) editOneMedian=%dms (ratio=%.4f) observerStable=%b"
        r.Cold r.RepeatMedian r.RepeatRatio r.EditOneMedian r.EditOneRatio r.ObserverTypeUnchanged

    0
