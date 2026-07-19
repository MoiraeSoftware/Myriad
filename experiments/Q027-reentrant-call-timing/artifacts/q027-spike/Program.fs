// Q027 - does the reentrant call (made from inside a later file's DocumentSource.Custom callback,
// while the outer ParseAndCheckProject is still unreturned) cost like a cold check or a warm one?
//
// Settles Q010's own review Objection 1, inherited unresolved by Q021/Q023/Q024/Q026.
//
// See ../../02-results.md for the run this produced.

open System
open System.Diagnostics
open System.Collections.Concurrent
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Text

let files = ConcurrentDictionary<string, string>()
let fileA = @"C:\virt-q027\A.fs"
let fileB = @"C:\virt-q027\B.fs"
let fileC = @"C:\virt-q027\C.fs"

// Q023's own mkPrefixSrc(i, extraTags=0) template (Q023-.../artifacts/q023-spike/Program.fs:24-42),
// renamed per module - genuine typecheck weight (generic record, Map/List pipeline, explicit
// recursion), not `let x = 5` padding (BACKLOG.md item 4's named methodology complaint).
let mkSrc (moduleName: string) =
    sprintf
        "module %s\n\
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
         let records = [ for k in 1..20 -> make k (sprintf \"item%%d\" k) [ \"a\"; \"b\"; string k ] ]\n\
         let total = records |> fold (fun acc r -> acc + r.Meta.Count) 0\n"
        moduleName

files.[fileA] <- mkSrc "A"
files.[fileB] <- mkSrc "B"
files.[fileC] <- mkSrc "C"

let sw = Stopwatch()

let timeCheck (checker: FSharpChecker) (path: string) (opts: FSharpProjectOptions) : int64 =
    sw.Restart()
    let _, ans =
        checker.ParseAndCheckFileInProject(path, 0, SourceText.ofString files.[path], opts)
        |> Async.RunSynchronously
    sw.Stop()
    match ans with
    | FSharpCheckFileAnswer.Aborted -> failwithf "check of %s aborted" path
    | FSharpCheckFileAnswer.Succeeded checkRes ->
        let errs = checkRes.Diagnostics |> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)
        if errs.Length > 0 then
            failwithf "check of %s had %d errors: %A" path errs.Length errs
    sw.ElapsedMilliseconds

let mutable checkerRef : FSharpChecker = Unchecked.defaultof<_>
let mutable optsRef : FSharpProjectOptions = Unchecked.defaultof<_>
let mutable reentrantMsA = -1L
let mutable reentrantMsB = -1L
let mutable reentrantCallbackFired = false

let makeDocSource () =
    DocumentSource.Custom(fun path ->
        async {
            if path = fileA then return Some(SourceText.ofString files.[fileA] :> ISourceText)
            elif path = fileB then return Some(SourceText.ofString files.[fileB] :> ISourceText)
            elif path = fileC then
                reentrantCallbackFired <- true
                // Reentrant: checkerRef is the SAME instance the outer ParseAndCheckProject call
                // (still unreturned on the stack) is using.
                reentrantMsA <- timeCheck checkerRef fileA optsRef
                reentrantMsB <- timeCheck checkerRef fileB optsRef
                return Some(SourceText.ofString files.[fileC] :> ISourceText)
            else return None
        })

[<EntryPoint>]
let main _ =
    let docSource = makeDocSource ()
    let checker = FSharpChecker.Create(keepAssemblyContents = true, documentSource = docSource, useTransparentCompiler = true)
    checkerRef <- checker

    let run =
        async {
            let! baseOpts, _ = checker.GetProjectOptionsFromScript(fileA, SourceText.ofString files.[fileA])
            let opts =
                { baseOpts with
                    ProjectFileName = @"C:\virt-q027\q027.fsproj"
                    SourceFiles = [| fileA; fileB; fileC |] }
            optsRef <- opts

            let! projResults = checker.ParseAndCheckProject(opts)
            let errs = projResults.Diagnostics |> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)
            if errs.Length > 0 then failwithf "project-wide errors: %A" errs

            // Warm repeat: same checker, now unambiguously idle (outer call above has returned).
            let warmMsA = timeCheck checker fileA opts
            let warmMsB = timeCheck checker fileB opts

            // Cold check: brand-new checker instances, never touched fileA/fileB before, but given
            // the SAME 3-file `opts` reentrant/warm use (not shrunk to a single-file project) so
            // "which checker instance" remains the only variable. Two corrections discovered while
            // running, both reported honestly in 02-results.md:
            // (1) a fresh checker with default DocumentSource.Filesystem, given the 3-file `opts`,
            //     tries to read the OTHER project files from real disk (they don't exist there) even
            //     when explicit text is supplied for the one file being checked - TransparentCompiler's
            //     snapshot construction needs every SourceFiles entry resolvable, not just the one
            //     being timed. Fixed by giving cold checkers the same static DocumentSource.Custom
            //     (returning each file's already-known text) rather than the filesystem default.
            // (2) shrinking cold's project to a single file (the first fix attempted) would have
            //     changed project shape as well as checker instance - two variables, not one - so
            //     that approach was discarded in favor of (1).
            let staticDocSource =
                DocumentSource.Custom(fun path ->
                    async { return match files.TryGetValue path with true, t -> Some(SourceText.ofString t :> ISourceText) | _ -> None })
            let coldCheckerA = FSharpChecker.Create(useTransparentCompiler = true, documentSource = staticDocSource)
            let coldMsA = timeCheck coldCheckerA fileA opts
            let coldCheckerB = FSharpChecker.Create(useTransparentCompiler = true, documentSource = staticDocSource)
            let coldMsB = timeCheck coldCheckerB fileB opts

            printfn "reentrantCallbackFired=%b" reentrantCallbackFired
            printfn "A: reentrant=%dms  warm=%dms  cold=%dms  (reentrant/cold=%.3f reentrant/warm=%.3f warm/cold=%.3f)"
                reentrantMsA warmMsA coldMsA
                (float reentrantMsA / float (max coldMsA 1L))
                (float reentrantMsA / float (max warmMsA 1L))
                (float warmMsA / float (max coldMsA 1L))
            printfn "B: reentrant=%dms  warm=%dms  cold=%dms  (reentrant/cold=%.3f reentrant/warm=%.3f warm/cold=%.3f)"
                reentrantMsB warmMsB coldMsB
                (float reentrantMsB / float (max coldMsB 1L))
                (float reentrantMsB / float (max warmMsB 1L))
                (float warmMsB / float (max coldMsB 1L))
            return 0
        }
    Async.RunSynchronously(run, timeout = 60_000)
