// Q027 REVIEW control spike (Movement 4). Decomposes reentrantA's ~516ms.
//
// The baseline spike (q027-spike) measures reentrantA as the VERY FIRST typecheck done in the
// process, on a checker built with keepAssemblyContents=true, and compares it to coldA which runs
// later in the same process (JIT+FCS static-init already paid) on a checker built WITHOUT
// keepAssemblyContents. Two uncontrolled variables ride along with "reentrancy". This spike
// separates them with explicit modes, each run as its own fresh process.
//
//   baseline          : replicate q027-spike exactly (sanity check).
//   warmup-reentrant  : pay JIT/FCS-init with a throwaway full project check FIRST, then run the
//                       real reentrant sequence. If reentrantA collapses -> its cost was
//                       first-in-process warmup, not reentrancy.
//   direct-first      : create checker1 (kac=true, reentrant docSource) and call
//                       ParseAndCheckFileInProject(A) DIRECTLY as the first op, no outer
//                       ParseAndCheckProject, no reentrancy. Isolates first-use-of-this-checker.
//   direct-first-warm : same as direct-first but after a throwaway warmup project.
//   cold-kac          : after warmup, cold check A with kac=true vs kac=false. Isolates the
//                       keepAssemblyContents contribution to the cold baseline.
//   reverse           : reentrant callback checks B THEN A (order reversed). Tests whether
//                       "first reentrant call expensive, second cheap" is a property of call ORDER
//                       (amortization theory) or of file IDENTITY.
//   third             : reentrant callback checks A, B, D (three reentrant calls). Tests whether
//                       the cost curve keeps dropping after the 2nd call.

open System
open System.Diagnostics
open System.Collections.Concurrent
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Text

let files = ConcurrentDictionary<string, string>()
let fileA = @"C:\virt-q027c\A.fs"
let fileB = @"C:\virt-q027c\B.fs"
let fileC = @"C:\virt-q027c\C.fs"
let fileD = @"C:\virt-q027c\D.fs"

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
files.[fileD] <- mkSrc "D"

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
        if errs.Length > 0 then failwithf "check of %s had %d errors: %A" path errs.Length errs
    sw.ElapsedMilliseconds

let staticDocSource =
    DocumentSource.Custom(fun path ->
        async { return match files.TryGetValue path with true, t -> Some(SourceText.ofString t :> ISourceText) | _ -> None })

let mutable checkerRef : FSharpChecker = Unchecked.defaultof<_>
let mutable optsRef : FSharpProjectOptions = Unchecked.defaultof<_>
let mutable r1 = -1L
let mutable r2 = -1L
let mutable r3 = -1L
let mutable fired = false

// order = the list of files to check reentrantly, from inside fileC's callback
let makeReentrantDocSource (order: string list) =
    DocumentSource.Custom(fun path ->
        async {
            if path = fileC then
                fired <- true
                match order with
                | [a] -> r1 <- timeCheck checkerRef a optsRef
                | [a; b] ->
                    r1 <- timeCheck checkerRef a optsRef
                    r2 <- timeCheck checkerRef b optsRef
                | [a; b; c] ->
                    r1 <- timeCheck checkerRef a optsRef
                    r2 <- timeCheck checkerRef b optsRef
                    r3 <- timeCheck checkerRef c optsRef
                | _ -> ()
                return Some(SourceText.ofString files.[fileC] :> ISourceText)
            else
                return match files.TryGetValue path with true, t -> Some(SourceText.ofString t :> ISourceText) | _ -> None
        })

let getOpts (srcFiles: string[]) =
    async {
        // Use a throwaway checker for options resolution so checker1's first CHECK is genuinely its
        // first check operation in modes that test "first use of checker1".
        let optChecker = FSharpChecker.Create(useTransparentCompiler = true, documentSource = staticDocSource)
        let! baseOpts, _ = optChecker.GetProjectOptionsFromScript(fileA, SourceText.ofString files.[fileA])
        return { baseOpts with ProjectFileName = @"C:\virt-q027c\q027c.fsproj"; SourceFiles = srcFiles }
    } |> Async.RunSynchronously

let warmupProject (opts: FSharpProjectOptions) =
    // Throwaway full project check on a separate fresh checker: pays process-global JIT + FCS
    // static init so that whatever runs next is not the first typecheck in the process.
    let sw2 = Stopwatch.StartNew()
    let c = FSharpChecker.Create(useTransparentCompiler = true, documentSource = staticDocSource)
    c.ParseAndCheckProject(opts) |> Async.RunSynchronously |> ignore
    sw2.Stop()
    sw2.ElapsedMilliseconds

[<EntryPoint>]
let main argv =
    let mode = if argv.Length > 0 then argv.[0] else "baseline"
    match mode with
    | "baseline" | "reverse" | "third" ->
        let srcFiles =
            if mode = "third" then [| fileA; fileB; fileD; fileC |] else [| fileA; fileB; fileC |]
        let order =
            match mode with
            | "reverse" -> [fileB; fileA]
            | "third" -> [fileA; fileB; fileD]
            | _ -> [fileA; fileB]
        let opts = getOpts srcFiles
        optsRef <- opts
        let checker = FSharpChecker.Create(keepAssemblyContents = true, documentSource = makeReentrantDocSource order, useTransparentCompiler = true)
        checkerRef <- checker
        checker.ParseAndCheckProject(opts) |> Async.RunSynchronously |> ignore
        printfn "mode=%s fired=%b  r1=%dms r2=%dms r3=%dms" mode fired r1 r2 r3

    | "warmup-reentrant" ->
        let opts = getOpts [| fileA; fileB; fileC |]
        optsRef <- opts
        let w = warmupProject opts
        let checker = FSharpChecker.Create(keepAssemblyContents = true, documentSource = makeReentrantDocSource [fileA; fileB], useTransparentCompiler = true)
        checkerRef <- checker
        checker.ParseAndCheckProject(opts) |> Async.RunSynchronously |> ignore
        printfn "mode=warmup-reentrant fired=%b  warmupProjectMs=%d  reentrantA=%dms reentrantB=%dms" fired w r1 r2

    | "direct-first" | "direct-first-warm" ->
        let opts = getOpts [| fileA; fileB; fileC |]
        optsRef <- opts
        let w = if mode = "direct-first-warm" then warmupProject opts else -1L
        // checker1 built identically to the reentrant checker, but we call it DIRECTLY (no outer
        // ParseAndCheckProject, so no reentrancy). Its docSource has reentrant logic but the fileC
        // branch never fires because we only check A and B directly.
        let checker = FSharpChecker.Create(keepAssemblyContents = true, documentSource = makeReentrantDocSource [fileA; fileB], useTransparentCompiler = true)
        checkerRef <- checker
        let dA = timeCheck checker fileA opts
        let dB = timeCheck checker fileB opts
        printfn "mode=%s warmupProjectMs=%d  directA=%dms directB=%dms" mode w dA dB

    | "cold-kac" ->
        let opts = getOpts [| fileA; fileB; fileC |]
        let w = warmupProject opts
        let cKac = FSharpChecker.Create(keepAssemblyContents = true, useTransparentCompiler = true, documentSource = staticDocSource)
        let aKac = timeCheck cKac fileA opts
        let cNo = FSharpChecker.Create(keepAssemblyContents = false, useTransparentCompiler = true, documentSource = staticDocSource)
        let aNo = timeCheck cNo fileA opts
        printfn "mode=cold-kac warmupProjectMs=%d  coldA_kacTrue=%dms coldA_kacFalse=%dms" w aKac aNo

    | other ->
        eprintfn "unknown mode %s" other
    0
