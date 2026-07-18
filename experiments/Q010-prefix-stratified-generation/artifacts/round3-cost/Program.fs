// Q010 Round 3 - cost sanity check (single sample, honestly labelled as such).
//
// Times two ways of producing the same Round 2 result:
//   (A) REENTRANT: one whole-project check whose DocumentSource.Custom callback computes
//       Stratified.fs's text by reentrantly checking the A.fs+B.fs prefix mid-check.
//   (B) STAGED:    precompute A.fs+B.fs's typed results in a separate, prior checker/opts,
//       derive Stratified.fs's text from that, then run one ordinary check of a static 3-file project.
//
// Single sample each, cold checkers. See ../../02-results.md.

open System.Collections.Concurrent
open System.Diagnostics
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text

let fileA = @"C:\virt\A.fs"
let fileB = @"C:\virt\B.fs"
let fileStratified = @"C:\virt\Stratified.fs"

let aSrc = "namespace Domain\n\ntype Person = { Name: string; Age: int }\n"
let bSrc = "namespace rec Domain\n\nmodule PersonLenses =\n    open Domain\n    let Name = ((fun (x: Person) -> x.Name), (fun (x: Person) (value: string) -> { x with Name = value }))\n    let Age = ((fun (x: Person) -> x.Age), (fun (x: Person) (value: int) -> { x with Age = value }))\n"

let rec collectMfvs decls = seq {
    for d in decls do
        match d with
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(v, _, _) -> yield v
        | FSharpImplementationFileDeclaration.Entity(_, sub) -> yield! collectMfvs sub
        | _ -> () }

let getterReturnTypeName (v: FSharpMemberOrFunctionOrValue) =
    try
        let t = v.FullType
        if t.IsTupleType && t.GenericArguments.Count >= 1 then
            let g = t.GenericArguments.[0]
            if g.IsFunctionType && g.GenericArguments.Count >= 2 then
                Some(g.GenericArguments.[g.GenericArguments.Count - 1].Format FSharpDisplayContext.Empty)
            else None
        else None
    with _ -> None

let synthesizePersonJson (checkedB: FSharpCheckFileResults) =
    let implB = checkedB.ImplementationFile |> Option.get
    let bindings =
        collectMfvs implB.Declarations
        |> Seq.filter (fun v -> not v.IsMember && v.DeclaringEntity.IsSome && v.DeclaringEntity.Value.DisplayName = "PersonLenses")
        |> Seq.map (fun v -> v.DisplayName, getterReturnTypeName v |> Option.defaultValue "?")
        |> List.ofSeq
    let letLines = bindings |> List.map (fun (n, _) -> sprintf "    let get%s, _ = Domain.PersonLenses.%s" n n) |> String.concat "\n"
    let jsonParts = bindings |> List.map (fun (n, ret) -> let spec = if ret.EndsWith "string" then "\\\"%s\\\"" else "%d" in sprintf "\\\"%s\\\":%s" (n.ToLowerInvariant()) spec) |> String.concat ","
    let jsonArgs = bindings |> List.map (fun (n, _) -> sprintf "(get%s p)" n) |> String.concat " "
    sprintf "module Domain.PersonJson\n\nlet serialize (p: Domain.Person) =\n%s\n    sprintf \"{%s}\" %s\n" letLines jsonParts jsonArgs

let errCount (diags: FSharpDiagnostic[]) =
    diags |> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error) |> Array.length

// ---- (A) reentrant path ----
let runReentrant () =
    let files = ConcurrentDictionary<string, string>()
    files.[fileA] <- aSrc
    files.[fileB] <- bSrc
    let mutable checkerRef : FSharpChecker = Unchecked.defaultof<_>
    let mutable optsRef : FSharpProjectOptions = Unchecked.defaultof<_>
    let docSource =
        DocumentSource.Custom(fun path ->
            async {
                if path = fileStratified then
                    let! _, _ = checkerRef.ParseAndCheckFileInProject(fileA, 0, SourceText.ofString files.[fileA], optsRef)
                    let! _, ansB = checkerRef.ParseAndCheckFileInProject(fileB, 0, SourceText.ofString files.[fileB], optsRef)
                    let checkedB = match ansB with FSharpCheckFileAnswer.Succeeded r -> r | _ -> failwith "B aborted"
                    let text = synthesizePersonJson checkedB
                    files.[fileStratified] <- text
                    return Some(SourceText.ofString text :> ISourceText)
                else
                    match files.TryGetValue path with
                    | true, t -> return Some(SourceText.ofString t :> ISourceText)
                    | _ -> return None })
    let checker = FSharpChecker.Create(keepAssemblyContents = true, documentSource = docSource, useTransparentCompiler = true)
    checkerRef <- checker
    async {
        let! baseOpts, _ = checker.GetProjectOptionsFromScript(fileA, SourceText.ofString aSrc)
        let opts = { baseOpts with ProjectFileName = @"C:\virt\r.fsproj"; SourceFiles = [| fileA; fileB; fileStratified |] }
        optsRef <- opts
        let sw = Stopwatch.StartNew()
        let! proj = checker.ParseAndCheckProject(opts)
        sw.Stop()
        let errs = proj.Diagnostics |> Array.filter (fun d -> d.FileName = fileStratified) |> errCount
        return sw.ElapsedMilliseconds, errs, files.[fileStratified]
    } |> Async.RunSynchronously

// ---- (B) staged path ----
let runStaged () =
    let sw = Stopwatch.StartNew()
    // Stage 1: separate prior checker/opts for the A+B prefix.
    let files1 = ConcurrentDictionary<string, string>()
    files1.[fileA] <- aSrc
    files1.[fileB] <- bSrc
    let docSource1 =
        DocumentSource.Custom(fun path ->
            async { match files1.TryGetValue path with true, t -> return Some(SourceText.ofString t :> ISourceText) | _ -> return None })
    let checker1 = FSharpChecker.Create(keepAssemblyContents = true, documentSource = docSource1, useTransparentCompiler = true)
    let stratText =
        async {
            let! baseOpts, _ = checker1.GetProjectOptionsFromScript(fileA, SourceText.ofString aSrc)
            let opts1 = { baseOpts with ProjectFileName = @"C:\virt\s1.fsproj"; SourceFiles = [| fileA; fileB |] }
            let! _, ansB = checker1.ParseAndCheckFileInProject(fileB, 0, SourceText.ofString bSrc, opts1)
            let checkedB = match ansB with FSharpCheckFileAnswer.Succeeded r -> r | _ -> failwith "B aborted"
            return synthesizePersonJson checkedB
        } |> Async.RunSynchronously
    // Stage 2: fresh checker/opts, ordinary check of a static 3-file project.
    let files2 = ConcurrentDictionary<string, string>()
    files2.[fileA] <- aSrc
    files2.[fileB] <- bSrc
    files2.[fileStratified] <- stratText
    let docSource2 =
        DocumentSource.Custom(fun path ->
            async { match files2.TryGetValue path with true, t -> return Some(SourceText.ofString t :> ISourceText) | _ -> return None })
    let checker2 = FSharpChecker.Create(keepAssemblyContents = true, documentSource = docSource2, useTransparentCompiler = true)
    let errs =
        async {
            let! baseOpts, _ = checker2.GetProjectOptionsFromScript(fileA, SourceText.ofString aSrc)
            let opts2 = { baseOpts with ProjectFileName = @"C:\virt\s2.fsproj"; SourceFiles = [| fileA; fileB; fileStratified |] }
            let! proj = checker2.ParseAndCheckProject(opts2)
            return proj.Diagnostics |> Array.filter (fun d -> d.FileName = fileStratified) |> errCount
        } |> Async.RunSynchronously
    sw.Stop()
    sw.ElapsedMilliseconds, errs, stratText

[<EntryPoint>]
let main _ =
    printfn "Q010 Round 3 - cost (single sample each; run #1 = cold process, run #2 = warm)"
    printfn ""
    // Run #1: the FIRST checker created pays one-time process cold-start (FCS JIT + assembly load).
    let r1Ms, r1Errs, rText = runReentrant ()
    printfn "run #1  REENTRANT (cold process): %dms, strat-errors=%d" r1Ms r1Errs
    let s1Ms, s1Errs, sText = runStaged ()
    printfn "run #1  STAGED    (process warmed by reentrant run): %dms, strat-errors=%d" s1Ms s1Errs
    printfn ""
    // Run #2: both paths now see a warm process - the comparable numbers.
    let r2Ms, _, _ = runReentrant ()
    printfn "run #2  REENTRANT (warm): %dms" r2Ms
    let s2Ms, _, _ = runStaged ()
    printfn "run #2  STAGED    (warm): %dms" s2Ms
    printfn ""
    printfn "Both strategies produced identical Stratified.fs text: %b" (rText = sText)
    printfn ""
    printfn "NOTE: single timed sample per path per run, no repeated-trial distribution - consistent"
    printfn "with every prior quartet in this repo. Run #1's reentrant number absorbs one-time process"
    printfn "cold-start that run #1's staged number does not; run #2 (both warm) is the fair comparison."
    0
