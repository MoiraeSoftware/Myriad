// Q003 falsifier: can FsiEvaluationSession (FSI) be hosted in the SAME process as the
// FSharpChecker used in Q001/Q002, and can a value it evaluates actually be used by host code?
// This is the mechanism type providers run on (compile-time evaluation against external data).
// See ../../02-results.md for the run this produced.

open System
open System.IO
open System.Text
open System.Collections.Concurrent
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open FSharp.Compiler.Interactive.Shell

let step (label: string) (f: unit -> unit) =
    printfn "=== %s ===" label
    try
        f ()
        printfn "OK"
    with ex ->
        printfn "THREW: %s" (ex.ToString())
    printfn ""

// ---------------------------------------------------------------------------
// Stage 1: FSharpChecker works on its own (sanity baseline, same pattern as Q001).
// ---------------------------------------------------------------------------

let files = ConcurrentDictionary<string, string>()
let fileA = @"C:\virt\A.fs"
files.[fileA] <- "module A\nlet x = 42\n"

let docSource =
    DocumentSource.Custom(fun path ->
        async {
            match files.TryGetValue path with
            | true, txt -> return Some(SourceText.ofString txt :> ISourceText)
            | _ -> return None
        })

let checker = FSharpChecker.Create(keepAssemblyContents = true, documentSource = docSource, useTransparentCompiler = true)

let checkA () =
    async {
        let src = SourceText.ofString files.[fileA]
        let! opts, _ = checker.GetProjectOptionsFromScript(fileA, src)
        let! _, ans = checker.ParseAndCheckFileInProject(fileA, 0, src, opts)
        match ans with
        | FSharpCheckFileAnswer.Succeeded r -> printfn "  A.fs diagnostics: %A" r.Diagnostics
        | FSharpCheckFileAnswer.Aborted -> failwith "check aborted"
    }
    |> Async.RunSynchronously

step "Stage 1: FSharpChecker alone, before FSI exists" checkA

// ---------------------------------------------------------------------------
// Stage 2: create an FsiEvaluationSession IN THE SAME PROCESS, evaluate a trivial expression.
// ---------------------------------------------------------------------------

let sbOut = StringBuilder()
let sbErr = StringBuilder()
let inStream = new StringReader("")
let outStream = new StringWriter(sbOut)
let errStream = new StringWriter(sbErr)
let fsiArgs = [| "fsi.exe"; "--noninteractive"; "--nologo"; "--gui-" |]
let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()

let mutable fsiSessionOpt: FsiEvaluationSession option = None

step "Stage 2: create FsiEvaluationSession alongside an already-created FSharpChecker" (fun () ->
    let session = FsiEvaluationSession.Create(fsiConfig, fsiArgs, inStream, outStream, errStream, collectible = true)
    fsiSessionOpt <- Some session
    printfn "  FsiEvaluationSession created successfully")

let fsiSession = fsiSessionOpt |> Option.defaultWith (fun () -> failwith "Stage 2 failed, cannot continue")

step "Stage 3: evaluate a trivial expression via FSI" (fun () ->
    let result, diagnostics = fsiSession.EvalExpressionNonThrowing("1 + 2")
    if diagnostics.Length > 0 then printfn "  diagnostics: %A" diagnostics
    match result with
    | Choice1Of2(Some value) -> printfn "  Result: %A : %s" value.ReflectionValue value.ReflectionType.FullName
    | Choice1Of2 None -> printfn "  Result: <none>"
    | Choice2Of2(exn: exn) -> failwithf "  FSI eval threw: %s" exn.Message)

step "Stage 4: re-check FSharpChecker AFTER FSI has run, in the same process" checkA

// ---------------------------------------------------------------------------
// Stage 5: the real test - evaluate a STRING-returning expression (BCL type, should be
// identity-safe across FSI's dynamic assembly and the host process).
// ---------------------------------------------------------------------------

step "Stage 5: FSI evaluates a string-returning expression, host reads it as a real string" (fun () ->
    let result, _ = fsiSession.EvalExpressionNonThrowing("sprintf \"Name:string;Age:int\"")
    match result with
    | Choice1Of2(Some value) ->
        let s = value.ReflectionValue :?> string
        printfn "  Host successfully cast FSI's result to System.String: \"%s\"" s
    | Choice1Of2 None -> failwith "  no value returned"
    | Choice2Of2(exn: exn) -> failwithf "  FSI eval threw: %s" exn.Message)

// ---------------------------------------------------------------------------
// Stage 6: the harder test - evaluate an F#-typed value (a tuple list) and try to use it
// as the host's own compiled (string * string) list type, not just via reflection/ToString.
// This is the real type-provider-shaped question: can compile-time-evaluated F# DATA
// (not just primitives) cross into host code as a directly usable value?
// ---------------------------------------------------------------------------

step "Stage 6: FSI evaluates an F#-typed (string * string) list, host tries to use it AS that type" (fun () ->
    let result, _ = fsiSession.EvalExpressionNonThrowing("[(\"Name\", \"string\"); (\"Age\", \"int\")]")
    match result with
    | Choice1Of2(Some value) ->
        printfn "  ReflectionType: %s" value.ReflectionType.FullName
        printfn "  ReflectionType.Assembly: %s" value.ReflectionType.Assembly.FullName
        printfn "  Host's own FSharp.Core identity: %s" (typeof<int list>.Assembly.FullName)
        try
            let asHostType = value.ReflectionValue :?> (string * string) list
            printfn "  Direct cast to host's (string*string) list SUCCEEDED: %A" asHostType
        with :? InvalidCastException as ex ->
            printfn "  Direct cast FAILED (InvalidCastException): %s" ex.Message
            printfn "  Falling back to reflection to read the value structurally:"
            let seqType = typeof<System.Collections.IEnumerable>
            let asEnumerable = value.ReflectionValue :?> System.Collections.IEnumerable
            for item in asEnumerable do
                printfn "    item via reflection: %A (type %s)" item (item.GetType().FullName)
    | Choice1Of2 None -> failwith "  no value returned"
    | Choice2Of2(exn: exn) -> failwithf "  FSI eval threw: %s" exn.Message)

printfn "Q003 falsifier run complete."
