// Q015 spike: direct follow-up to Q014's own adversarial review (follow-up 1). Q014 never actually
// hosted FSI - both the general implementation and its static config were host-compile-time source
// literals. This spike closes that specific gap: a real FsiEvaluationSession evaluates a general
// recursive implementation read from a SEPARATE FILE (never referenced by name anywhere in this
// file) plus a specializing config read from an ENVIRONMENT VARIABLE (never a literal here either),
// and the resulting quotation is run through Q014's own already-proven reification pipeline
// unchanged. See ../../00-hypothesis.md and ../../01-design.md.
//
// Round 1: cheapest falsifier - get a reflectable MethodInfo out of a LIVE FSI session (never a
//          host-compile-time reference), confirm it genuinely originates from FSI's dynamic
//          assembly, confirm Expr.TryGetReflectedDefinition works on it.
// Round 2: capability claim - full Q014 pipeline, but n comes from an env var and the "expected"
//          reference value is obtained by invoking the FSI-obtained MethodInfo via reflection, not
//          by calling the general implementation by name (impossible from host code).
// Round 3: single-sample cost of the combined FSI-hosting + pipeline sequence.

open System
open System.IO
open System.Diagnostics
open System.Reflection
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Interactive.Shell
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.ExprShape
open Swensen.Unquote

let step (label: string) (f: unit -> unit) =
    printfn "=== %s ===" label
    try
        f ()
        printfn "OK"
    with ex ->
        printfn "THREW: %s" (ex.ToString())
    printfn ""

// ---------------------------------------------------------------------------------------------
// Setup: the "not known at host-compile-time" plumbing.
// ---------------------------------------------------------------------------------------------

let pluginPath = Path.Combine(Directory.GetCurrentDirectory(), "plugin", "PowerPlugin.fsx")
let pluginSourceText = File.ReadAllText(pluginPath)
printfn "Plugin source read from: %s" pluginPath
printfn "Plugin source text:\n%s" pluginSourceText

let n =
    match Environment.GetEnvironmentVariable("Q015_POWER_N") with
    | null | "" -> 5
    | s -> int s
printfn "Specializing config n = %d (from Q015_POWER_N env var, default 5 if unset)" n

let checker = FSharpChecker.Create(keepAssemblyContents = true, useTransparentCompiler = true)

let typecheckSnippet (label: string) (source: string) : FSharpDiagnostic[] * FSharpDiagnostic[] =
    async {
        let fileName = sprintf "%s.fsx" label
        let src = SourceText.ofString source
        let! opts, _ = checker.GetProjectOptionsFromScript(fileName, src)
        let! _parseRes, checkAnswer = checker.ParseAndCheckFileInProject(fileName, 0, src, opts)
        match checkAnswer with
        | FSharpCheckFileAnswer.Succeeded r ->
            let errors = r.Diagnostics |> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)
            return errors, r.Diagnostics
        | FSharpCheckFileAnswer.Aborted -> return failwith "check aborted"
    }
    |> Async.RunSynchronously

// ---------------------------------------------------------------------------------------------
// Reification pipeline, copied verbatim from Q014 (unchanged - this quartet's new surface is
// entirely about how powerMi is obtained, not this half).
// ---------------------------------------------------------------------------------------------

let miOf (e: Expr) =
    match e with
    | Call(_, mi, _) -> mi
    | _ -> failwithf "miOf: expected a Call shape, got %A" e

let addMi = miOf <@ 1 + 1 @>
let subMi = miOf <@ 1 - 1 @>
let mulMiFloat = miOf <@ 1.0 * 1.0 @>
let eqMiInt = miOf <@ 1 = 1 @>

let rec translate (e: Expr) : string =
    match e with
    | Value((:? int as v), t) when t = typeof<int> -> string v
    | Value((:? float as v), t) when t = typeof<float> -> sprintf "%f" v
    | Value((:? bool as v), t) when t = typeof<bool> -> string v
    | Var v -> v.Name
    | Lambda(v, body) -> sprintf "(fun (%s: %s) -> %s)" v.Name (v.Type.FullName) (translate body)
    | Call(None, mi, [a; b]) when mi = addMi -> sprintf "(%s + %s)" (translate a) (translate b)
    | Call(None, mi, [a; b]) when mi = subMi -> sprintf "(%s - %s)" (translate a) (translate b)
    | Call(None, mi, [a; b]) when mi = mulMiFloat -> sprintf "(%s * %s)" (translate a) (translate b)
    | Call(None, mi, [a; b]) when mi = eqMiInt -> sprintf "(%s = %s)" (translate a) (translate b)
    | _ -> failwithf "translate: unsupported node shape: %A" e

let rec substVar (var: Var) (replacement: Expr) (expr: Expr) : Expr =
    match expr with
    | ShapeVar v -> if v = var then replacement else expr
    | ShapeLambda(v, body) -> Expr.Lambda(v, substVar var replacement body)
    | ShapeCombination(op, args) -> RebuildShapeCombination(op, List.map (substVar var replacement) args)

let rec evalIntExpr (e: Expr) : int =
    match e with
    | Value((:? int as v), t) when t = typeof<int> -> v
    | Call(None, mi, [a; b]) when mi = subMi -> evalIntExpr a - evalIntExpr b
    | _ -> failwithf "evalIntExpr: unsupported shape %A" e

/// Mutable, set at the end of Round 1 - the whole point of this quartet is that this MethodInfo
/// comes from a LIVE FSI SESSION, never a host-compile-time quotation.
let mutable powerMi : MethodInfo = Unchecked.defaultof<_>

let rec unroll (e: Expr) : Expr =
    match e with
    | IfThenElse(Call(None, mi, [nExpr; Value((:? int as z), zt)]), thenB, elseB) when mi = eqMiInt && zt = typeof<int> && z = 0 ->
        if evalIntExpr nExpr = 0 then unroll thenB else unroll elseB
    | Call(None, mi, [xExpr; recCall]) when mi = mulMiFloat ->
        Expr.Call(mulMiFloat, [xExpr; unroll recCall])
    | Call(None, mi, [nArgExpr; xArgExpr]) when mi = powerMi ->
        let nVal = evalIntExpr nArgExpr
        match Expr.TryGetReflectedDefinition(powerMi) with
        | Some(Lambda(nVar, Lambda(xVar, innerBody))) ->
            let substituted = innerBody |> substVar nVar (Expr.Value(nVal)) |> substVar xVar xArgExpr
            unroll substituted
        | other -> failwithf "unroll: unexpected reflected-definition shape: %A" other
    | Value((:? float as _v), t) when t = typeof<float> -> e
    | _ -> failwithf "unroll: unsupported node shape: %A" e

// ---------------------------------------------------------------------------------------------
// Round 1: cheapest falsifier - get a reflectable MethodInfo out of a LIVE FSI session.
// ---------------------------------------------------------------------------------------------

let sbOut = Text.StringBuilder()
let sbErr = Text.StringBuilder()
let inStream = new StringReader("")
let outStream = new StringWriter(sbOut)
let errStream = new StringWriter(sbErr)
let fsiArgs = [| "fsi.exe"; "--noninteractive"; "--nologo"; "--gui-" |]
let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()
let fsiSession = FsiEvaluationSession.Create(fsiConfig, fsiArgs, inStream, outStream, errStream, collectible = true)

let mutable round1Ok = false
let sw1 = Stopwatch()

step "Round 1: obtain powerMi from a live FSI session, never a host-compile-time reference" (fun () ->
    sw1.Start()
    // Step A: define Target.power INSIDE the FSI session, from text read from a separate file.
    let defResult, defDiags = fsiSession.EvalInteractionNonThrowing(pluginSourceText)
    printfn "  plugin eval diagnostics: %A" defDiags
    match defResult with
    | Choice2Of2(exn: exn) -> failwithf "  defining plugin in FSI threw: %s" exn.Message
    | Choice1Of2(_: FsiValue option) -> printfn "  plugin defined inside FSI session"

    // Step B: ask FSI ITSELF to quote a call to its own just-defined function and hand back the
    // MethodInfo - the host never needs to reference `Target.power` by name anywhere.
    let quoteExpr =
        "match <@ Target.power 0 0.0 @> with \
         | Microsoft.FSharp.Quotations.Patterns.Call(_, mi, _) -> mi \
         | _ -> failwith \"unexpected shape\""
    let miResult, miDiags = fsiSession.EvalExpressionNonThrowing(quoteExpr)
    printfn "  quote-and-extract diagnostics: %A" miDiags
    match miResult with
    | Choice1Of2(Some value) ->
        let mi = value.ReflectionValue :?> MethodInfo
        printfn "  obtained MethodInfo: %s.%s" mi.DeclaringType.FullName mi.Name
        printfn "  mi.Module.Assembly:      %s" mi.Module.Assembly.FullName
        printfn "  host entry assembly:     %s" (Assembly.GetEntryAssembly().FullName)
        if mi.Module.Assembly = Assembly.GetEntryAssembly() then
            failwith "  MethodInfo came from the HOST's own assembly, not FSI's dynamic one - dynamic-origin claim is FALSE"
        printfn "  CONFIRMED: MethodInfo originates from FSI's dynamic assembly, not the host's"
        powerMi <- mi

        match Expr.TryGetReflectedDefinition(powerMi) with
        | Some body ->
            printfn "  TryGetReflectedDefinition SUCCEEDED on an FSI-sourced MethodInfo: %A" body
            round1Ok <- true
        | None ->
            failwith "  TryGetReflectedDefinition returned None for an FSI-sourced MethodInfo - KILL"
    | Choice1Of2 None -> failwith "  no value returned from FSI"
    | Choice2Of2(exn: exn) -> failwithf "  FSI eval threw: %s" exn.Message
    sw1.Stop())

if not round1Ok then
    printfn "Round 1 failed - stopping per pre-registered KILL threshold. Not attempting Round 2."
    exit 1

// ---------------------------------------------------------------------------------------------
// Round 2: capability claim - full pipeline, n from env var, reference value via FSI-obtained mi.
// ---------------------------------------------------------------------------------------------

let mutable round2Ok = false
let mutable round2SpecializedText: string option = None
let sw2 = Stopwatch()

step (sprintf "Round 2: power(%d, x) specialized, both origin AND config dynamic" n) (fun () ->
    sw2.Start()
    match Expr.TryGetReflectedDefinition(powerMi) with
    | None -> failwith "no reflected definition for the FSI-sourced powerMi"
    | Some(Lambda(nVar, Lambda(xVar, innerBody))) ->
        printfn "  raw reflected body (from FSI): %A" innerBody
        let bodyN = substVar nVar (Expr.Value(n)) innerBody
        let unrolled = unroll bodyN
        printfn "  unrolled Expr (in terms of %s only): %A" xVar.Name unrolled
        let specializedLambda = Expr.Lambda(xVar, unrolled)

        let text =
            try decompile specializedLambda
            with _ -> sprintf "fun (%s: float) -> %s" xVar.Name (translate unrolled)
        round2SpecializedText <- Some text
        printfn "  specialized source text: %s" text

        let source = sprintf "module Power%dModule\nlet powerN : float -> float = %s\n" n text
        let errors, allDiags = typecheckSnippet (sprintf "round2-power%d" n) source
        printfn "  all diagnostics: %A" allDiags
        if errors.Length > 0 then failwithf "typecheck failed: %A" errors

        // Reference value: invoke the FSI-obtained MethodInfo directly (host cannot call
        // Target.power by name - that identifier does not exist in host-compiled code at all).
        let reference (x: float) : float = powerMi.Invoke(null, [| box n; box x |]) :?> float

        for x in [ 2.0; 3.5 ] do
            let expected = reference x
            let evaluated = Expr.Cast<float>(substVar xVar (Expr.Value x) unrolled).Eval()
            printfn "  [Expr-eval check] x=%g  FSI-power(%d,x)=%g  evaluated-unrolled-Expr=%g  match=%b" x n expected evaluated (expected = evaluated)
            if expected <> evaluated then failwithf "MISMATCH (Expr-eval) at x=%g" x

        let tempDir = Path.Combine(Path.GetTempPath(), "q015-round2-" + Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(tempDir) |> ignore
        let srcPath = Path.Combine(tempDir, sprintf "Power%dModule.fs" n)
        let dllPath = Path.Combine(tempDir, sprintf "Power%dModule.dll" n)
        File.WriteAllText(srcPath, source)
        let compileArgs = [| "fsc.exe"; srcPath; "-o"; dllPath; "--target:library"; "--nowin32manifest"; "--nologo" |]
        let errors2, _ = checker.Compile(compileArgs) |> Async.RunSynchronously
        printfn "  checker.Compile errors: %A" errors2
        if errors2 |> Array.exists (fun (d: FSharpDiagnostic) -> d.Severity = FSharpDiagnosticSeverity.Error) then
            failwithf "checker.Compile failed: %A" errors2
        let asm = Assembly.LoadFrom(dllPath)
        let moduleType = asm.GetType(sprintf "Power%dModule" n)
        let mi2 = moduleType.GetMethod("powerN")
        for x in [ 2.0; 3.5 ] do
            let expected = reference x
            let actual = mi2.Invoke(null, [| box x |]) :?> float
            printfn "  [compiled-and-executed check] x=%g  FSI-power(%d,x)=%g  compiled-powerN(x)=%g  match=%b" x n expected actual (expected = actual)
            if expected <> actual then failwithf "MISMATCH (compiled-and-executed) at x=%g" x
        sw2.Stop()

        let mentionsPower = text.Contains("power") || text.Contains("Target")
        printfn "  specialized text mentions 'power'/'Target' (should be false): %b" mentionsPower
        if mentionsPower then failwith "specialized text still references the general implementation - not actually unrolled"

        round2Ok <- true
    | other -> failwithf "unexpected reflected-definition shape: %A" other)

printfn "Round 1 wall-clock (FSI session create + define + extract + TryGetReflectedDefinition): %dms" sw1.ElapsedMilliseconds
printfn "Round 2 wall-clock (substitute/unroll/translate/splice/typecheck/compile/execute): %dms" sw2.ElapsedMilliseconds
printfn "Combined (Round 1 + Round 2): %dms" (sw1.ElapsedMilliseconds + sw2.ElapsedMilliseconds)

printfn "=== SUMMARY ==="
printfn "Round 1 (FSI-sourced MethodInfo, confirmed non-host-assembly, TryGetReflectedDefinition works): %b" round1Ok
printfn "Round 2 (full pipeline, n=%d from env var, reference via FSI-obtained mi) succeeded: %b" n round2Ok
printfn "Round 2 specialized text: %s" (defaultArg round2SpecializedText "<none>")
printfn "Q015 spike run complete."
