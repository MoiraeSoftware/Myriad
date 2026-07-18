// Q014 spike: can FSI-adjacent quotation computation ("stage" real code, not just data) be
// reified into real F# source text that splices into an FSharpChecker virtual project and
// typechecks? See ../../00-hypothesis.md and ../../01-design.md for the pre-registration.
//
// Round 1: cheapest falsifier - does ANY quotation survive round-trip to reparseable text?
// Round 2: capability claim - power-function specialization (n=4), unrolled via a partial
//          evaluator over the quotation obtained from [<ReflectedDefinition>], no manual
//          Expr.Call construction.
// Round 3: single-sample cost of the Round 2 pipeline.

open System
open System.Diagnostics
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open FSharp.Compiler.Diagnostics
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

let checker = FSharpChecker.Create(keepAssemblyContents = true, useTransparentCompiler = true)

/// Typecheck a standalone F# source snippet via GetProjectOptionsFromScript, the same basic
/// splice-and-check pattern Q003's own harness used. Returns (errorDiagnostics, allDiagnostics).
let typecheckSnippet (label: string) (source: string) : FSharpDiagnostic[] * FSharpDiagnostic[] =
    async {
        let fileName = sprintf "%s.fsx" label
        let src = SourceText.ofString source
        let! opts, _optsDiags = checker.GetProjectOptionsFromScript(fileName, src)
        let! _parseRes, checkAnswer = checker.ParseAndCheckFileInProject(fileName, 0, src, opts)
        match checkAnswer with
        | FSharpCheckFileAnswer.Succeeded r ->
            let errors = r.Diagnostics |> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)
            return errors, r.Diagnostics
        | FSharpCheckFileAnswer.Aborted -> return failwith "check aborted"
    }
    |> Async.RunSynchronously

// ---------------------------------------------------------------------------------------------
// Round 1: cheapest falsifier
// ---------------------------------------------------------------------------------------------

let mutable round1TranslatorWorks = false
let mutable round1UnquoteText : string option = None

step "Round 1a: Unquote decompile of <@ 1 + 2 @>" (fun () ->
    let trivial = <@ 1 + 2 @>
    let text = decompile trivial
    printfn "  decompile output: %s" text
    round1UnquoteText <- Some text
    let source = sprintf "let trivialResult = %s\n" text
    printfn "  spliced source:\n%s" source
    let errors, allDiags = typecheckSnippet "round1-unquote" source
    printfn "  all diagnostics: %A" allDiags
    if errors.Length = 0 then
        printfn "  REPARSED AND TYPECHECKED CLEANLY"
        round1TranslatorWorks <- true
    else
        printfn "  FAILED to typecheck cleanly: %A" errors)

// ---------------------------------------------------------------------------------------------
// Hand-rolled narrow translator, tried regardless of Round 1a's outcome, since Round 2 needs a
// translator this code fully controls and understands the exact output shape of.
// ---------------------------------------------------------------------------------------------

/// Grab a MethodInfo by quoting a trivial use of the operator, instead of manual reflection
/// (Falanx-pain-avoidance technique named in 01-design.md's recon section).
let miOf (e: Expr) =
    match e with
    | Call(_, mi, _) -> mi
    | _ -> failwithf "miOf: expected a Call shape, got %A" e

let addMi = miOf <@ 1 + 1 @>
let subMi = miOf <@ 1 - 1 @>
let mulMiFloat = miOf <@ 1.0 * 1.0 @>
let eqMiInt = miOf <@ 1 = 1 @>

/// Narrow Expr -> F# source text translator. Deliberately covers only the node shapes this
/// spike's two scenarios produce (Value of int/float/bool, Var, Lambda, +/-/*/= over int/float).
/// Anything outside this whitelist raises, rather than silently mistranslating.
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

let mutable round1HandRolledWorks = false

step "Round 1b: hand-rolled translator of <@ 1 + 2 @>" (fun () ->
    let trivial = <@ 1 + 2 @>
    let text = translate trivial
    printfn "  translated text: %s" text
    let source = sprintf "let trivialResult2 = %s\n" text
    let errors, allDiags = typecheckSnippet "round1-handrolled" source
    printfn "  all diagnostics: %A" allDiags
    if errors.Length = 0 then
        printfn "  REPARSED AND TYPECHECKED CLEANLY"
        round1HandRolledWorks <- true
    else
        printfn "  FAILED to typecheck cleanly: %A" errors)

// ---------------------------------------------------------------------------------------------
// Round 2: capability claim - power-function specialization via partial evaluation.
// ---------------------------------------------------------------------------------------------

module Target =
    [<ReflectedDefinition>]
    let rec power (n: int) (x: float) : float =
        if n = 0 then 1.0 else x * power (n - 1) x

let powerMi = miOf <@ Target.power 0 0.0 @>

/// Generic ExprShape-based substitution: replace every occurrence of `var` with `replacement`.
let rec substVar (var: Var) (replacement: Expr) (expr: Expr) : Expr =
    match expr with
    | ShapeVar v -> if v = var then replacement else expr
    | ShapeLambda(v, body) -> Expr.Lambda(v, substVar var replacement body)
    | ShapeCombination(op, args) -> RebuildShapeCombination(op, List.map (substVar var replacement) args)

/// Evaluate a fully-closed int expression built only from Value/subtraction nodes - exactly the
/// shapes `power`'s own recursive-call argument (`n - 1`) can produce once `n` is known.
let rec evalIntExpr (e: Expr) : int =
    match e with
    | Value((:? int as v), t) when t = typeof<int> -> v
    | Call(None, mi, [a; b]) when mi = subMi -> evalIntExpr a - evalIntExpr b
    | _ -> failwithf "evalIntExpr: unsupported shape %A" e

/// Narrow partial evaluator/unroller for exactly `power`'s known body shape:
///   IfThenElse(n = 0, 1.0, x * power (n - 1) x)
/// Fully inlines the recursion because `n` is always statically known by the time this is called.
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
            let substituted =
                innerBody
                |> substVar nVar (Expr.Value(nVal))
                |> substVar xVar xArgExpr
            unroll substituted
        | other -> failwithf "unroll: unexpected reflected-definition shape: %A" other
    | Value((:? float as _v), t) when t = typeof<float> -> e
    | _ -> failwithf "unroll: unsupported node shape: %A" e

let mutable round2Ok = false
let mutable round2SpecializedText : string option = None
let sw2 = Stopwatch()

step "Round 2: power(4, x) specialized via partial evaluation over the quotation" (fun () ->
    sw2.Start()
    match Expr.TryGetReflectedDefinition(powerMi) with
    | None -> failwith "no reflected definition found for Target.power - check [<ReflectedDefinition>] wiring"
    | Some(Lambda(nVar, Lambda(xVar, innerBody))) ->
        printfn "  raw reflected body: %A" innerBody
        let body4 = substVar nVar (Expr.Value(4)) innerBody
        let unrolled = unroll body4
        printfn "  unrolled Expr (in terms of %s only): %A" xVar.Name unrolled
        let specializedLambda = Expr.Lambda(xVar, unrolled)

        // Pick the translator: prefer Unquote's decompile of the whole lambda if Round 1a worked
        // and can handle a Lambda node; fall back to the hand-rolled translator otherwise.
        let text =
            if round1TranslatorWorks then
                try Some(decompile specializedLambda) with _ -> None
            else None
            |> Option.defaultWith (fun () -> sprintf "fun (%s: float) -> %s" xVar.Name (translate unrolled))
        round2SpecializedText <- Some text
        printfn "  specialized source text: %s" text

        let source = sprintf "module Power4Module\nlet power4 : float -> float = %s\n" text
        let errors, allDiags = typecheckSnippet "round2-power4" source
        printfn "  all diagnostics: %A" allDiags
        if errors.Length > 0 then
            failwithf "typecheck failed: %A" errors

        let direct = fun (x: float) -> Target.power 4 x

        // Weaker check: evaluate the unrolled Expr directly via Unquote's `eval`, independent of
        // the text-translation step, so this half doesn't depend on translate/decompile being
        // right too.
        for x in [ 2.0; 3.5 ] do
            let expected = direct x
            let evaluated = Expr.Cast<float>(substVar xVar (Expr.Value x) unrolled).Eval()
            printfn "  [Expr-eval check] x=%g  Target.power 4 x=%g  evaluated-unrolled-Expr=%g  match=%b" x expected evaluated (expected = evaluated)
            if expected <> evaluated then failwithf "MISMATCH (Expr-eval) at x=%g" x

        // Stronger check: actually compile the SPLICED TEXT (not the Expr) to a real assembly via
        // checker.Compile, load it, and invoke the real compiled power4 - closes the gap that the
        // Expr-eval check above doesn't touch the translation step's runtime fidelity at all.
        let tempDir = IO.Path.Combine(IO.Path.GetTempPath(), "q014-round2-" + Guid.NewGuid().ToString("N"))
        IO.Directory.CreateDirectory(tempDir) |> ignore
        let srcPath = IO.Path.Combine(tempDir, "Power4Module.fs")
        let dllPath = IO.Path.Combine(tempDir, "Power4Module.dll")
        IO.File.WriteAllText(srcPath, source)
        let compileArgs =
            [| "fsc.exe"; srcPath; "-o"; dllPath; "--target:library"
               "--nowin32manifest"; "--nologo" |]
        let errors2, _ = checker.Compile(compileArgs) |> Async.RunSynchronously
        printfn "  checker.Compile errors: %A" errors2
        if errors2 |> Array.exists (fun (d: FSharpDiagnostic) -> d.Severity = FSharpDiagnosticSeverity.Error) then
            failwithf "checker.Compile failed: %A" errors2
        let asm = Reflection.Assembly.LoadFrom(dllPath)
        let moduleType = asm.GetType("Power4Module")
        let mi = moduleType.GetMethod("power4")
        for x in [ 2.0; 3.5 ] do
            let expected = direct x
            let actual = mi.Invoke(null, [| box x |]) :?> float
            printfn "  [compiled-and-executed check] x=%g  Target.power 4 x=%g  compiled-power4(x)=%g  match=%b" x expected actual (expected = actual)
            if expected <> actual then failwithf "MISMATCH (compiled-and-executed) at x=%g" x
        sw2.Stop()

        // Structural-difference check: the specialized text must not reference `power`, `n`, or
        // contain any recursive call - grep the emitted text itself.
        let mentionsPower = text.Contains("power") || text.Contains("Target")
        printfn "  specialized text mentions 'power'/'Target' (should be false): %b" mentionsPower
        if mentionsPower then failwith "specialized text still references the general implementation - not actually unrolled"

        round2Ok <- true
    | other -> failwithf "unexpected reflected-definition shape: %A" other)

printfn "Round 2 wall-clock: %dms" sw2.ElapsedMilliseconds

// ---------------------------------------------------------------------------------------------
// Summary
// ---------------------------------------------------------------------------------------------

printfn "=== SUMMARY ==="
printfn "Round 1a (Unquote decompile, trivial case) reparsed+typechecked: %b" round1TranslatorWorks
printfn "Round 1b (hand-rolled translator, trivial case) reparsed+typechecked: %b" round1HandRolledWorks
printfn "Round 2 (power-4 specialization, full pipeline) succeeded: %b" round2Ok
printfn "Round 2 specialized text: %s" (defaultArg round2SpecializedText "<none>")
printfn "Q014 spike run complete."
