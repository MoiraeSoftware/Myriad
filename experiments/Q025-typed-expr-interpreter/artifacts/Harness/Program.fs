module Harness.Program

// Q025 — typed-expression interpreter. Hand-walks a real, checker-resolved FSharpExpr tree and
// interprets it directly, reflection-invoking already-compiled reference-assembly members for the
// leaves. No FsiEvaluationSession, no Reflection.Emit, anywhere in this file.

open System
open System.IO
open System.Reflection
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open FSharp.Compiler.Symbols
open FSharp.Compiler.Diagnostics
open Microsoft.FSharp.Reflection

// ---------------------------------------------------------------------------
// Locate this quartet's own artifacts folder (the first "artifacts" ancestor
// walking up from the running exe), so this harness is relocatable and does
// not hardcode an absolute repo path.
// ---------------------------------------------------------------------------

let artifactsDir =
    let baseDir = AppContext.BaseDirectory
    let mutable d = DirectoryInfo(baseDir)
    while d <> null && d.Name <> "artifacts" do
        d <- d.Parent
    if d = null then failwith "Could not locate 'artifacts' ancestor directory from AppContext.BaseDirectory"
    d.FullName

// Loaded from THIS project's own output directory (where the ProjectReference to RefLib.fsproj
// copies RefLib.dll), not from artifacts/RefLib/bin/... directly -- so the Assembly.LoadFrom
// below (used to resolve types for the interpreter) and the compile-time reference used by
// round1's "directly constructed" comparison value end up pointing at the identical physical
// assembly, giving them the same runtime Type identity. Two different copies of an
// identically-named assembly would otherwise fail Object.Equals with a type-identity mismatch
// that has nothing to do with the interpreter itself.
let refLibDll =
    Path.Combine(AppContext.BaseDirectory, "RefLib.dll")

let refPackDir =
    let root = @"C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref"
    Directory.GetDirectories(root)
    |> Array.filter (fun d -> Path.GetFileName(d).StartsWith "9.")
    |> Array.sort
    |> Array.last
    |> fun v -> Path.Combine(v, "ref", "net9.0")

let fsharpCore = typeof<int list>.Assembly.Location

printfn "artifactsDir = %s" artifactsDir
printfn "refLibDll    = %s (exists=%b)" refLibDll (File.Exists refLibDll)
printfn "refPackDir   = %s" refPackDir
printfn "fsharpCore   = %s" fsharpCore
printfn ""

if not (File.Exists refLibDll) then
    failwith "RefLib.dll not found next to the Harness output — expected the ProjectReference build step to place it there"

// ---------------------------------------------------------------------------
// Checker + non-script FSharpProjectOptions, same pattern as Q008/Q010/Q019.
// ---------------------------------------------------------------------------

let checker = FSharpChecker.Create(keepAssemblyContents = true)

let mkOptions (sampleFile: string) =
    let sysRefs =
        Directory.GetFiles(refPackDir, "*.dll")
        |> Array.map (fun r -> "-r:" + r)
        |> Array.toList
    let otherOptions =
        [ "--targetprofile:netcore"
          "--noframework"
          "-r:" + fsharpCore
          "-r:" + refLibDll ]
        @ sysRefs
    { ProjectFileName = Path.Combine(Path.GetDirectoryName sampleFile, "Harness.fsproj")
      ProjectId = None
      SourceFiles = [| sampleFile |]
      OtherOptions = Array.ofList otherOptions
      ReferencedProjects = [||]
      IsIncompleteTypeCheckEnvironment = false
      UseScriptResolutionRules = false
      LoadTime = DateTime.Now
      UnresolvedReferences = None
      OriginalLoadReferences = []
      Stamp = None }

let mutable version = 0

/// Parse+check a single real on-disk file. Returns diagnostics and, on success, the
/// FSharpCheckFileResults (from which ImplementationFile is read).
let checkFile (path: string) =
    version <- version + 1
    let text = File.ReadAllText path
    let source = SourceText.ofString text
    let opts = mkOptions path
    let _parseResults, answer =
        checker.ParseAndCheckFileInProject(path, version, source, opts)
        |> Async.RunSynchronously
    match answer with
    | FSharpCheckFileAnswer.Succeeded res -> res.Diagnostics, Some res
    | FSharpCheckFileAnswer.Aborted -> [||], None

let printDiags (diags: FSharpDiagnostic[]) =
    printfn "  %d diagnostic(s)" diags.Length
    for d in diags do
        printfn "    %s (%d,%d): %s" (string d.Severity) d.StartLine d.StartColumn d.Message

let hasErrors (diags: FSharpDiagnostic[]) =
    diags |> Array.exists (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)

// ---------------------------------------------------------------------------
// Reference assembly, loaded once, used to resolve FSharpEntity/FSharpMemberOrFunctionOrValue
// back to a real reflectable System.Type / MethodInfo.
// ---------------------------------------------------------------------------

let refLibAsm = Assembly.LoadFrom refLibDll

/// FSharpEntity -> a loadable runtime System.Type in the already-compiled reference assembly.
/// Path used: entity.FullName (e.g. "RefLib.Widget", "RefLib.Ops") looked up directly via
/// Assembly.GetType on the reference DLL loaded by path above.
let resolveRuntimeType (entity: FSharpEntity) : Type =
    let fullName = entity.FullName
    match refLibAsm.GetType(fullName) with
    | null -> failwithf "resolveRuntimeType: could not find runtime type '%s' in %s" fullName refLibAsm.FullName
    | t -> t

/// FSharpMemberOrFunctionOrValue -> a loadable runtime MethodInfo. Path used:
/// mfv.DeclaringEntity (the module, e.g. RefLib.Ops) resolved to its compiled static class via
/// resolveRuntimeType above, then GetMethod(mfv.CompiledName) — F# modules compile to a static
/// class whose let-bound functions are public static methods.
let resolveMethod (mfv: FSharpMemberOrFunctionOrValue) : MethodInfo =
    match mfv.DeclaringEntity with
    | None -> failwithf "resolveMethod: '%s' has no DeclaringEntity" mfv.CompiledName
    | Some declEntity ->
        let declType = resolveRuntimeType declEntity
        match declType.GetMethod(mfv.CompiledName, BindingFlags.Public ||| BindingFlags.Static ||| BindingFlags.NonPublic) with
        | null -> failwithf "resolveMethod: could not find method '%s' on %s" mfv.CompiledName declType.FullName
        | mi -> mi

// ---------------------------------------------------------------------------
// The interpreter. Only the patterns the two rounds below actually need are implemented;
// everything else falls through to a named NotImplementedException naming the unmatched
// expression's .Type, per 01-design.md's own instruction not to silently narrow scope.
// ---------------------------------------------------------------------------

let rec interpret (env: Map<string, obj>) (expr: FSharpExpr) : obj =
    match expr with
    | FSharpExprPatterns.Const(value, _ty) ->
        value

    | FSharpExprPatterns.NewRecord(recordType, argExprs) ->
        let runtimeType = resolveRuntimeType recordType.TypeDefinition
        let values = argExprs |> List.map (interpret env) |> Array.ofList
        FSharpValue.MakeRecord(runtimeType, values)

    | FSharpExprPatterns.Call(objExprOpt, memberOrFunc, _typeArgs, _methodTypeArgs, argExprs) ->
        let mi = resolveMethod memberOrFunc
        let args = argExprs |> List.map (interpret env) |> Array.ofList
        let receiver =
            match objExprOpt with
            | Some oe -> interpret env oe
            | None -> null
        mi.Invoke(receiver, args)

    | FSharpExprPatterns.Let((bindingVal, bindingExpr, _debugPoint), bodyExpr) ->
        let v = interpret env bindingExpr
        let env' = env.Add(bindingVal.LogicalName, v)
        interpret env' bodyExpr

    | FSharpExprPatterns.Value(v) ->
        match env.TryFind v.LogicalName with
        | Some value -> value
        | None -> failwithf "interpret: unbound value '%s' in interpreter env" v.LogicalName

    | FSharpExprPatterns.IfThenElse(condExpr, thenExpr, elseExpr) ->
        let cond = interpret env condExpr :?> bool
        if cond then interpret env thenExpr else interpret env elseExpr

    | other ->
        raise (NotImplementedException(
            sprintf "interpret: unhandled FSharpExpr shape, expr.Type = %s, expr = %A" (string other.Type) other))

// ---------------------------------------------------------------------------
// Walk ImplementationFile.Declarations to find a named top-level let binding's bound FSharpExpr —
// never a hand-built FSharpExpr or a quotation, per the hypothesis's validity preconditions.
// ---------------------------------------------------------------------------

let rec findBinding (name: string) (decls: FSharpImplementationFileDeclaration list) : FSharpExpr option =
    decls
    |> List.tryPick (fun d ->
        match d with
        | FSharpImplementationFileDeclaration.Entity(_e, subDecls) ->
            findBinding name subDecls
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(mfv, _curriedArgs, body) ->
            if mfv.LogicalName = name then Some body else None
        | FSharpImplementationFileDeclaration.InitAction _ ->
            None)

// ---------------------------------------------------------------------------
// Round 1 — cheapest falsifier: Sample.fs's `w` binding is a bare NewRecord over RefLib.Widget.
// ---------------------------------------------------------------------------

let round1 () =
    printfn "===== ROUND 1: NewRecord cheapest falsifier ====="
    let sampleFile = Path.Combine(artifactsDir, "Harness", "Sample.fs")
    printfn "checking %s" sampleFile
    let diags, resOpt = checkFile sampleFile
    printDiags diags
    if hasErrors diags then
        printfn "ROUND 1: KILL — Sample.fs itself failed to typecheck, see diagnostics above."
        false
    else
        match resOpt with
        | None ->
            printfn "ROUND 1: KILL — FSharpCheckFileAnswer.Aborted, no results at all."
            false
        | Some res ->
            match res.ImplementationFile with
            | None ->
                printfn "ROUND 1: KILL — ImplementationFile is None even with keepAssemblyContents=true."
                false
            | Some implFile ->
                printfn "ImplementationFile populated. Declarations count (top level) = %d" implFile.Declarations.Length
                match findBinding "w" implFile.Declarations with
                | None ->
                    printfn "ROUND 1: KILL — could not find a top-level binding named 'w' in Declarations."
                    false
                | Some wExpr ->
                    printfn "Found binding 'w'. Expr shape: %s" (
                        match wExpr with
                        | FSharpExprPatterns.NewRecord _ -> "NewRecord (as expected)"
                        | _ -> sprintf "UNEXPECTED SHAPE: %A" wExpr)
                    try
                        let interpreted = interpret Map.empty wExpr
                        let direct : RefLib.Widget = { RefLib.Name = "a"; RefLib.Count = 1 }
                        printfn "interpreted value = %A" interpreted
                        printfn "direct value      = %A" direct
                        let eq = Object.Equals(interpreted, box direct)
                        printfn "Object.Equals(interpreted, direct) = %b" eq
                        if eq then
                            printfn "ROUND 1: PASS"
                            true
                        else
                            printfn "ROUND 1: KILL — interpreted value does not equal directly-constructed value."
                            false
                    with ex ->
                        printfn "ROUND 1: KILL — interpreter threw: %s" (ex.ToString())
                        false

// ---------------------------------------------------------------------------
// Round 2 — richer shape: Let/Call/IfThenElse chain, plus a stand-in codegen function whose
// output shape (member-name list) differs depending on the interpreted result.
// ---------------------------------------------------------------------------

/// Stand-in for "a generator choosing which members to emit based on generation-time computation."
/// Not a real Myriad generator or type provider — a plain function meeting the same "list/count
/// drives shape" bar Q007/Q014 used, per 01-design.md's own note that this is sufficient evidence
/// for this quartet's narrower claim.
let mkMembers (flag: string) : string list =
    match flag with
    | "big" -> [ "Detail"; "Summary" ]
    | _ -> [ "Summary" ]

let round2 () =
    printfn "\n===== ROUND 2: Let/Call/IfThenElse chain, result drives generated-shape stand-in ====="
    let sampleFile = Path.Combine(artifactsDir, "Harness", "Sample2.fs")
    printfn "checking %s" sampleFile
    let diags, resOpt = checkFile sampleFile
    printDiags diags
    if hasErrors diags then
        printfn "ROUND 2: FAIL — Sample2.fs itself failed to typecheck, see diagnostics above."
        false
    else
        match resOpt with
        | None ->
            printfn "ROUND 2: FAIL — FSharpCheckFileAnswer.Aborted."
            false
        | Some res ->
            match res.ImplementationFile with
            | None ->
                printfn "ROUND 2: FAIL — ImplementationFile is None."
                false
            | Some implFile ->
                match findBinding "result" implFile.Declarations with
                | None ->
                    printfn "ROUND 2: FAIL — could not find a top-level binding named 'result'."
                    false
                | Some resultExpr ->
                    printfn "Found binding 'result'. Raw expr:\n%A\n" resultExpr
                    try
                        let interpreted = interpret Map.empty resultExpr
                        printfn "interpreted 'result' = %A" interpreted
                        let asString = interpreted :?> string
                        let expected = "big" // bump 7 = 17 > 15 = true -> "big"
                        let correct = asString = expected
                        printfn "expected = %s, actual = %s, match = %b" expected asString correct

                        let members = mkMembers asString
                        printfn "mkMembers(%A) = %A  (interpreted result drives this shape)" asString members

                        // Contrast: what the *other* branch would have produced, to make the
                        // "drives a real difference in generated shape" claim concrete rather than
                        // asserted.
                        let otherMembers = mkMembers (if asString = "big" then "small" else "big")
                        printfn "contrast — mkMembers(other branch) = %A" otherMembers
                        let shapesDiffer = members <> otherMembers
                        printfn "shapes differ between branches = %b" shapesDiffer

                        if correct && shapesDiffer then
                            printfn "ROUND 2: PASS"
                            true
                        else
                            printfn "ROUND 2: FAIL"
                            false
                    with ex ->
                        printfn "ROUND 2: FAIL — interpreter threw: %s" (ex.ToString())
                        false

// ---------------------------------------------------------------------------

[<EntryPoint>]
let main _argv =
    printfn "No FsiEvaluationSession is referenced or constructed anywhere in this file (grep-checkable)."
    printfn ""
    let r1 = round1 ()
    if not r1 then
        printfn "\nSTOPPING per 01-design.md: Round 1 (cheapest falsifier) did not pass — KILL verdict, Round 2 not attempted."
        1
    else
        let r2 = round2 ()
        printfn "\nSummary: Round1=%b Round2=%b" r1 r2
        if r1 && r2 then 0 else 1
