namespace FsiParamTPImplementation

open ProviderImplementation.ProvidedTypes
open FSharp.Core.CompilerServices
open System
open System.IO
open System.Reflection
open FSharp.Compiler.Interactive.Shell

// FsiParamTP: a generative provider whose static parameter is a plain string, but whose
// DefineStaticParameters instantiation function hosts a real FsiEvaluationSession internally
// (created and evaluated INSIDE this function, i.e. on the call stack of whatever outer
// compiler-service host -- fsc.exe/dotnet build, or FSharpChecker.ParseAndCheckFileInProject --
// is in the middle of resolving/instantiating this provided type) and evaluates the string as
// real F# source. The evaluated value's runtime shape then drives what gets generated:
//   - an `int`            -> Round 1 shape: N generated int properties P0..P(N-1)
//   - a `string list`     -> Round 2 shape: one string property per list element, named after
//                            the element's own text, returning that text
[<TypeProvider>]
type FsiParamProvider (config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(
        config,
        assemblyReplacementMap = [ ("FsiParamTP.DesignTime", "FsiParamTP.Runtime") ],
        addDefaultProbingLocation = true)

    let ns = "FsiParamTP.Provided"
    let asm = Assembly.GetExecutingAssembly()

    /// Create a fresh, headless FsiEvaluationSession (stdin/stdout/stderr redirected to a
    /// StringWriter, collectible = false, matching Q003's coexistence-tested configuration),
    /// evaluate `exprText` as a real F# expression, and return its ReflectionValue/ReflectionType.
    /// Raises with the FSI diagnostics text on failure -- this function's own success/failure is
    /// exactly Q007's cheapest falsifier: it runs synchronously inside DefineStaticParameters'
    /// callback, which the OUTER compiler-service host is itself in the middle of servicing.
    let evalFsiExpression (exprText: string) : obj * Type =
        let redirected = new StringWriter()
        let inStream = new StringReader("")
        let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()
        let argv = [| "fsi.exe"; "--noninteractive"; "--nologo"; "--gui-" |]
        // `false` here is FsiEvaluationSession.Create's optional `collectible` parameter --
        // F# lets an optional parameter be passed as a bare value (auto-wrapped), not `Some false`;
        // confirmed against the actual API by a standalone probe before writing this file.
        use session = FsiEvaluationSession.Create(fsiConfig, argv, inStream, redirected, redirected, false)
        let result, diags = session.EvalExpressionNonThrowing(exprText)
        match result with
        | Choice1Of2 (Some v) -> v.ReflectionValue, v.ReflectionType
        | Choice1Of2 None ->
            let diagText = String.Join("; ", diags |> Array.map (fun d -> d.Message))
            failwithf "FsiParamTP: FSI evaluation of '%s' produced no value. Diagnostics: %s" exprText diagText
        | Choice2Of2 ex ->
            let diagText = String.Join("; ", diags |> Array.map (fun d -> d.Message))
            failwithf "FsiParamTP: FSI evaluation of '%s' threw %s. Diagnostics: %s" exprText (ex.ToString()) diagText

    let createType typeName (exprText: string) =
        let provAsm = ProvidedAssembly()
        let myType = ProvidedTypeDefinition(provAsm, ns, typeName, Some typeof<obj>, isErased = false)

        let ctor = ProvidedConstructor([], invokeCode = fun _args -> <@@ () @@>)
        myType.AddMember ctor

        let value, valueType = evalFsiExpression exprText

        // Always expose what FSI actually evaluated to (independent verifiers read this back too),
        // so a run can confirm generation was driven by the real evaluated value, not a stand-in.
        let evaluatedTypeNameProp =
            ProvidedProperty(
                "EvaluatedTypeName", typeof<string>, isStatic = true,
                getterCode = fun _args -> <@@ valueType.FullName @@>)
        myType.AddMember evaluatedTypeNameProp

        if valueType = typeof<int> then
            // Round 1 -- cheapest falsifier: Expr="1+2" evaluates to 3, drives 3 properties.
            let n = value :?> int
            for i in 0 .. n - 1 do
                let prop =
                    ProvidedProperty(
                        sprintf "P%d" i, typeof<int>, isStatic = true,
                        getterCode = fun _args -> <@@ i @@>)
                myType.AddMember prop
        elif valueType = typeof<string list> then
            // Round 2 -- richer value drives generated-member SHAPE: one property per list
            // element, named after the element's own text.
            let items = value :?> string list
            for item in items do
                let prop =
                    ProvidedProperty(
                        item, typeof<string>, isStatic = true,
                        getterCode = fun _args -> <@@ item @@>)
                myType.AddMember prop
        else
            failwithf "FsiParamTP: unsupported FSI-evaluated type '%s' for Expr='%s' (only int and string list are handled by this spike)"
                valueType.FullName exprText

        provAsm.AddTypes [ myType ]
        myType

    let containerType =
        let t = ProvidedTypeDefinition(asm, ns, "Container", Some typeof<obj>, isErased = false)
        t.DefineStaticParameters(
            [ ProvidedStaticParameter("Expr", typeof<string>) ],
            fun typeName args -> createType typeName (args.[0] :?> string))
        t

    do this.AddNamespace(ns, [ containerType ])
