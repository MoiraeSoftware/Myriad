namespace ProbeTwoParamsImplementation

open System
open System.IO
open System.Collections.Generic
open System.Diagnostics
open System.Threading
open System.Reflection
open FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes

// ProbeTwoParams: SHAPE (b) -- TWO static parameters (second unused), NO instantiation-time I/O.
// Isolates static-parameter count alone against ProbeSimple's one-parameter shape.
module private Instr =
    let logDir =
        match Environment.GetEnvironmentVariable "PROBE_LOG_DIR" with
        | null | "" -> Path.GetTempPath()
        | d -> d

    let firstFcsFrame (st: StackTrace) =
        match st.GetFrames() with
        | null -> "NONE"
        | frames ->
            frames
            |> Array.tryPick (fun f ->
                match f.GetMethod() with
                | null -> None
                | m ->
                    let dt = m.DeclaringType
                    if not (isNull dt) && not (isNull dt.FullName)
                       && dt.FullName.StartsWith("FSharp.Compiler") then
                        Some (sprintf "%s.%s" dt.FullName m.Name)
                    else None)
            |> Option.defaultValue "NONE"

[<TypeProvider>]
type ProbeProvider (config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(config, assemblyReplacementMap=[("ProbeTwoParams.DesignTime","ProbeTwoParams.Runtime")], addDefaultProbingLocation=true)

    let ns = "ProbeTwoParams.Provided"
    let asm = Assembly.GetExecutingAssembly()

    let mutable invocationCount = 0
    let log = ResizeArray<string>()
    let compactPath = Path.Combine(Instr.logDir, "ProbeTwoParams.log")
    let stackPath = Path.Combine(Instr.logDir, "ProbeTwoParams.stack.txt")

    let instrument (label: string) =
        invocationCount <- invocationCount + 1
        let n = invocationCount
        let st = StackTrace(true)
        let frame = Instr.firstFcsFrame st
        let time = DateTime.UtcNow.ToString("HH:mm:ss.fff")
        let tid = Thread.CurrentThread.ManagedThreadId
        let line = sprintf "%d|%s|%d|%s|%s" n time tid label frame
        log.Add line
        try File.AppendAllText(compactPath, line + Environment.NewLine) with _ -> ()
        try
            File.AppendAllText(stackPath,
                sprintf "===== invocation %d (%s) label=%s firstFcsFrame=%s =====%s%s%s%s"
                    n time label frame Environment.NewLine (st.ToString()) Environment.NewLine Environment.NewLine)
        with _ -> ()

    let cache = Dictionary<string * string * string, ProvidedTypeDefinition>()

    let createTypeUncached (typeName: string) (fieldSpec: string) (_extra: string) =
        let providedAsm = ProvidedAssembly()
        let t = ProvidedTypeDefinition(providedAsm, ns, typeName, Some typeof<obj>, isErased = false)
        t.AddMember(ProvidedConstructor([], invokeCode = fun _ -> <@@ () @@>))
        t.AddMember(ProvidedProperty("Tag", typeof<string>, isStatic = false, getterCode = fun _ -> <@@ fieldSpec @@>))
        t.AddMember(ProvidedProperty("InvocationCount", typeof<int>, isStatic = false,
                        getterCode = fun _ -> let c = invocationCount in <@@ c @@>))
        t.AddMember(ProvidedProperty("CallLog", typeof<string>, isStatic = false,
                        getterCode = fun _ -> let s = String.Join("\n", log) in <@@ s @@>))
        providedAsm.AddTypes [ t ]
        t

    let createType (typeName: string) (fieldSpec: string) (extra: string) =
        instrument "createType"
        let key = (typeName, fieldSpec, extra)
        match cache.TryGetValue key with
        | true, t -> t
        | false, _ ->
            let t = createTypeUncached typeName fieldSpec extra
            cache.[key] <- t
            t

    let probeType =
        let t = ProvidedTypeDefinition(asm, ns, "Probe", Some typeof<obj>, isErased = false)
        t.DefineStaticParameters(
            [ ProvidedStaticParameter("FieldSpec", typeof<string>)
              ProvidedStaticParameter("Extra", typeof<string>) ],
            fun typeName args -> createType typeName (args.[0] :?> string) (args.[1] :?> string))
        t

    do this.AddNamespace(ns, [ probeType ])
