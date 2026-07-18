namespace ProbeSimpleImplementation

open System
open System.IO
open System.Collections.Generic
open System.Diagnostics
open System.Threading
open System.Reflection
open FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes

// ProbeSimple: SHAPE (a) -- one static parameter, NO instantiation-time I/O.
// Mirrors Q008/Q009's working ClientTP shape. Instruments its own instantiation with
// invocation count + stack-trace capture, exposed BOTH as provided members
// (InvocationCount/CallLog, read back from compiled output) AND as design-time file logs
// (the reliable Q011 MiniTP channel, used as ground truth independent of the baking behavior).
module private Instr =
    // Design-time file logging so the true invocation count/stack is observable regardless of
    // whether the baked provided-member value freezes at type-build time. Directory from env var
    // so the harness can clear it before each scenario.
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

module ProbeCore =
    let ns = "ProbeSimple.Provided"

[<TypeProvider>]
type ProbeProvider (config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(config, assemblyReplacementMap=[("ProbeSimple.DesignTime","ProbeSimple.Runtime")], addDefaultProbingLocation=true)

    let ns = ProbeCore.ns
    let asm = Assembly.GetExecutingAssembly()

    // Per-provider-instance instrumentation state (lifetime = one design-time load).
    let mutable invocationCount = 0
    let log = ResizeArray<string>()
    let compactPath = Path.Combine(Instr.logDir, "ProbeSimple.log")
    let stackPath = Path.Combine(Instr.logDir, "ProbeSimple.stack.txt")

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

    let cache = Dictionary<string * string, ProvidedTypeDefinition>()

    let createTypeUncached (typeName: string) (tag: string) =
        let providedAsm = ProvidedAssembly()
        let t = ProvidedTypeDefinition(providedAsm, ns, typeName, Some typeof<obj>, isErased = false)
        t.AddMember(ProvidedConstructor([], invokeCode = fun _ -> <@@ () @@>))
        // The shape property.
        t.AddMember(ProvidedProperty("Tag", typeof<string>, isStatic = false, getterCode = fun _ -> <@@ tag @@>))
        // Instrumentation members. getterCode is invoked LAZILY (at IL-emit time, after all
        // instantiation firings), so these snapshot the accumulated design-time state at that point.
        t.AddMember(ProvidedProperty("InvocationCount", typeof<int>, isStatic = false,
                        getterCode = fun _ -> let c = invocationCount in <@@ c @@>))
        t.AddMember(ProvidedProperty("CallLog", typeof<string>, isStatic = false,
                        getterCode = fun _ -> let s = String.Join("\n", log) in <@@ s @@>))
        providedAsm.AddTypes [ t ]
        t

    // Memoization is load-bearing (Q011 correction 1): FCS invokes this function more than once per
    // logical instantiation; an unmemoized callback creates a fresh ProvidedAssembly each call and
    // breaks type identity. The invocation COUNT is incremented before the cache check so it reflects
    // every firing, while the built type is produced once.
    let createType (typeName: string) (tag: string) =
        instrument "createType"
        let key = (typeName, tag)
        match cache.TryGetValue key with
        | true, t -> t
        | false, _ ->
            let t = createTypeUncached typeName tag
            cache.[key] <- t
            t

    let probeType =
        let t = ProvidedTypeDefinition(asm, ns, "Probe", Some typeof<obj>, isErased = false)
        t.DefineStaticParameters(
            [ ProvidedStaticParameter("Tag", typeof<string>) ],
            fun typeName args -> createType typeName (args.[0] :?> string))
        t

    // Positive control: an ERASING provided type (isErased=true), same one static parameter and
    // same instrumentation, differing from `Probe` ONLY in erased-vs-generative. Used to test whether
    // ParseAndCheckFileInProject fails for ALL provided types here or specifically for GENERATIVE ones.
    let createErased (typeName: string) (tag: string) =
        instrument "createErased"
        let et = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>, isErased = true)
        et.AddMember(ProvidedConstructor([], invokeCode = fun _ -> <@@ obj() @@>))
        et.AddMember(ProvidedProperty("Tag", typeof<string>, isStatic = false, getterCode = fun _ -> <@@ tag @@>))
        et

    let erasedType =
        let t = ProvidedTypeDefinition(asm, ns, "ProbeErased", Some typeof<obj>, isErased = true)
        t.DefineStaticParameters(
            [ ProvidedStaticParameter("Tag", typeof<string>) ],
            fun typeName args -> createErased typeName (args.[0] :?> string))
        t

    do this.AddNamespace(ns, [ probeType; erasedType ])
