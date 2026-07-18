namespace SchemaTPImplementation

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open SchemaTP.Runtime

// Q012 instrumentation retrofit onto Q011's REAL SchemaProvider (rest of file verbatim from Q011).
module private Q012Instr =
    open System.Diagnostics
    open System.Threading
    let logDir =
        match Environment.GetEnvironmentVariable "PROBE_LOG_DIR" with
        | null | "" -> Path.GetTempPath()
        | d -> d
    let mutable count = 0
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
                    if not (isNull dt) && not (isNull dt.FullName) && dt.FullName.StartsWith("FSharp.Compiler") then
                        Some (sprintf "%s.%s" dt.FullName m.Name) else None)
            |> Option.defaultValue "NONE"
    let log (label: string) =
        count <- count + 1
        let st = StackTrace(true)
        let line = sprintf "%d|%s|%d|%s|%s" count (DateTime.UtcNow.ToString("HH:mm:ss.fff")) Thread.CurrentThread.ManagedThreadId label (firstFcsFrame st)
        try File.AppendAllText(Path.Combine(logDir, "SchemaTP.log"), line + Environment.NewLine) with _ -> ()

// Assembly.Load(bytes) (used below, deliberately, to avoid a Windows file lock -- see
// readClientConsumes) does NOT do LoadFrom's directory-based dependency probing, so a
// resolver is needed to find a loaded-by-bytes assembly's own co-located dependencies
// (e.g. ClientCo.dll -> ClientTP.Runtime.dll sitting next to it on disk). AppDomain-level
// AssemblyResolve (not AssemblyLoadContext, which needs netcoreapp, not netstandard2.0) is
// used since the design-time assembly targets netstandard2.0.
module private AssemblyProbing =
    let private probeDirs = HashSet<string>(StringComparer.OrdinalIgnoreCase)
    let mutable private hooked = false
    let registerProbeDir (path: string) =
        probeDirs.Add(Path.GetDirectoryName(path)) |> ignore
        if not hooked then
            hooked <- true
            AppDomain.CurrentDomain.add_AssemblyResolve(ResolveEventHandler(fun _ args ->
                let name = AssemblyName(args.Name).Name
                probeDirs
                |> Seq.tryPick (fun dir ->
                    let candidate = Path.Combine(dir, name + ".dll")
                    if File.Exists candidate then
                        try Some(Assembly.LoadFrom candidate) with _ -> None
                    else None)
                |> Option.toObj))

// SchemaTP: generative provider.
//   Static params: FieldSpec = "Name:v1;Age:v2;Email:v3", KnownClientPaths = ";"-list of client DLLs.
//   1. builds one provided property per field, each stamped with FieldProvenanceAttribute(name,version)
//      (Q009's mechanism, unchanged).
//   2. loads each known client DLL, reads its ConsumesFieldAttribute(fieldName, consumedVersion) stamps,
//      and REFUSES to generate when a client's recorded dependency is broken by the current schema:
//        - field consumed but no longer present  -> "REMOVED" failure
//        - field present but version disagrees    -> "STALE" failure
[<TypeProvider>]
type SchemaProvider (config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(config, assemblyReplacementMap=[("SchemaTP.DesignTime","SchemaTP.Runtime")], addDefaultProbingLocation=true)

    let ns = "SchemaTP.Provided"
    let asm = Assembly.GetExecutingAssembly()

    let fieldProvenanceCtor =
        typeof<FieldProvenanceAttribute>.GetConstructor([| typeof<string>; typeof<string> |])

    let parseSpec (spec: string) =
        spec.Split([| ';' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun pair ->
            let bits = pair.Split(':')
            bits.[0].Trim(), bits.[1].Trim())
        |> Array.toList

    // Collect (fieldName, consumedVersion) recorded in a client assembly's IL as ConsumesFieldAttribute.
    // Assembly.Load(bytes), NOT Assembly.LoadFrom(path): LoadFrom was observed to hold a Windows file
    // lock on the client DLL for the life of this process (see 02-results.md, Round 2 step 6's
    // "unwedge" rebuild), blocking MSBuild's copy-to-output step when the client is rebuilt while a
    // checking session that has already read it is still alive. Load(bytes) reads-and-closes.
    let readClientConsumes (path: string) =
        AssemblyProbing.registerProbeDir path
        let bytes = File.ReadAllBytes(path)
        let a = Assembly.Load(bytes)
        let flags = BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static ||| BindingFlags.Instance
        [ for t in a.GetTypes() do
            let members : MemberInfo list =
                [ yield! t.GetProperties(flags) |> Seq.cast<MemberInfo>
                  yield! t.GetMethods(flags)    |> Seq.cast<MemberInfo> ]
            for m in members do
                for cad in m.GetCustomAttributesData() do
                    if cad.AttributeType.FullName = "ClientTP.Runtime.ConsumesFieldAttribute" then
                        let fn  = cad.ConstructorArguments.[0].Value :?> string
                        let ver = cad.ConstructorArguments.[1].Value :?> string
                        yield (fn, ver) ]
        |> List.distinct

    // Memoization is load-bearing, not an optimization: FCS invokes the DefineStaticParameters
    // instantiation function MORE THAN ONCE for a single logical instantiation (observed directly,
    // see 02-results.md); each unmemoized call creates a FRESH ProvidedAssembly() with a randomly
    // named temp file, so a second call for identical args produces a TYPE-IDENTITY MISMATCH against
    // the first call's result ("couldn't find type X in assembly tmpYYYYY") purely from FCS's own
    // repeated-invocation behavior, unrelated to the enforcement logic below.
    let successCache = Dictionary<string * string * string, ProvidedTypeDefinition>()

    let createTypeUncached (typeName: string) (fieldSpec: string) (knownClientPaths: string) =
        let providedAsm = ProvidedAssembly()
        let fields = parseSpec fieldSpec
        let t = ProvidedTypeDefinition(providedAsm, ns, typeName, Some typeof<obj>, isErased = false)
        t.AddMember(ProvidedConstructor([], invokeCode = fun _ -> <@@ () @@>))
        for (fn, ver) in fields do
            let prop = ProvidedProperty(fn, typeof<string>, isStatic = false, getterCode = fun _ -> <@@ ver @@>)
            prop.AddCustomAttribute {
                new CustomAttributeData() with
                    member _.Constructor = fieldProvenanceCtor
                    member _.ConstructorArguments =
                        upcast [| CustomAttributeTypedArgument(typeof<string>, box fn)
                                  CustomAttributeTypedArgument(typeof<string>, box ver) |]
                    member _.NamedArguments = upcast [||] }
            t.AddMember prop

        // REVERSED-ARROW ENFORCEMENT: check every known client's recorded dependency against current schema.
        let fieldMap = dict fields
        let clientPaths =
            knownClientPaths.Split([| ';' |], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map (fun s -> s.Trim())
        let failures = ResizeArray<string>()
        for cp in clientPaths do
            for (fn, consumedVer) in readClientConsumes cp do
                match fieldMap.TryGetValue fn with
                | false, _ ->
                    failures.Add(sprintf "REMOVED field: client '%s' consumes field '%s' (recorded at version '%s') but that field no longer exists in the schema" cp fn consumedVer)
                | true, currentVer when currentVer <> consumedVer ->
                    failures.Add(sprintf "STALE dependency: client '%s' recorded field '%s' at version '%s' but the schema now declares '%s'" cp fn consumedVer currentVer)
                | true, _ -> ()
        if failures.Count > 0 then
            failwithf "SchemaTP consumer-driven-contract violation (%d issue(s)): %s" failures.Count (String.Join(" || ", failures))

        providedAsm.AddTypes [ t ]
        t

    let createType (typeName: string) (fieldSpec: string) (knownClientPaths: string) =
        Q012Instr.log "createType"
        let key = (typeName, fieldSpec, knownClientPaths)
        match successCache.TryGetValue key with
        | true, t -> t
        | false, _ ->
            // NOT try/cached on the exception path: a repeat call after a failure recomputes and
            // re-raises (deterministic), matching the "reported twice" duplicate-diagnostic behavior
            // Q008/Q009 both documented for their own mismatch cases.
            let t = createTypeUncached typeName fieldSpec knownClientPaths
            successCache.[key] <- t
            t

    let schemaType =
        let t = ProvidedTypeDefinition(asm, ns, "Schema", Some typeof<obj>, isErased = false)
        t.DefineStaticParameters(
            [ ProvidedStaticParameter("FieldSpec", typeof<string>)
              ProvidedStaticParameter("KnownClientPaths", typeof<string>, parameterDefaultValue = "") ],
            fun typeName args -> createType typeName (args.[0] :?> string) (args.[1] :?> string))
        t

    do this.AddNamespace(ns, [ schemaType ])
