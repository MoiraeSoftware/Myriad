namespace ClientTPImplementation

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open ClientTP.Runtime

// Q012 instrumentation retrofit onto Q011's REAL ClientProvider (rest of file verbatim from Q011).
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
        try File.AppendAllText(Path.Combine(logDir, "ClientTP.log"), line + Environment.NewLine) with _ -> ()

// Assembly.Load(bytes) (used below, deliberately, to avoid a Windows file lock -- see
// SchemaProvider.fs's identical note) does not do LoadFrom's directory-based dependency probing.
// AppDomain-level AssemblyResolve (not AssemblyLoadContext, netcoreapp-only) since this design-time
// assembly targets netstandard2.0.
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

// ClientTP: generative provider (the REVERSED arrow's producer of provenance).
//   Static params: SchemaAssemblyPath, FieldNames = ";"-list of field names (NO client-declared versions).
//   For each requested field it reads that field's CURRENT FieldProvenanceAttribute version from the
//   schema assembly, generates a real provided property giving access to it, and stamps that member with
//   ConsumesFieldAttribute(fieldName, currentVersionReadFromSchema). The recorded version is whatever the
//   schema said at generation time -- so "recompile the client" alone re-reads the new version.
[<TypeProvider>]
type ClientProvider (config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(config, assemblyReplacementMap=[("ClientTP.DesignTime","ClientTP.Runtime")], addDefaultProbingLocation=true)

    let ns = "ClientTP.Provided"
    let asm = Assembly.GetExecutingAssembly()

    let consumesCtor =
        typeof<ConsumesFieldAttribute>.GetConstructor([| typeof<string>; typeof<string> |])

    // Read fieldName -> currentVersion from the schema assembly's FieldProvenanceAttribute stamps.
    // Assembly.Load(bytes), not LoadFrom(path) -- see SchemaProvider.fs for why (avoids a Windows file
    // lock on the referenced assembly for the life of this process).
    let readSchemaFields (schemaPath: string) =
        AssemblyProbing.registerProbeDir schemaPath
        let bytes = File.ReadAllBytes(schemaPath)
        let a = Assembly.Load(bytes)
        let flags = BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static ||| BindingFlags.Instance
        [ for t in a.GetTypes() do
            for p in t.GetProperties(flags) do
                for cad in p.GetCustomAttributesData() do
                    if cad.AttributeType.FullName = "SchemaTP.Runtime.FieldProvenanceAttribute" then
                        let fn  = cad.ConstructorArguments.[0].Value :?> string
                        let ver = cad.ConstructorArguments.[1].Value :?> string
                        yield (fn, ver) ]
        |> List.distinct

    let stampConsumes (prop: ProvidedProperty) (fn: string) (ver: string) =
        prop.AddCustomAttribute {
            new CustomAttributeData() with
                member _.Constructor = consumesCtor
                member _.ConstructorArguments =
                    upcast [| CustomAttributeTypedArgument(typeof<string>, box fn)
                              CustomAttributeTypedArgument(typeof<string>, box ver) |]
                member _.NamedArguments = upcast [||] }

    // Memoization is load-bearing (see SchemaProvider.fs for why): FCS may invoke the
    // DefineStaticParameters instantiation function more than once per logical instantiation, and an
    // unmemoized call would create a fresh ProvidedAssembly (fresh temp file) each time.
    let successCache = Dictionary<string * string * string, ProvidedTypeDefinition>()

    let createTypeUncached (typeName: string) (schemaPath: string) (fieldNames: string) =
        let providedAsm = ProvidedAssembly()
        let schemaFields = readSchemaFields schemaPath |> dict
        let names =
            fieldNames.Split([| ';' |], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map (fun s -> s.Trim())
        let t = ProvidedTypeDefinition(providedAsm, ns, typeName, Some typeof<obj>, isErased = false)
        t.AddMember(ProvidedConstructor([], invokeCode = fun _ -> <@@ () @@>))
        let missing = ResizeArray<string>()
        for name in names do
            match schemaFields.TryGetValue name with
            | true, ver ->
                let prop = ProvidedProperty(name, typeof<string>, isStatic = false, getterCode = fun _ -> <@@ ver @@>)
                stampConsumes prop name ver
                t.AddMember prop
            | false, _ ->
                missing.Add name
        if missing.Count > 0 then
            failwithf "ClientTP: schema '%s' has no field(s): %s" schemaPath (String.Join(", ", missing))
        providedAsm.AddTypes [ t ]
        t

    let createType (typeName: string) (schemaPath: string) (fieldNames: string) =
        Q012Instr.log "createType"
        let key = (typeName, schemaPath, fieldNames)
        match successCache.TryGetValue key with
        | true, t -> t
        | false, _ ->
            let t = createTypeUncached typeName schemaPath fieldNames
            successCache.[key] <- t
            t

    let clientType =
        let t = ProvidedTypeDefinition(asm, ns, "Client", Some typeof<obj>, isErased = false)
        t.DefineStaticParameters(
            [ ProvidedStaticParameter("SchemaAssemblyPath", typeof<string>)
              ProvidedStaticParameter("FieldNames", typeof<string>) ],
            fun typeName args -> createType typeName (args.[0] :?> string) (args.[1] :?> string))
        t

    // Round 1's isolated falsifier: a generative type with ONE property stamped with a
    // ConsumesFieldAttribute, NO schema reflection at all -- proves client-emitted member attributes
    // survive into independently-reflectable IL, in isolation. Static-parameterized (like Q009's
    // Round 1) so the generative type is actually baked into the consumer assembly's IL.
    let createRound1 (typeName: string) (fieldName: string) =
        let providedAsm = ProvidedAssembly()
        let t = ProvidedTypeDefinition(providedAsm, ns, typeName, Some typeof<obj>, isErased = false)
        t.AddMember(ProvidedConstructor([], invokeCode = fun _ -> <@@ () @@>))
        let prop = ProvidedProperty(fieldName, typeof<string>, isStatic = false, getterCode = fun _ -> <@@ "v1" @@>)
        stampConsumes prop fieldName "v1"
        t.AddMember prop
        providedAsm.AddTypes [ t ]
        t

    let round1Type =
        let t = ProvidedTypeDefinition(asm, ns, "Round1Stamped", Some typeof<obj>, isErased = false)
        t.DefineStaticParameters(
            [ ProvidedStaticParameter("FieldName", typeof<string>) ],
            fun typeName args -> createRound1 typeName (args.[0] :?> string))
        t

    do this.AddNamespace(ns, [ clientType; round1Type ])
