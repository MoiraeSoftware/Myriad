namespace SatelliteTPImplementation

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Quotations

module private LoadHelper =
    // try/with directly inside a list comprehension needs an FSharp.Core surface
    // (RuntimeHelpers.EnumerateTryWith) not present at the pinned 4.7.2 for this design-time project --
    // found by running (FS0193 build error), not anticipated. A plain helper function avoids it.
    let tryLoadFrom (p: string) : Assembly option =
        try Some(Assembly.LoadFrom p) with _ -> None

    // A single function call (not an inline list comprehension) so the `inherit` expression that uses
    // it stays a single simple call -- inlining the comprehension directly into the `inherit` argument
    // list tripped the parser ("value or constructor 'sourceAssemblies' is not defined").
    let loadSourceAssemblies (config: TypeProviderConfig) : Assembly list =
        config.ReferencedAssemblies |> Array.toList |> List.choose tryLoadFrom

// Co-located-dependency resolver (needed only if the satellite ever touches a type from
// Myriad.Plugins/Myriad.Core at runtime, which it doesn't here -- Generated.fs has no Myriad
// dependency -- kept anyway, cheap and defensive), mirroring
// Q011-consumer-driven-contracts/artifacts/SchemaTP.DesignTime/SchemaProvider.fs's own AssemblyProbing.
module private AssemblyProbing =
    let private probeDirs = HashSet<string>(StringComparer.OrdinalIgnoreCase)
    let mutable private hooked = false
    let registerProbeDir (path: string) =
        probeDirs.Add(Path.GetDirectoryName(path: string)) |> ignore
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

// SatelliteTP: a generative provider that does NOT hand-build ProvidedTypes members from Myriad's
// untyped AST (item 14's own open problem). Instead it re-exposes Myriad's real compiled members via
// reflection-forwarding: each ProvidedMethod's invokeCode is Expr.Call(realMethodInfo, args) -- the
// emitted IL calls Myriad's actual generated code directly, the provider contributes no logic of its
// own. This is the cross-project case (BACKLOG.md item 15): by the time this provider resolves, the
// satellite DLL is, by construction, an already-compiled, externally-referenced artifact -- Q006's
// same-compilation wall never applies, because nothing here asks the compiler to resolve a type from
// the compilation currently in progress.
//
// Found by running, not anticipated by the design -- three failed attempts before this one (kept as
// history in `loadSatellite`'s own comment below): the one that actually works is to make Satellite.dll
// a genuine SOURCE assembly for THIS provider from the moment it's constructed, via
// TypeProviderForNamespaces's own `sourceAssemblies` constructor parameter
// (ProvidedTypes.fsi:445 -- "the design-time assemblies available to use as a basis for authoring
// provided types... By default Assembly.GetCallingAssembly() and its transitive dependencies"). The
// satellite's exact path isn't known until a static parameter is applied, but `config.ReferencedAssemblies`
// (every assembly the CONSUMER references, via its own -r: flags) already lists it at CONSTRUCTION
// time -- the consumer must reference the satellite directly anyway (see Harness's own compileConsumer
// comment). Loading every one of those paths as a source assembly up front means Satellite.dll (once a
// consumer references it) is a real, correctly-identity-matched source assembly by the time any static
// parameter is applied, with no further registration trickery needed.
[<TypeProvider>]
type SatelliteProvider (config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(config, sourceAssemblies = LoadHelper.loadSourceAssemblies config, assemblyReplacementMap=[("SatelliteTP.DesignTime","SatelliteTP.Runtime")], addDefaultProbingLocation=true)

    let ns = "SatelliteTP.Provided"
    let asm = Assembly.GetExecutingAssembly()

    // Memoization is load-bearing, not an optimization (Q011's lesson): FCS invokes the
    // DefineStaticParameters instantiation function more than once per logical instantiation; an
    // unmemoized call creates a fresh ProvidedAssembly() each time, causing a type-identity mismatch
    // against the first call's result.
    let successCache = Dictionary<string * string * string, ProvidedTypeDefinition>()
    let watchers = Dictionary<string, FileSystemWatcher>()

    // Found by running, not anticipated by the design -- three attempts before this one:
    // (1) a bare Assembly.Load(bytes), used only for the provider's own internal reflection, was
    //     rejected by FCS's cross-targeting validation ("not found in target/design-time assembly
    //     set") because it was never registered into ProvidedTypesContext's own tables at all;
    // (2) RegisterGeneratedTargetAssembly (ProvidedTypes.fs:16372) registers it into the TARGET table,
    //     which fixed the reference-set errors, but every ProvidedParameter/ProvidedMethod built from
    //     ITS types (a "tgt type", ProvidedTypes.fs's own TargetTypeDefinition wrapper) then rejected
    //     the SDK's own invokeCode arg Vars, which are ALWAYS built in SOURCE terms regardless of the
    //     declared parameter type ("Expected 'tgt type System.String', but received type
    //     'System.String'" -- confirmed by printing GetType().FullName: TargetTypeDefinition vs
    //     RuntimeType) -- the whole invokeCode result is meant to be authored in source terms and
    //     converted to target automatically by the SDK afterward (`convCodeToTgt` internally), not
    //     assembled from target-side pieces directly;
    // (3) a manual Assembly.Load(bytes), source-only, no RegisterGeneratedTargetAssembly at all --
    //     still failed identically to (2) ("not found in design-time assembly set"), which is what
    //     revealed FCS itself, not this provider's own code, auto-registers every `config
    //     .ReferencedAssemblies` entry as a TARGET assembly the moment a consumer references it (here,
    //     via the Harness's own `-r:Satellite.dll`) -- so a real SOURCE-side counterpart was needed
    //     regardless of what loadSatellite itself did, and none of (1)-(3) ever provided one.
    // (4) this, what actually works: `sourceAssemblies` above already loaded Satellite.dll (once a
    //     consumer references it) as a genuine source assembly at CONSTRUCTION time. `Assembly.LoadFrom`
    //     (not Load(bytes)) here returns the runtime's own CACHED instance for an identical path --
    //     the SAME Assembly object the constructor already loaded, not a fresh, unregistered one.
    //     Reintroduces the Windows file-lock risk Q011 first found with LoadFrom (Round 2 tests this
    //     directly, honestly, rather than assuming it away) -- but Load(bytes) cannot give the identity
    //     match this mechanism needs, so LoadFrom is the correct choice here, not an oversight.
    let loadSatellite (dllPath: string) =
        AssemblyProbing.registerProbeDir dllPath
        Assembly.LoadFrom dllPath

    // FileSystemWatcher-driven Invalidate(): the mechanism BACKLOG.md's "External-signal Invalidate()"
    // idea names (TypeProviderForNamespaces.Invalidate(), a plain method any live provider instance can
    // call) but that had never been built in this repo before this quartet. Registered once per
    // satellite path, memoized in `watchers` for the same reason `successCache` is memoized.
    let ensureWatcher (dllPath: string) =
        if not (watchers.ContainsKey dllPath) then
            let dir = Path.GetDirectoryName(dllPath: string)
            let file = Path.GetFileName(dllPath: string)
            let w = new FileSystemWatcher(dir, file)
            w.NotifyFilter <- NotifyFilters.LastWrite ||| NotifyFilters.CreationTime ||| NotifyFilters.Size
            let onChange (_: FileSystemEventArgs) =
                successCache.Clear()
                this.Invalidate()
            w.Changed.Add(onChange)
            w.Created.Add(onChange)
            w.EnableRaisingEvents <- true
            watchers.[dllPath] <- w

    let createTypeUncached (typeName: string) (dllPath: string) (moduleTypeName: string) (recordTypeName: string) =
        ensureWatcher dllPath
        let satAsm = loadSatellite dllPath
        let moduleType =
            match satAsm.GetType(moduleTypeName) with
            | null -> failwithf "SatelliteTP: module type '%s' not found in '%s'" moduleTypeName dllPath
            | t -> t
        let recordType =
            match satAsm.GetType(recordTypeName) with
            | null -> failwithf "SatelliteTP: record type '%s' not found in '%s'" recordTypeName dllPath
            | t -> t

        let flags = BindingFlags.Public ||| BindingFlags.Static
        let createMi = moduleType.GetMethod("create", flags)
        let nameMi = moduleType.GetMethod("name", flags)
        let ageMi = moduleType.GetMethod("age", flags)
        if createMi = null || nameMi = null || ageMi = null then
            failwithf "SatelliteTP: expected create/name/age static methods on '%s', found create=%b name=%b age=%b"
                moduleTypeName (createMi <> null) (nameMi <> null) (ageMi <> null)
        // Round 2 regeneration adds an 'email' field/getter; only wired up when present so v1
        // satellites (no email) still resolve cleanly.
        let emailMi = moduleType.GetMethod("email", flags)
        // Q017: the real, function-typed-parameter shape Q016's own review named as the untested
        // generalization gap -- `map`'s parameters include two FSharpFunc<_,_>-typed arguments plus a
        // Person, exactly the shape Q016's reflection-forwarding kernel never exercised.
        let mapMi = moduleType.GetMethod("map", flags)

        // Found by running, not anticipated by the design: ctxt.ConvertSourceExprToTarget (tried first)
        // is for converting expressions the PROVIDER AUTHOR builds from source-side pieces, not for
        // re-processing the SDK's own invokeCode argument Vars -- applying it to them replaced those
        // Vars with fresh ones absent from the emitter's own parameter/local tracking ("unknown
        // parameter/field" at IL-emission time). Declaring ProvidedParameter with
        // ConvertSourceTypeToTarget typeof<string> (tried second) also didn't produce a type identical
        // to createMi's own real parameter type ("Expected 'tgt type System.String', but received type
        // System.String"). The fix that actually works: derive every ProvidedParameter/return type
        // DIRECTLY from the real target MethodInfo's own signature via reflection
        // (createMi.GetParameters(), nameMi.ReturnType, ...) rather than reconstructing a supposedly
        // equivalent type by hand -- guarantees exact identity with what Expr.Call itself expects, no
        // conversion needed at all.
        let provAsm = ProvidedAssembly()
        let t = ProvidedTypeDefinition(provAsm, ns, typeName, Some typeof<obj>, isErased = false)
        t.AddMember(ProvidedConstructor([], invokeCode = fun _ -> <@@ () @@>))

        let createParams =
            [ for p in createMi.GetParameters() -> ProvidedParameter(p.Name, p.ParameterType) ]
        let createM =
            ProvidedMethod("Create", createParams, createMi.ReturnType, isStatic = true,
                invokeCode = fun args -> Expr.Call(createMi, args))
        t.AddMember createM

        let getNameM =
            ProvidedMethod("GetName", [ ProvidedParameter("person", (nameMi.GetParameters().[0]).ParameterType) ], nameMi.ReturnType, isStatic = true,
                invokeCode = fun args -> Expr.Call(nameMi, args))
        t.AddMember getNameM

        let getAgeM =
            ProvidedMethod("GetAge", [ ProvidedParameter("person", (ageMi.GetParameters().[0]).ParameterType) ], ageMi.ReturnType, isStatic = true,
                invokeCode = fun args -> Expr.Call(ageMi, args))
        t.AddMember getAgeM

        if emailMi <> null then
            let getEmailM =
                ProvidedMethod("GetEmail", [ ProvidedParameter("person", (emailMi.GetParameters().[0]).ParameterType) ], emailMi.ReturnType, isStatic = true,
                    invokeCode = fun args -> Expr.Call(emailMi, args))
            t.AddMember getEmailM

        if mapMi <> null then
            // Same construction pattern as createM/getNameM/getAgeM -- the only difference is that two
            // of mapMi.GetParameters()'s entries are FSharpFunc<string,string>/FSharpFunc<int,int>
            // rather than string/int/Person. Q017's whole question is whether that difference matters.
            let mapParams =
                [ for p in mapMi.GetParameters() -> ProvidedParameter(p.Name, p.ParameterType) ]
            let mapM =
                ProvidedMethod("Map", mapParams, mapMi.ReturnType, isStatic = true,
                    invokeCode = fun args -> Expr.Call(mapMi, args))
            t.AddMember mapM

        provAsm.AddTypes [ t ]
        t

    let createType (typeName: string) (dllPath: string) (moduleTypeName: string) (recordTypeName: string) =
        let key = (dllPath, moduleTypeName, recordTypeName)
        match successCache.TryGetValue key with
        | true, t -> t
        | false, _ ->
            let t = createTypeUncached typeName dllPath moduleTypeName recordTypeName
            successCache.[key] <- t
            t

    let containerType =
        let t = ProvidedTypeDefinition(asm, ns, "MyriadSatellite", Some typeof<obj>, isErased = false)
        t.DefineStaticParameters(
            [ ProvidedStaticParameter("SatelliteDllPath", typeof<string>)
              ProvidedStaticParameter("ModuleTypeName", typeof<string>)
              ProvidedStaticParameter("RecordTypeName", typeof<string>) ],
            fun typeName args -> createType typeName (args.[0] :?> string) (args.[1] :?> string) (args.[2] :?> string))
        t

    do this.AddNamespace(ns, [ containerType ])
