namespace SatelliteTPImplementation

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Runtime.Loader
open FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Quotations

// Q018: replaces Assembly.LoadFrom (the default "LoadFrom context", which is NOT collectible and holds
// a Windows file-lock on the DLL for the process's lifetime -- confirmed the root cause of Q016's Round
// 2 failure) with a collectible AssemblyLoadContext per satellite path, cached so repeated loads of the
// SAME path within one provider instance's lifetime return the IDENTICAL Assembly object (needed for
// Round 1's source/target identity match -- .NET's own LoadFrom-context used to give this for free by
// path-caching; a collectible ALC does not, so this cache reimplements it manually).
// Found by running, not anticipated by the design: a bare `AssemblyLoadContext(isCollectible = true)`
// with no `Load` override does NOT reliably delegate a dependency like FSharp.Core back to
// AssemblyLoadContext.Default -- it can resolve its OWN separate copy via ordinary probing, producing a
// SECOND, DISTINCT `FSharpFunc<string,string>` identity that fails to type-match the host's own
// `FSharpFunc<string,string>` ("Expected 'FSharpFunc...', but received type 'FSharpFunc...'" -- same
// printed name, different identity), poisoning the whole provided type exactly the way Q017's own
// arity-mismatch finding did. Fix: explicitly delegate any assembly already loaded into Default back to
// Default, only falling through to this ALC's own probing for genuinely private dependencies.
type private DelegatingCollectibleAlc(name: string) =
    inherit AssemblyLoadContext(name = name, isCollectible = true)
    override _.Load(assemblyName: AssemblyName) : Assembly =
        AssemblyLoadContext.Default.Assemblies
        |> Seq.tryFind (fun a -> a.GetName().Name = assemblyName.Name)
        |> Option.toObj

module private AlcCache =
    let private cache = Dictionary<string, AssemblyLoadContext * Assembly>(StringComparer.OrdinalIgnoreCase)

    // Found by running, not anticipated by the design: naively LoadFromAssemblyPath-ing EVERY entry in
    // config.ReferencedAssemblies (as the original Assembly.LoadFrom-based code did implicitly) is wrong
    // once collectible ALCs are involved. Assembly.LoadFrom transparently reused whatever identical
    // assembly was ALREADY loaded elsewhere in the process (e.g. FSharp.Core, already loaded by the host);
    // AssemblyLoadContext.LoadFromAssemblyPath does NOT -- it always creates a genuinely new, separate
    // instance into whichever ALC you call it on. Loading FSharp.Core.dll's own path this way created a
    // SECOND, isolated FSharp.Core instance distinct from the one the SDK's own sourceAssemblies
    // registration and this provider's own reflection actually resolve against, producing a same-printed-
    // name-different-identity "Expected X, received X" error -- for a reason that had nothing to do with
    // the delegating Load() override above (which only fires for a DEPENDENCY of something already being
    // loaded, not for a path passed directly to LoadFromAssemblyPath). Fix: only create a fresh collectible
    // ALC for a path whose assembly ISN'T already loaded anywhere in the process; everything else (the
    // framework/shared references every consumer pulls in) just reuses the existing instance directly,
    // exactly matching what Assembly.LoadFrom did for those paths all along. This narrows collectible-ALC
    // treatment to the one assembly we actually intend to make evictable -- the satellite itself.
    let private findAlreadyLoaded (path: string) : Assembly option =
        let simpleName = Path.GetFileNameWithoutExtension(path: string)
        AssemblyLoadContext.All
        |> Seq.collect (fun alc -> alc.Assemblies)
        |> Seq.tryFind (fun a -> String.Equals(a.GetName().Name, simpleName, StringComparison.OrdinalIgnoreCase))

    let loadOrGet (path: string) : Assembly =
        match cache.TryGetValue path with
        | true, (_, asm) -> asm
        | false, _ ->
            match findAlreadyLoaded path with
            | Some existing ->
                eprintfn "[Q018-DIAG] reusing already-loaded assembly for %s (not collectible)" path
                existing
            | None ->
                let alc = DelegatingCollectibleAlc("SatelliteALC:" + path) :> AssemblyLoadContext
                let asm = alc.LoadFromAssemblyPath path
                cache.[path] <- (alc, asm)
                eprintfn "[Q018-DIAG] loaded into a fresh collectible ALC for %s" path
                asm

    // Drops the cache entry and calls Unload() on its ALC -- does NOT itself force collection (Round A
    // found that requires the CALLER's own GC.Collect()/WaitForPendingFinalizers() loop, ideally with
    // TieredCompilation=false in the hosting process). Removing the cache entry first is required: our
    // own Dictionary holds a strong reference to the Assembly (and transitively the ALC) that would
    // block collection forever if left in place, a failure mode distinct from anything Round A tested.
    let evict (path: string) =
        match cache.TryGetValue path with
        | true, (alc, _) ->
            cache.Remove path |> ignore
            alc.Unload()
        | false, _ -> ()

// Q018 Round C, public and reflection-callable on purpose: the FileSystemWatcher-driven onChange handler
// below can only fire AFTER SatelliteBuilder's overwrite already succeeds -- but releasing the lock is a
// PRECONDITION for that overwrite to succeed in the first place, not a consequence of it. That is a
// chicken-and-egg problem the original design didn't name. This hook lets the Harness itself force
// eviction+collection from OUTSIDE, simulating an idle-timeout or "about to write" signal a real host
// might use instead, to test the mechanism's correctness independent of that triggering-timing problem.
module Q018Hooks =
    let evictAndForceCollect (path: string) : bool =
        AlcCache.evict path
        let mutable iterations = 0
        let mutable released = false
        while not released && iterations < 30 do
            GC.Collect(2, GCCollectionMode.Forced, true, true)
            GC.WaitForPendingFinalizers()
            GC.Collect(2, GCCollectionMode.Forced, true, true)
            Threading.Thread.Sleep(50)
            iterations <- iterations + 1
            try
                use fs = File.Open(path, FileMode.Open, FileAccess.Write, FileShare.None)
                released <- true
            with _ -> released <- false
        eprintfn "[Q018-DIAG] evictAndForceCollect(%s): released=%b after %d iterations" path released iterations
        released

module private LoadHelper =
    let tryLoadFrom (p: string) : Assembly option =
        try Some(AlcCache.loadOrGet p) with _ -> None

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
// Q018 Round B: a process-wide counter to determine whether FCS constructs a NEW SatelliteProvider
// instance each time Invalidate() triggers a re-resolution, or reuses the SAME instance -- this
// determines which Round C mitigation shape (refreshable vs. fixed sourceAssemblies) is even attemptable.
// Written to stderr in the constructor so it's visible in Harness's own captured output regardless of
// which ALC/isolation context FCS loads this design-time assembly into (Console streams are process-wide).
module private DiagCounter =
    let mutable constructedCount = 0

[<TypeProvider>]
type SatelliteProvider (config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(config, sourceAssemblies = LoadHelper.loadSourceAssemblies config, assemblyReplacementMap=[("SatelliteTP.DesignTime","SatelliteTP.Runtime")], addDefaultProbingLocation=true)

    do
        DiagCounter.constructedCount <- DiagCounter.constructedCount + 1
        eprintfn "[Q018-DIAG] SatelliteProvider CONSTRUCTED (instance #%d)" DiagCounter.constructedCount

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
        AlcCache.loadOrGet dllPath

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
                // Q018: this fires AFTER the file already changed, so evicting here cannot have helped
                // release the lock that blocked THIS write -- kept for realism/parity with the original
                // design (a real host still benefits from evicting stale handles after a successful
                // external change), but this is not where Round C's fix has to come from.
                AlcCache.evict dllPath
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
        // Q018 diagnostic: pin down whether Map's type-mismatch is a real FSharp.Core identity split.
        if mapMi <> null then
            let p0 = mapMi.GetParameters().[0].ParameterType
            eprintfn "[Q018-DIAG] mapMi param0 type=%s asm=%s location=%s hash=%d"
                p0.FullName (p0.Assembly.FullName) p0.Assembly.Location (p0.Assembly.GetHashCode())
            let hostFuncType = typeof<string -> string>
            eprintfn "[Q018-DIAG] host FSharpFunc<string,string> type=%s asm=%s location=%s hash=%d"
                hostFuncType.FullName (hostFuncType.Assembly.FullName) hostFuncType.Assembly.Location (hostFuncType.Assembly.GetHashCode())
            eprintfn "[Q018-DIAG] same assembly object=%b" (Object.ReferenceEquals(p0.Assembly, hostFuncType.Assembly))

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
