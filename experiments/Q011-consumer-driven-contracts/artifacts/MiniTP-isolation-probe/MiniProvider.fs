namespace MiniTPImplementation
open System
open System.Collections.Generic
open System.Reflection
open FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes

[<TypeProvider>]
type MiniProvider (config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(config, assemblyReplacementMap=[("MiniTP.DesignTime","MiniTP.Runtime")], addDefaultProbingLocation=true)
    let ns = "MiniTP.Provided"
    let asm = Assembly.GetExecutingAssembly()
    let cache = Dictionary<string*string, ProvidedTypeDefinition>()

    let createTypeUncached (typeName: string) (tag: string) =
        let providedAsm = ProvidedAssembly()
        let t = ProvidedTypeDefinition(providedAsm, ns, typeName, Some typeof<obj>, isErased = false)
        t.AddMember(ProvidedConstructor([], invokeCode = fun _ -> <@@ () @@>))
        let prop = ProvidedProperty("Tag", typeof<string>, isStatic = false, getterCode = fun _ -> <@@ tag @@>)
        t.AddMember prop
        providedAsm.AddTypes [ t ]
        t

    let createType (typeName: string) (tag: string) =
        let key = (typeName, tag)
        match cache.TryGetValue key with
        | true, t -> t
        | false, _ ->
            let t = createTypeUncached typeName tag
            cache.[key] <- t
            t

    let miniType =
        let t = ProvidedTypeDefinition(asm, ns, "Mini", Some typeof<obj>, isErased = false)
        t.DefineStaticParameters([ ProvidedStaticParameter("Tag", typeof<string>) ], fun typeName args -> createType typeName (args.[0] :?> string))
        t

    do this.AddNamespace(ns, [ miniType ])
