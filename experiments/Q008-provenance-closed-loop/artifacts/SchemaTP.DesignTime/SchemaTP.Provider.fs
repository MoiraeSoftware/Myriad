namespace SchemaTPImplementation

open ProviderImplementation.ProvidedTypes
open FSharp.Core.CompilerServices
open System
open System.Reflection

// SchemaTP: a generative provider that emits one provided type stamped with a REAL
// custom attribute SchemaVersionAttribute(Version), plus a couple of trivial members so
// it is not a bare marker type.
[<TypeProvider>]
type SchemaProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (
        config,
        assemblyReplacementMap = [("SchemaTP.DesignTime", "SchemaTP.Runtime")],
        addDefaultProbingLocation = true)

    let ns = "SchemaTP.Provided"
    let asm = Assembly.GetExecutingAssembly()

    do assert (typeof<SchemaTP.Runtime.SchemaRuntimeHelper>.Assembly.GetName().Name = asm.GetName().Name)

    // Build CustomAttributeData for SchemaVersionAttribute(version). The Constructor points at
    // the (design-time copy of the) runtime attribute type; the SDK's IL emit path remaps its
    // declaring type to SchemaTP.Runtime via assemblyReplacementMap.
    let schemaVersionAttr (version: string) =
        { new CustomAttributeData() with
            member _.Constructor = typeof<SchemaTP.Runtime.SchemaVersionAttribute>.GetConstructors().[0]
            member _.ConstructorArguments = upcast [| CustomAttributeTypedArgument(typeof<string>, version) |]
            member _.NamedArguments = upcast [| |] }

    let createType typeName (version: string) =
        let provAsm = ProvidedAssembly()
        let myType = ProvidedTypeDefinition(provAsm, ns, typeName, Some typeof<obj>, isErased = false)

        // stamp the provenance attribute on the provided type
        myType.AddCustomAttribute(schemaVersionAttr version)

        let ctor = ProvidedConstructor([], invokeCode = fun _args -> <@@ () @@>)
        myType.AddMember ctor

        // a couple of trivial members so it is not a bare marker type
        let vprop =
            ProvidedProperty(
                "DeclaredVersion", typeof<string>, isStatic = true,
                getterCode = fun _args -> <@@ version @@>)
        myType.AddMember vprop

        let fprop =
            ProvidedProperty(
                "FieldCount", typeof<int>, isStatic = true,
                getterCode = fun _args -> <@@ 1 @@>)
        myType.AddMember fprop

        provAsm.AddTypes [myType]
        myType

    let containerType =
        let t = ProvidedTypeDefinition(asm, ns, "Schema", Some typeof<obj>, isErased = false)
        t.DefineStaticParameters(
            [ ProvidedStaticParameter("Version", typeof<string>) ],
            fun typeName args -> createType typeName (args.[0] :?> string))
        t

    do this.AddNamespace(ns, [containerType])
