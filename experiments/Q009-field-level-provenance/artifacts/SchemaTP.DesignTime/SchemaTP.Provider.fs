namespace SchemaTPImplementation

open ProviderImplementation.ProvidedTypes
open FSharp.Core.CompilerServices
open System
open System.Reflection

// SchemaTP: a generative provider that emits ONE provided type with MULTIPLE provided
// properties, EACH individually stamped with its own FieldProvenanceAttribute(fieldName, version).
// The field spec arrives as a single string static parameter, e.g. "Name:v1;Age:v2;Email:v3".
// This is the member-level analogue of Q008's SchemaTP, which stamped a single attribute on the
// whole type. Here the attribute is stamped on each ProvidedProperty via AddCustomAttribute.
[<TypeProvider>]
type SchemaProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (
        config,
        assemblyReplacementMap = [("SchemaTP.DesignTime", "SchemaTP.Runtime")],
        addDefaultProbingLocation = true)

    let ns = "SchemaTP.Provided"
    let asm = Assembly.GetExecutingAssembly()

    do assert (typeof<SchemaTP.Runtime.SchemaRuntimeHelper>.Assembly.GetName().Name = asm.GetName().Name)

    // Parse "Name:v1;Age:v2;Email:v3" -> [("Name","v1");("Age","v2");("Email","v3")]
    let parseSpec (spec: string) =
        spec.Split([| ';' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun part ->
            let kv = part.Split([| ':' |])
            (kv.[0].Trim(), kv.[1].Trim()))
        |> Array.toList

    // Reproduce the design's Name:string, Age:int, Email:string typing; everything else is string.
    let fieldType (name: string) =
        if name = "Age" then typeof<int> else typeof<string>

    // Build CustomAttributeData for FieldProvenanceAttribute(fieldName, version). The Constructor
    // points at the (design-time copy of the) runtime attribute type; the SDK's IL emit path
    // (ProvidedTypes.fs, defineCustomAttrs on pb.SetCustomAttribute for properties) remaps its
    // declaring type to SchemaTP.Runtime via assemblyReplacementMap.
    let fieldProvenanceAttr (fieldName: string) (version: string) =
        { new CustomAttributeData() with
            member _.Constructor = typeof<SchemaTP.Runtime.FieldProvenanceAttribute>.GetConstructors().[0]
            member _.ConstructorArguments =
                upcast [| CustomAttributeTypedArgument(typeof<string>, fieldName)
                          CustomAttributeTypedArgument(typeof<string>, version) |]
            member _.NamedArguments = upcast [| |] }

    let createType typeName (spec: string) =
        let fields = parseSpec spec
        let provAsm = ProvidedAssembly()
        let myType = ProvidedTypeDefinition(provAsm, ns, typeName, Some typeof<obj>, isErased = false)

        let ctor = ProvidedConstructor([], invokeCode = fun _args -> <@@ () @@>)
        myType.AddMember ctor

        // one provided property per field, each stamped with its OWN field-level attribute
        for (fieldName, version) in fields do
            let ty = fieldType fieldName
            let prop =
                if ty = typeof<int> then
                    ProvidedProperty(fieldName, ty, isStatic = true, getterCode = fun _args -> <@@ 0 @@>)
                else
                    ProvidedProperty(fieldName, ty, isStatic = true, getterCode = fun _args -> <@@ "" @@>)
            prop.AddCustomAttribute(fieldProvenanceAttr fieldName version)
            myType.AddMember prop

        provAsm.AddTypes [myType]
        myType

    let containerType =
        let t = ProvidedTypeDefinition(asm, ns, "Schema", Some typeof<obj>, isErased = false)
        t.DefineStaticParameters(
            [ ProvidedStaticParameter("Spec", typeof<string>) ],
            fun typeName args -> createType typeName (args.[0] :?> string))
        t

    do this.AddNamespace(ns, [containerType])
