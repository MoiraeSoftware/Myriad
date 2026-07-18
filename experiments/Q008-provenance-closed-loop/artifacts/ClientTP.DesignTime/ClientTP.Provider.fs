namespace ClientTPImplementation

open ProviderImplementation.ProvidedTypes
open FSharp.Core.CompilerServices
open System
open System.IO
open System.Reflection

// ClientTP: a SECOND, genuinely separate generative provider. At static-parameter
// instantiation it reflects into SchemaTP's *compiled output* (a separate assembly on disk),
// reads the SchemaVersionAttribute stamped by SchemaTP, and compares that declared provenance
// against ClientTP's own ExpectedVersion argument. Match => generate real members.
// Mismatch => raise a descriptive exception naming both versions.
[<TypeProvider>]
type ClientProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (
        config,
        assemblyReplacementMap = [("ClientTP.DesignTime", "ClientTP.Runtime")],
        addDefaultProbingLocation = true)

    let ns = "ClientTP.Provided"
    let asm = Assembly.GetExecutingAssembly()

    // Read the declared schema version from the compiled schema assembly, via plain reflection
    // over custom attribute metadata. Concrete working mechanism (the design's flagged open
    // question): the absolute compiled-assembly path arrives as a static argument; Assembly.LoadFrom
    // opens it, and the LoadFrom probing context resolves the co-located SchemaTP.Runtime.dll (which
    // carries the SchemaVersionAttribute type) with NO custom AssemblyResolve handler required —
    // verified empirically by removing one and re-running.
    let readDeclaredVersion (schemaAssemblyPath: string) (schemaTypeName: string) =
        let schemaAsm = Assembly.LoadFrom schemaAssemblyPath
        // Try the named type first; fall back to scanning for the stamped attribute.
        let candidateTypes =
            match schemaAsm.GetType(schemaTypeName) with
            | null -> schemaAsm.GetTypes() :> seq<Type>
            | t -> Seq.singleton t
        candidateTypes
        |> Seq.tryPick (fun t ->
            t.GetCustomAttributesData()
            |> Seq.tryPick (fun cad ->
                if cad.AttributeType.Name = "SchemaVersionAttribute"
                   && cad.ConstructorArguments.Count > 0 then
                    Some (t.FullName, string cad.ConstructorArguments.[0].Value)
                else None))

    let createType typeName (schemaAssemblyPath: string) (schemaTypeName: string) (expectedVersion: string) =
        let declared =
            match readDeclaredVersion schemaAssemblyPath schemaTypeName with
            | Some (foundName, v) -> v
            | None ->
                failwithf "ClientTP: no SchemaVersionAttribute found on type '%s' in assembly '%s'"
                    schemaTypeName schemaAssemblyPath

        if declared <> expectedVersion then
            // The provenance conflict. Raise from within the instantiation function.
            failwithf "ClientTP provenance mismatch: schema type '%s' declares SchemaVersion '%s' but the client was told to expect '%s'"
                schemaTypeName declared expectedVersion

        // Match: generate real provided members so PASS means genuine generation, not just
        // absence of an exception.
        let provAsm = ProvidedAssembly()
        let myType = ProvidedTypeDefinition(provAsm, ns, typeName, Some typeof<obj>, isErased = false)
        let ctor = ProvidedConstructor([], invokeCode = fun _args -> <@@ () @@>)
        myType.AddMember ctor

        let verifiedProp =
            ProvidedProperty(
                "VerifiedVersion", typeof<string>, isStatic = true,
                getterCode = fun _args -> <@@ expectedVersion @@>)
        myType.AddMember verifiedProp

        let okProp =
            ProvidedProperty(
                "ProvenanceOk", typeof<bool>, isStatic = true,
                getterCode = fun _args -> <@@ true @@>)
        myType.AddMember okProp

        provAsm.AddTypes [myType]
        myType

    let containerType =
        let t = ProvidedTypeDefinition(asm, ns, "Client", Some typeof<obj>, isErased = false)
        t.DefineStaticParameters(
            [ ProvidedStaticParameter("SchemaAssemblyPath", typeof<string>)
              ProvidedStaticParameter("SchemaTypeName", typeof<string>)
              ProvidedStaticParameter("ExpectedVersion", typeof<string>) ],
            fun typeName args ->
                createType typeName (args.[0] :?> string) (args.[1] :?> string) (args.[2] :?> string))
        t

    do this.AddNamespace(ns, [containerType])
