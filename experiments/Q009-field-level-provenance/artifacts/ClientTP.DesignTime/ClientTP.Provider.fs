namespace ClientTPImplementation

open ProviderImplementation.ProvidedTypes
open FSharp.Core.CompilerServices
open System
open System.Reflection

// ClientTP: a SECOND, genuinely separate generative provider. At static-parameter
// instantiation it reflects into SchemaTP's *compiled output* (a separate assembly on disk),
// reads the per-PROPERTY FieldProvenanceAttribute stamped by SchemaTP, and enforces provenance
// for ONLY the subset of fields named in its DependsOn argument. Fields not listed are never
// inspected. Full match => generate real members. Any declared-field mismatch => raise, naming
// EVERY mismatched field (not just the first) with declared and expected versions.
[<TypeProvider>]
type ClientProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (
        config,
        assemblyReplacementMap = [("ClientTP.DesignTime", "ClientTP.Runtime")],
        addDefaultProbingLocation = true)

    let ns = "ClientTP.Provided"
    let asm = Assembly.GetExecutingAssembly()

    // Parse "Name:v1;Age:v2" -> [("Name","v1");("Age","v2")]
    let parseSpec (spec: string) =
        spec.Split([| ';' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun part ->
            let kv = part.Split([| ':' |])
            (kv.[0].Trim(), kv.[1].Trim()))
        |> Array.toList

    // Read the per-field declared versions from the compiled schema assembly, via plain
    // reflection over each property's custom-attribute metadata. Returns fieldName -> version.
    let readDeclaredFieldVersions (schemaAssemblyPath: string) (schemaTypeName: string) =
        let schemaAsm = Assembly.LoadFrom schemaAssemblyPath
        let ty =
            match schemaAsm.GetType(schemaTypeName) with
            | null ->
                // fall back to scanning for a type carrying any FieldProvenanceAttribute
                schemaAsm.GetTypes()
                |> Array.tryFind (fun t ->
                    t.GetProperties()
                    |> Array.exists (fun p ->
                        p.GetCustomAttributesData()
                        |> Seq.exists (fun cad -> cad.AttributeType.Name = "FieldProvenanceAttribute")))
                |> Option.defaultWith (fun () ->
                    failwithf "ClientTP: schema type '%s' not found in assembly '%s'" schemaTypeName schemaAssemblyPath)
            | t -> t
        ty.GetProperties()
        |> Array.choose (fun p ->
            p.GetCustomAttributesData()
            |> Seq.tryPick (fun cad ->
                if cad.AttributeType.Name = "FieldProvenanceAttribute"
                   && cad.ConstructorArguments.Count >= 2 then
                    let fieldName = string cad.ConstructorArguments.[0].Value
                    let version = string cad.ConstructorArguments.[1].Value
                    Some (fieldName, version)
                else None))
        |> Map.ofArray

    let createType typeName (schemaAssemblyPath: string) (schemaTypeName: string) (dependsOn: string) =
        let declared = readDeclaredFieldVersions schemaAssemblyPath schemaTypeName
        let deps = parseSpec dependsOn

        // Check ONLY the declared dependencies; collect EVERY mismatch, not just the first.
        let mismatches =
            deps
            |> List.choose (fun (field, expected) ->
                match Map.tryFind field declared with
                | None ->
                    Some (sprintf "field '%s' (client expects '%s') is not present in schema type '%s'"
                            field expected schemaTypeName)
                | Some actual when actual <> expected ->
                    Some (sprintf "field '%s': schema declares '%s' but client expects '%s'"
                            field actual expected)
                | Some _ -> None)

        if not (List.isEmpty mismatches) then
            // The provenance conflict. Raise from within the instantiation function, naming EVERY
            // mismatched field the client depends on.
            failwithf "ClientTP provenance mismatch (%d field(s)): %s"
                mismatches.Length (String.Join("; ", mismatches))

        // Full match: generate real provided members so PASS means genuine generation, not just
        // absence of an exception.
        let provAsm = ProvidedAssembly()
        let myType = ProvidedTypeDefinition(provAsm, ns, typeName, Some typeof<obj>, isErased = false)
        let ctor = ProvidedConstructor([], invokeCode = fun _args -> <@@ () @@>)
        myType.AddMember ctor

        let checkedCount = deps.Length
        let countProp =
            ProvidedProperty(
                "CheckedFieldCount", typeof<int>, isStatic = true,
                getterCode = fun _args -> <@@ checkedCount @@>)
        myType.AddMember countProp

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
              ProvidedStaticParameter("DependsOn", typeof<string>) ],
            fun typeName args ->
                createType typeName (args.[0] :?> string) (args.[1] :?> string) (args.[2] :?> string))
        t

    do this.AddNamespace(ns, [containerType])
