namespace MyriadPreviewImplementation

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Quotations
open Fantomas.FCS.Syntax
open Myriad.Core

// Q019: an ERASED provider that never asks the compiler to resolve anything at design time. It
// parses its own SourceFilePath static parameter with Myriad's own real, unmodified parsing code
// (Myriad.Core.Ast.fromFilename / Ast.extractRecords -- a direct library reference to
// src/Myriad.Core/bin/Release/net9.0/Myriad.Core.dll, not a reimplementation), finds the named
// record's field list syntactically, and exposes one erased member per field. Because the type is
// erased, its members need no independently-compiled backing type for FCS to validate against --
// only the erasure target (`obj`) needs to exist, and that's already compiled (it's the BCL). This
// is the mechanism that should sidestep Q006's wall: nothing here ever asks FCS to resolve the named
// record type itself, only to typecheck this provider's own self-invented members.
module private AstHelpers =
    // A SynTypeDefn's own SynComponentInfo carries only the type's OWN name (e.g. "Person"), not the
    // enclosing namespace -- found by running (Round 1 first attempt matched nothing because this was
    // assumed away, not verified). Ast.extractRecords already returns (namespaceIdent * SynTypeDefn
    // list) pairs precisely to carry that namespace alongside; the fully-qualified name has to be
    // reassembled from both halves, not read off the SynTypeDefn alone.
    let getTypeName (SynTypeDefn(componentInfo, _typeDefRepr, _memberDefs, _implicitCtor, _range, _trivia)) =
        let (SynComponentInfo(_attrs, _typeParams, _constraints, longId, _doc, _preferPostfix, _access, _ciRange)) = componentInfo
        longId |> List.map (fun i -> i.idText) |> String.concat "."

    let getFullTypeName (ns: LongIdent) (td: SynTypeDefn) =
        let nsPart = ns |> List.map (fun i -> i.idText) |> String.concat "."
        let typePart = getTypeName td
        if String.IsNullOrEmpty nsPart then typePart else nsPart + "." + typePart

    let getFieldNames (SynTypeDefn(_componentInfo, repr, _memberDefs, _implicitCtor, _range, _trivia)) =
        match repr with
        | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record(_, fields, _), _) ->
            fields
            |> List.choose (fun (SynField.SynField(_, _, idOpt, _, _, _, _, _, _)) -> idOpt |> Option.map (fun i -> i.idText))
        | _ -> []

[<TypeProvider>]
type MyriadPreviewProvider (config: TypeProviderConfig) as this =
    // assemblyReplacementMap: included defensively, mirroring Q016's generative satellite-forwarding
    // provider. NOT confirmed load-bearing here -- an early build attempt hit an FS0074 "must add a
    // reference to assembly 'MyriadPreview.DesignTime'" error that seemed to resolve after adding this,
    // but Q019's own adversarial review (03-review.md) removed it as a controlled single-variable test
    // and all three rounds still passed identically, including a clean dotnet build/run. For an
    // all-obj erased provider (nothing here crosses the design-time/runtime assembly boundary in a way
    // that would need remapping), this argument is harmless but not shown to be necessary; the original
    // fix was most likely a post-hoc misattribution against a multi-variable change or stale build.
    // Left in place as defensive convention, not because it's proven required.
    inherit TypeProviderForNamespaces(config, assemblyReplacementMap = [ ("MyriadPreview.DesignTime", "MyriadPreview.Runtime") ], addDefaultProbingLocation = true)

    let ns = "MyriadPreview.Provided"
    let asm = Assembly.GetExecutingAssembly()

    // Memoization is load-bearing, not an optimization -- Q011's finding (DefineStaticParameters'
    // instantiation function fires more than once per logical check) applies here too, checked
    // directly rather than assumed safe.
    let successCache = Dictionary<string * string, ProvidedTypeDefinition>()
    let watchers = Dictionary<string, FileSystemWatcher>()

    let ensureWatcher (path: string) (onChange: unit -> unit) =
        if not (watchers.ContainsKey path) then
            let dir = Path.GetDirectoryName(path: string)
            let file = Path.GetFileName(path: string)
            let w = new FileSystemWatcher(dir, file)
            w.NotifyFilter <- NotifyFilters.LastWrite ||| NotifyFilters.CreationTime ||| NotifyFilters.Size
            let handler (_: FileSystemEventArgs) = onChange ()
            w.Changed.Add handler
            w.Created.Add handler
            w.EnableRaisingEvents <- true
            watchers.[path] <- w

    let createTypeUncached (typeName: string) (sourcePath: string) (recordName: string) =
        ensureWatcher sourcePath (fun () -> successCache.Clear(); this.Invalidate())

        let parsed, _warnings = Ast.fromFilename sourcePath |> Async.RunSynchronously |> Array.head
        let records = Ast.extractRecords parsed
        let matchOne =
            records
            |> List.collect (fun (ns, tds) -> tds |> List.map (fun td -> AstHelpers.getFullTypeName ns td, td))
            |> List.tryFind (fun (fullName, td) ->
                fullName = recordName || fullName.EndsWith("." + recordName) || AstHelpers.getTypeName td = recordName)
            |> Option.map snd

        match matchOne with
        | None -> failwithf "MyriadPreview: record '%s' not found in '%s' (self-parsed via Myriad.Core.Ast, no compiler resolution attempted)" recordName sourcePath
        | Some td ->
            let fieldNames = AstHelpers.getFieldNames td
            if fieldNames.IsEmpty then
                failwithf "MyriadPreview: record '%s' parsed with zero fields -- not actually a record shape?" recordName

            let t = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>, isErased = true)
            t.AddMember(ProvidedConstructor([ ProvidedParameter("instance", typeof<obj>) ], invokeCode = fun args -> args.[0]))

            for fieldName in fieldNames do
                let prop =
                    ProvidedProperty(
                        fieldName,
                        typeof<obj>,
                        getterCode = fun args ->
                            <@@
                                let o: obj = %%args.[0]
                                let ty = o.GetType()
                                let p = ty.GetProperty(fieldName)
                                if isNull (box p) then
                                    failwithf "MyriadPreview: runtime type '%s' has no property '%s'" (ty.FullName) fieldName
                                p.GetValue(o)
                            @@>)
                t.AddMember prop

            t

    let createType (typeName: string) (sourcePath: string) (recordName: string) =
        let key = (sourcePath, recordName)
        match successCache.TryGetValue key with
        | true, t -> t
        | false, _ ->
            let t = createTypeUncached typeName sourcePath recordName
            successCache.[key] <- t
            t

    let containerType =
        let t = ProvidedTypeDefinition(asm, ns, "Fields", Some typeof<obj>, isErased = true)
        t.DefineStaticParameters(
            [ ProvidedStaticParameter("SourceFilePath", typeof<string>)
              ProvidedStaticParameter("RecordName", typeof<string>) ],
            fun typeName args -> createType typeName (args.[0] :?> string) (args.[1] :?> string))
        t

    do this.AddNamespace(ns, [ containerType ])
