namespace DiagTPImplementation

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Quotations
open Fantomas.FCS.Syntax
open Myriad.Core
open SharedAnalysis

[<TypeProvider>]
type DiagTPProvider (config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(config, assemblyReplacementMap = [ ("DiagTP.DesignTime", "DiagTP.Runtime") ], addDefaultProbingLocation = true)

    let ns = "DiagTP.Provided"
    let asm = Assembly.GetExecutingAssembly()

    // ---- Round 1 + 2: a fixed, no-static-parameter type isolating the obsolete-attribute mechanism
    // itself (use-site anchoring, severity control) and the synthetic-member discoverability question,
    // independent of Q019's self-parsing (already validated separately).
    let sampleType =
        let t = ProvidedTypeDefinition(asm, ns, "Sample", Some typeof<obj>, isErased = true)
        t.AddMember(ProvidedConstructor([], invokeCode = fun _ -> <@@ obj() @@>))

        let goodProp = ProvidedProperty("Good", typeof<int>, getterCode = fun _ -> <@@ 1 @@>)
        t.AddMember goodProp

        let badProp = ProvidedProperty("Bad", typeof<int>, getterCode = fun _ -> <@@ 2 @@>)
        badProp.AddObsoleteAttribute("Q020 test message", false)
        t.AddMember badProp

        let badErrorProp = ProvidedProperty("BadError", typeof<int>, getterCode = fun _ -> <@@ 3 @@>)
        badErrorProp.AddObsoleteAttribute("Q020 test message", true)
        t.AddMember badErrorProp

        // Round 2: a synthetic member whose name carries a diagnostic message directly, for a finding
        // with no natural member to hang off. Not stamped obsolete itself -- the name IS the payload.
        let syntheticName = "warning MYR099: synthetic diagnostic with no natural member"
        let syntheticProp = ProvidedProperty(syntheticName, typeof<int>, getterCode = fun _ -> <@@ 0 @@>)
        t.AddMember syntheticProp

        t

    // ---- Round 3: Q019's self-parsing Fields<SourceFilePath, RecordName> shape, extended to call
    // SharedAnalysis.Analyze.analyze (the SAME function EmitterA calls) and poison any member whose
    // field the analysis flagged, instead of leaving it plain -- the "one analysis, two channels can't
    // disagree" claim.
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

    let getTypeName (SynTypeDefn(componentInfo, _, _, _, _, _)) =
        let (SynComponentInfo(_, _, _, longId, _, _, _, _)) = componentInfo
        longId |> List.map (fun i -> i.idText) |> String.concat "."

    let getFullTypeName (ns: LongIdent) (td: SynTypeDefn) =
        let nsPart = ns |> List.map (fun i -> i.idText) |> String.concat "."
        let typePart = getTypeName td
        if String.IsNullOrEmpty nsPart then typePart else nsPart + "." + typePart

    let getFieldNames (SynTypeDefn(_, repr, _, _, _, _)) =
        match repr with
        | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record(_, fields, _), _) ->
            fields
            |> List.choose (fun (SynField.SynField(_, _, idOpt, _, _, _, _, _, _)) -> idOpt |> Option.map (fun i -> i.idText))
        | _ -> []

    let createFieldsTypeUncached (typeName: string) (sourcePath: string) (recordName: string) =
        ensureWatcher sourcePath (fun () -> successCache.Clear(); this.Invalidate())

        let parsed, _warnings = Ast.fromFilename sourcePath |> Async.RunSynchronously |> Array.head
        let records = Ast.extractRecords parsed
        let matchOne =
            records
            |> List.collect (fun (ns, tds) -> tds |> List.map (fun td -> getFullTypeName ns td, td))
            |> List.tryFind (fun (fullName, td) ->
                fullName = recordName || fullName.EndsWith("." + recordName) || getTypeName td = recordName)
            |> Option.map snd

        match matchOne with
        | None -> failwithf "DiagTP: record '%s' not found in '%s'" recordName sourcePath
        | Some td ->
            let fieldNames = getFieldNames td
            // The identical analysis call EmitterA makes -- same compiled SharedAnalysis.dll, same
            // function, same file. This is what makes the "cannot disagree" claim structural rather
            // than coincidental.
            let diagnosticsByMember =
                Analyze.analyze sourcePath
                |> List.choose (fun d -> d.Member |> Option.map (fun m -> m, d))
                |> Map.ofList

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
                                    failwithf "DiagTP: runtime type '%s' has no property '%s'" (ty.FullName) fieldName
                                p.GetValue(o)
                            @@>)
                match diagnosticsByMember.TryFind fieldName with
                | Some d ->
                    let isError = (d.Severity = Severity.Error)
                    prop.AddObsoleteAttribute(d.Message, isError)
                | None -> ()
                t.AddMember prop

            t

    let createFieldsType (typeName: string) (sourcePath: string) (recordName: string) =
        let key = (sourcePath, recordName)
        match successCache.TryGetValue key with
        | true, t -> t
        | false, _ ->
            let t = createFieldsTypeUncached typeName sourcePath recordName
            successCache.[key] <- t
            t

    let fieldsContainerType =
        let t = ProvidedTypeDefinition(asm, ns, "Fields", Some typeof<obj>, isErased = true)
        t.DefineStaticParameters(
            [ ProvidedStaticParameter("SourceFilePath", typeof<string>)
              ProvidedStaticParameter("RecordName", typeof<string>) ],
            fun typeName args -> createFieldsType typeName (args.[0] :?> string) (args.[1] :?> string))
        t

    do this.AddNamespace(ns, [ sampleType; fieldsContainerType ])
