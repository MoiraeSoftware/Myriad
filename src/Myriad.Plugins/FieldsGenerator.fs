namespace Myriad.Plugins

open Fantomas.FCS.Syntax
open Myriad.Core
open Myriad.Core.Ast

module internal Create =
    open Fantomas.FCS.Text.Range

    let createFieldMap (parent: LongIdent) (field: SynField)  =
        let (SynField.SynField(_,_,id,_,_,_,_,_,_)) = field
        let fieldName = match id with None -> failwith "no field name" | Some f -> f

        let recordType = SynType.CreateFromLongIdent parent

        let varName = "x"
        let pattern =
            let name = SynLongIdent.CreateString fieldName.idText
            let arg =
                let named = SynPat.CreateNamed(Ident.Create varName)
                SynPat.CreateTyped(named, recordType)
                |> SynPat.CreateParen

            SynPat.CreateLongIdent(name, [arg])

        let expr =
            let ident = SynLongIdent.Create [varName; fieldName.idText]
            SynExpr.CreateLongIdent(false, ident, None)

        let valData =
            let argInfo = SynArgInfo.CreateIdString "x"
            let valInfo = SynValInfo.SynValInfo([[argInfo]], SynArgInfo.Empty)
            SynValData.SynValData(None, valInfo, None)

        SynModuleDecl.CreateLet [SynBinding.Let(pattern = pattern, expr = expr, valData = valData)]

    let createCreate (parent: LongIdent) (fields: SynField list) =
        let varIdent = SynLongIdent.CreateString "create"

        let recordType = SynType.CreateFromLongIdent parent

        let pattern =
            let arguments =
                fields
                |> List.map (fun (SynField.SynField(_,_,id,typ,_,_,_,_,_)) ->
                                 let name = SynPat.CreateNamed(Ast.Ident.asCamelCase id.Value)
                                 SynPat.CreateTyped(name, typ) |> SynPat.CreateParen)

            SynPat.CreateLongIdent(varIdent, arguments)

        let expr =
            let fields =
                fields
                |> List.map (fun (SynField.SynField(_,_,id,_,_,_,_,_,_)) ->
                                 let fieldIdent = match id with None -> failwith "no field name" | Some f -> f
                                 let name = SynLongIdent.Create([fieldIdent.idText])
                                 let ident = SynExpr.CreateIdent(Ast.Ident.asCamelCase fieldIdent)
                                 SynExprRecordField.SynExprRecordField(RecordFieldName(name, true), Some range0, Some ident, None))

            let newRecord = SynExpr.Record(None, None, fields, range0 )
            SynExpr.CreateTyped(newRecord, recordType)

        let returnTypeInfo = SynBindingReturnInfo.Create(recordType)
        SynModuleDecl.CreateLet [SynBinding.Let(pattern = pattern, expr = expr, returnInfo = returnTypeInfo)]

    let createMap (recordId: LongIdent) (recordFields: SynField list) : SynModuleDecl =
        let varIdent = SynLongIdent.CreateString "map"
        let recordPrimeIdent =  Ident.Create "record'"

        let createFieldMapNameIdent (id: Ident option ) =
            Ident.Create ("map" + id.Value.idText)

        let pattern =
            let arguments =
                recordFields
                |> List.map (fun (SynField.SynField(_,_,id, fieldType,_,_,_,_,_)) ->
                                 let funType = SynType.CreateFun(fieldType, fieldType)
                                 let ident = createFieldMapNameIdent id
                                 let name = SynPat.CreateNamed(ident)
                                 SynPat.CreateTyped(name, funType)
                                 |> SynPat.CreateParen)

            let recordParam =
                let name = SynPat.CreateNamed(recordPrimeIdent)
                let typ =
                    SynLongIdent.Create (recordId |> List.map (fun i -> i.idText))
                    |> SynType.CreateLongIdent

                SynPat.CreateTyped(name, typ)
                |> SynPat.CreateParen

            let allArgs = [yield! arguments; yield recordParam]

            SynPat.CreateLongIdent(varIdent, allArgs)

        let expr =
            let fieldUpdates =
                let mapField (SynField(_,_,id,_,_,_,_,_,_)) =
                    let lid = SynLongIdent.Create [id.Value.idText]
                    let rfn = RecordFieldName(lid, true)

                    let update =
                        let funcExpr = SynExpr.CreateIdent (createFieldMapNameIdent id)
                        let argExpr =
                            let longIdentWithDots = SynLongIdent.Create [recordPrimeIdent.idText; id.Value.idText]
                            SynExpr.CreateLongIdent(false, longIdentWithDots, None)
                        SynExpr.CreateApp(funcExpr, argExpr)

                    SynExprRecordField.SynExprRecordField(rfn, Some range0, Some update, None)

                let arguments =
                    recordFields
                    |> List.map mapField

                arguments

            SynExpr.Record(None, None, fieldUpdates, range0 )

        let returnTypeInfo =
            SynLongIdent.Create (recordId |> List.map (fun i -> i.idText))
            |> SynType.CreateLongIdent
            |> SynBindingReturnInfo.Create

        SynModuleDecl.CreateLet
            [ SynBinding.Let(pattern = pattern, expr = expr, returnInfo = returnTypeInfo) ]

    let createRecordModule (namespaceId: LongIdent) (typeDefn: SynTypeDefn) (config: (string * obj) seq) =
        let (SynTypeDefn(synComponentInfo, synTypeDefnRepr, _members, _implicitCtor, _, _)) = typeDefn
        let (SynComponentInfo(_attributes, _typeParams, _constraints, recordId, _doc, _preferPostfix, _access, _)) = synComponentInfo
        match synTypeDefnRepr with
        | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record(_accessibility, recordFields, _recordRange), _) ->

            let openParent = SynModuleDecl.CreateOpen namespaceId

            let fieldMaps = recordFields |> List.map (createFieldMap recordId)

            let create = createCreate recordId recordFields

            let map = createMap recordId recordFields

            let decls = [
                yield openParent
                yield! fieldMaps
                yield create
                yield map ]

            let info = SynComponentInfo.Create recordId
            let mdl = SynModuleDecl.CreateNestedModule(info,  decls)
            let fieldsNamespace = GeneratorConfig.getOrDefault "namespace" "UnknownNamespace" config

            SynModuleOrNamespace.CreateNamespace(Ident.CreateLong fieldsNamespace, isRecursive = true, decls = [mdl])
        | _ -> failwithf "Not a record type"

[<MyriadGenerator("fields")>]
type FieldsGenerator() =

    interface IMyriadGenerator with
        member _.ValidInputExtensions = seq {".fs"}
        member _.Generate(context: GeneratorContext) =
            //_myriadConfigKey is not currently used but could be a failover config section to use when the attribute passes no config section, or used as a root config
            GeneratorHelpers.generateModules<Generator.FieldsAttribute> context Ast.extractRecords Create.createRecordModule
