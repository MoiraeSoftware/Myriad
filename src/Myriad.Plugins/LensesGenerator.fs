namespace Myriad.Plugins

open System
open Fantomas.FCS.Syntax
open Fantomas.FCS.Xml
open Myriad.Core
open Myriad.Core.Ast
open Fantomas.FCS.Text.Range
open Fantomas.FCS.SyntaxTrivia

module internal CreateLenses =
    let private wrap (wrapperName : Option<string>) lens =
        match wrapperName with
        | Some name when not (String.IsNullOrWhiteSpace(name)) ->
            let wrapperVar = SynExpr.CreateLongIdent (false, SynLongIdent.CreateString name, None)
            SynExpr.App (ExprAtomicFlag.NonAtomic, false, wrapperVar, SynExpr.CreateParen lens, range0)
        | _ -> lens

    let private createLensForRecordField (parent: LongIdent) (wrapperName : Option<string>) (aetherStyle: bool) (field: SynField) =
        let (SynField.SynField(_,_,id,fieldType,_,_,_,_,_)) = field
        let fieldName = match id with None -> failwith "no field name" | Some f -> f

        let recordType = SynType.CreateFromLongIdent parent
                    
        let letPat = SynPat.CreateNamed fieldName
        let lambdaGetBody = SynExpr.CreateLongIdent(SynLongIdent.Create ["x"; fieldName.idText])
        let lambdaGetPats = [GeneratorHelpers.createTypedNamedParen (Ident.Create "x") recordType]
        
        let lambdaSetBody =
            let innerPats =
                if aetherStyle then
                    [GeneratorHelpers.createTypedNamedParen (Ident.Create "value") fieldType
                     GeneratorHelpers.createTypedNamedParen (Ident.Create "x") recordType]
                else
                    [GeneratorHelpers.createTypedNamedParen (Ident.Create "x") recordType
                     GeneratorHelpers.createTypedNamedParen (Ident.Create "value") fieldType]
                    
            let innerBody =
                let copySrc = SynExpr.CreateLongIdent(false, SynLongIdent.Create ["x"], None)
                let recordUpdateName :RecordFieldName = (SynLongIdent.CreateString fieldName.idText, true)
                let fieldList = [(recordUpdateName, Some(SynExpr.Ident (Ident.Create "value")))]
                SynExpr.CreateRecordUpdate (copySrc, fieldList)
                
            SynExpr.CreateLambda(pats = innerPats, body = innerBody)
        let lambdaSetPats = []
                                              
        let letBody =
            SynExpr.CreateTuple[
                SynExpr.CreateParen(SynExpr.CreateLambda(pats = lambdaGetPats, body = lambdaGetBody))
                SynExpr.CreateParen(SynExpr.CreateLambda(pats = lambdaSetPats, body = lambdaSetBody))] |> wrap wrapperName
            
        SynModuleDecl.CreateLet [SynBinding.Let(pattern = letPat, expr = letBody)]

    let private createLensForDU (requiresQualifiedAccess : bool) (parent: LongIdent) (wrapperName : Option<string>) (du : SynUnionCase) =
        let id = GeneratorHelpers.getCaseIdent du
        let (SynUnionCase.SynUnionCase(_,_,duType,_,_,_,_)) = du
        let (SynField.SynField(_,_,_,fieldType,_,_,_,_,_)) =
            match duType with
            | SynUnionCaseKind.Fields [singleCase] -> singleCase
            | SynUnionCaseKind.Fields (_ :: _) -> failwith "It is not possible to create a lens for a DU with several cases"
            | _ -> failwithf "Unsupported type"

        let duType = SynType.CreateFromLongIdent parent

        let getterName = Ident("getter", range0)
        let pattern =
            SynPat.CreateLongIdent(SynLongIdent.CreateString "Lens'", [])

        // The name of the DU case, optionally preceded by the name of the DU itself, if fully qualified access is required
        let fullCaseName = GeneratorHelpers.resolveCaseIdent requiresQualifiedAccess parent id

        let lensExpression =
            let matchCase =
                let caseVariableName = "x"
                let args = [SynPat.CreateLongIdent (SynLongIdent.CreateString caseVariableName, [])]
                let matchCaseIdent = SynPat.CreateLongIdent(fullCaseName, args)

                let rhs = SynExpr.CreateIdent (Ident.Create caseVariableName)
                SynMatchClause.Create(matchCaseIdent, None, rhs)

            let getterArgName = "x"
            let matchOn =
                let ident = SynLongIdent.CreateString getterArgName
                SynExpr.CreateLongIdent(false, ident, None)

            let matchExpression = SynExpr.CreateMatch(matchOn, [matchCase])

            let setter =
                let valueIdent = Ident.Create "value"

                let valueArgPatterns = [GeneratorHelpers.createTypedNamedParen valueIdent fieldType]

                let duType = SynType.CreateFromLongIdent parent

                let createCase = SynExpr.App (ExprAtomicFlag.NonAtomic, false, SynExpr.LongIdent (false, fullCaseName, None, range0), SynExpr.Ident valueIdent, range0)
                
                let innerLambdaWithValue = SynExpr.CreateLambda([], createCase) //inner does not have pats as they are pushed in via the outer lambda

                let getArgs = [GeneratorHelpers.createTypedNamedParen (Ident.Create "_") duType
                               GeneratorHelpers.createTypedNamedParen valueIdent fieldType] //inner lambdas pat ∆

                SynExpr.CreateLambda(pats = getArgs, body = innerLambdaWithValue)

            let tuple = SynExpr.CreateTuple [ SynExpr.Ident getterName; setter ]

            let getterLet =
                let valData = SynValData.SynValData(None, SynValInfo.Empty, None)
                let synPat = GeneratorHelpers.createTypedNamedParen (Ident.Create "x") duType

                let synPat = SynPat.LongIdent (SynLongIdent.CreateString "getter", None, None, SynArgPats.Pats [synPat], None, range0)

                let trivia =
                  { SynBindingTrivia.LeadingKeyword = SynLeadingKeyword.Let range0
                    InlineKeyword = None
                    EqualsRange = Some range0 }
                SynBinding.SynBinding (None, SynBindingKind.Normal, false, false, [], PreXmlDoc.Empty, valData, synPat, None, matchExpression, range0, DebugPointAtBinding.NoneAtDo, trivia)

            let lens = SynExpr.LetOrUse (false, false, [getterLet], tuple, range0, { InKeyword = None })

            lens |> wrap wrapperName

        SynModuleDecl.CreateLet [ SynBinding.Let(pattern = pattern, expr = lensExpression) ]
    let private updateLastItem list updater =
        let folder item state =
            match state with
            | [] -> [updater item]
            | l -> item :: l

        List.foldBack folder list []

    let private (|LongIdentLid|) (ident : SynLongIdent) =
        ident.LongIdent

    let private (|SynTypeAppTypeName|_|) (expr : SynType) =
        match expr with
        | SynType.App (name, _, _, _, _, _, _) -> Some name
        | _ -> None

    let createLensModule (namespaceId: LongIdent) (typeDefn: SynTypeDefn) (attr: SynAttribute) (usePipedSetter: bool) =
        let (SynTypeDefn(synComponentInfo, synTypeDefnRepr, _members, _implicitCtor, _range, _trivia)) = typeDefn
        let (SynComponentInfo(_attributes, _typeParams, _constraints, recordId, _doc, _preferPostfix, _access, _range)) = synComponentInfo

        // Append "Lenses" to the module name
        let moduleIdent = updateLastItem recordId (fun i -> Ident.Create (sprintf $"%s{i.idText}Lenses"))

        let wrapperName =
            match attr.ArgExpr with
            | SynExpr.Const _
            | SynExpr.Paren(SynExpr.Const _,_,_,_) -> None
            | SynExpr.Paren(SynExpr.Tuple(_,[_thisIsTheConfig; SynExpr.Const(SynConst.String(s,_synStringKind,_), _)],_,_),_,_,_) -> Some s
            | SynExpr.Paren(SynExpr.Tuple(_,[_thisIsTheConfig
                                             SynExpr.TypeApp (SynExpr.Ident ident, _, [SynTypeAppTypeName(SynType.LongIdent longIdent)], _, _, _, _)],_,_),_,_,_)
                                             when ident.idText = "typedefof" || ident.idText = "typeof" ->
                                             Some longIdent.AsString
            | expr-> failwithf $"Unsupported syntax of specifying the wrapper name for type %A{recordId}.\nExpr: %A{expr}"

        let openParent = SynModuleDecl.CreateOpen namespaceId
        let moduleInfo = SynComponentInfo.Create moduleIdent

        match synTypeDefnRepr with
        | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record(_accessibility, recordFields, _recordRange), _range) ->
            let fieldLenses = recordFields |> List.map (createLensForRecordField recordId wrapperName usePipedSetter)
            let declarations = [yield openParent; yield! fieldLenses ]
            SynModuleDecl.CreateNestedModule(moduleInfo, declarations)

        | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Union(_accessibility, [singleCase], _recordRange), _range) ->
            let requiresQualifiedAccess = Ast.getAttribute<RequireQualifiedAccessAttribute> typeDefn |> Option.isSome
            let lens = createLensForDU requiresQualifiedAccess recordId wrapperName singleCase
            let declarations = [ openParent; lens ]
            SynModuleDecl.CreateNestedModule(moduleInfo, declarations)

        | _ -> failwithf $"%A{recordId} is not a record type."

[<MyriadGenerator("lenses")>]
type LensesGenerator() =

    interface IMyriadGenerator with
        member _.ValidInputExtensions = seq {".fs"}
        member _.Generate(context: GeneratorContext) =
            //context.ConfigKey is not currently used but could be a failover config section to use when the attribute passes no config section, or used as a root config
            let ast, _ = GeneratorHelpers.parseInputAst context

            let processTypeList namespaceAndTypes =
                namespaceAndTypes
                |> List.collect (
                    fun (ns, types) ->
                    types
                    |> List.choose (fun t ->
                        let attr = Ast.getAttribute<Generator.LensesAttribute> t
                        Option.map (fun a -> t, a) attr)
                    |> List.map (fun (typeDefn, attrib) ->
                        let config = Generator.getConfigFromAttribute<Generator.LensesAttribute> context.ConfigGetter typeDefn
                        let typeNamespace = GeneratorConfig.getOrDefault "namespace" "UnknownNamespace" config
                        let usePipedSetter = GeneratorConfig.getOrDefault "pipedsetter" false config
                        let synModule = CreateLenses.createLensModule ns typeDefn attrib usePipedSetter
                        SynModuleOrNamespace.CreateNamespace(Ident.CreateLong typeNamespace, isRecursive = true, decls = [synModule])))

            let recordsModules = processTypeList (Ast.extractRecords ast)
            let duModules = processTypeList (Ast.extractDU ast)

            Output.Ast [yield! recordsModules; yield! duModules]
