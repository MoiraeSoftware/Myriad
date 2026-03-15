namespace Myriad.Plugins

open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Myriad.Core
open Myriad.Core.Ast

module internal CreateDUModule =
    open Fantomas.FCS.Text.Range

    let createDuInputPattern (varIdent: SynLongIdent) (duType: SynType) : SynPat =
        let inputIdent = "x"
        let ident = Ident(inputIdent, range0)
        let name = SynPat.CreateNamed(ident)
        let args = SynPat.CreateTyped(name, duType) |> SynPat.CreateParen
        SynPat.CreateLongIdent(varIdent, [args])

    let createMatchOnIdent (inputIdent: string) : SynExpr =
        let ident = SynLongIdent.CreateString inputIdent
        SynExpr.CreateLongIdent(false, ident, None)

    let createCaseMatchClause (requiresQualifiedAccess: bool) (parent: LongIdent) (id: Ident) (hasFields: bool) (rhs: SynExpr) : SynMatchClause =
        let indent = GeneratorHelpers.resolveCaseIdent requiresQualifiedAccess parent id
        let args = if hasFields then [SynPat.CreateWild] else []
        let p = SynPat.CreateLongIdent(indent, args)
        SynMatchClause.Create(p, None, rhs)

    let createDuLetBinding (varName: string) (inputType: SynType) (returnType: SynType) (buildMatchClauses: unit -> SynMatchClause list) : SynModuleDecl =
        let varIdent = SynLongIdent.CreateString varName
        let inputIdent = "x"
        let pattern = createDuInputPattern varIdent inputType
        let matchOn = createMatchOnIdent inputIdent
        let expr = SynExpr.Match(DebugPointAtBinding.NoneAtLet, matchOn, buildMatchClauses(), range0, SynExprMatchTrivia.Zero)
        let returnTypeInfo = SynBindingReturnInfo.Create returnType
        SynModuleDecl.CreateLet [SynBinding.Let(pattern = pattern, expr = expr, returnInfo = returnTypeInfo)]

    let createToString (requiresQualifiedAccess: bool) (parent: LongIdent) (cases: SynUnionCase list) =
        let duType = SynType.CreateFromLongIdent parent
        createDuLetBinding "toString" duType (SynType.String()) (fun () ->
            cases
            |> List.map (fun unionCase ->
                let id = GeneratorHelpers.getCaseIdent unionCase
                let rhs = SynExpr.CreateConst(SynConst.CreateString id.idText)
                createCaseMatchClause requiresQualifiedAccess parent id unionCase.HasFields rhs
            )
        )

    let createFromString (requiresQualifiedAccess: bool) (parent: LongIdent) (cases: SynUnionCase list) =
        let duType = SynType.CreateFromLongIdent parent
        let inputType = SynLongIdent.CreateString "string" |> SynType.CreateLongIdent
        createDuLetBinding "fromString" inputType (SynType.Option duType) (fun () ->
            let matches =
                cases
                //Only provide `fromString` for cases with no fields
                |> List.filter (fun c -> not c.HasFields)
                |> List.map (fun unionCase ->
                    let id = GeneratorHelpers.getCaseIdent unionCase
                    let con = SynConst.CreateString id.idText
                    let pat = SynPat.CreateConst(con)
                    let rhs =
                        let f = SynExpr.Ident (Ident("Some", range0))
                        let fullCaseName = GeneratorHelpers.resolveCaseIdent requiresQualifiedAccess parent id
                        let x = SynExpr.CreateLongIdent fullCaseName
                        SynExpr.App(ExprAtomicFlag.NonAtomic, false, f, x, range0)
                    SynMatchClause.Create(pat, None, rhs)
                )
            let wildCase =
                let rhs = SynExpr.Ident (Ident("None", range0))
                SynMatchClause.Create(SynPat.CreateWild, None, rhs)
            [yield! matches; wildCase]
        )

    let createToTag (requiresQualifiedAccess: bool) (parent: LongIdent) (cases: SynUnionCase list) =
        let duType = SynType.CreateFromLongIdent parent
        createDuLetBinding "toTag" duType (SynType.Int()) (fun () ->
            cases
            |> List.mapi (fun i case ->
                let id = GeneratorHelpers.getCaseIdent case
                let rhs = SynExpr.Const(SynConst.Int32 i, range0)
                createCaseMatchClause requiresQualifiedAccess parent id case.HasFields rhs
            )
        )

    let createIsCase (requiresQualifiedAccess: bool) (parent: LongIdent) (cases: SynUnionCase list) =
        let duType = SynType.CreateFromLongIdent parent
        [ for case in cases do
            let id = GeneratorHelpers.getCaseIdent case
            createDuLetBinding $"is%s{id.idText}" duType (SynType.Bool()) (fun () ->
                let matchCase =
                    let rhs = SynExpr.CreateConst(SynConst.Bool true)
                    createCaseMatchClause requiresQualifiedAccess parent id case.HasFields rhs
                let wildCase =
                    let rhs = SynExpr.CreateConst(SynConst.Bool false)
                    SynMatchClause.Create(SynPat.CreateWild, None, rhs)
                [matchCase; wildCase]
            )
        ]

    let createDuModule (namespaceId: LongIdent) (typeDefn: SynTypeDefn) (config: (string * obj) seq) =
        let (SynTypeDefn(synComponentInfo, synTypeDefnRepr, _members, _implicitCtor, _range, _trivia)) = typeDefn
        let (SynComponentInfo(_attributes, _typeParams, _constraints, recordId, _doc, _preferPostfix, _access, _range)) = synComponentInfo
        match synTypeDefnRepr with
        | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Union(_accessibility, cases, _recordRange), _range) ->

            let openParent = SynModuleDecl.CreateOpen namespaceId
            let requiresQualifiedAccess =
                Ast.hasAttribute<RequireQualifiedAccessAttribute> typeDefn
                || config |> Seq.exists (fun (n, v) -> n = "alwaysFullyQualify" && v :?> bool = true)
            
            let toString = createToString requiresQualifiedAccess recordId cases
            let fromString = createFromString requiresQualifiedAccess recordId cases
            let toTag = createToTag requiresQualifiedAccess recordId cases
            let isCase = createIsCase requiresQualifiedAccess recordId cases

            let declarations = [
                openParent
                toString
                fromString
                toTag
                yield! isCase ]

            let info = SynComponentInfo.Create recordId

            let mdl = SynModuleDecl.CreateNestedModule(info,  declarations)
            let dusNamespace = GeneratorConfig.getOrDefault "namespace" "UnknownNamespace" config
            SynModuleOrNamespace.CreateNamespace(Ident.CreateLong dusNamespace, isRecursive = true, decls = [mdl])
        | _ -> failwithf "Not a record type"

[<MyriadGenerator("dus")>]
type DUCasesGenerator() =

    interface IMyriadGenerator with
        member _.ValidInputExtensions = seq {".fs"}
        member _.Generate(context: GeneratorContext) =
            //context.ConfigKey is not currently used but could be a failover config section to use when the attribute passes no config section, or used as a root config
            GeneratorHelpers.generateModules<Generator.DuCasesAttribute> context Ast.extractDU CreateDUModule.createDuModule