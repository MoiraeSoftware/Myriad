namespace Myriad.Core

open System
open Fantomas.FCS.Syntax
open Fantomas.FCS.Text.Range
open Fantomas.FCS.Text.Position
open Fantomas.FCS.Xml
open Fantomas.FCS.SyntaxTrivia

[<AutoOpen>]
module AstExtensions =
    let private dotsOrCommas ls =
      match ls with
      | [] -> []
      | xs -> [ for _ in 1..xs.Length - 1 do range0 ]

    /// Builds a generic SynType.App node. Use hasAngles=false only for postfix array syntax.
    let private mkGenericType (typeName: SynType) (typeArgs: SynType list) (isPostfix: bool) (hasAngles: bool) =
        SynType.App(
            typeName = typeName,
            typeArgs = typeArgs,
            commaRanges = [],
            isPostfix = isPostfix,
            range = range0,
            greaterRange = (if hasAngles then Some range0 else None),
            lessRange  = (if hasAngles then Some range0 else None)
        )

    type ParsedImplFileInputTrivia with
        static member Zero =
            { ParsedImplFileInputTrivia.ConditionalDirectives = []
              CodeComments = [] }
            
    type SynModuleOrNamespaceTrivia with
        static member Zero =
            { SynModuleOrNamespaceTrivia.LeadingKeyword = SynModuleOrNamespaceLeadingKeyword.None }

    type SynMemberDefnImplicitCtorTrivia with
        static member Zero =
            { SynMemberDefnImplicitCtorTrivia.AsKeyword = Some range0 }
            
    type SynBindingReturnInfoTrivia with
        static member Zero =
            { SynBindingReturnInfoTrivia.ColonRange = Some range0 }

    type Ident with
        static member Create text =
            Ident(text, range0)
        static member CreateLong (text: string) =
            text.Split([|'.'|]) |> List.ofArray |> List.map Ident.Create

    type SynLongIdent with
        static member CreateFromLongIdent (longIdent: LongIdent) =
            SynLongIdent(longIdent, dotsOrCommas longIdent, List.replicate longIdent.Length None)
            
        static member Create (texts) =
            SynLongIdent.CreateFromLongIdent (texts |> List.map Ident.Create)
            
        static member CreateString (text: string) =
            SynLongIdent.CreateFromLongIdent (Ident.CreateLong text)

        member x.AsString =
            let sb = Text.StringBuilder()
            for i in 0 .. x.LongIdent.Length - 2 do
                sb.Append x.LongIdent[i].idText |> ignore
                sb.Append '.' |> ignore
            sb.Append x.LongIdent[x.LongIdent.Length-1].idText |> ignore
            sb.ToString()

    type SynComponentInfo with
        static member Create(id: LongIdent, ?attributes, ?parameters, ?constraints, ?xmldoc, ?preferPostfix, ?access) =
            let attributes = defaultArg attributes SynAttributes.Empty
            let constraints = defaultArg constraints []
            let xmldoc = defaultArg xmldoc PreXmlDoc.Empty
            let preferPostfix = defaultArg preferPostfix false
            let access = defaultArg access None
            let range = range0
            SynComponentInfo(attributes, parameters, constraints,id, xmldoc,preferPostfix, access, range)   
    
    type SynArgPats with
        static member Empty =
            SynArgPats.Pats[]


    type QualifiedNameOfFile with
        static member Create name =
            QualifiedNameOfFile(Ident.Create name)

    type SynMemberFlags with
        static member InstanceMember : SynMemberFlags =
            { IsInstance = true
              MemberKind = SynMemberKind.Member
              IsDispatchSlot = false
              IsOverrideOrExplicitImpl = false
              IsFinal = false
              GetterOrSetterIsCompilerGenerated = false }
        static member StaticMember =
            { SynMemberFlags.InstanceMember with IsInstance = false }

    type SynConst with
        /// Creates a <see href="SynStringKind.Regular">Regular</see> string
        static member CreateString s =
            SynConst.String(s, SynStringKind.Regular, range0)
            
    type SynExprMatchTrivia with
        static member Zero =
            { MatchKeyword = range0
              WithKeyword = range0 }

    type SynExpr with
        static member CreateConst cnst =
            SynExpr.Const(cnst, range0)
        static member CreateConstString s =
            SynExpr.CreateConst (SynConst.CreateString s)
        static member CreateTyped (expr, typ) =
            SynExpr.Typed(expr, typ, range0)
        static member CreateApp (funcExpr, argExpr) =
            SynExpr.App(ExprAtomicFlag.NonAtomic, false, funcExpr, argExpr, range0)
        static member CreateAppInfix (funcExpr, argExpr) =
            SynExpr.App(ExprAtomicFlag.NonAtomic, true, funcExpr, argExpr, range0)
        static member CreateIdent id =
            SynExpr.Ident(id)
        static member CreateIdentString id =
            SynExpr.Ident(Ident.Create id)
        static member CreateLongIdent (isOptional, id, altNameRefCell) =
            SynExpr.LongIdent(isOptional, id, altNameRefCell, range0)
        static member CreateLongIdent id =
            SynExpr.CreateLongIdent(false, id, None)
        static member CreateParen expr =
            SynExpr.Paren(expr, range0, Some range0, range0)
        static member CreateTuple list =
            SynExpr.Tuple(false, list, dotsOrCommas list, range0)
        static member CreateParenedTuple list =
            SynExpr.CreateTuple list
            |> SynExpr.CreateParen
        static member CreateUnit =
            SynExpr.CreateConst SynConst.Unit
            
        static member CreatePipeRight =
            SynExpr.CreateIdent(Ident.Create "op_PipeRight")
            
        static member CreateNull =
            SynExpr.Null(range0)
        static member CreateRecord (fields: list<RecordFieldName * option<SynExpr>>) =
            let fields = fields |> List.map (fun (rfn, synExpr) -> SynExprRecordField (rfn, None, synExpr, None))
            SynExpr.Record(None, None, fields, range0)

        static member CreateRecordUpdate (copyInfo: SynExpr, fieldUpdates) =
            let blockSep = (range0, None) : BlockSeparator
            let fields = fieldUpdates |> List.map (fun (rfn, synExpr) -> SynExprRecordField(rfn, Some range0, synExpr, Some blockSep))
            let copyInfo = Some (copyInfo, blockSep)
            SynExpr.Record(None, copyInfo, fields, range0)

        static member CreateRecordUpdate (copyInfo: SynExpr, fieldUpdates ) =
            let blockSep = (range0, None) : BlockSeparator
            let copyInfo = Some (copyInfo, blockSep)
            SynExpr.Record (None, copyInfo, fieldUpdates, range0)

        /// Creates:
        ///
        /// ```
        /// match matchExpr with
        /// | clause1
        /// | clause2
        /// ...
        /// | clauseN
        /// ```
        static member CreateMatch(matchExpr, clauses) =
            SynExpr.Match(DebugPointAtBinding.Yes range0, matchExpr, clauses, range0, SynExprMatchTrivia.Zero)
            
        static member CreateInstanceMethodCall(instanceAndMethod : SynLongIdent, args) =
            let valueExpr = SynExpr.CreateLongIdent instanceAndMethod
            SynExpr.CreateApp(valueExpr, args)
        /// Creates : `instanceAndMethod()`
        static member CreateInstanceMethodCall(instanceAndMethod : SynLongIdent) =
            SynExpr.CreateInstanceMethodCall(instanceAndMethod, SynExpr.CreateUnit)
        /// Creates : `instanceAndMethod<type1, type2,... type}>(args)`
        static member CreateInstanceMethodCall(instanceAndMethod : SynLongIdent, instanceMethodsGenericTypes, args) =
            let valueExpr = SynExpr.CreateLongIdent instanceAndMethod
            let valueExprWithType = SynExpr.TypeApp(valueExpr, range0, instanceMethodsGenericTypes, dotsOrCommas instanceMethodsGenericTypes, None, range0, range0)
            SynExpr.CreateApp(valueExprWithType, args)
        /// Creates: expr1; expr2; ... exprN
        static member CreateSequential exprs =
            let seqExpr expr1 expr2 = SynExpr.Sequential(DebugPointAtSequential.SuppressBoth, false, expr1, expr2, range0, SynExprSequentialTrivia.Zero)
            let rec inner exprs state =
                match state, exprs with
                | None, [] -> SynExpr.CreateConst SynConst.Unit
                | Some expr, [] -> expr
                | None, [single] -> single
                | None, [one;two] -> seqExpr one two
                | Some exp, [single] -> seqExpr exp single
                | None, head::shoulders::tail ->
                    seqExpr head shoulders
                    |> Some
                    |> inner tail
                | Some expr, head::tail ->
                    seqExpr expr head
                    |> Some
                    |> inner tail
            inner exprs None

        static member CreateWorkflow(ident : SynLongIdent, statements : SynExpr list) =
            let steps = SynExpr.CreateSequential statements
            SynExpr.CreateApp(SynExpr.CreateLongIdent ident, SynExpr.ComputationExpr(false, steps, range0))

    type SynType with
        static member CreateApp (typ, args, ?isPostfix) =
            SynType.App(typ, Some range0, args, dotsOrCommas args, Some range0, (defaultArg isPostfix false), range0)
        static member CreateLongIdent id =
            SynType.LongIdent(id)
        static member CreateLongIdent s =
            SynType.CreateLongIdent(SynLongIdent.CreateString s)
        static member CreateFromLongIdent (parent: LongIdent) =
            SynLongIdent.Create (parent |> List.map (fun i -> i.idText))
            |> SynType.CreateLongIdent
        static member CreateUnit =
            SynType.CreateLongIdent("unit")
        static member CreateFun (fieldTypeIn, fieldTypeOut) =
            SynType.Fun (fieldTypeIn, fieldTypeOut, range0, { ArrowRange = range0 })

        static member Create(name: string) = SynType.CreateLongIdent name

        static member Option(inner: SynType, ?isPostfix : bool) =
            let isPostfix = defaultArg isPostfix false
            mkGenericType (SynType.CreateLongIdent (if isPostfix then "option" else "Option")) [ inner ] isPostfix true

        static member ResizeArray(inner: SynType) =
            mkGenericType (SynType.CreateLongIdent "ResizeArray") [ inner ] false true

        static member Set(inner: SynType) =
            mkGenericType (SynType.CreateLongIdent "Set") [ inner ] false true

        static member NativePointer(inner: SynType, ?isPostfix : bool) =
            mkGenericType (SynType.CreateLongIdent "nativeptr") [ inner ] (defaultArg isPostfix false) true

        static member Option(inner: string, ?isPostfix : bool) =
            let isPostfix = defaultArg isPostfix false
            mkGenericType (SynType.CreateLongIdent (if isPostfix then "option" else "Option")) [ SynType.Create inner ] isPostfix true

        static member Dictionary(key, value) =
            mkGenericType (SynType.LongIdent(SynLongIdent.Create [ "System"; "Collections"; "Generic"; "Dictionary" ])) [ key; value ] false true

        static member Map(key, value) =
            mkGenericType (SynType.CreateLongIdent "Map") [ key; value ] false true

        static member List(inner: SynType, ?isPostfix : bool) =
            let isPostfix = defaultArg isPostfix false
            mkGenericType (SynType.CreateLongIdent (if isPostfix then "list" else "List")) [ inner ] isPostfix true

        static member Array(inner: SynType) =
            mkGenericType (SynType.CreateLongIdent "array") [ inner ] true false

        static member List(inner: string, ?isPostfix : bool) =
            let isPostfix = defaultArg isPostfix false
            mkGenericType (SynType.CreateLongIdent (if isPostfix then "list" else "List")) [ SynType.Create inner ] isPostfix true

        static member DateTimeOffset() =
            SynType.LongIdent(SynLongIdent.Create [ "System"; "DateTimeOffset" ])

        static member DateTime() =
            SynType.LongIdent(SynLongIdent.Create [ "System"; "DateTime" ])

        static member Guid() =
            SynType.LongIdent(SynLongIdent.Create [ "System"; "Guid" ])

        static member Int() =
            SynType.Create "int"

        static member UInt() =
            SynType.Create "uint"

        static member Int8() =
            SynType.Create "int8"

        static member UInt8() =
            SynType.Create "uint8"

        static member Int16() =
            SynType.Create "int16"

        static member UInt16() =
            SynType.Create "uint16"

        static member Int64() =
            SynType.Create "int64"

        static member UInt64() =
            SynType.Create "uint64"

        static member String() =
            SynType.Create "string"

        static member Bool() =
            SynType.Create "bool"

        static member Float() =
            SynType.Create "float"

        static member Float32() =
            SynType.Create "float32"

        static member Double() =
            SynType.Create "float"

        static member Decimal() =
            SynType.Create "decimal"

        static member Unit() =
            SynType.Create "unit"

        static member BigInt() =
            SynType.Create "bigint"

        static member Byte() =
            SynType.Create "byte"

        static member Char() =
            SynType.Create "char"

    type SynArgInfo with
        static member Empty =
            SynArgInfo(SynAttributes.Empty, false, None)
        static member CreateId id =
            SynArgInfo(SynAttributes.Empty, false, Some id)
        static member CreateIdString id =
            SynArgInfo.CreateId(Ident.Create id)

    type SynValInfo with
        static member Empty =
            SynValInfo([], SynArgInfo.Empty)

    type SynMemberDefn with
        static member CreateImplicitCtor (ctorArgs : SynPat list) =
            SynMemberDefn.ImplicitCtor(None, SynAttributes.Empty, SynPat.Tuple(false, ctorArgs, dotsOrCommas ctorArgs, range0), None, PreXmlDoc.Empty, range0, SynMemberDefnImplicitCtorTrivia.Zero)
        static member CreateImplicitCtor() =
            SynMemberDefn.CreateImplicitCtor []

        static member CreateInterface(interfaceType, members) =
            SynMemberDefn.Interface(interfaceType, None, members, range0)



    type SynModuleDecl with

        static member CreateOpen id =
            SynModuleDecl.Open(id, range0)
        static member CreateOpen (fullNamespaceOrModuleName: string) =
            SynModuleDecl.Open(SynOpenDeclTarget.ModuleOrNamespace(SynLongIdent.CreateString fullNamespaceOrModuleName, range0), range0)
        static member CreateOpen (namespaceId: LongIdent) =
            SynModuleDecl.Open(SynOpenDeclTarget.ModuleOrNamespace(SynLongIdent.CreateFromLongIdent namespaceId, range0), range0)
        static member CreateHashDirective (directive, values) =
            SynModuleDecl.HashDirective (ParsedHashDirective (directive, values, range0), range0)

        static member CreateAttribute(ident, expr, isProp, ?target) =
                { SynAttribute.TypeName = ident
                  SynAttribute.ArgExpr = expr
                  SynAttribute.Target = target
                  SynAttribute.AppliesToGetterAndSetter = isProp
                  SynAttribute.Range = range0 }
        static member CreateAttributes(attributes) =
            SynModuleDecl.Attributes(attributes, range0)

    type SynAttributeList with
        static member Create(attrs): SynAttributeList =
            {
                Attributes = attrs
                Range = range0
            }

        static member Create(attr): SynAttributeList =
            {
                Attributes = [ attr ]
                Range = range0
            }

        static member Create([<ParamArray>] attrs): SynAttributeList =
            {
                Attributes = List.ofArray attrs
                Range = range0
            }

    type SynAttribute with
        static member Create(name: string) : SynAttribute =
            {
               AppliesToGetterAndSetter = false
               ArgExpr = SynExpr.Const (SynConst.Unit, range0)
               Range = range0
               Target = None
               TypeName = SynLongIdent.CreateString name
            }

        static member Create(name: string, argument: string) : SynAttribute =
            {
               AppliesToGetterAndSetter = false
               ArgExpr = SynExpr.Const (SynConst.String(argument, SynStringKind.Regular, range0), range0)
               Range = range0
               Target = None
               TypeName = SynLongIdent.CreateString name
            }

        static member Create(name: string, argument: bool) : SynAttribute =
            {
               AppliesToGetterAndSetter = false
               ArgExpr = SynExpr.Const (SynConst.Bool argument, range0)
               Range = range0
               Target = None
               TypeName = SynLongIdent.CreateString name
            }

        static member Create(name: string, argument: int) : SynAttribute =
            {
               AppliesToGetterAndSetter = false
               ArgExpr = SynExpr.Const (SynConst.Int32 argument, range0)
               Range = range0
               Target = None
               TypeName = SynLongIdent.CreateString name
            }

        static member Create(name: string, argument: SynConst) : SynAttribute =
            {
               AppliesToGetterAndSetter = false
               ArgExpr = SynExpr.Const (argument, range0)
               Range = range0
               Target = None
               TypeName = SynLongIdent.CreateString name
            }

        static member Create(name: Ident, argument: SynConst) : SynAttribute =
            {
               AppliesToGetterAndSetter = false
               ArgExpr = SynExpr.Const (argument, range0)
               Range = range0
               Target = None
               TypeName = SynLongIdent([name], [ ], [])
            }

        static member Create(name: Ident list, argument: SynConst) : SynAttribute =
            {
               AppliesToGetterAndSetter = false
               ArgExpr = SynExpr.Const (argument, range0)
               Range = range0
               Target = None
               TypeName = SynLongIdent(name, [ ], [])
            }

        static member RequireQualifiedAccess() =
            SynAttribute.Create("RequireQualifiedAccess")

        static member CompiledName(valueArg: string) =
            SynAttribute.Create("CompiledName", valueArg)

    type PreXmlDoc with
        static member Create (lines: string list) =
            let lines = List.toArray lines
            let lineMaxIndex = Array.length lines - 1
            let s = mkPos 0 0
            let e = mkPos lineMaxIndex 0
            let containingRange = mkRange "" s e
            PreXmlDoc.Create(lines, containingRange)

        static member Create (docs: string option) =
            PreXmlDoc.Create [
                if docs.IsSome
                then docs.Value
            ]

        static member Create(docs: string) =
            PreXmlDoc.Create [
                if not (String.IsNullOrWhiteSpace docs)
                then docs
            ]

    type SynSimplePat with
        static member CreateTyped(ident, ``type``) =
            let ssp = SynSimplePat.Id(ident, None, false, false, false, range0)
            SynSimplePat.Typed(ssp, ``type``, range0 )

        static member CreateId(ident, ?altNameRefCell, ?isCompilerGenerated, ?isThis, ?isOptional) =
            SynSimplePat.Id(ident, altNameRefCell,
                            Option.defaultValue false isCompilerGenerated,
                            Option.defaultValue false isThis,
                            Option.defaultValue false isOptional,
                            range0)

    type SynSimplePats with
        static member Create(patterns) =
            SynSimplePats.SimplePats(patterns, dotsOrCommas patterns, range0)

    type SynTypeDefn with
        static member CreateFromRepr(name : Ident, repr : SynTypeDefnRepr, ?members : SynMemberDefns, ?xmldoc : PreXmlDoc) =
            let name = SynComponentInfo.Create([name], xmldoc = defaultArg xmldoc PreXmlDoc.Empty)
            let extraMembers, trivia =
                match members with
                | None -> SynMemberDefns.Empty, SynTypeDefnTrivia.Zero
                | Some defns -> defns, { SynTypeDefnTrivia.Zero with WithKeyword = Some range0 }

            SynTypeDefn(name, repr, extraMembers, None, range0, trivia)

        static member CreateUnion (name : Ident, cases : SynUnionCase list, ?members : SynMemberDefns, ?xmldoc : PreXmlDoc) =
            let repr = SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Union(None, cases, range0), range0)
            SynTypeDefn.CreateFromRepr(name, repr, defaultArg members SynMemberDefns.Empty, defaultArg xmldoc PreXmlDoc.Empty)

        static member  CreateRecord (name : Ident, fields : SynField seq, ?members : SynMemberDefns, ?xmldoc : PreXmlDoc) =
            let repr = SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record(None, Seq.toList fields, range0), range0)
            SynTypeDefn.CreateFromRepr(name, repr, defaultArg members SynMemberDefns.Empty, defaultArg xmldoc PreXmlDoc.Empty)

    type SynField with
        static member Create(fieldType : SynType, ?name : Ident, ?attributes : SynAttributes, ?access : SynAccess, ?xmldoc : PreXmlDoc) =
            let xmldoc = defaultArg xmldoc PreXmlDoc.Empty
            let attributes = defaultArg attributes SynAttributes.Empty 
            SynField(attributes, false, name, fieldType, false, xmldoc, access, range0, SynFieldTrivia.Zero)
    
    type SynUnionCase with
        static member Create(name : Ident, fields : SynField list, ?attributes : SynAttributes, ?access : SynAccess, ?xmldoc : PreXmlDoc) =
            let trivia : SynUnionCaseTrivia = { BarRange = None }
            let attributes = defaultArg attributes SynAttributes.Empty
            let xmldoc = defaultArg xmldoc PreXmlDoc.Empty
            SynUnionCase(attributes, SynIdent(name, None) , SynUnionCaseKind.Fields(fields), xmldoc, access, range0, trivia)