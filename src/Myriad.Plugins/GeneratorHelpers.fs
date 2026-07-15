namespace Myriad.Plugins

open Fantomas.FCS.Syntax
open Myriad.Core
open Myriad.Core.Ast

module internal GeneratorHelpers =

    /// Extracts the Ident from a SynUnionCase.
    let getCaseIdent (SynUnionCase.SynUnionCase(_, SynIdent(id, _), _, _, _, _, _)) : Ident = id

    /// Extracts the field name `Ident` from a `SynField` id option, raising when absent.
    let getFieldName (id: Ident option) : Ident =
        match id with
        | None -> failwith "no field name"
        | Some f -> f

    /// Resolves the fully-qualified SynLongIdent for a DU case identifier.
    /// When RequireQualifiedAccess is in effect the parent type name is prepended to the case name.
    let resolveCaseIdent (requiresQualifiedAccess: bool) (parent: LongIdent) (id: Ident) : SynLongIdent =
        let parts =
            if requiresQualifiedAccess then
                (parent |> List.map (fun i -> i.idText)) @ [id.idText]
            else
                [id.idText]
        SynLongIdent.Create parts


    /// Parses the input file specified in the generator context and returns the first parsed AST.
    let parseInputAst (context: GeneratorContext) =
        Ast.fromFilename context.InputFilename
        |> Async.RunSynchronously
        |> Array.head

    /// Filters a list of (namespace, types) pairs, keeping only those namespaces that
    /// contain at least one type decorated with the given attribute.
    let filterByAttribute<'A> (namespacedTypes: (LongIdent * SynTypeDefn list) list) =
        namespacedTypes
        |> List.choose (fun (ns, types) ->
            match types |> List.filter Ast.hasAttribute<'A> with
            | [] -> None
            | types -> Some (ns, types))

    /// Creates `(name: typ)` — a parenthesised, typed, named pattern.
    /// Eliminates the repeated `SynPat.CreateParen(SynPat.CreateTyped(SynPat.CreateNamed …, …))` idiom.
    let createTypedNamedParen (name: Ident) (typ: SynType) : SynPat =
        SynPat.CreateNamed name
        |> fun p -> SynPat.CreateTyped(p, typ)
        |> SynPat.CreateParen

    /// Reads the "namespace" key from config (defaulting to "UnknownNamespace") and wraps
    /// the given module declaration in a recursive SynModuleOrNamespace.
    let createNamespacedModule (config: (string * obj) seq) (mdl: SynModuleDecl) : SynModuleOrNamespace =
        let ns = GeneratorConfig.getOrDefault "namespace" "UnknownNamespace" config
        SynModuleOrNamespace.CreateNamespace(Ident.CreateLong ns, isRecursive = true, decls = [mdl])

    /// Runs the standard generator pipeline: parse input AST, extract types, filter by attribute,
    /// and collect modules. Eliminates the boilerplate shared by DUCasesGenerator and FieldsGenerator.
    let generateModules<'Attr> (context: GeneratorContext) (extract: ParsedInput -> (LongIdent * SynTypeDefn list) list) (create: LongIdent -> SynTypeDefn -> (string * obj) seq -> SynModuleOrNamespace) : Output =
        let ast, _ = parseInputAst context
        let namespacedTypes = extract ast |> filterByAttribute<'Attr>
        let modules =
            namespacedTypes
            |> List.collect (fun (ns, types) ->
                types |> List.map (fun t ->
                    let config = Generator.getConfigFromAttribute<'Attr> context.ConfigGetter t
                    create ns t config))
        Output.Ast modules

    /// Runs the standard generator pipeline for generators that also require the matched SynAttribute
    /// to construct their output modules. Behaves like <c>generateModules</c> but additionally
    /// resolves and forwards the matched attribute to the create function.
    let generateModulesWithAttr<'Attr> (context: GeneratorContext) (extract: ParsedInput -> (LongIdent * SynTypeDefn list) list) (create: LongIdent -> SynTypeDefn -> SynAttribute -> (string * obj) seq -> SynModuleOrNamespace) : Output =
        let ast, _ = parseInputAst context
        let namespacedTypes = extract ast |> filterByAttribute<'Attr>
        let modules =
            namespacedTypes
            |> List.collect (fun (ns, types) ->
                types |> List.choose (fun t ->
                    Ast.getAttribute<'Attr> t
                    |> Option.map (fun attr ->
                        let config = Generator.getConfigFromAttribute<'Attr> context.ConfigGetter t
                        create ns t attr config)))
        Output.Ast modules
