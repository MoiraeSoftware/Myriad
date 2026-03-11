namespace Myriad.Plugins

open Fantomas.FCS.Syntax
open Myriad.Core
open Myriad.Core.Ast

module internal GeneratorHelpers =

    /// Builds a qualified SynLongIdent for a DU case identifier, respecting RequireQualifiedAccess.
    let resolveCaseIdent (requiresQualifiedAccess: bool) (parent: LongIdent) (id: Fantomas.FCS.Syntax.Ident) : SynLongIdent =
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
