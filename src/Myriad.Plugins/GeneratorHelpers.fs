namespace Myriad.Plugins

open Fantomas.FCS.Syntax
open Myriad.Core
open Myriad.Core.Ast

module internal GeneratorHelpers =

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

    /// Resolves a DU case identifier, optionally qualifying it with the parent type name
    /// when RequireQualifiedAccess is present.
    let resolveCaseIdent (requiresQualifiedAccess: bool) (parent: LongIdent) (id: Ident) : SynLongIdent =
        let parts =
            if requiresQualifiedAccess then
                (parent |> List.map (fun i -> i.idText)) @ [id.idText]
            else
                [id.idText]
        SynLongIdent.Create parts
