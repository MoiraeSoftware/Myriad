// Round C, built for real (not just reasoned from source, per 03-review's own criticism of the
// first pass at this quartet): a syntax-only cross-file resolver for the SAME nested-dispatch
// question Round A/B answered with typed access. Uses Myriad's OWN parsing and attribute-matching
// logic, copied verbatim from src/Myriad.Core/Ast.fs (credited inline below), not a reimplementation
// built to be easy to beat. Fantomas.Core 7.0.5 matches Myriad's own pin (paket.lock).

open System
open System.Diagnostics
open Fantomas.FCS.Syntax
open Fantomas.Core

// ---------------------------------------------------------------------------
// Copied verbatim from src/Myriad.Core/Ast.fs (Ast module), unmodified except
// for dropping the enclosing `module Ast =` and Fantomas.FCS.Xml/SyntaxTrivia
// opens this file doesn't need. This is Myriad's real attribute-matching logic,
// including its own documented caveat on the line marked below.
// ---------------------------------------------------------------------------

let typeNameMatches (attributeType: Type) (attrib: SynAttribute) =
    match attrib.TypeName with
    | SynLongIdent(ident, _range, _) ->
        let ident =
            ident
            |> List.map (fun id -> id.ToString())
            |> String.concat "."
            |> function s -> if s.EndsWith "Attribute" then s else s + "Attribute"
        // Myriad's own comment, verbatim: "Support both full name and opened namespace - this is
        // not 100% accurate, and it can't be without full type information, but it should be good
        // enough in practice". Round C exists to test that claim concretely.
        if ident.Contains "." then
            attributeType.FullName.Replace("+", ".").EndsWith(ident)
        else
            ident = attributeType.Name

type SynComponentInfo with
    member x.attributes =
        let (SynComponentInfo(attributes, _typeParams, _constraints, _recordIdent, _doc, _preferPostfix, _access, _ciRange)) = x
        attributes

let hasAttribute<'a> (SynTypeDefn(synComponentInfo, _typeDefRepr, _memberDefs, _implicitCtor, _range, _trivia)) =
    synComponentInfo.attributes
    |> List.collect (fun n -> n.Attributes)
    |> List.exists (typeNameMatches typeof<'a>)

let rec private extractTypesFromDecls (moduleDecls: SynModuleDecl list) (ns: LongIdent) =
    [ for moduleDecl in moduleDecls do
          match moduleDecl with
          | SynModuleDecl.Types(types, _) -> yield (ns, types)
          | SynModuleDecl.NestedModule(SynComponentInfo(_, _, _, longId, _, _, _, _), _, decls, _, _, _) ->
              let combined = longId |> List.append ns
              yield! extractTypesFromDecls decls combined
          | _ -> () ]

let extractTypeDefn (ast: ParsedInput) =
    [ match ast with
      | ParsedInput.ImplFile(ParsedImplFileInput(_name, _isScript, _qualifiedNameOfFile, _scopedPragmas, _hashDirectives, modules, _g, _, _)) ->
          for SynModuleOrNamespace(namespaceId, _isRec, _isModule, moduleDecls, _preXmlDoc, _attributes, _access, _, _) in modules do
              yield! extractTypesFromDecls moduleDecls namespaceId
      | _ -> () ]

let isRecord (SynTypeDefn(_componentInfo, typeDefRepr, _memberDefs, _, _, _)) =
    match typeDefRepr with
    | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record _, _) -> true
    | _ -> false

let private filterTypes predicate types =
    types |> List.map (fun (ns, types) -> ns, types |> List.filter predicate)

let extractRecords (ast: ParsedInput) =
    extractTypeDefn ast |> filterTypes isRecord

// ---------------------------------------------------------------------------
// New code for Round C: not in Myriad today. What a plugin author would have
// to write themselves to do cross-file lookup, since GeneratorContext/
// GeneratorHelpers.generateModules only ever hand a generator one file
// (verified in 01-design.md).
// ---------------------------------------------------------------------------

type FieldsAttribute(configKey: string) =
    inherit Attribute()

let parse (src: string) : ParsedInput =
    match CodeFormatter.ParseAsync(false, src) |> Async.RunSynchronously with
    | results when results.Length > 0 -> fst results.[0]
    | _ -> failwith "parse produced no result"

let recordName (SynTypeDefn(componentInfo, _, _, _, _, _)) =
    let (SynComponentInfo(_, _, _, longId, _, _, _, _)) = componentInfo
    (List.last longId).idText

let rec simpleFieldTypeName (t: SynType) : string =
    match t with
    | SynType.LongIdent(SynLongIdent(ids, _, _)) -> (List.last ids).idText
    | SynType.App(typeName, _, _, _, _, _, _) -> simpleFieldTypeName typeName
    | _ -> "<unrecognized-type-shape>"

let fields (SynTypeDefn(_, typeDefRepr, _, _, _, _)) =
    match typeDefRepr with
    | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record(_, recordFields, _), _) -> recordFields
    | _ -> []

/// Naive syntax-only cross-file lookup: for a field's syntactic type name, search every parsed
/// file for ANY record declaration with a matching simple name, and report whether each carries
/// the Fields attribute. This is what a plugin author gets from Myriad's Ast module today - no
/// open-tracking, no scope resolution, because Myriad's Ast module has none either.
let findRecordsNamed (name: string) (parsedFiles: (string * ParsedInput) list) =
    [ for (path, ast) in parsedFiles do
          for (_ns, typeDefns) in extractRecords ast do
              for td in typeDefns do
                  if recordName td = name then
                      yield path, hasAttribute<FieldsAttribute> td ]

let printCandidates label candidates =
    printfn "%s: %d candidate(s) found by simple-name search" label (List.length candidates)
    for (path, attributed) in candidates do
        printfn "    %s -> Fields-attributed: %b" path attributed

// ---------------------------------------------------------------------------
// Scenario 1: unambiguous - one file per type, matches Q002's typed-side setup exactly.
// ---------------------------------------------------------------------------

let domain1Src =
    """module Domain1
type FieldsAttribute(configKey: string) = inherit System.Attribute()
[<Fields("fields")>]
type Address = { Street: string; City: string }
"""

let person1Src =
    """module Person
open Domain1
[<Fields("fields")>]
type Person = { Name: string; HomeAddress: Address }
"""

printfn "=== Scenario 1: unambiguous (one Address type) ==="
let sw1 = Stopwatch.StartNew()
let parsedDomain1 = parse domain1Src
let parsedPerson1 = parse person1Src
let personRecord1 = extractRecords parsedPerson1 |> List.collect snd |> List.find (fun td -> recordName td = "Person")
let homeAddressFieldType1 =
    fields personRecord1
    |> List.pick (fun (SynField.SynField(_, _, id, fieldType, _, _, _, _, _)) -> if id.Value.idText = "HomeAddress" then Some fieldType else None)
let targetName1 = simpleFieldTypeName homeAddressFieldType1
let candidates1 = findRecordsNamed targetName1 [ "Domain1.fs", parsedDomain1; "Person.fs", parsedPerson1 ]
sw1.Stop()
printCandidates "Scenario 1" candidates1
printfn "Elapsed: %dms" sw1.ElapsedMilliseconds
printfn ""

// ---------------------------------------------------------------------------
// Scenario 2: the adversarial case - TWO records named "Address" in different modules.
// Person.fs opens Domain1, intending Domain1.Address. A naive simple-name search (what
// Myriad's Ast module supports today, with zero enhancement) cannot tell them apart.
// ---------------------------------------------------------------------------

let domain2Src =
    """module Domain2
type Address = { Zip: string }
"""

printfn "=== Scenario 2: adversarial (two Address types, different modules, Person opens Domain1) ==="
let parsedDomain2 = parse domain2Src

let candidatesOrderA = findRecordsNamed "Address" [ "Domain1.fs", parsedDomain1; "Domain2.fs", parsedDomain2; "Person.fs", parsedPerson1 ]
printCandidates "File order [Domain1; Domain2; Person]" candidatesOrderA

let candidatesOrderB = findRecordsNamed "Address" [ "Domain2.fs", parsedDomain2; "Domain1.fs", parsedDomain1; "Person.fs", parsedPerson1 ]
printCandidates "File order [Domain2; Domain1; Person] (only the file ORDER changed)" candidatesOrderB

let firstMatchA = candidatesOrderA |> List.tryHead
let firstMatchB = candidatesOrderB |> List.tryHead
printfn ""
printfn "\"First match wins\" strategy result depends on file processing order:"
printfn "  order A picks: %A" firstMatchA
printfn "  order B picks: %A" firstMatchB
printfn "  Same input, same field, DIFFERENT answer depending on an arbitrary detail (file list order)"
printfn "  that has nothing to do with F#'s actual scoping rules. Person.fs's `open Domain1` means"
printfn "  the CORRECT answer is always Domain1's attributed Address - a naive simple-name search"
printfn "  has no way to know that; it would need to parse Person.fs's `open` declarations AND"
printfn "  implement F#'s open/shadowing resolution order to get this right reliably."
