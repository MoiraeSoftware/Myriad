namespace SharedAnalysis

open Fantomas.FCS.Syntax
open Myriad.Core

type Severity =
    | Warning
    | Error

type DiagnosticInfo =
    { Code: string
      Severity: Severity
      Message: string
      /// (startLine, startCol, endLine, endCol), 1-based lines, 0-based cols (FCS convention).
      Range: int * int * int * int
      Member: string option }

// Q020: the one analysis function both channels (Myriad-CLI-style emitter, and DiagTP's live
// obsolete-attribute channel) call -- neither computes anything independently, so they cannot
// disagree about *what* the problem is, only about how each renders it.
module Analyze =
    let private primitives =
        set [ "string"; "int"; "int32"; "int64"; "float"; "double"; "decimal"; "bool"; "byte"
              "sbyte"; "int16"; "uint16"; "uint32"; "uint64"; "char"; "obj"; "unit" ]

    let private typeName (t: SynType) =
        match t with
        | SynType.LongIdent(SynLongIdent(ids, _, _)) ->
            ids |> List.map (fun i -> i.idText) |> String.concat "." |> Some
        | _ -> None

    /// Real, non-contrived condition (Q019's own named open question): a field whose declared type is
    /// itself another record defined in the SAME file. Such a field cannot be resolved to a real CLR
    /// type by a same-file-uncompiled self-parsing provider (Q019's exact limitation) -- worth a
    /// diagnostic naming the field and the unresolvable type, rather than silently exposing it as obj.
    let analyze (filePath: string) : DiagnosticInfo list =
        let parsed, _warnings = Ast.fromFilename filePath |> Async.RunSynchronously |> Array.head
        let records = Ast.extractRecords parsed
        let recordNames =
            records
            |> List.collect snd
            |> List.choose (fun (SynTypeDefn(ci, _, _, _, _, _)) ->
                let (SynComponentInfo(_, _, _, longId, _, _, _, _)) = ci
                match longId with
                | [] -> None
                | _ -> Some (longId |> List.last).idText)
            |> Set.ofList

        [ for (_ns, tds) in records do
            for td in tds do
                let (SynTypeDefn(_, repr, _, _, _, _)) = td
                match repr with
                | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record(_, fields, _), _) ->
                    for field in fields do
                        let (SynField.SynField(_, _, idOpt, fieldType, _, _, _, fieldRange, _)) = field
                        match idOpt, typeName fieldType with
                        | Some id, Some tn when not (primitives.Contains tn) && recordNames.Contains tn ->
                            let r = fieldType.Range
                            yield
                                { Code = "MYR012"
                                  Severity = Warning
                                  Message =
                                    sprintf
                                        "field '%s' has type '%s', which is declared in this same file and cannot be resolved without a real build; it will be exposed as 'obj'"
                                        id.idText tn
                                  Range = (r.StartLine, r.StartColumn, r.EndLine, r.EndColumn)
                                  Member = Some id.idText }
                            ignore fieldRange
                        | _ -> ()
                | _ -> () ]
