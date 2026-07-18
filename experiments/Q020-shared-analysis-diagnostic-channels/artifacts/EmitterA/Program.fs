module EmitterA.Program

open SharedAnalysis

// Q020 Round 3, emitter A: a standalone, Myriad-CLI-style console program that renders
// SharedAnalysis.Analyze.analyze's output as a canonical MSBuild diagnostic line, anchored at the
// field's DECLARATION range. Deliberately NOT wired into Myriad's real CLI (src/Myriad/Program.fs) --
// this demonstrates the format and the shared-analysis claim, wiring it into the real CLI is a named
// follow-up if this quartet ships, not part of the spike.
let severityWord =
    function
    | Warning -> "warning"
    | Error -> "error"

[<EntryPoint>]
let main argv =
    match argv with
    | [| filePath |] ->
        let diags = Analyze.analyze filePath
        for d in diags do
            let (sl, sc, el, ec) = d.Range
            // MSBuild canonical format: origin(line,col,line,col): category code: text
            // (columns are 0-based in FCS ranges; MSBuild/editors expect 1-based columns, so +1).
            printfn "%s(%d,%d,%d,%d): %s %s: %s" filePath sl (sc + 1) el (ec + 1) (severityWord d.Severity) d.Code d.Message
        0
    | _ ->
        eprintfn "usage: EmitterA <path-to-.fs-file>"
        2
