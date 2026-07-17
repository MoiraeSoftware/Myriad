namespace Myriad.Core

open Fantomas.FCS.Text.Range

module Diagnostics =
    let private severityText =
        function
        | DiagnosticSeverity.Error -> "error"
        | DiagnosticSeverity.Warning -> "warning"
        | DiagnosticSeverity.Info -> "info"

    /// Renders a MyriadDiagnostic as a canonical MSBuild diagnostic line
    /// (`path(line,col,line,col): severity CODE: message`), anchored at the diagnostic's own
    /// range when present, falling back to line 1 of `inputFilename` otherwise (MSBuild/editors
    /// still need a valid path to anchor the squiggle/error-list entry to).
    let format (inputFilename: string) (diagnostic: MyriadDiagnostic) : string =
        let path, startLine, startCol, endLine, endCol =
            match diagnostic.Range with
            | Some r -> r.FileName, r.StartLine, r.StartColumn + 1, r.EndLine, r.EndColumn + 1
            | None -> inputFilename, 1, 1, 1, 1

        sprintf
            "%s(%d,%d,%d,%d): %s %s: %s"
            path
            startLine
            startCol
            endLine
            endCol
            (severityText diagnostic.Severity)
            diagnostic.Code
            diagnostic.Message
