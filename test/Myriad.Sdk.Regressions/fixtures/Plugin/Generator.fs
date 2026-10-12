namespace RegressionPlugin

open Myriad.Core

[<MyriadGenerator("regression")>]
type RegressionGen() =
    interface IMyriadGenerator with
        member _.ValidInputExtensions = seq { ".fs" }

        member _.Generate(_context: GeneratorContext) =
            // run.fsx edits this marker to check that a rebuilt plugin regenerates the output.
            let marker = "MARKER_A"
            let names = [ "one"; "two" ] |> List.map (fun n -> n.ToUpperInvariant())
            Output.Source $"module Generated\n\nlet marker = \"%s{marker}\"\nlet names = %A{names}\n"
