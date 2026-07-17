namespace Myriad
open System
open System.IO
open Fantomas.FCS.Syntax
open Argu
open Fantomas.Core
open Myriad.Core
open Tomlyn
open System.Collections.Generic
open System.Diagnostics
open Myriad.Core.Ast
open Tomlyn.Model
open McMaster.NETCore.Plugins

module Implementation =
    let findPlugins (isVerbose: bool) (path: string) =

        let loader = PluginLoader.CreateFromAssemblyFile(path, [|typeof<MyriadGeneratorAttribute>; typeof<IMyriadGenerator> |], fun config -> config.PreferSharedTypes <- true)
        let assembly = loader.LoadDefaultAssembly()
        let types =
            try
                assembly.GetTypes()
            with
            | :? Reflection.ReflectionTypeLoadException as ex ->
                if isVerbose then
                    eprintfn "ReflectionTypeLoadException loading '%s':" path
                    for loaderEx in ex.LoaderExceptions do
                        if loaderEx <> null then
                            eprintfn "  - %s" (loaderEx.Message)
                ex.Types |> Array.filter (fun t -> t <> null)
        let gens =
            [ for t in types do
                if t.GetCustomAttributes(typeof<MyriadGeneratorAttribute>, true).Length > 0
                then yield t ]
        gens
        
    let getConfigHandler (isVerbose: bool) (config: Model.TomlTable option) (name: string) =
        if isVerbose then
            printfn $"Looking for: %s{name} in config"

        match config with
        | Some config ->
            match config.TryGetValue name with
            | true, x -> //when x.Kind = Model.ObjectKind.Table ->
                try
                    let x = (x :?> Model.TomlTable)
                    let x = (x :> IDictionary<string,obj>)
                    x |> Seq.map (|KeyValue|)
                with
                | _ ->
                    printfn "Failure while creating config sequence"
                    Seq.empty
            | _ ->
                printfn $"Failed to find key %s{name}"
                Seq.empty
        | None ->
            if isVerbose then
                printfn "No configuration passed"
            Seq.empty

    let getConfig (configFile) =
        let configFile =
            configFile
            |> Option.defaultValue (Path.Combine(Environment.CurrentDirectory, "myriad.toml"))
        if File.Exists configFile then
            let configFileCnt = File.ReadAllText configFile
            let tomlDocument = Toml.Parse(configFileCnt, configFile)
            let tomlTable = tomlDocument.ToModel()
            Some tomlTable
        else None

    /// One file's worth of codegen work - either the single --inputfile/--outputfile pair from the
    /// command line, or one [[unit]] entry from a --manifest file. Batching many of these into one
    /// process (one MSBuild <Exec> per project instead of one per file) is the reason this is its own
    /// type rather than reading each field off `results` inline, the way the single-file path used to.
    type CodegenUnit =
        { InputFile: string
          OutputFile: string option
          ConfigKey: string option
          AdditionalParams: IDictionary<string, string>
          InlineGeneration: bool
          GeneratorFilters: string list }

    /// Mirrors Argu's EqualsAssignment behaviour for `--additionalparams key=value`: split once on the
    /// first '=' into a single (key, value) entry. The MSBuild-side manifest writer flattens a file's
    /// possibly-multiple <MyriadParams> entries into one 'key=value|key2=value2'-shaped string (see
    /// Myriad.Sdk.targets), but this matches the CLI's existing single-entry behaviour rather than
    /// silently changing it - nothing in this repo reads AdditionalParameters today, so there's no
    /// evidence multi-key was ever meant to work differently, and this isn't the place to guess.
    let parseAdditionalParams (flattened: string) : IDictionary<string, string> =
        if String.IsNullOrEmpty flattened then
            dict []
        else
            match flattened.IndexOf '=' with
            | -1 -> dict []
            | idx -> dict [ flattened.Substring(0, idx), flattened.Substring(idx + 1) ]

    let parseManifest (path: string) : CodegenUnit list =
        let model = Toml.Parse(File.ReadAllText path, path).ToModel()
        match model.TryGetValue "unit" with
        | true, (:? Tomlyn.Model.TomlTableArray as units) ->
            [ for unit in units do
                let getStr key =
                    match unit.TryGetValue key with
                    | true, (v: obj) -> v :?> string
                    | _ -> ""
                let inputFile = getStr "inputfile"
                let configKeyRaw = getStr "configkey"
                let paramsRaw = getStr "params"
                let generatorsRaw = getStr "generators"
                let inlineGeneration =
                    match unit.TryGetValue "inline" with
                    | true, (v: obj) -> v :?> bool
                    | _ -> false
                { InputFile = inputFile
                  OutputFile = Some(getStr "outputfile")
                  ConfigKey = (if configKeyRaw = "" then None else Some configKeyRaw)
                  AdditionalParams = parseAdditionalParams paramsRaw
                  InlineGeneration = inlineGeneration
                  GeneratorFilters =
                    if generatorsRaw = "" then
                        []
                    else
                        generatorsRaw.Split '|' |> Array.toList } ]
        | _ -> []

module Main =
    open Implementation
    type Arguments =
        | InputFile of string
        | OutputFile of string
        | ConfigFile of string
        | ConfigKey of string
        | [<Hidden>] ContextFile of string
        | Plugin of string
        | [<CustomCommandLine("--wait-for-debugger")>] WaitForDebugger
        | Verbose
        | [<EqualsAssignment;CustomCommandLine("--additionalparams")>] AdditionalParams of key:string * value:string
        | InlineGeneration
        | [<CustomCommandLine("--generator-filter")>] GeneratorFilter of string list
        | [<CustomCommandLine("--manifest")>] Manifest of string
    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | InputFile _ -> "Specify a file to use as input."
                | OutputFile _ -> "Specify the file name that the generated code will be written to."
                | ConfigFile _ -> "Specify a myriad.toml file to use as config."
                | ConfigKey _ -> "Specify a key in the config that will be passed to the generator."
                | ContextFile _ -> "Specify a context file for the generator to use."
                | Plugin _ -> "Register an assembly plugin."
                | WaitForDebugger -> "Wait for the debugger to attach."
                | Verbose -> "Verbose output."
                | AdditionalParams _ -> "Specify additional parameters."
                | InlineGeneration -> "Generate code for the input file at the end of the input file."
                | GeneratorFilter _-> "A list of generators to run, only the specifid generators will be run, all others will be ignored."
                | Manifest _ -> "Process every codegen unit listed in this TOML manifest file in one process, instead of a single --inputfile/--outputfile pair."



    [<EntryPoint>]
    let main argv =
        let parser = ArgumentParser.Create<Arguments>(programName = "myriad")

        try
            let results = parser.Parse argv
            let verbose = results.Contains Verbose

            if results.Contains WaitForDebugger then
                while not(Debugger.IsAttached) do
                  Threading.Thread.Sleep(100)
                Debugger.Break()

            let config = getConfig(results.TryGetResult ConfigFile)
            let plugins = results.GetResults Plugin
            let contextFile = results.TryGetResult ContextFile

            let projectContext =
                match contextFile with
                | Some file when File.Exists file ->
                    let result = Toml.Parse(File.ReadAllText(file), file).ToModel()
                    let project = result.["project"] :?> String
                    let projectPath = result.["projectPath"] :?> String
                    let refs = result.["referencePaths"] :?> TomlArray |> Seq.cast<string> |> Array.ofSeq
                    let compileBefore = result.["compileBefore"] :?> TomlArray |> Seq.cast<string> |> Array.ofSeq
                    let compile = result.["compile"] :?> TomlArray |> Seq.cast<string> |> Array.ofSeq
                    let compileAfter = result.["compileAfter"] :?> TomlArray |> Seq.cast<string> |> Array.ofSeq
                    let defineConstants = result.["defineConstants"] :?> TomlArray |> Seq.cast<string> |> Array.ofSeq
                    Some {project = project; projectPath = projectPath; refs = refs; compileBefore = compileBefore; compile = compile; compileAfter = compileAfter; defineConstants = defineConstants}
                | _ -> None


            if verbose then
                printfn "------------------------------------\nMyriad starting, plugins found:"
                plugins |> List.iter (printfn "- '%s'")

            let generators =
                plugins
                |> List.collect (findPlugins verbose)

            if verbose then
                printfn "Generators found:"
                generators |> List.iter (fun t -> printfn $"- %s{t.FullName}")

            let units =
                match results.TryGetResult Manifest with
                | Some manifestFile -> parseManifest manifestFile
                | None ->
                    let inputFile =
                        match results.TryGetResult InputFile with
                        | Some f -> f
                        | None -> failwith "Error: either --inputfile or --manifest must be specified."
                    [ { InputFile = inputFile
                        OutputFile = results.TryGetResult OutputFile
                        ConfigKey = results.TryGetResult ConfigKey
                        AdditionalParams = results.GetResults AdditionalParams |> dict
                        InlineGeneration = results.Contains InlineGeneration
                        GeneratorFilters = (results.GetResults GeneratorFilter) |> List.concat } ]

            // One process, looped over every unit in `units`, instead of one process per file - the
            // single-file CLI path above just builds a one-element list and falls through to the same
            // loop, so its behaviour is unchanged.
            let processUnit (unit: CodegenUnit) =
                let configHandler = getConfigHandler verbose config

                let runGenerator (genType: Type) =
                    let rawInstance = Activator.CreateInstance(genType)
                    let instance = rawInstance :?> IMyriadGenerator

                    if verbose then
                        printfn $"Executing Generator: %s{genType.FullName} for %s{unit.InputFile}"

                    let result, errors =
                        try
                            if instance.ValidInputExtensions |> Seq.contains (Path.GetExtension(unit.InputFile))
                            then
                                let context = GeneratorContext.Create(unit.ConfigKey, configHandler, unit.InputFile, projectContext, unit.AdditionalParams)

                                match rawInstance with
                                | :? IMyriadGeneratorWithDiagnostics as diagnosticsInstance ->
                                    let output, diagnostics = diagnosticsInstance.GenerateWithDiagnostics(context)

                                    for diagnostic in diagnostics do
                                        printfn "%s" (Diagnostics.format unit.InputFile diagnostic)

                                    match diagnostics |> List.tryFind (fun d -> d.Severity = DiagnosticSeverity.Error) with
                                    | Some errorDiagnostic ->
                                        let info = $"%s{genType.Name} Failure"
                                        None, Some ($"%s{info}%s{Environment.NewLine}!CompilationError%s{Environment.NewLine}%s{errorDiagnostic.Message}")
                                    | None -> output, None
                                | _ -> Some (instance.Generate(context)), None
                            else None, None
                        with
                        | exc ->
                            let info = $"%s{genType.Name} Failure"
                            let message = exc.ToString()
                            None, Some ($"%s{info}%s{Environment.NewLine}!CompilationError%s{Environment.NewLine}%s{message}")

                    if verbose then printfn $"Result: %A{result}"

                    genType, result, errors

                let generated =
                    if verbose then
                        if unit.GeneratorFilters.IsEmpty then
                            printfn "GeneratorFilters <No filters>"
                        else printfn $"GeneratorFilters %A{unit.GeneratorFilters}"
                    generators
                    |> List.filter (fun g -> if unit.GeneratorFilters.IsEmpty then
                                                 if verbose then
                                                    printfn $"- %s{g.Name}: is included"
                                                 true
                                             else
                                                 let isOk = unit.GeneratorFilters |> List.contains g.Name
                                                 if verbose then
                                                     if isOk then
                                                         printfn $"- %s{g.Name}: is included"
                                                     else
                                                         printfn $"- %s{g.Name}: is excluded"
                                                 isOk)
                    |> List.map runGenerator

                let formattedCode =
                    let outputCode =
                        let filename =
                            if unit.InlineGeneration then unit.InputFile
                            else if unit.OutputFile.IsSome then unit.OutputFile.Value
                            else failwith "Error: No OutputFile was included, and --selfgeneration was not specified."

                        let cfg = Myriad.Core.EditorConfig.readConfiguration filename

                        generated
                        |> List.map (fun (genType, output, errors) ->
                            //if theres an error just fail here
                            match errors with
                            | Some error -> failwithf $"Error in %A{genType}: %s{error}"
                            | None -> ()

                            match output with
                            | Some(Output.Ast ast) ->
                                let parseTree = ParsedInput.ImplFile(ParsedImplFileInput.CreateFs(filename, modules = ast))
                                if verbose then
                                    printfn $"""
Parsed Input :------------------------------------
%A{parseTree}"
--------------------------------------------------
About to format generated ouptut from %A{genType}"""

                                CodeFormatter.FormatASTAsync(parseTree, cfg) |> Async.RunSynchronously
                            | Some (Output.Source source) -> source
                            | None -> "")

                    outputCode |> String.concat Environment.NewLine

                let code = Generation.getHeaderedCode formattedCode
                if verbose then
                    printfn $"Generated Code:\n%A{code}"

                if unit.InlineGeneration then
                    let tempFile = Path.GetTempFileName()
                    let linesToKeep = Generation.linesToKeep unit.InputFile

                    if verbose then printfn $"Inline generation: Writing to temp file: '%s{tempFile}'"
                    File.WriteAllLines(tempFile, seq{ yield! linesToKeep; yield! code} )
                    if verbose then printfn $"Inline generation: Removing input file: '%s{tempFile}'"
                    File.Delete(unit.InputFile)
                    if verbose then
                        printfn $"Inline generation: Renaming temp file to input file: '%s{tempFile}' -> '%s{unit.InputFile}'"
                    File.Move(tempFile, unit.InputFile)
                else
                    match unit.OutputFile with
                    | Some filename ->
                        if verbose then printfn $"Code generation: Writing output file: '%s{filename}'"
                        File.WriteAllLines(filename, code)
                    | None -> failwith "Error: No OutputFile was included, and --inlinegeneration was not specified."

            units |> List.iter processUnit

            0 // return an integer exit code

        with
        | :? ArguParseException as ae when ae.ErrorCode = ErrorCode.HelpText ->
            printfn $"%s{ae.Message}"
            3
        | :? ArguParseException as ae ->
            printfn $"%s{ae.Message}"
            match ae.ErrorCode with
            | ErrorCode.HelpText -> 0
            | _ -> 2
        | :? FileNotFoundException as fnf ->
            printfn $"ERROR: inputfile %s{fnf.FileName} doesn not exist\n%s{parser.PrintUsage()}"
            4
        | error ->
            printfn $"OTHER: %A{error}"
            1
