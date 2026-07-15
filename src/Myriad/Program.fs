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

module Main =
    open Implementation
    type Arguments =
        | [<Mandatory>] InputFile of string
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

            let inputFile = results.GetResult InputFile
            let outputFile = results.TryGetResult OutputFile
            let configKey = results.TryGetResult ConfigKey
            let config = getConfig(results.TryGetResult ConfigFile)
            let additionalParams = results.GetResults AdditionalParams |> dict
            let inlineGeneration = results.Contains InlineGeneration
            let plugins = results.GetResults Plugin
            let contextFile = results.TryGetResult ContextFile
            let generatorsFilters = (results.GetResults GeneratorFilter) |> List.concat
                        
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

            let runGenerator (inputFile: string) (genType: Type) =
                let instance = Activator.CreateInstance(genType) :?> IMyriadGenerator

                let configHandler = getConfigHandler verbose config

                if verbose then
                    printfn $"Executing Generator: %s{genType.FullName} for %s{inputFile}"

                let result, errors =
                    try
                        if instance.ValidInputExtensions |> Seq.contains (Path.GetExtension(inputFile))
                        then
                            let context = GeneratorContext.Create(configKey, configHandler, inputFile, projectContext, additionalParams)
                            Some (instance.Generate(context)), None
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
                    if generatorsFilters.IsEmpty then
                        printfn "GeneratorFilters <No filters>"
                    else printfn $"GeneratorFilters %A{generatorsFilters}"
                generators
                |> List.filter (fun g -> if generatorsFilters.IsEmpty then
                                             if verbose then
                                                printfn $"- %s{g.Name}: is included"
                                             true
                                         else
                                             let isOk = generatorsFilters |> List.contains g.Name
                                             if verbose then
                                                 if isOk then
                                                     printfn $"- %s{g.Name}: is included"
                                                 else
                                                     printfn $"- %s{g.Name}: is excluded"
                                             isOk)
                |> List.map (runGenerator inputFile)

            let formattedCode =
                let outputCode =
                    let filename =
                        if inlineGeneration then inputFile
                        else if outputFile.IsSome then outputFile.Value
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
            
            let code =  Generation.getHeaderedCode formattedCode
            if verbose then
                printfn $"Generated Code:\n%A{code}"

            if inlineGeneration then
                let tempFile = Path.GetTempFileName()
                let linesToKeep = Generation.linesToKeep inputFile

                if verbose then printfn $"Inline generation: Writing to temp file: '%s{tempFile}'"
                File.WriteAllLines(tempFile, seq{ yield! linesToKeep; yield! code} )
                if verbose then printfn $"Inline generation: Removing input file: '%s{tempFile}'"
                File.Delete(inputFile)
                if verbose then
                    printfn $"Inline generation: Renaming temp file to input file: '%s{tempFile}' -> '%s{inputFile}'"
                File.Move(tempFile, inputFile)
            else
                match outputFile with
                | Some filename ->
                    if verbose then printfn $"Code generation: Writing output file: '%s{filename}'"
                    File.WriteAllLines(filename, code)
                | None -> failwith "Error: No OutputFile was included, and --inlinegeneration was not specified."

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
