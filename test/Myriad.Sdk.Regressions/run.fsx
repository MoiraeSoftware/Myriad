// End-to-end regression checks for Myriad.Sdk as a consumer gets it from a package feed, which the
// integration tests (importing Myriad.Sdk.targets from source) cannot see:
//
//   1. A fresh consumer with an empty NuGet cache builds in one pass: the CLI ships inside Myriad.Sdk.
//      (1.1.0 failed with MSB3954: its PackageDownload of the CLI was never seen by restore.)
//   2. A plugin built with the SDK's defaults (its own target framework and FSharp.Core) loads.
//      (1.1.0 failed with exit code 4 and "inputfile FSharp.Core, Version=10.1.0.0 ... doesn not exist".)
//   3. Rebuilding a plugin regenerates the output, reverting the rebuild regenerates it again, and a
//      build with nothing changed does not run Myriad. (1.1.0 left the output stale.)
//
// Usage, from the repository root after `dotnet tool restore` and `dotnet paket restore`:
//
//   dotnet fsi test/Myriad.Sdk.Regressions/run.fsx [--feed <dir> --version <version>]
//                                                 [--plugin-tfm <tfm>] [--plugin-fsharp-core <version>]
//                                                 [--extra-source <feed>]
//
// Without --feed, Myriad, Myriad.Core and Myriad.Sdk are packed from this checkout under a unique
// prerelease version, the way build.proj's Pack target does. With --feed and --version, packages
// already in that feed are used instead (e.g. --feed https://api.nuget.org/v3/index.json --version 1.1.0
// to watch the published release fail). Packages not in the feed come from --extra-source, nuget.org
// by default. Everything runs in a new temporary directory, with NuGet
// caches of its own, so the user's NuGet cache is neither used nor changed.

open System
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions

let args = fsi.CommandLineArgs |> Array.skip 1 |> List.ofArray

let rec option name =
    function
    | flag :: value :: _ when flag = name -> Some value
    | _ :: rest -> option name rest
    | [] -> None

let repoRoot = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", ".."))
let work = Path.Combine(Path.GetTempPath(), $"myriad-sdk-regressions-{DateTime.Now:yyyyMMddHHmmss}")
Directory.CreateDirectory work |> ignore
printfn $"Working in {work}"

/// Runs dotnet with the given arguments, returning its exit code and combined output.
let dotnet (dir: string) (nugetPackages: string option) (arguments: string list) =
    let psi = ProcessStartInfo("dotnet", WorkingDirectory = dir, RedirectStandardOutput = true, RedirectStandardError = true, UseShellExecute = false)
    arguments |> List.iter psi.ArgumentList.Add
    psi.Environment["DOTNET_CLI_TELEMETRY_OPTOUT"] <- "1"
    psi.Environment["DOTNET_NOLOGO"] <- "1"
    nugetPackages |> Option.iter (fun path -> psi.Environment["NUGET_PACKAGES"] <- path)
    use proc = Process.Start psi
    let stderr = proc.StandardError.ReadToEndAsync()
    let stdout = proc.StandardOutput.ReadToEnd()
    proc.WaitForExit()
    proc.ExitCode, stdout + stderr.Result

let failWithOutput (what: string) (output: string) =
    printfn "%s" output
    failwith what

let extraSource = option "--extra-source" args |> Option.defaultValue "https://api.nuget.org/v3/index.json"

let feed, version =
    match option "--feed" args, option "--version" args with
    | Some feed, Some version -> (if feed.Contains "://" then feed else Path.GetFullPath feed), version
    | None, None ->
        let version = $"99.0.0-regression{DateTime.Now:yyyyMMddHHmmss}"
        let feed = Path.Combine(work, "feed")
        let sdkRestore = Path.Combine(work, "sdk-restore-packages")
        printfn $"Packing Myriad {version} from {repoRoot}"
        for step in
            [ [ "pack"; "src/Myriad.sln"; "-c"; "Release"; "-o"; feed; $"/p:Version={version}" ]
              [ "restore"; "src/Myriad.Sdk/Myriad.Sdk.proj"; $"/p:Version={version}"; "--packages"; sdkRestore; "--source"; feed; "--source"; extraSource ]
              [ "pack"; "src/Myriad.Sdk/Myriad.Sdk.proj"; "-c"; "Release"; "-o"; feed; $"/p:Version={version}"; "--no-restore" ] ] do
            let exitCode, output = dotnet repoRoot None step
            if exitCode <> 0 then failWithOutput $"""dotnet {String.Join(" ", step)} failed""" output
        feed, version
    | _ -> failwith "--feed and --version go together"

// The SDK a consumer outside this repository gets, and the target framework `dotnet new` would give it.
let _, sdkVersion = dotnet work None [ "--version" ]
let sdkTfm = $"net{sdkVersion.Trim().Split('.')[0]}.0"
let pluginTfm = option "--plugin-tfm" args |> Option.defaultValue sdkTfm
let pluginFSharpCore = option "--plugin-fsharp-core" args |> Option.defaultValue ""
printfn $"Myriad {version} from {feed}; .NET SDK {sdkVersion.Trim()}; plugin targets {pluginTfm}"

let rec copyDir (source: string) (target: string) =
    Directory.CreateDirectory target |> ignore
    for file in Directory.GetFiles source do
        File.Copy(file, Path.Combine(target, Path.GetFileName file))
    for dir in Directory.GetDirectories source do
        copyDir dir (Path.Combine(target, Path.GetFileName dir))

copyDir (Path.Combine(__SOURCE_DIRECTORY__, "fixtures")) work

File.WriteAllText(
    Path.Combine(work, "nuget.config"),
    $"""<configuration><packageSources><clear /><add key="myriad" value="{feed}" /><add key="extra" value="{extraSource}" /></packageSources></configuration>""")

File.WriteAllText(
    Path.Combine(work, "Directory.Build.props"),
    $"""<Project><PropertyGroup>
  <MyriadVersion>{version}</MyriadVersion>
  <PluginTargetFramework>{pluginTfm}</PluginTargetFramework>
  <PluginFSharpCoreVersion>{pluginFSharpCore}</PluginFSharpCoreVersion>
  <ConsumerTargetFramework>{sdkTfm}</ConsumerTargetFramework>
</PropertyGroup></Project>""")

let pluginDir = Path.Combine(work, "Plugin")
let consumerDir = Path.Combine(work, "Consumer")
let generatorSource = Path.Combine(pluginDir, "Generator.fs")
let generatedFile = Path.Combine(consumerDir, "Generated.fs")
let pluginPackages = Path.Combine(work, "plugin-packages")
// Empty until the consumer's first build, which is the point of case 1.
let consumerPackages = Path.Combine(work, "consumer-packages")

let mutable failures = []

let check (name: string) (ok: bool) (detail: string) =
    printfn "  %s %s%s" (if ok then "PASS" else "FAIL") name (if String.IsNullOrEmpty detail then "" else $": {detail}")
    if not ok then failures <- name :: failures

let buildPlugin () =
    let exitCode, output = dotnet pluginDir (Some pluginPackages) [ "build"; "-c"; "Debug"; "-nodeReuse:false" ]
    if exitCode <> 0 then failWithOutput "the plugin did not build" output

let buildConsumer () = dotnet consumerDir (Some consumerPackages) [ "build"; "-nodeReuse:false" ]

/// Touched by Myriad.Sdk.targets only when it runs Myriad, which leaves unchanged output unwritten.
let stampTime () =
    Directory.GetFiles(Path.Combine(consumerDir, "obj"), "*.MyriadSdkCodeGenInputs.cache.generated", SearchOption.AllDirectories)
    |> Array.tryHead
    |> Option.map File.GetLastWriteTimeUtc

let generatedMarker () =
    if File.Exists generatedFile then
        let m = Regex.Match(File.ReadAllText generatedFile, "MARKER_[A-Z]")
        if m.Success then m.Value else "no marker"
    else
        "no Generated.fs"

let setMarker (marker: string) =
    File.WriteAllText(generatorSource, Regex.Replace(File.ReadAllText generatorSource, "MARKER_[A-Z]", marker))

buildPlugin ()

let pluginDll = Path.Combine(pluginDir, "bin", "Debug", pluginTfm, "Plugin.dll")
let pluginFSharpCoreVersion =
    let deps = File.ReadAllText(Path.ChangeExtension(pluginDll, ".deps.json"))
    let m = Regex.Match(deps, "\"FSharp.Core/([^\"]+)\"")
    if m.Success then m.Groups[1].Value else "unknown"

printfn ""
printfn "1. Fresh consumer, empty NuGet cache, one build"
let exitCode, output = buildConsumer ()
check "consumer builds" (exitCode = 0) (if exitCode = 0 then "" else $"exit code {exitCode}{Environment.NewLine}{output}")
check "output generated" (generatedMarker () = "MARKER_A") (generatedMarker ())
let cached = Directory.GetDirectories consumerPackages |> Array.map Path.GetFileName
check "no separate Myriad CLI package needed" (exitCode = 0 && not (Array.contains "myriad" cached)) (String.Join(", ", cached))

printfn ""
printfn $"2. Plugin with the SDK's defaults ({pluginTfm}, FSharp.Core {pluginFSharpCoreVersion}) loads"
check "plugin loaded and ran" (exitCode = 0 && generatedMarker () = "MARKER_A") ""
match Int32.TryParse(pluginFSharpCoreVersion.Split('.')[0]) with
| true, major when major > 9 -> ()
| _ -> printfn $"  NOTE FSharp.Core {pluginFSharpCoreVersion} is not newer than Myriad's 9.0, so a newer FSharp.Core was not exercised; use .NET SDK 10 or --plugin-fsharp-core"

printfn ""
printfn "3. Plugin rebuilds"

let step (name: string) (change: unit -> unit) (expectRun: bool) (expectMarker: string) =
    change ()
    let before = stampTime ()
    let exitCode, output = buildConsumer ()
    let ran = stampTime () <> before
    let detail = $"""Myriad {(if ran then "ran" else "did not run")}, output has {generatedMarker ()}"""
    check name (exitCode = 0 && ran = expectRun && generatedMarker () = expectMarker) (if exitCode = 0 then detail else output)

if exitCode = 0 then
    step "no-op build does not run Myriad" ignore false "MARKER_A"
    step "edited plugin regenerates" (fun () -> setMarker "MARKER_B"; buildPlugin ()) true "MARKER_B"
    step "reverted plugin regenerates" (fun () -> setMarker "MARKER_A"; buildPlugin ()) true "MARKER_A"
    step "no-op build does not run Myriad" ignore false "MARKER_A"
else
    check "skipped, the consumer does not build" false ""

printfn ""
match failures with
| [] ->
    printfn "All checks passed."
    exit 0
| _ ->
    printfn $"{failures.Length} check(s) failed. Left in {work} for inspection."
    exit 1
