# Notes about MSBuild machinery

## How is a rebuild determined?

Rebuilding is determined by:
* the hash of the version of myriad
* reference paths
* every attributed file's content, output path, config key and params

The MSBuild implementation relies on `_MyriadSdkCodeGenInputCache` property which is defined in [Myriad.Sdk.targets](./src/Myriad.Sdk/build/Myriad.Sdk.targets) - one combined hash over every codegen input, not a per-file hash. Because of that, `MyriadSdkGenerateCode`'s own up-to-date check is a single stamp file (`_MyriadSdkGenerateStamp`) covering the whole project, not one output-file check per attributed file: editing any one attributed file already invalidates the combined hash and forces every file to regenerate, so a per-file check would only have been misleading, not actually finer-grained. Confirmed by editing a single file and observing every generated file get rewritten, not just the one that depended on it.

## Why one process, not one per file

`MyriadSdkGenerateCode` runs Myriad exactly once per build (when anything needs regenerating), for every attributed file in the project together, via a `--manifest <path-to-toml>` argument rather than one `--inputfile`/`--outputfile`/`--configkey` invocation per file. The manifest is a small TOML file, one `[[unit]]` table per file, written by MSBuild into `$(IntermediateOutputPath)myriad.manifest.toml` just before the `<Exec>`. `Program.fs` loops over every unit in one process, loading plugins once rather than once per file. The CLI's single-file flags (`--inputfile`, `--outputfile`, `--configkey`, `--additionalparams`, `--generator-filter`, `--inlinegeneration`) still work unchanged for direct/non-MSBuild invocation; `--manifest` is an alternative to that whole group, not an addition to it.

Per-item metadata (`FlattenedParams`, `GeneratorsPipeDelimited`) needed for the manifest is computed in a separate target, `_MyriadSdkFlattenParams`, batched per `MyriadCodegen` item over `%(MyriadCodegen.OutputPath)` (the one metadata value guaranteed unique per attributed file - batching on the input file's identity instead undercounts whenever several files share the same `MyriadFile`). `;` is swapped for `|` in both values before they're embedded in the manifest, because the manifest-building step assigns them into an MSBuild `Include` attribute, which silently re-splits any unescaped `;` into separate items.

## A target's `Condition` runs before its own dependencies

`MyriadSdkGenerateCode` does not gate on `'@(MyriadCodegen)' != ''` at the target level, even though an empty project should skip codegen entirely. A target's own `Condition` is evaluated before its `DependsOnTargets`/`BeforeTargets` chain has run, so `@(MyriadCodegen)` - populated by `_MyriadSdkFilesList`, one of that chain's own targets - always reads as empty at that point and the target would skip unconditionally, every build. The guard instead sits on the `<Exec>` (and its follow-up `<Touch>`) inside the target body, evaluated after the dependency chain has actually populated the item list.
