# Q001 artifacts

Four standalone console projects, one per round described in `../01-design.md`. Each is
independently runnable, pinned to `FSharp.Compiler.Service` 43.9.101 (Myriad's own pin, see
`../../../paket.lock`).

```
cd round1-mechanism-alias && dotnet run
cd round2-incrementality-staleness && dotnet run
cd round3-scaling-comparison && dotnet run
cd round4-fields-generator-port && dotnet run
```

Not buildable via `dotnet fsi` as `.fsx` scripts — fsi is itself built on FCS, and loading a
different FCS version into an fsi host produces a `MissingMethodException` from the version
clash. Compiled console apps sidestep this since each is a fresh process. This was tried and
failed once; noted here so it isn't retried.

Round outputs are transcribed verbatim into `../02-results.md`; re-running should reproduce the
same correctness results, though exact millisecond timings will vary by machine.
