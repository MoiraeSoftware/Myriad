# Q010 artifacts

Three standalone console projects, one per round described in `../01-design.md`. Each is
independently runnable, pinned to `FSharp.Compiler.Service` 43.9.101 (Myriad's own pin, see
`../../../paket.lock`).

```
cd round1-reentrancy       && dotnet run -c Release
cd round2-cross-generator  && dotnet run -c Release
cd round3-cost             && dotnet run -c Release
```

Compiled console apps, not `dotnet fsi` `.fsx` scripts: fsi is itself built on FCS, and loading a
different FCS version into an fsi host produces a version clash (noted in Q001's artifacts README).

Round outputs are transcribed into `../02-results.md`; re-running reproduces the same correctness
results, though exact millisecond timings vary by machine and process warmth.
