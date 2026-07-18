# Q012 artifacts — compiler-behavior probe

Self-contained, re-runnable source for every provider and harness built in Q012. `vendor/` holds the
`ProvidedTypes.fs`/`.fsi` copied from `FSharp.TypeProviders.SDK` commit
`0a95768a2247daba80b24a2604f77f89fc88ff1f` (the pin every Thread 2 quartet uses), so no external SDK
checkout is needed to rebuild. Deliberately keeping full source here is the fix for the credibility
gap named in `FINDINGS.md` (Q006/Q008/Q009 kept none).

## Layout

- `ProbeSimple.*` — shape (a): one static parameter, no I/O. Also exposes an ERASING variant
  `ProbeErased` used as the positive control.
- `ProbeTwoParams.*` — shape (b): two static parameters (second unused), no I/O.
- `ProbeTwoParamsIO.*` — shape (c): two static parameters + `File.ReadAllBytes`/`Assembly.Load`
  instantiation-time I/O over `DummyAsm.dll`.
- `SchemaTP.*` / `ClientTP.*` — Q011's REAL providers, source verbatim except a small `Q012Instr`
  file-logging module + one `Q012Instr.log "createType"` call added (Round 3).
- `DummyAsm` — the tiny DLL shape (c) reads during instantiation.
- `Harness` — FCS 43.9.101 driver. Modes: `all`, `matrix`, `sharpen`, `round3`.
- `run-logs/` — captured output of the actual runs the write-up cites, plus the first-invocation
  stack trace showing the FCS frames.

## Reproduce

The harness hard-codes `ROOT = C:\Users\Dave\tp-compiler-behavior-probe-spike`. Copy this tree there
(or edit `ROOT` in `Harness/Program.fs`), then:

```
dotnet build DummyAsm/DummyAsm.fsproj -c Release
dotnet build ProbeSimple.Runtime/ProbeSimple.Runtime.fsproj -c Release       # triggers DesignTime
dotnet build ProbeTwoParams.Runtime/ProbeTwoParams.Runtime.fsproj -c Release
dotnet build ProbeTwoParamsIO.Runtime/ProbeTwoParamsIO.Runtime.fsproj -c Release
dotnet build SchemaTP.Runtime/SchemaTP.Runtime.fsproj -c Release
dotnet build ClientTP.Runtime/ClientTP.Runtime.fsproj -c Release
dotnet build Harness/Harness.fsproj -c Release

dotnet run --project Harness/Harness.fsproj -c Release --no-build -- all      # Round 1 + matrix
dotnet run --project Harness/Harness.fsproj -c Release --no-build -- sharpen  # generative vs erased
dotnet run --project Harness/Harness.fsproj -c Release --no-build -- round3   # real Q011 providers
```
