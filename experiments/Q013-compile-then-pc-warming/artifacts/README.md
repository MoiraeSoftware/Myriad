# Q013 artifacts — compile-then-PC warming probe

Self-contained, re-runnable source for the Q013 harness plus the provider sources it tests. The
provider projects (`ProbeSimple.*`, `SchemaTP.*`, `ClientTP.*`) and `vendor/ProvidedTypes.*` are
copied verbatim from `Q012-compiler-behavior-probe/artifacts/` so "same scenario" is guaranteed
identical to what Q012 measured. Only the `Harness/` is new to Q013.

`vendor/` holds `ProvidedTypes.fs`/`.fsi` from `FSharp.TypeProviders.SDK` commit
`0a95768a2247daba80b24a2604f77f89fc88ff1f` (the pin every Thread 2 quartet uses), so no external SDK
checkout is needed to rebuild.

## What the harness does

`Harness/Program.fs` reuses Q012's `parseAndCheck` (`ParseAndCheckFileInProject`, "PC") and `compile`
(`checker.Compile`) helpers verbatim in structure, parameterized to accept the `FSharpChecker`
instance and the static-argument tag. It tests the one ordering Q012 never ran: **Compile a generative
scenario, then PC the same scenario on the same checker instance.**

- `round1` — the falsifier. Per repeat (`r1a`/`r1b`/`r1c`): a cold PC of a never-compiled tag (fresh
  checker, expected to reproduce Q012's cold-fail), then a same-checker Compile-then-PC of a fresh
  warm tag. 3 repeats, fresh checker + fresh tags each, for determinism.
- `all` — runs `round1`; only if Round 1 is positive does it proceed to `round2`
  (cross-instance: Compile on checker A, PC on fresh checker B) and `round3` (retrofit onto Q011's
  real `SchemaTP`/`ClientTP`). Round 1 came back NULL, so Round 2/3 were not run (per design).

## Reproduce

The harness hard-codes `ROOT = C:\Users\Dave\q013-warm-spike` (a scratch dir OUTSIDE the Myriad repo,
so the repo's `Directory.Build.props`/`global.json` do not leak into the provider builds — Q012 used
the same out-of-repo approach). Overridable via the `Q013_ROOT` env var. Copy this tree there, then:

```
dotnet build ProbeSimple.Runtime/ProbeSimple.Runtime.fsproj -c Release   # triggers DesignTime
dotnet build Harness/Harness.fsproj -c Release
Q013_ROOT=<scratch-path> dotnet run --project Harness/Harness.fsproj -c Release --no-build -- round1
```

For Round 2/3 (only meaningful if Round 1 is positive), also build `SchemaTP.Runtime` and
`ClientTP.Runtime` and run `-- all`.

## run-logs

- `run-round1.txt` — full captured output of the Round 1 run cited by `02-results.md`.
- `run-round1-rerun.txt` — a second, independent process run; byte-identical verdict, for
  cross-process determinism.
- `ProbeSimple.last-trial.log` — the provider's own design-time file-log lines from the final warm
  trial (the ground-truth invocation count independent of the harness).
