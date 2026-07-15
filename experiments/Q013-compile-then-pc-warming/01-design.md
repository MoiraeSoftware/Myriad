# Q013-compile-then-pc-warming / Movement 2 — Design

**Status:** NOT YET EXECUTED.
**Location:** scratch projects under a temp working directory, not part of any committed repo, mirroring
Q012's own setup.
**Pins:** `FSharp.Compiler.Service` `43.9.101`; `ProvidedTypes.fs`/`.fsi` from
`FSharp.TypeProviders.SDK` commit `0a95768a2247daba80b24a2604f77f89fc88ff1f` (same as Q006/Q008/Q009/
Q011/Q012). `FSharp.Core` `4.7.2` for provider projects.

**Non-negotiable, same standard as Q012:** `artifacts/` must contain real, buildable source and raw
run logs for everything built — not optional.

## Reused infrastructure (do not rebuild from scratch)

Copy, don't rewrite, from `experiments/Q012-compiler-behavior-probe/artifacts/`:
- `ProbeSimple.DesignTime/` and `ProbeSimple.Runtime/` — the minimal generative provider Q012 already
  proved fails cold via PC (`isErased = false`, one static parameter).
- The `parseAndCheck` and `compile` helper functions from `Harness/Program.fs` (lines ~140–168 for
  `compile`, and the equivalent `parseAndCheck` above it) — reuse verbatim so "same scenario" is
  guaranteed identical to what Q012 already measured, not a hand-rebuilt approximation.
- `Q011-consumer-driven-contracts/artifacts/SchemaTP.DesignTime/` and `ClientTP.DesignTime/` (via
  Q012's own copies under `Q012-compiler-behavior-probe/artifacts/SchemaTP.*`/`ClientTP.*`, already
  instrumented) for Round 3.

New harness: `Q013-compile-then-pc-warming/artifacts/Harness/Program.fs`, a small F# console app
(`FSharp.Compiler.Service` `43.9.101` referenced the same way Q012's was) that imports/adapts the
reused helpers rather than depending on Q012's project directly (keep Q013 self-contained and
reproducible on its own, per the folder schema in `README.md`).

## Round 1 — falsifier: same-checker-instance compile-then-PC

Construct one `FSharpChecker` instance (`FSharpChecker.Create(...)`, same construction Q012 used —
copy the exact options). Using `ProbeSimple`'s generative shape with a fresh tag string (e.g.
`"warm-r1"`, distinct from any tag Q012 literally ran, to rule out an accidental artifact of reusing
an identical source string):

1. Call `checker.Compile(args)` once for the scenario. Record exit code, error count, elapsed ms.
2. Immediately after, on the **same checker instance**, call
   `checker.ParseAndCheckFileInProject(...)` on the identical scenario (same file path, same source
   text, same project options — literally the same values passed to step 1's `Compile` args where
   applicable). Record whether it resolves with zero error diagnostics or reproduces Q012's
   "couldn't find type" diagnostic.
3. Repeat steps 1–2 three times with fresh tag strings each repeat (`"warm-r1a"`, `"warm-r1b"`,
   `"warm-r1c"`) to check for determinism, each against a **freshly created checker instance** per
   repeat (so each repeat is an independent same-instance compile-then-PC trial, not accumulating
   state across repeats in a way that would confound the result).

**This alone is the cheapest falsifier named in `00-hypothesis.md`.** If PC fails to resolve even
immediately after a same-instance `Compile` of the identical scenario, record NULL and stop — do not
build Round 2 or Round 3 (per the NULL threshold, no point testing a fresh-instance variant of an
effect that doesn't exist at all).

## Round 2 — only if Round 1 shows a positive effect: does the effect need the same instance?

If Round 1's compile-then-PC ordering flips the outcome: repeat the same sequence, but with `Compile`
run on checker instance A and PC run on a **freshly constructed** checker instance B, same process,
immediately after. This isolates whether the warming effect lives in instance-local state (e.g. the
`FSharpChecker`'s own internal caches) or process-global state (e.g. a static cache inside
`FSharp.Compiler.Service`, or the type-provider host's own assembly-resolution cache, which by design
can be process-wide regardless of which `FSharpChecker` object is asking).

Report both conditions' outcomes explicitly — this matters for how useful the reconciliation is: if
only same-instance warming works, that's a narrower, more specific explanation for what an ordinary
harness would need to do (reuse one checker across a compile-then-check flow) than if any
same-process Compile call warms PC for any subsequent checker instance.

## Round 3 — only if Round 1 (and ideally Round 2) show a positive effect: retrofit onto Q011's real providers

Using the already-instrumented copies of `SchemaTP.DesignTime`/`ClientTP.DesignTime` from Q012's
`artifacts/` (do not modify the provider logic — same precedent as Q012's own Round 3, confirming
against the real thing rather than a fresh reconstruction that might silently diverge):

1. Construct the same scenario Q012's Round 3 used (its exact virtual file/project setup for
   `SchemaTP`/`ClientTP`, copied verbatim from `Q012-compiler-behavior-probe/artifacts/run-logs/
   run-round3.txt` and the harness code that produced it).
2. Apply whichever ordering condition(s) Round 1/2 found effective (same-instance and/or
   cross-instance-same-process compile-then-PC).
3. Record whether PC now resolves the real generative provider's type where Q012 established it
   fails cold, using the same diagnostic-comparison method Q012 used (exact diagnostic text
   match/mismatch, not just pass/fail).

This is the decisive test for the reconciliation claim: a toy-shape-only effect (SHIP for Round 1,
no confirmation in Round 3) is a real but narrower finding than one that also holds for the actual
disputed providers.

## Reproduction

```
mkdir -p Q013-compile-then-pc-warming/artifacts
cp -r Q012-compiler-behavior-probe/artifacts/ProbeSimple.Runtime Q013-compile-then-pc-warming/artifacts/
cp -r Q012-compiler-behavior-probe/artifacts/ProbeSimple.DesignTime Q013-compile-then-pc-warming/artifacts/
cp -r Q012-compiler-behavior-probe/artifacts/SchemaTP.Runtime Q013-compile-then-pc-warming/artifacts/
cp -r Q012-compiler-behavior-probe/artifacts/SchemaTP.DesignTime Q013-compile-then-pc-warming/artifacts/
cp -r Q012-compiler-behavior-probe/artifacts/ClientTP.Runtime Q013-compile-then-pc-warming/artifacts/
cp -r Q012-compiler-behavior-probe/artifacts/ClientTP.DesignTime Q013-compile-then-pc-warming/artifacts/
cp -r Q012-compiler-behavior-probe/artifacts/vendor Q013-compile-then-pc-warming/artifacts/
dotnet new console -lang F# -o Q013-compile-then-pc-warming/artifacts/Harness
# Harness references FSharp.Compiler.Service 43.9.101, builds the copied provider projects,
# implements Round 1/2/3 as described above
dotnet build Q013-compile-then-pc-warming/artifacts/ProbeSimple.Runtime Q013-compile-then-pc-warming/artifacts/ProbeSimple.DesignTime
dotnet build Q013-compile-then-pc-warming/artifacts/SchemaTP.Runtime Q013-compile-then-pc-warming/artifacts/SchemaTP.DesignTime
dotnet build Q013-compile-then-pc-warming/artifacts/ClientTP.Runtime Q013-compile-then-pc-warming/artifacts/ClientTP.DesignTime
dotnet run --project Q013-compile-then-pc-warming/artifacts/Harness
```

No MSBuild integration with Myriad's own `Myriad.Sdk` targets — independent of Myriad by design, same
as every Thread 2 quartet.

## Explicit instruction to whoever executes this (do not silently deviate)

- Do not edit this file or `00-hypothesis.md` once execution starts. Report design-vs-reality
  corrections honestly in `02-results.md`, same precedent as every quartet since Q001.
- **Save every provider and harness source file, plus raw run logs, under `artifacts/`.** Not
  optional — same standard Q012 set after the Q006/Q008/Q009 gap.
- Round 1 is the whole ballgame for whether this quartet has legs. If it comes back NULL, stop there,
  write up the NULL result fully (it directly elevates Q008/Q09 reconstruction to top priority per
  `00-hypothesis.md`'s NULL threshold), and do not force Round 2/3 to happen anyway.
- Repeat each condition at least 3 times before reporting determinism either way — this is a
  pre-registered validity precondition, not optional polish.
- If Round 1 shows a positive effect but it's flaky (not reproducible across the 3 repeats with no
  identifiable controlling variable), report KILL per the pre-registered threshold rather than
  picking the favorable run to headline.
- Round 3, if reached, must use the real Q011 provider source copied from Q012's own artifacts
  unmodified except for whatever instrumentation Q012 already added — don't rewrite it.
