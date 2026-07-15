# Q012-compiler-behavior-probe / Movement 2 — Design

**Status:** NOT YET EXECUTED.
**Location:** scratch projects under a temp working directory, not part of any committed repo.
**Pins:** `FSharp.Compiler.Service` `43.9.101`; `ProvidedTypes.fs`/`.fsi` from
`FSharp.TypeProviders.SDK` commit `0a95768a2247daba80b24a2604f77f89fc88ff1f` (same as Q006/Q008/Q009/
Q011). `FSharp.Core` `4.7.2` for provider projects.

**Non-negotiable, per the credibility gap named in `FINDINGS.md` on 2026-07-15:** this quartet's
`artifacts/` folder is required to contain real, buildable source for every provider and harness built
— not optional, not "same precedent as Q006/Q008/Q009 which didn't keep it." Save everything.

## Shared instrumentation shape (used by all three probe providers)

Each probe provider's instantiation function, on every call:
1. Increments a per-provider-instance `int` counter (a mutable field on the provider class — persists
   across calls within one design-time load, the same lifetime `TypeProviderForNamespaces` instances
   already have).
2. Captures `DateTime.UtcNow.ToString("HH:mm:ss.fff")`, the managed thread ID, and
   `System.Diagnostics.StackTrace(true).ToString()` (with file/line info, to make FCS-internal frames
   identifiable if present).
3. Appends a compact log line (`"{count}|{time}|{threadId}|{first FSharp.Compiler.* frame found, or
   NONE}"`) to an in-memory list.
4. Exposes the accumulated log as a **real provided member** — `InvocationCount: int` and
   `CallLog: string` (newline-joined) — readable by an independent consumer program after the check,
   not just visible in internal logging. This is the load-bearing difference from Q011's `MiniTP`,
   which only ever logged to a file.

## Round 1 — falsifier: does the probe mechanism itself work?

`ProbeSimple.Provided.Probe<Tag: string>` — one static parameter, one property returning `Tag`, plus
the shared instrumentation above exposed as `InvocationCount`/`CallLog`. No file I/O, no reflection
over other assemblies during instantiation — the minimal shape, mirroring Q008/Q09's working
`ClientTP`.

Check `Probe<"A">` once via `checker.ParseAndCheckFileInProject` (the specific API Q011 found failing
for its own shape — using it here too, deliberately, since shape (a) is expected to *succeed* via this
API, giving a clean success-path baseline to compare shapes (b)/(c) against). From the **compiled
output** (not internal state), instantiate the generated type in a real consumer and read
`InvocationCount`/`CallLog` back.

**Record:** does `InvocationCount` exceed 1 for one logical check (replicating Q011's `MiniTP` finding
through provided members instead of file logs)? Does `CallLog` contain any line whose captured frame
is *not* `NONE` (i.e. a recognizable `FSharp.Compiler.*` frame was captured)? If `InvocationCount = 1`
or every captured frame is `NONE`, say so plainly — either result is reportable, per the pre-registered
KILL/REVISE conditions, not something to route around.

## Round 2 — the isolation matrix (the capability claim)

Two more probe providers, differing from `ProbeSimple` by exactly one factor each:

- `ProbeTwoParams.Provided.Probe<FieldSpec: string, Extra: string>` — **two** static parameters
  (`Extra` unused by the logic, present only to match Q011's `SchemaTP`'s parameter *count*), same
  instrumentation, still **no** file I/O during instantiation. Isolates parameter count alone.
- `ProbeTwoParamsIO.Provided.Probe<FieldSpec: string, DummyFilePath: string>` — same two parameters,
  but the instantiation function additionally does `File.ReadAllBytes(DummyFilePath)` and
  `Assembly.Load(bytes)` against a small pre-built dummy DLL (structurally mirroring `SchemaProvider`'s
  `readClientConsumes`, without any enforcement/failure logic — just the I/O and reflection shape) before
  building the provided type. Isolates instantiation-time I/O on top of the two-parameter shape.

Check all three (`ProbeSimple`, `ProbeTwoParams`, `ProbeTwoParamsIO`) via the **same**
`checker.ParseAndCheckFileInProject` call pattern, same process, same checker instance where practical
(a fresh checker per shape if state needs isolating — report which was used and why). For each:

- Does it resolve with **zero diagnostics** (success), or does the "couldn't find type" diagnostic
  Q011 documented appear (failure)?
- If it succeeds: read `InvocationCount`/`CallLog` back from the compiled output, same as Round 1.
- If it fails: `InvocationCount`/`CallLog` can't be read from a non-existent compiled type. Fall back
  to Q011's own file-logging technique for **this specific case only**, explicitly labeled as a
  fallback observation channel, not the primary mechanism under test — report which shapes needed the
  fallback and which didn't, since needing it is itself informative (it would mean the failure happens
  before the type — and therefore its members — ever exist to be read back).

**This table is the direct answer to Q011's open question.** If `ProbeSimple` succeeds and both
`ProbeTwoParams` and `ProbeTwoParamsIO` fail: parameter count is the factor, independent of I/O. If
`ProbeSimple` and `ProbeTwoParams` both succeed but `ProbeTwoParamsIO` fails: I/O is the factor,
independent of parameter count. If all three succeed: neither factor alone explains Q011's regression,
and the cause lies in something else about `SchemaProvider`'s actual shape (NULL, per the
pre-registered threshold). If all three fail: `ParseAndCheckFileInProject` may be broken for *any*
generative provider in this environment, which would be a surprising, load-bearing finding of its own,
warranting an immediate sanity re-check against a provider already known to work (e.g. rebuild and
re-run Q009's own `ClientTP` shape as a positive control before concluding anything).

## Round 3 — confirm against the real artifact (only if Round 2 isolates a candidate)

If Round 2 identifies a specific factor, retrofit the **same instrumentation** (invocation counter,
timestamp, stack-trace capture, exposed as provided members) directly onto a copy of Q011's actual
`SchemaTP.DesignTime/SchemaProvider.fs` and `ClientTP.DesignTime/ClientProvider.fs`
(`experiments/Q011-consumer-driven-contracts/artifacts/`), and re-run the same
`ParseAndCheckFileInProject` check against the real providers. Confirm the identified factor's
predicted outcome (success or failure, and the invocation pattern) actually holds for the real
artifacts, not just the minimal analogs built in Rounds 1–2. This is what upgrades the finding from "a
plausible explanation found via a simplified stand-in" to "the actual, confirmed cause of Q011's
regression."

If Round 2 does not isolate a candidate (the NULL case), skip Round 3 and report the NULL result as
the quartet's own conclusion — don't force a confirmation round onto a finding that never named
anything to confirm.

## Reproduction

```
mkdir tp-compiler-behavior-probe-spike && cd tp-compiler-behavior-probe-spike
dotnet new classlib -lang F# -o ProbeSimple.Runtime
dotnet new classlib -lang F# -o ProbeSimple.DesignTime
dotnet new classlib -lang F# -o ProbeTwoParams.Runtime
dotnet new classlib -lang F# -o ProbeTwoParams.DesignTime
dotnet new classlib -lang F# -o ProbeTwoParamsIO.Runtime
dotnet new classlib -lang F# -o ProbeTwoParamsIO.DesignTime
dotnet new classlib -lang F# -o DummyAsm          # the small DLL ProbeTwoParamsIO reads during instantiation
dotnet new console -lang F# -o Harness            # FSharp.Compiler.Service 43.9.101, Rounds 1-2
# Round 3, if reached: copies Q011's real provider source in-place with instrumentation added
dotnet build ProbeSimple.Runtime ProbeSimple.DesignTime
dotnet build ProbeTwoParams.Runtime ProbeTwoParams.DesignTime
dotnet build ProbeTwoParamsIO.Runtime ProbeTwoParamsIO.DesignTime
dotnet build DummyAsm
dotnet run --project Harness
```

No MSBuild integration with Myriad's own `Myriad.Sdk` targets — this idea is independent of Myriad by
design, same as every Thread 2 quartet.

## Explicit instruction to whoever executes this (do not silently deviate)

- Do not edit this file or `00-hypothesis.md` once execution starts. Report design-vs-reality
  corrections honestly in `02-results.md`, same precedent as every quartet since Q001.
- **Save every provider and harness source file under `artifacts/`.** This is not optional for this
  quartet — see the credibility-gap note at the top of this file and in `FINDINGS.md`.
- If Round 1's falsifier fails (no useful stack-trace info), still attempt Round 2 using
  invocation-count-and-success/failure alone, per the REVISE threshold — don't stop the whole quartet
  on a partial falsifier failure the way a full KILL condition would require.
- Round 2's three-way table is the single most important result in this quartet. Report the raw
  success/failure outcome for all three shapes explicitly, even if the pattern doesn't match what
  might seem like the "expected" or "hoped-for" answer — a NULL result here is exactly as valuable to
  this repo's methodology as a clean isolation, per every prior quartet's own stated discipline.
- If reached, Round 3 must use Q011's actual artifact source as the base (copy it, add instrumentation,
  don't rewrite it from scratch) — the point is confirming against the real thing, not a fresh
  reconstruction that might silently diverge from what actually shipped in Q011.
