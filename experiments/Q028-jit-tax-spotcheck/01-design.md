# Q028 — design

## Method

`artifacts/q028-spike/Program.fs` is a byte-for-byte copy of
`Q023-scale-cost-reentrant-callback/artifacts/q023-spike/Program.fs` (verbatim through `runForN` and the
original `[<EntryPoint>]` argument-parsing shape), with one addition: an optional `runWarmup()` step,
gated by a new trailing `"warmup"` literal argv token, that runs a single throwaway
`ParseAndCheckProject` on a minimal, unrelated 1-file project (a two-line record + function, its own
fresh `FSharpChecker`, its own `DocumentSource.Custom` serving only that one virtual file) *before*
`buildProject`/`runForN` for the real N-file project begins. When `warmup` is omitted, the code path is
identical to Q023's own spike — confirmed below to reproduce Q023's original numbers.

This isolates the tax the same way Q027's review did: pay the one-time JIT/FCS-static-init/
referenced-assembly-metadata-read cost on a cheap, throwaway check first, so it doesn't land on the real
measurement's own `cold` data point.

## Conditions

Two N values from Q023/Q024's own tested range, each run twice (no-warmup control, then warmup),
one process per invocation (matching Q023/Q024's own "fresh process per N" methodology exactly, so this
isolates only the warmup variable):

- **N=10** — smallest N tested by Q023/Q024, where a ~500ms fixed tax would be the largest fraction of
  `cold`.
- **N=300** — largest N tested, where `cold` itself is much bigger and the same fixed tax would be a
  smaller fraction, showing whether the tax's *absolute* size scales with N or stays fixed.

`editIdx=0` throughout (Q023's own original, worst-case-successor-count edit position — kept fixed here
since this spot-check is about the `cold` baseline, not the position-sweep question Q023/Q024 already
answered).

## What's measured

Reuses Q023's own `runForN` output verbatim: `cold` (first `ParseAndCheckProject` call), 3× `repeat`
(no-op re-checks), 3× `editOne` (edit `Prefix0000.fs`, re-check), plus the derived `repeatRatio` and
`editOneRatio` (each median over `cold`) — the exact quantities Q023's `02-results.md` and Q024's
regression are built from.

## Reproduction

```
cd experiments/Q028-jit-tax-spotcheck/artifacts/q028-spike
dotnet build -c Release
dotnet bin/Release/net9.0/q028-spike.dll 10 0          # control
dotnet bin/Release/net9.0/q028-spike.dll 10 0 warmup   # warmup
dotnet bin/Release/net9.0/q028-spike.dll 300 0
dotnet bin/Release/net9.0/q028-spike.dll 300 0 warmup
```

## No unstated assumptions

Whether a throwaway 1-file `ParseAndCheckProject` on an *unrelated* project actually collapses the tax
for a *different*, later `FSharpChecker` instance's first check (rather than the tax being somehow
process-wide-but-checker-instance-specific in a way that doesn't transfer) is not assumed — it's exactly
what running both conditions checks. If the warmup made no difference, that itself would be informative
about the tax's actual scope (checker-instance-local rather than process-global), not just a null result.
