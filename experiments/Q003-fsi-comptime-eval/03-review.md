# Q003-fsi-comptime-eval / Movement 4 — Adversarial review (falsifier only)

## Three strongest objections

1. **Only the simplest hosting configuration was tested, and it's not the one that matters for a
   real plugin host.** Myriad's actual plugin loader isolates plugins into separate
   `AssemblyLoadContext`s (`McMaster.NETCore.Plugins`, established earlier this session). ALC
   isolation commonly gives isolated code its own separate loaded copy of `FSharp.Core` — exactly
   the condition under which Stage 6's direct cast would plausibly have thrown
   `InvalidCastException` instead of succeeding. This falsifier passed in the configuration least
   likely to expose the problem it was designed to look for.
2. **This tested "can data cross the boundary," not "can this drive real generation."** The
   pre-registered "PASS further" threshold — an FSI-evaluated value driving text generation
   spliced into the same project and typechecked, closing the loop the way Q001/Q002 did for
   their own claims — was not attempted. What's proven is necessary, not sufficient.
3. **Only pure, trivial, synchronous expressions were evaluated.** A real schema-reading use case
   means FSI executing code that does I/O — reads a file, maybe a database or an HTTP call. That
   introduces sandboxing/security questions (arbitrary code running at generation time, in every
   dev's and CI's build), non-determinism, and interacts with Q001's incremental-caching findings
   in ways this falsifier didn't touch at all: does a generator whose comptime evaluation reads an
   external file get correctly invalidated when that file changes, the same way Q001 had to learn
   `InvalidateConfiguration` the hard way for source-file dependencies?

## Verdict

**SHIP the falsifier's own narrow claim; the capability claim stays open.** `FsiEvaluationSession`
and `FSharpChecker` coexist in one process without collision, and — the one genuinely uncertain
part — an FSI-evaluated F#-typed value (not just a BCL primitive) crossed into host code as a
directly usable, correctly-typed value with no cast failure. That clears the single biggest "does
this even work at all" doubt, cheaply, exactly as a falsifier should.

It does not clear the production-relevant case (ALC isolation), and it does not yet demonstrate
the actual claim behind "type providers, but with real emitted source" — that still requires the
full loop. Two concrete next steps, not one, now that the cheap gate has passed:

- **Test under ALC isolation.** Load the FSI-evaluated code (or the FsiEvaluationSession itself)
  into a separate `AssemblyLoadContext` the way a real plugin would be loaded, and rerun Stage 6.
  If the cast fails there, the mitigation (marshal via a shared contract type, or reflection-based
  structural reads, both already sketched as a fallback in `artifacts/`) is known and cheap — but
  it needs to be tested, not assumed safe from this run.
- **Build the full loop.** FSI evaluates a small schema description, the result drives generation
  of real F# source text, that text is spliced into the same `FSharpChecker` project as Q001/Q002
  and typechecked. That is the spike that actually earns the type-provider comparison.
