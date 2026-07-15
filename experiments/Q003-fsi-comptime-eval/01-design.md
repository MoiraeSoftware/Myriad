# Q003-fsi-comptime-eval / Movement 2 — Design (falsifier only)

**Status:** DONE (falsifier). Full end-to-end (FSI-evaluated schema → generated source →
spliced → typechecked, the actual capability claim) not yet built — see `03-review.md`.
**Location:** `artifacts/fsi-in-process-falsifier/`, `FSharp.Compiler.Service` 43.9.101, same
pin as Q001/Q002. `FsiEvaluationSession` ships in the same package — no extra dependency.

## Method

Six staged checks in one process, using the standard `step` harness (prints OK or the full
exception on failure, doesn't stop the process on a single stage's failure so later stages still
report even if an earlier one breaks something):

1. `FSharpChecker` alone, baseline sanity (same pattern as Q001).
2. Create `FsiEvaluationSession` in the same process as the already-created `FSharpChecker`.
3. Evaluate a trivial expression (`1 + 2`) via FSI.
4. Re-check with `FSharpChecker` *after* FSI has run, to catch any state corruption FSI's
   presence might cause.
5. Evaluate a `string`-returning expression via FSI, cast the result to `System.String` in host
   code — a BCL type, expected to be identity-safe regardless of hosting setup.
6. The real test: evaluate an **F#-typed** value (`(string * string) list`, not a BCL primitive)
   via FSI, and attempt to use `FsiValue.ReflectionValue` as the host's own compiled
   `(string * string) list` type via a direct cast — not just `.ToString()` or reflection
   traversal. This is the one that could plausibly fail: FSI dynamically compiles and runs
   evaluated code, and if that code's `FSharp.Core` isn't identity-equal to the host's own, the
   cast throws `InvalidCastException` even though the shapes match, a well-known class of
   assembly-identity gotcha in script-hosting scenarios (Roslyn scripting has the same failure
   mode). Fallback path (reflection-based structural read) included in case it does.

## Reproduction

```
cd artifacts/fsi-in-process-falsifier
dotnet run
```
