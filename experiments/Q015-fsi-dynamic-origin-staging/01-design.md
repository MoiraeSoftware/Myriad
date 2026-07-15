# Q015-fsi-dynamic-origin-staging / Movement 2 — Design

**Status:** NOT YET EXECUTED.
**Location:** `artifacts/spike/`, `dotnet new console -lang F#`, same packages as Q014
(`FSharp.Compiler.Service 43.9.101`, `Unquote 7.0.1`). Reuses Q014's `translate`/`substVar`/`unroll`/
`miOf` helpers verbatim (copied, not re-derived) — this quartet's new surface is entirely about how the
`MethodInfo` is obtained, not the reification pipeline, which stays exactly as Q014 proved it.

## Setup: making "not known at host-compile-time" real, not asserted

- `artifacts/spike/plugin/PowerPlugin.fsx` — a **separate file**, never `#load`ed or referenced by name
  anywhere in `Program.fs`, containing:
  ```fsharp
  module Target =
      [<ReflectedDefinition>]
      let rec power (n: int) (x: float) : float =
          if n = 0 then 1.0 else x * power (n - 1) x
  ```
  `Program.fs` reads this file's contents via `IO.File.ReadAllText` at runtime. Built-time check: grep
  `Program.fs` itself for the literal substring `"power"` before running — if found outside a string
  literal read from the plugin file, the "dynamic origin" claim is compromised and must be reported as
  such, not quietly fixed and rerun.
- The specializing config `n` is read via `Environment.GetEnvironmentVariable("Q015_POWER_N")`,
  defaulting to `5` (deliberately different from Q014's `4`) if unset, printed at the top of the run so
  the actual value used is always visible in `run-output.txt`.

## Round 1 — cheapest falsifier: get a reflectable `MethodInfo` out of a live FSI session

1. Create `FsiEvaluationSession` exactly as Q003/Q014's harness (`--noninteractive`, no ALC isolation).
2. `fsiSession.EvalInteractionNonThrowing(pluginSourceTextReadFromFile)` — defines `Target.power` inside
   the session only.
3. `fsiSession.EvalExpressionNonThrowing("match <@ Target.power 0 0.0 @> with | Microsoft.FSharp.Quotations.Patterns.Call(_, mi, _) -> mi | _ -> failwith \"unexpected shape\"")`
   — the quoting happens *inside* FSI, where `Target` is a bound, resolvable name; the host never needs
   to reference `Target.power` itself. Cast the returned `FsiValue.ReflectionValue` to
   `System.Reflection.MethodInfo` host-side.
4. Record and print `mi.Module.Assembly.FullName` vs. `Reflection.Assembly.GetEntryAssembly().FullName`
   — must differ, proving the `MethodInfo` genuinely came from FSI's dynamic assembly, not the host's own
   compiled code (which never mentions `power` at all, per the setup's grep check).
5. `Expr.TryGetReflectedDefinition(mi)` from host code. Must return `Some` with the same
   `IfThenElse(n=0, 1.0, x * power (n-1) x)` shape Q014 observed from the host-compile-time case.

If step 5 returns `None` or throws: stop, do not attempt Round 2, record as KILL per the pre-registered
threshold — this would mean `[<ReflectedDefinition>]` metadata doesn't survive FSI's compilation path,
a general finding worth having regardless of this quartet's fate.

## Round 2 — capability claim: full pipeline with genuinely dynamic origin

Identical to Q014's Round 2 (`substVar` → `unroll` → `decompile`/`translate` → splice →
`ParseAndCheckFileInProject` → `checker.Compile` → `Assembly.LoadFrom` → invoke), with two changes:

- `n` comes from the environment variable read at startup (Round 2 substitutes this value, not a
  literal `4`).
- The reference/"expected" value for the correctness table is obtained by invoking the **FSI-obtained**
  `mi` via reflection (`mi.Invoke(null, [| box n; box x |])`), never by calling `Target.power` from host
  F# source — that identifier is not resolvable in the host's own compiled code, which is the entire
  point being tested.

**What Round 2 must record**, matching Q014's own bar: zero diagnostics on the spliced text; the
correctness table (FSI-invoked general implementation vs. compiled-and-executed specialized output) for
at least two `x` values; confirmation the emitted text contains no reference to `power`/`Target`
(structural-difference check, reused verbatim from Q014 — already flagged by Q014's review as adequate
for this exact scenario, not claimed as a general test).

## Round 3 — cost (single sample, honestly labeled)

Time the full Round 1 + Round 2 sequence together (FSI session creation through final invoke), since
this quartet's actual claim includes FSI startup cost that Q014's number never paid. Report separately
from Q014's own 652/657ms so the two aren't conflated as comparable.

## Reproduction

```
dotnet new console -lang F# -o q015-spike
cd q015-spike
dotnet add package FSharp.Compiler.Service --version 43.9.101
dotnet add package Unquote --version 7.0.1
Q015_POWER_N=5 dotnet run
```

## Explicit instruction to whoever executes this

- If reality diverges from this design (FSI's `EvalExpressionNonThrowing` doesn't accept a
  multi-line/match expression the way assumed, the `MethodInfo` cast fails, `TryGetReflectedDefinition`
  behaves differently for an FSI-sourced method), report the correction honestly in `02-results.md`,
  the same rule Q014 itself was held to.
- Do not weaken the "genuinely not known at host-compile-time" setup to make the spike easier — that
  would silently reproduce Q014's own central defect. If it turns out impossible to avoid some
  host-compile-time knowledge of the plugin's shape, report that explicitly as a finding, not a detail.
