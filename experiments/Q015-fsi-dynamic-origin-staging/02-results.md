# Q015-fsi-dynamic-origin-staging / Movement 3 — Results

**Status:** EXECUTED. See `artifacts/spike/` (`spike.fsproj`, `Program.fs`, `plugin/PowerPlugin.fsx`,
`run-output.txt` — full captured console output of the run this file reports, `Q015_POWER_N=6`).

**Environment:** net9.0, `FSharp.Compiler.Service 43.9.101`, `Unquote 7.0.1`, same `FSharp.Core`
`NU1608` warning as Q014, harmless.

## Deviation from `01-design.md`, reported honestly

One build-time bug, unrelated to the research question: `EvalInteractionNonThrowing` returns
`Choice<FsiValue option, exn> * FSharpDiagnostic[]`, not `Choice<unit, exn> * FSharpDiagnostic[]` as
first assumed (a reasonable but wrong guess by analogy with `EvalInteraction`'s throwing form) — fixed
the pattern match to `Choice1Of2(_: FsiValue option)`. No other deviation; the design's setup (separate
plugin file, env-var config, FSI quoting its own call to get the `MethodInfo`) was built and run exactly
as specified.

**One design-time overcorrection worth flagging, since I set the bar myself and should report against
it honestly:** `01-design.md`'s setup section said to grep `Program.fs` for the literal substring
`"power"` and treat any hit as compromising the dynamic-origin claim. That check as literally stated is
too blunt — `Program.fs` necessarily contains the substring `"power"` in variable names (`powerMi`),
labels, and comments describing the experiment itself (23 occurrences). The check that actually matters
is whether `Target.power` appears anywhere the **host's own F# compiler resolves as a name** — and it
does not: `grep -n "Target\.power" Program.fs` finds it only inside two comments and inside the
`quoteExpr` **string literal** (line 158) that is handed to FSI for evaluation in FSI's own separate
compilation, never parsed or resolved by the host's own `dotnet build` of `spike.fsproj`. Reporting the
looser, more meaningful check's actual result rather than quietly narrowing the pre-registered one after
the fact: the host's own compiled assembly has no compiler-resolved reference to `Target.power`.

## Round 1 — cheapest falsifier: `MethodInfo` from a live FSI session

1. `fsiSession.EvalInteractionNonThrowing(pluginSourceText)` — `pluginSourceText` read via
   `File.ReadAllText` from `plugin/PowerPlugin.fsx`, a file `Program.fs` never `#load`s, `#r`s, or
   otherwise references. Zero diagnostics; `Target.power` now exists only inside the FSI session's own
   dynamic assembly.
2. `fsiSession.EvalExpressionNonThrowing("match <@ Target.power 0 0.0 @> with | ... Call(_, mi, _) -> mi | ...")`
   — the quoting happens **inside FSI**, where `Target` is a name FSI itself just bound; the host process
   never resolves that name. Returned a `MethodInfo` via `FsiValue.ReflectionValue`.
3. **Confirmed, not assumed:** `mi.Module.Assembly.FullName` = `"FSI-ASSEMBLY, Version=0.0.0.32767,
   Culture=neutral, PublicKeyToken=null"`; the host's own `Assembly.GetEntryAssembly().FullName` =
   `"spike, Version=0.8.6.0, Culture=neutral, PublicKeyToken=null"`. Different assemblies — the
   `MethodInfo` genuinely originates from FSI's dynamic compilation, not the host's.
4. `Expr.TryGetReflectedDefinition(mi)`, called from host code on that FSI-sourced `MethodInfo`,
   **returned `Some`** with the expected shape:
   `Lambda(n, Lambda(x, IfThenElse(Call(op_Equality,[n; Value 0]), Value 1.0, Call(op_Multiply,[x; Call(power,[Call(op_Subtraction,[n; Value 1]); x])]))))`
   — identical in structure to Q014's host-compile-time case. `[<ReflectedDefinition>]` metadata
   survives FSI's own compilation path exactly as it does the host's ordinary one; this was the one
   genuinely unknown mechanism this quartet existed to test, and it holds.

**Round 1 passed cleanly.** Wall-clock for FSI session creation through `TryGetReflectedDefinition`:
845ms (includes FSI startup cost Q014's number never paid — not comparable to Q014's total).

## Round 2 — capability claim: full pipeline, both origin and config dynamic

`n = 6` (from `Q015_POWER_N=6`, deliberately different from both Q014's `4` and the design's documented
default of `5`, so the result can't be explained by a hardcoded or cached value). Substituting into the
FSI-obtained body and running the same `unroll` from Q014 (unchanged) produced:

```fsharp
fun x -> x * (x * (x * (x * (x * (x * 1.0)))))
```

Six nested multiplications, matching `n = 6` exactly, zero references to `power`/`Target`. Spliced as
`module Power6Module\nlet powerN : float -> float = fun x -> ...`, reparsed and typechecked with **zero
diagnostics**.

**Correctness, two independent ways, both against the FSI-obtained `MethodInfo` invoked by reflection**
(not a host-side re-implementation — no such reference is possible, since `Target.power` is not a
resolvable identifier in host code):

| x | FSI `power(6,x)` via `powerMi.Invoke` | Expr-eval | Compiled-and-executed |
|---|---|---|---|
| 2.0 | 64 | 64 (match) | 64 (match) |
| 3.5 | 1838.27 | 1838.27 (match) | 1838.27 (match) |

The compiled-and-executed check reused Q014's stronger form unchanged: wrote the spliced module to a
real `.fs`, `checker.Compile`d it to a real `.dll`, `Assembly.LoadFrom`'d it, and invoked the real
compiled `powerN` by reflection. Zero errors from `checker.Compile`; both test points matched.

Structural check (reused verbatim from Q014, same scope caveat as before — a substring search, adequate
for this case, not a general "no residual generality" test): emitted text contains neither `"power"` nor
`"Target"` — confirmed false on both.

**Round 2 passed cleanly.** Wall-clock: 687ms. Combined Round 1 + Round 2: 1532ms, one sample — roughly
2.3x Q014's 652/657ms, almost entirely attributable to FSI session startup (845ms of the total), which
Q014 never paid since it never hosted FSI at all.

## Reading against the pre-registered thresholds

- Cheapest falsifier (FSI-sourced `MethodInfo` + `TryGetReflectedDefinition`): **passed.**
- Mechanism claim: **passed** — confirmed via a genuine cross-assembly check (`mi.Module.Assembly`
  differs from the host's), not asserted.
- Capability claim: **passed** — full pipeline with both the general implementation's source and its
  specializing config genuinely unavailable to the host at its own compile time, correctness verified
  two independent ways including real compiled execution, structural difference confirmed.

Against `00-hypothesis.md`'s thresholds, this reads as **SHIP**, and unlike Q014, the specific gap that
sank Q014's own SHIP reading (no FSI ran; both pieces were compile-time literals) is directly closed
here: FSI genuinely ran, and neither the implementation nor `n` was known to the host at its own compile
time, verified rather than asserted for both.

## What a review should press on

- **`unroll` is still the same hand-matched, single-shape rewrite Q014 shipped**, unchanged, and Q014's
  review named generalizing it (its follow-up 2) as explicitly out of scope for this quartet. This
  quartet does not touch that gap; a review should confirm this write-up doesn't imply otherwise anywhere.
- **`decompile`'s reparseable-surface coverage remains uncharacterized** beyond nested arithmetic and one
  lambda — same open item Q014's review named (its follow-up 3), not addressed here.
- **Q014's review's follow-up 4** (whether `TryGetReflectedDefinition`-style reflection can reach a
  general implementation living in the compilation *currently being built*, not an already-compiled FSI
  or plugin assembly — Myriad's actual dominant same-project usage) is **not tested by this quartet
  either.** FSI's dynamic assembly is, for this purpose, structurally the same kind of "already compiled,
  separately referenced" artifact as Q006's type-provider case — this quartet closes the "FSI ran, origin
  was dynamic" gap, not the "origin was the in-progress compilation" gap, and a review should check this
  write-up doesn't blur the two.
- **Single-sample timing**, same caveat as every prior quartet, no repeated trials.
- **ALC isolation remains untested**, inherited unresolved from Q003/Q014 and explicitly out of scope
  here too.
- **The design-time grep-check overcorrection** (see Deviations above) — a review should confirm the
  looser check I substituted (`Target.power` as compiler-resolved syntax, not any occurrence of the
  substring `"power"`) is actually the right bar, not a self-serving weakening.
