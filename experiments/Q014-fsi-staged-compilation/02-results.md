# Q014-fsi-staged-compilation / Movement 3 — Results

**Status:** EXECUTED. See `artifacts/spike/` for the real project (`spike.fsproj`, `Program.fs`,
`run-output.txt` — full captured console output of the run this file reports).

**Environment:** `dotnet run` (net9.0), `FSharp.Compiler.Service 43.9.101` (matches
`Myriad/paket.lock`), `Unquote 7.0.1` (latest on NuGet at spike time; no version was pre-pinned in
`01-design.md`, since Round 1 itself decides whether Unquote is usable at all — recorded here for
reproducibility). Same `FSharp.Core` version-constraint warning (`NU1608`, resolved `9.0.303` vs.
FCS's declared `9.0.101`) every prior quartet using `FSharp.Compiler.Service` has hit; harmless, not
investigated further, consistent with prior quartets' own treatment of it.

## Deviations from `01-design.md`, reported honestly per this repo's own discipline

1. **Two of my own harness bugs, unrelated to the research question, found and fixed while
   running — not silently patched:**
   - A `sprintf "%LF"` format specifier for float literals was invalid F# (`FS0741`); fixed to
     `%f`. Pure typo, no bearing on any claim.
   - The first correctness-strengthening pass (see point 2) spliced `let power4 (x: float) : float
     = <decompiled text>`, but the decompiled text is itself already `fun x -> ...` — binding it
     under an extra explicit parameter produced a function returning a function, which correctly
     failed to typecheck (`FS0auto: This function takes too many arguments`). Fixed by reverting to
     `let power4 : float -> float = <text>` (binding the lambda directly, exactly as the design's
     own Round 2 sketch specified). This was a mistake in my own splice code, not a translator
     defect — recorded because it's exactly the kind of thing this repo's discipline says to report
     rather than quietly fix and move on.
2. **Strengthened the Round 2 correctness check beyond what `01-design.md` explicitly specified.**
   The design's own text said only "the generated specialized code's output matches the general
   implementation's output," without pinning down *how*. My first implementation checked this by
   evaluating the *quotation* (`Expr.Cast<float>(...).Eval()` via Unquote) — real, but it never
   touches whether the *translated text itself*, once compiled, behaves correctly; a bug specific to
   `decompile`'s text rendering could pass that check invisibly. I added a second, independent check:
   write the spliced source to a real `.fs` file, compile it with `checker.Compile` to a real `.dll`,
   `Assembly.LoadFrom` it, and invoke the actual compiled `power4` via reflection. Both checks now
   run and both are reported below. **Flagging this plainly for whoever runs Movement 4:** this
   strengthening was my own addition during execution, not literally pre-registered — the review
   should treat it as a real result to verify on its own merits, not accept it on my say-so just
   because it happens to make the outcome look more solid.

No other deviations. The rest of the design (translator choice logic, the `power`/`n=4`
specialization scenario, the `[<ReflectedDefinition>]`/`ExprShape` mechanism) was built and run
exactly as `01-design.md` specified.

## Round 1 — cheapest falsifier

**Round 1a (Unquote's `decompile`).** `Swensen.Unquote.Operators.decompile <@ 1 + 2 @>` produced the
string `"1 + 2"`. Spliced as `let trivialResult = 1 + 2`, checked via
`checker.ParseAndCheckFileInProject` against `GetProjectOptionsFromScript` — **zero diagnostics,
reparsed and typechecked cleanly.**

**Round 1b (hand-rolled fallback).** A narrow `ExprShape`-based translator, covering only `Value`
(int/float/bool), `Var`, `Lambda`, and `Call` nodes matched against `MethodInfo`s obtained by
quoting trivial uses of the operator (`miOf <@ 1 + 1 @>`, etc. — the Falanx-pain-avoidance technique
named in the design's recon section, not manual `typeof<...>.GetMethod(...)` reflection). Produced
`"(1 + 2)"` for the same trivial case — **zero diagnostics, reparsed and typechecked cleanly.**

**Both translators cleared the cheapest falsifier.** Round 2 was built to prefer Unquote's `decompile`
when available and fall back to the hand-rolled translator otherwise (per the design's stated
preference order), rather than picking one in advance.

## Round 2 — capability claim: `power` specialized at `n = 4`

`Target.power`'s body, retrieved via `Expr.TryGetReflectedDefinition` on a `MethodInfo` obtained by
quoting `<@ Target.power 0 0.0 @>` (again, no manual `Expr.Call` construction anywhere in this
pipeline):

```
IfThenElse (Call (None, op_Equality, [n, Value (0)]), Value (1.0),
    Call (None, op_Multiply,
        [x, Call (None, power, [Call (None, op_Subtraction, [n, Value (1)]), x])]))
```

After substituting `n = 4` (via the generic `ExprShape`-based `substVar`) and running the narrow,
hand-matched `unroll` partial evaluator (which inlines the recursive `power` call and folds the
`IfThenElse` once its condition is statically known — exactly the mechanism the design's recon
section named, not general-purpose partial evaluation over arbitrary recursive shapes):

```
Call (None, op_Multiply, [x,
  Call (None, op_Multiply, [x,
    Call (None, op_Multiply, [x,
      Call (None, op_Multiply, [x, Value (1.0)])])])])
```

**Zero recursive calls, zero reference to `n` or `power` remain — the unroll terminated correctly
in exactly 4 steps, bounded by the statically-known `n = 4`, as predicted.**

**Translation:** `decompile` (Unquote), applied to the *whole specialized lambda*
(`Expr.Lambda(xVar, unrolled)`) — not just the trivial case Round 1 tested — produced:

```fsharp
fun x -> x * (x * (x * (x * 1.0)))
```

This reparsed and typechecked with **zero diagnostics** when spliced as
`let power4 : float -> float = fun x -> x * (x * (x * (x * 1.0)))`. Confirms Round 1a's translator
generalizes past the trivial single-operator case to a real nested-`Lambda`/multi-level-`Call` shape,
which was genuinely unverified going in — the design flagged this exact generalization as unknown.

**Correctness, checked two independent ways:**

| x | `Target.power 4 x` | Expr-eval (`Unquote`'s `Eval()` on the unrolled `Expr`) | Compiled-and-executed (`checker.Compile` → load → invoke) |
|---|---|---|---|
| 2.0 | 16 | 16 (match) | 16 (match) |
| 3.5 | 150.0625 | 150.0625 (match) | 150.0625 (match) |

The second, stronger check compiled the actual spliced source text to a real `.dll` via
`checker.Compile(...)`, `Assembly.LoadFrom`'d it, and invoked the real compiled `power4` by
reflection — not a re-evaluation of the quotation, an execution of the artifact FCS itself accepted.
Both checks agree; no divergence between the quotation's own evaluation and the compiled text's
runtime behavior was observed for either test point.

**Structural-difference check:** the emitted text contains neither `"power"` nor `"Target"` —
confirmed by direct substring check against the spliced text — meaning no residual reference to the
general recursive implementation survives. This is the concrete evidence for "specialized, not
re-templated," the distinction the hypothesis's SHIP threshold turns on.

## Round 3 — cost (single sample)

Full Round 2 pipeline (`TryGetReflectedDefinition` → substitute → unroll-to-fixpoint → `decompile` →
splice → `ParseAndCheckFileInProject` → **`checker.Compile` to a real assembly → load → invoke twice**)
measured **652ms**, one sample, `System.Diagnostics.Stopwatch`. This number includes the strengthened
correctness check's real `checker.Compile` call, which Q011/Q012/Q013 already established costs
materially more than `ParseAndCheckFileInProject` alone (106–201ms in their measurements, for
comparison) — so 652ms is not directly comparable to any single-mechanism number from an earlier
quartet; it's the cost of this quartet's specific, more thorough pipeline, reported as one sample per
this repo's own standing caveat about not running repeated trials.

## Reading against the pre-registered thresholds

- Cheapest falsifier: **passed**, both translators.
- Mechanism claim (an `Expr` translates to text FCS accepts, reparses, and typechecks): **passed**,
  for both the trivial case and the real specialization scenario's `Lambda`/nested-`Call` shape.
- Capability claim (genuine specialization, verified correct, structurally different from the
  general implementation): **passed**, correctness confirmed two independent ways including actual
  compiled execution, structural difference confirmed by direct inspection of the emitted text.

Against `00-hypothesis.md`'s own thresholds, this result reads as **SHIP** on the letter of what was
pre-registered. That verdict is not declared here — Movement 4's job, not Movement 3's, and this
quartet's own design flagged real, specific angles a review should press on before accepting it (see
below), consistent with this session's standing rule not to let the execution step's own framing
stand in for adversarial review.

## What a review should press on, named honestly rather than left for Movement 4 to discover cold

- **Exactly one recursive shape was tested (`power`), and `unroll` is hand-matched to that shape's
  specific structure** (a single `IfThenElse` guarding one self-recursive `Call` inside one
  multiplication) — not a general partial evaluator over arbitrary recursive quotations. The design
  pre-registered this narrowness explicitly, but a review should still check whether "SHIP" here
  risks being read as "staged compilation over arbitrary recursive F# functions now works," which
  this result does not show.
- **`N = 4` is small.** The design chose it as "the smallest N that still makes unrolled-vs-recursive
  unambiguous," but this was not stress-tested against a larger N where the unrolled expression tree
  or translated text might hit a different limit (translator output length, FCS parse depth,
  `decompile`'s own behavior on a much longer chain).
- **The structural-difference check is a literal substring search** (`text.Contains("power")`) —
  crude but, for this specific case, sufficient (the alternative would be false only if `decompile`
  happened to name something coincidentally containing that substring, which didn't happen and isn't
  a live risk for this exact scenario).
- **Round 2's correctness-check strengthening (point 2 above) happened during execution, not before
  it** — real and independently verified, but a review should confirm it actually does what it
  claims (compiles and runs the *spliced text*, not some other artifact) rather than taking this
  write-up's framing at face value, per this session's own standing rule about execution-step
  self-assessment.
- **Single-sample timing**, as flagged, and no comparison was drawn against a non-staged baseline
  (e.g., what a template-driven generator's cost would be for the same output) — the design didn't
  pre-register one, and none is claimed here.
- **ALC isolation remains untested**, exactly as scoped out in `01-design.md`, inherited unresolved
  from Q003.
