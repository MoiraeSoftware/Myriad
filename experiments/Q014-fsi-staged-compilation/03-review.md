# Q014-fsi-staged-compilation / Movement 4 — Adversarial review

## Verified directly before writing this review

- **The spike reproduces cleanly, independently re-run.** `dotnet run` in `artifacts/spike/` a second
  time produced the identical emitted text (`fun x -> x * (x * (x * (x * 1.0)))`), identical Round 1a/1b
  pass/pass, identical Round 2 correctness table (x=2→16, x=3.5→150.0625, all four cells match across
  both the Expr-eval and compiled-and-executed checks), identical `mentions 'power'/'Target': false`, and
  a Round 2 wall-clock of 657ms against the recorded 652ms — a single-sample delta, consistent. Unlike
  Q006/Q008/Q009, this quartet kept full, re-runnable source and it runs.
- **The strengthened correctness check does what `02-results.md` claims it does.** Read
  `Program.fs:210-231` directly, not the write-up's paraphrase: it writes the *spliced module source*
  (`module Power4Module\nlet power4 : float -> float = fun x -> ...`) to a real `.fs` on disk, calls
  `checker.Compile` with `fsc`-style args to a real `.dll`, `Assembly.LoadFrom`s it,
  `GetMethod("power4")`, and `Invoke`s it with a boxed `float`. This is a genuine execution of the
  artifact FCS accepted, not a re-evaluation of the in-memory `Expr`. The strengthening is real, it was
  added mid-run as the results file honestly flags, and it is the single most valuable thing in the
  spike — it closes the gap where `decompile`'s text could be subtly wrong while the quotation still
  evaluates correctly. Credit where due: this is a stronger check than the pre-registration required, and
  the executor flagged it as their own addition rather than smuggling it in.
- **The unroll terminated to a genuine multiplication chain with no residual recursion.** The raw
  reflected body (`IfThenElse(n=0, 1.0, x * power (n-1) x)`) and the unrolled result (four nested
  `op_Multiply` calls bottoming out at `Value 1.0`) are exactly as printed. Zero `power`/`n` references
  survive. The structural claim is real for this case.

## Strongest objections

1. **No FSI ran. The titular mechanism — and the SHIP threshold's own "partially evaluated *at FSI
   time*" clause — is absent from the spike entirely.** This is the finding the review exists to surface,
   and neither `02-results.md`'s deviations section nor its "what a review should press on" list mentions
   it. Read the whole of `Program.fs`: there is no `FsiEvaluationSession`, no
   `FSharp.Compiler.Interactive.Shell`, no FSI hosting of any kind. The "general implementation"
   (`Target.power`) is an ordinary module compiled into the spike's own assembly; its quotation is pulled
   by `Expr.TryGetReflectedDefinition` via plain reflection; and the "partial evaluation" runs as ordinary
   compiled host code (`unroll`, `Program.fs:147-164`). Nothing is evaluated at generation time by an FSI
   session. The hypothesis (`00`) repeatedly frames the claim as "can FSI evaluate real *computation* at
   Myriad-generation-time" and its SHIP threshold names "partially evaluated **at FSI time**" as an
   explicit conjunct. That conjunct was not satisfied — not narrowly, but not at all. The design (`01`)
   had already silently substituted ReflectedDefinition-at-host-compile-time for FSI-evaluation-at-
   generation-time (its recon and reproduction steps never instantiate FSI either), and the execution
   faithfully ran that FSI-free design. Under a charitable reading the hypothesis's own scoping note
   (`00:40-50`) pivots the target to an `Expr<'T>` quotation and away from "arbitrary FSI-evaluated
   closure," so "FSI" can be read as loose shorthand for "generation-time metaprogramming." But the
   quotation here came from a `[<ReflectedDefinition>]` attribute baked into the host at *host* compile
   time, which is a materially different mechanism from Q003's evaluate-in-FSI→splice→typecheck loop this
   quartet claims to complete. Q003's dependency (FSI/FSharpChecker coexistence, cited as a validity
   precondition) was neither exercised nor re-validated. The result is real; it is not the thing on the
   manifest.

2. **The "static configuration" is a source-level literal in the same program, so the staging boundary
   that would make this a Myriad capability was never crossed.** `n = 4` is written directly as
   `Expr.Value(4)` at `Program.fs:176`, and `Target.power` is defined a few lines up in the same file.
   Both the general implementation and its specializing argument are known when the spike itself is
   compiled. The entire reason Myriad (or Q003's FSI motivation) wants generation-time evaluation is that
   in the real case *neither* is known at host-compile-time: the general implementation lives in a
   dynamically-loaded plugin, and the "static config" comes from the attributed source being generated
   against. This spike is a compile-time-constant specialization performed inside a program where
   everything relevant is already a compile-time constant. That is a legitimate partial-evaluation
   demonstration, but it does not exercise the dynamic-origin boundary, and it is the same shape of trap
   Q001 named: a mechanism that looks clean in a harness where the hard part is absent.

3. **`unroll` is not a partial evaluator over a subset of shapes — it is a bespoke rewrite of exactly
   one function's AST, written by hand.** Its three match arms (`Program.fs:149-153`) are literally
   `power`'s three node types: the `IfThenElse(n = 0, ...)` guard, the `x * <rec>` multiplication, and
   the `power` self-call; `evalIntExpr` (`:138-142`) handles only `Value` and subtraction — exactly the
   `n - 1` argument. Change `power` to `x * x * power (n-2) x`, to an accumulator form, or to `if n <= 0`,
   and every arm throws "unsupported node shape." The design admitted hand-matching and the results
   flagged it, but it is worth stating more sharply than either did: this is not "a partial evaluator that
   happens to cover a narrow subset," it is one function transcribed into a rewrite rule. Notably, the
   design's own recon (`01:22-29`) named the generic `ExprShape`-based "expand" pattern (inline any
   `ReflectedDefinition` call, beta-reduce) as "most of the partial evaluator Round 2 needs, already
   designed" — that generic expander was **not** built. Only the generic *substitution* (`substVar`,
   `:130-134`) is shape-agnostic; the fold/unroll that does the actual specialization is not. The more
   defensible mechanism the design gestured at is untested; the bespoke one is what shipped. The
   capability claim generalizes zero inches past `power`.

4. **The translator that produced the shipping result is not the one whose coverage was characterized.**
   The validity precondition (`00:114-120`) demanded reporting "which `Expr` node shapes the translation
   handles and which it doesn't." That was met for the hand-rolled `translate` (`Program.fs:90-101`:
   `Value` int/float/bool, `Var`, `Lambda`, +/−/*/= over int/float) — but `translate` was only ever
   exercised on `<@ 1 + 2 @>` in Round 1b. Round 2's actual output came from Unquote's `decompile`
   (`round1TranslatorWorks` was true, so the `Option.defaultWith` fallback to `translate` at `:187` never
   fired). `decompile` is a general-purpose `Expr` pretty-printer Unquote maintains for human-readable
   test-failure messages; its reparseable-shape coverage is neither characterized nor bounded anywhere in
   this quartet. The mechanism claim as tested is therefore "Unquote's `decompile` renders nested
   arithmetic and a lambda into reparseable text," and how far that generalizes past nested `op_Multiply`
   is exactly what is not shown. The design flagged `decompile` as "not guaranteed reparseable" and Round
   1 tested `1 + 2`; Round 2 tested a multiplication chain — both squarely inside the resugarable-
   arithmetic surface. The claim "an `Expr` survives translation to reparseable text" holds only as far as
   `decompile`'s own resugaring is reparseable, which is an open-ended, untested surface.

5. **The mechanism has its own "must already be compiled" precondition that re-opens a version of
   Q006's wall — and the hypothesis's "Thread 1 was never subject to that wall" framing glosses over
   it.** `Expr.TryGetReflectedDefinition` reads a quotation that the compiler embedded into a *compiled
   assembly*. In the spike that assembly is the spike itself, so the general implementation is trivially
   available. In Myriad's dominant usage (`[<Attribute>]` on code in the file/project currently being
   built), the general implementation would be in the compilation-in-progress and there would be no
   compiled assembly to reflect on — structurally the same "can't see the compilation you're part of"
   constraint Q006 hit, arriving by a different door. It is real only for the referenced-pre-compiled-
   assembly case, which is the narrow cross-assembly slice, not Myriad's common shape. No part of the
   spike touches Myriad's plugin loader, `McMaster.NETCore.Plugins`, or `AssemblyLoadContext` isolation
   (inherited unresolved from Q003, as scoped) — so whether this composes with Myriad's real architecture
   at all is untested, and there is at least one concrete reason to think the same-file case does not.

6. **The substring structural check is adequate here but must not be read as a general "no residual
   generality" test.** `text.Contains("power") || text.Contains("Target")` (`:235`) is sound for this
   exact output — I can see the emitted text and it contains neither. But it only asserts two specific
   strings are absent; it would pass a specialization that legitimately still referenced a shared helper,
   and it says nothing about residual structure beyond those two names. For this scenario it is not a live
   risk; do not carry it forward as a reusable "is it really specialized" check.

7. **Single-sample timing, dominated by the strengthening's `checker.Compile`.** 652/657ms is one
   sample, includes a full FCS compile plus first-call JIT warmup of the whole stack in one number, has no
   baseline, and is not gated by the design. Correctly caveated; not load-bearing; nothing rests on it.

## Verdict

**REVISE.**

The four literal checkboxes of the SHIP threshold were ticked and reproduced: the cheapest falsifier
passed (both translators), a genuine specialization scenario translated to spliced source that
typechecked with zero diagnostics, correctness was verified two independent ways (including actual
compiled execution, stronger than pre-registered), and the specialized output is demonstrably different
in shape from the general implementation. On a purely mechanical letter-of-the-checklist reading, and by
the Q010 precedent (SHIP with heavy scoping caveats travelling), one could label this a scoped SHIP.

I do not, for a threshold-anchored reason, not vibes: the SHIP threshold names "a genuine specialization
scenario (general implementation partially evaluated **at FSI time** against concrete static
configuration)" — and no FSI evaluation occurred (objection 1). The spike performed compile-time-constant
specialization in ordinary host code over a `[<ReflectedDefinition>]` quotation, with both the
implementation and the config baked in as source literals (objection 2). That is not the mechanism the
SHIP conjunct specifies. Meanwhile the result is a near-verbatim instance of the REVISE threshold's own
description: "the mechanism works but only for a narrow, explicitly-bounded subset of quotation shapes
(e.g. arithmetic and simple conditionals...) — real and worth keeping, but scoped to 'staged compilation
over a defined quotation subset,' not the general claim." Arithmetic and one simple conditional is exactly
the covered surface. NULL is wrong (the output genuinely is structurally different from what a template
could produce — a data-driven generator cannot unroll `power` without a partial evaluator, so this clears
the bar Q003 did not). KILL is wrong (the falsifier passed cleanly, twice).

**The narrower claim that survives, stated exactly:** a compiler-built F# quotation obtained via
`[<ReflectedDefinition>]` + `TryGetReflectedDefinition`, specialized by a hand-written rewrite matched to a
single recursive-arithmetic shape, can be rendered by Unquote's `decompile` into F# source text that FCS
reparses, typechecks with zero diagnostics, and — verified by compiling and executing that text, not just
evaluating the quotation — runs correctly with the general recursion fully unrolled away. This proves
*code*, not merely Q003's *data*, crosses the generation-time→source boundary and executes, for the
arithmetic/conditional subset. That is a real advance over Q003 and worth keeping.

**What it does not establish, and must not be cited for:**
- FSI-time evaluation of anything (no FSI session ran).
- Any dynamic (non-source-literal) origin for either the general implementation or the static
  configuration — the staging boundary Myriad actually needs was not crossed.
- Partial evaluation beyond the one hand-matched `power` shape (`unroll` is bespoke, not general).
- `decompile`'s reparseable coverage on any shape past nested arithmetic and a single lambda.
- Reachability within Myriad's dominant same-project usage, where the general implementation is in the
  compilation-in-progress and there is no compiled assembly to reflect on — a re-appearance of Q006's
  wall for this specific mechanism (objection 5).
- ALC isolation, still inherited unresolved from Q003.

**Follow-ups, if this line continues:**
1. Actually host `FsiEvaluationSession` and evaluate the general implementation and/or its config from a
   string not known at host-compile-time, then reify — that is the claim the hypothesis's title makes and
   the one this spike did not test. Only then does the "build the full loop Q003 named" framing hold.
2. Replace the bespoke `unroll` with the generic `ExprShape` "expand"/beta-reduce evaluator the design's
   own recon named but did not build, and re-run against at least a second, differently-shaped recursive
   function, to separate "the mechanism specializes recursive functions" from "someone transcribed
   `power` by hand."
3. Characterize `decompile`'s reparseable surface deliberately (feed it `Let`, `IfThenElse`,
   `PropertyGet`, `NewObject`, tuple/list literals) and record which round-trip and which do not, before
   any claim that "quotations round-trip to source" is generalized past nested arithmetic.
4. Test whether `TryGetReflectedDefinition` can reach a general implementation that lives in the
   compilation currently being built (objection 5) — if it cannot, this mechanism is confined to the
   cross-assembly case, the same boundary Q004 was queued to probe.
