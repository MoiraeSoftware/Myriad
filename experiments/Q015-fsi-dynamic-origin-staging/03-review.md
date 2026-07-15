# Q015-fsi-dynamic-origin-staging / Movement 4 — Adversarial review

## Verified directly before writing this review

- **The spike reproduces cleanly at a third, independent `n`.** I built and ran with
  `Q015_POWER_N=7` (the executor used `6`, Q014 used `4`). It produced
  `fun x -> x * (x * (x * (x * (x * (x * (x * 1.0))))))` — exactly seven nested multiplications —
  zero typecheck diagnostics, zero `checker.Compile` errors, and correctness matching at both test
  points two independent ways (Expr-eval and compiled-and-executed): `x=2 → 128`, `x=3.5 → 6433.93`,
  i.e. `2^7` and `3.5^7`. The value of `n` is genuinely not baked in anywhere: a fresh env-var value
  produced a correctly-different unroll depth and correct arithmetic. Round 1 833ms / Round 2 672ms /
  combined 1505ms against the recorded 845/687/1532 — single-sample deltas, consistent. This quartet
  kept full, re-runnable source and it runs.
- **FSI genuinely ran, and the `MethodInfo` genuinely originates from FSI's dynamic assembly.** This is
  the specific thing Q014 did not do (Q014 objection 1: "no FSI ran"). In my own run
  `mi.Module.Assembly.FullName` = `FSI-ASSEMBLY, Version=0.0.0.32767` and `mi.DeclaringType.FullName`
  = `FSI_0001+Target.power`, while the host's `GetEntryAssembly().FullName` = `spike, Version=0.8.6.0`.
  Different assemblies, printed and checked in-code (`Program.fs:169`, which `failwith`s if they're
  equal), not asserted in prose. The general implementation's compiled IL is genuinely not in the host
  assembly. Q014's objection 1 is closed for real.
- **`TryGetReflectedDefinition` on an FSI-sourced `MethodInfo` returned `Some` with the identical shape
  to Q014's host-compiled case.** My run printed
  `Lambda(n, Lambda(x, IfThenElse(Call(op_Equality,[n; Value 0]), Value 1.0, Call(op_Multiply,[x; Call(power,[Call(op_Subtraction,[n; Value 1]); x])]))))`
  — byte-for-byte the structure Q014 got from a host-compile-time method. No workaround, no degraded or
  differently-shaped `Expr`. This is the one genuinely-new mechanism the quartet existed to test, and it
  holds: `[<ReflectedDefinition>]` metadata survives FSI's own dynamic-assembly compilation path and is
  retrievable from host code. Going in this could have been KILL (the metadata could have failed to
  survive FSI's path); it didn't.
- **The substituted grep-check is sound, and I re-ran it myself.** `Target.power` appears in `Program.fs`
  only at lines 148, 156 (comments), 158 (inside the `quoteExpr` **string literal** handed to FSI), and
  219 (comment). The host's own F# compiler resolves `Target.power` as a name at exactly zero sites. The
  bare substring `power` appears 22 times (variable `powerMi`, `Power%dModule`, comments) — which is why
  the design's literal "any `power` hit compromises the claim" check is unusable, and the executor was
  right to narrow it (see objection 4).

## Strongest objections

1. **FSI performs none of the staging. It compiles a plugin and hands back a `MethodInfo`; 100% of the
   partial-evaluation is host-compiled code byte-identical to Q014.** This is the single most important
   thing that must travel with any citation of this quartet, and the results file does not foreground it.
   Trace what FSI actually evaluates: (a) `EvalInteractionNonThrowing(pluginSourceText)` — this *defines*
   `Target.power` (parse/typecheck/emit into a dynamic assembly), it does not run it; (b)
   `EvalExpressionNonThrowing("match <@ Target.power 0 0.0 @> with Call(_, mi, _) -> mi | ...")` — this
   evaluates a **quotation-destructure one-liner**. `<@ Target.power 0 0.0 @>` builds an `Expr` tree and
   pattern-matches out a `MethodInfo`; it never executes `power`. The `0` and `0.0` are placeholder
   arguments needed only to form a well-typed call to quote. So FSI's total contribution is: compile the
   plugin, and return a `MethodInfo`. The `unroll` / `substVar` / `evalIntExpr` that do the actual
   specialization (`Program.fs:98-128`) are ordinary host-compiled functions, unchanged from Q014, and
   they run in host code. Even the correctness reference (`powerMi.Invoke`, `:220`) is plain reflection on
   the `MethodInfo`, not FSI evaluating anything. The lineage's grand framing — Q014's title and the
   FINDINGS digest's "can FSI evaluate real *computation* at generation time" — is **not** what happens
   here: FSI is a delivery mechanism for a `MethodInfo` that Q014 got for free at compile time, plus the
   genuinely-new fact that the `ReflectedDefinition` metadata rides along through FSI's compilation path.
   This does not sink Q015 (see verdict — Q015's own claims never asserted FSI does the staging), but it
   sharply narrows what "closed the gap" means.

2. **The origin is dynamic-body / static-interface — the host still knows the plugin's exact shape at its
   own compile time.** The `quoteExpr` string (`:158`) hardcodes `Target.power 0 0.0`: the host author
   knew the module name (`Target`), the function name (`power`), the arity (2), and the argument types
   (`int`, then `float`) when they wrote `Program.fs`. Change the plugin to a differently-named function,
   a different arity, or a non-`int→float→float` signature and line 158 fails to compile inside FSI.
   Worse, `unroll` (`:114-128`) is still the same three-arm rewrite hand-matched to `power`'s exact node
   shapes (`IfThenElse(n=0…)`, `op_Multiply`, the `powerMi` self-call) that Q014's review objection 3
   dissected. So what genuinely arrives at runtime is the recursive *body text*; the *interface and the
   entire specialization strategy* are still static host code. "Dynamic origin" is true of the
   implementation body and the scalar config, and only those. The config, moreover, is a single `int` from
   an env var — the cheapest possible "dynamic" input, nothing like parsing attributed source.

3. **Composition with Myriad's real architecture (Q014 objection 2) remains untested, and FSI's dynamic
   assembly is a soft version of the boundary Myriad actually needs.** Myriad's real case is: general
   implementation in a separate project loaded through `McMaster.NETCore.Plugins` under an
   `AssemblyLoadContext`-isolated context, with config coming from parsing the attributed source being
   generated against. This spike uses FSI's **non-isolated** dynamic assembly (ALC isolation explicitly
   out of scope, inherited from Q003/Q014 — and the reason Q003's clean cross-boundary cast worked at all
   was that FSI resolved to the host's own `FSharp.Core` identity, precisely the thing ALC isolation
   changes). The results file's own "what a review should press on" concedes FSI's dynamic assembly is
   "structurally the same kind of already-compiled, separately-referenced artifact" as the referenced-
   assembly case — which is correct, and means this quartet says nothing about the in-progress-compilation
   wall (Q014 follow-up 4 / Q006's wall), and nothing about whether the pipeline survives ALC isolation.
   It is the friendly slice of the dynamic-origin space, not the Myriad-relevant one.

4. **The substituted grep-check is legitimate, but what it certifies is narrower than the write-up's
   "dynamic origin" language implies.** The executor replaced the design's blind `Contains("power")` bar
   with "does the host's compiler resolve `Target.power` as a name," and that substitution is correct, not
   self-serving: the blind check would flag `powerMi`, the variable whose entire existence is the point of
   the quartet, so the pre-registered check was simply mis-specified. But the honest reading of the result
   is that the host source *does* contain `Target.power` as a hand-written string literal (`:158`) — the
   check certifies "no *compiler-resolved* reference," which is true, while that very string is the
   evidence for objection 2 (the host knows the interface). The check is the right bar; it should not be
   read as "the host knows nothing about the plugin," only as "the host does not link or invoke the
   plugin's compiled code."

5. **Is this "Q014 again with extra ceremony" (the pre-registered NULL)? Nearly, but not quite — one
   genuinely-new, could-have-failed mechanism fact separates them.** The specialization output is
   identical in kind to what Q014 produced; the pipeline is byte-identical; the staging logic is
   identical; the config's influence (unroll depth) is the same mechanism Q014 already had. If the only
   change were "the `MethodInfo` took a scenic route through FSI," this would be NULL. What rescues it from
   NULL is narrow and real: nobody had established that `[<ReflectedDefinition>]` metadata survives FSI's
   dynamic-assembly compilation and that `TryGetReflectedDefinition` works from host code on an
   FSI-sourced method. That was a live KILL risk going in (the pre-registration says so explicitly), and
   it resolved positive with the *identical* `Expr` shape. That is a new capability datum, not ceremony —
   but it is one datum, and it is the whole of what's new over Q014.

6. **Single-sample timing, and the FSI-startup cost dominates.** 845ms of the 1532ms combined is Round 1,
   almost entirely FSI session creation. One sample, no baseline, correctly caveated, not load-bearing on
   any verdict. Note only that the "2.3x Q014" comparison in the results is apples-to-oranges by the
   results' own admission (Q014 never paid FSI startup), so it should not be read as a regression.

## Verdict

**SHIP, narrowly scoped — like Q010, not like Q014.**

The distinction from Q014 is threshold-mechanical, not a matter of taste. Q014 was held below SHIP for a
**literally unmet SHIP conjunct**: its threshold named "partially evaluated *at FSI time*" and no
`FsiEvaluationSession` ran anywhere in that spike. Q015's four SHIP conjuncts (`00:109-115`) are each
literally met and I independently reproduced all four at `n=7`:

1. Cheapest falsifier passes — FSI-sourced `MethodInfo` obtained, confirmed cross-assembly,
   `TryGetReflectedDefinition` returns `Some` with the expected shape. **Met, verified.**
2. Full pipeline with both the general implementation and its config genuinely unavailable to the host at
   compile time produces spliced source typechecking with zero diagnostics. The implementation *body* is
   genuinely unavailable (compiled only inside FSI's dynamic assembly, verified cross-assembly; the host
   cannot link, invoke, or self-reflect it), and the config is a runtime env var. **Met, verified** —
   with the objection-2 caveat that the *interface* is still static, which the conjunct's wording
   ("general implementation … unavailable") does not reach, since a string naming a function is not the
   implementation.
3. Correctness verified by compiling and executing the spliced text, matching the FSI-defined
   implementation's behavior via reflection on the FSI-obtained `MethodInfo`. **Met, verified** at `n=7`.
4. Output structurally unrolled, no residual reference to the general implementation. **Met, verified.**

The **pre-registered REVISE condition did not occur.** REVISE was defined (`00:116-119`) as the mechanism
working "only under a narrower condition … e.g. `TryGetReflectedDefinition` on an FSI-sourced
`MethodInfo` returns `None` or a differently-shaped `Expr` … requiring a workaround not originally
anticipated." The opposite happened: it returned `Some` with the *identical* shape, no workaround, first
clean run. Downgrading to REVISE anyway would require importing a new bar the pre-registration did not set
— exactly the goalpost-moving this methodology exists to prevent. **NULL is wrong** by a thin but real
margin (objection 5: one genuinely-new, could-have-been-KILL mechanism fact was established). **KILL is
wrong** (falsifier passed, reproduced).

So: SHIP on the letter. But the scoping caveats must travel as loudly as Q010's did, because a bare
"Q015 SHIP" will otherwise be cited as "dynamic-origin FSI staged compilation works," which overclaims on
two specific axes.

**The narrower claim that actually survives, stated exactly:** a `[<ReflectedDefinition>]`-bearing
`MethodInfo` for a recursive function *defined only inside a live `FsiEvaluationSession`* (from plugin
text the host reads at runtime and never compiles) can be handed back across the FSI/host boundary, and
`TryGetReflectedDefinition` yields the same quotation shape as the host-compiled case, so Q014's unchanged
reification pipeline specializes it — driven by a config value read at runtime from an env var — into F#
source that typechecks and, verified by compiling and executing it, runs correctly with the recursion
fully unrolled. This closes Q014's specific "no FSI ran / both pieces were compile-time literals" gap: FSI
genuinely ran, and both the implementation *body* and the config *value* were genuinely outside the host's
compiled source.

**What it does NOT establish, and must not be cited for:**
- **FSI evaluating the computation or performing the staging.** FSI compiled a plugin and returned a
  `MethodInfo`; the partial-evaluation is 100% host-compiled code identical to Q014 (objection 1). The
  lineage's "FSI evaluates real computation at generation time" framing is still not demonstrated.
- **A fully dynamic interface.** The host hardcodes `Target.power`'s name, arity, and types (`:158`), and
  `unroll` is still hand-matched to `power`'s node shapes; only the body text and a scalar config are
  dynamic (objections 2, 4).
- **Composition with Myriad's real architecture** — ALC-isolated plugin loading, config from parsed
  attributed source, generic specialization — all untested; FSI's non-isolated dynamic assembly is the
  friendly slice, not the Myriad-relevant one (objection 3).
- **Anything about the in-progress-compilation wall** (Q006 / Q014 follow-up 4), **generalizing `unroll`
  past `power`** (Q014 follow-up 2), or **`decompile`'s reparseable surface** (Q014 follow-up 3) — all
  explicitly scoped out here and correctly so.

Does the "MethodInfo-extraction one-liner vs. FSI-evaluates-actual-computation" distinction change my read
of what this quartet proved? Yes, decisively for *scope*, not for *verdict*. It means the honest headline
is "an FSI-sourced `ReflectedDefinition` `MethodInfo` feeds the unchanged host-side pipeline," not "FSI
does staged compilation." Q015's own pre-registered claims (`00:29-40`) were correctly written to the
former, narrower thing, so the distinction narrows the citation without breaching any conjunct.

**Follow-ups, if this line continues:**
1. Make FSI do work that is actually *computation*, not quotation-destructuring: have the FSI session
   itself run the general implementation against the config and return a value/quotation the host could
   not have produced structurally — otherwise FSI's role stays "compile a plugin, hand back a
   `MethodInfo`," which reflection over a referenced assembly already does more cheaply.
2. Discover the plugin's interface at runtime instead of hardcoding `Target.power 0 0.0` — enumerate the
   FSI assembly's `[<ReflectedDefinition>]` members reflectively and pick the target by attribute, so the
   host does not statically encode the name/arity/types. Only then is the *interface* dynamic, not just
   the body.
3. Re-run under `AssemblyLoadContext` isolation (the configuration Myriad's `McMaster.NETCore.Plugins`
   loader actually uses), the pillar inherited unresolved from Q003/Q014 — the clean cross-boundary
   behavior here depends on FSI sharing the host's `FSharp.Core` identity, which isolation changes.
4. Replace the env-var `int` with config parsed from attributed F# source, to make the config side as
   dynamic as Myriad's real input rather than a single scalar.
