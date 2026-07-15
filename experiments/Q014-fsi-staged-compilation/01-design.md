# Q014-fsi-staged-compilation / Movement 2 — Design

**Status:** NOT YET EXECUTED.
**Location:** scratch console project, `dotnet new console -lang F#`, package
`FSharp.Compiler.Service` pinned to `43.9.101` (matches `Myriad/paket.lock`, same convention as
Q001-Q005/Q010). Not part of any committed repo; artifacts saved under `artifacts/` in this quartet
folder once built. `FSharp.Compiler.Interactive.Shell` (the FSI host, `FsiEvaluationSession`) ships in
the same `FSharp.Compiler.Service` package — no extra reference needed, per Q003's own harness.

## Techniques recon (grounds the design in verified APIs, not guesses)

Before picking a mechanism, three real F# quotation facilities were checked against actual
documentation/source (not assumed), directly motivated by the reflection-construction pain named for
this quartet from real Falanx experience — hand-building `Expr.Call(methodInfo, args)` nodes requires
doing the compiler's own overload resolution and calling-convention matching by hand:

1. **`[<ReflectedDefinition>]` + `Expr.TryGetReflectedDefinition(methodBase)`.** A function marked
   `[<ReflectedDefinition>]` has its quotation embedded in the compiled assembly by the compiler
   itself; `TryGetReflectedDefinition` retrieves it via reflection on the function's `MethodInfo`. This
   quartet's "general implementation" (the thing to be specialized) is written this way — the `Expr` for
   it is compiler-correct by construction, no manual `Expr.Call` node-building at all.
2. **`FSharp.Quotations.ExprShape`'s `ShapeVar`/`ShapeLambda`/`ShapeCombination` active patterns plus
   `RebuildShapeCombination`.** A generic recursive decompose/rebuild pattern that doesn't require
   enumerating every `Expr` DU case by hand. A documented pattern built on exactly this ("Expanding
   quotations", F# Snippets) inlines every `ReflectedDefinition`-marked call with its body and
   beta-reduces `Let`/`Application` nodes — this is most of the partial evaluator Round 2 needs, already
   designed, not invented fresh here. Source-grounded reading only; the actual snippet's code has not
   been run as part of this repo and must be re-verified against this quartet's pinned `FSharp.Core`
   before being trusted, not copied blind.
3. **Quote-to-grab-a-`MethodInfo`, e.g. `match <@ 1 - 1 @> with | Call(_, mi, _) -> mi`**, used anywhere
   this design needs to recognize a specific operator/function by its `MethodInfo` (for constant-folding
   or for the translator's whitelist below) instead of manually reflecting for it — sidesteps the exact
   class of error (wrong overload, wrong calling convention) the Falanx-style manual construction risked.

**Left as unverified until Round 1, not assumed:** `Swensen.Unquote`'s `decompile` operator
(`Swensen.Unquote.Operators`) renders an `Expr` as a single-line, non-light-syntax string, per its own
documented decompiler features (precedence-aware parenthesization, resugared lambdas/applications,
binary/unary operators, type-name printing). It is designed for human-readable test-failure messages,
not guaranteed to be *reparseable* F# — that gap is exactly what Round 1 tests before this design
commits to using it as the translation mechanism. If added, it's a `PackageReference` in the scratch
project only (`experiments/` scratch, not `src/`), version pinned once confirmed usable, not before.

## Round 1 — cheapest falsifier: does any quotation survive round-trip to reparseable text?

Two candidate translators, tried in order of cost, on the single trivial case
`let trivial = <@ 1 + 2 @>`:

1. **Unquote's `decompile`.** Call it on `trivial`, capture the string, and directly test it — do not
   eyeball it — by splicing it as the body of a virtual file in the same `DocumentSource.Custom`
   harness Q001/Q003/Q010 already validated (`FSharpChecker.Create(..., documentSource =
   DocumentSource.Custom callback)`), then `checker.ParseAndCheckFileInProject`. Record whether it
   reparses and typechecks with zero diagnostics, or what specifically breaks (wrong operator spacing,
   an identifier `decompile` emits that isn't valid standalone F#, non-light-syntax assumptions that
   don't hold for a bare module-level binding, etc.).
2. **Hand-written fallback**, built only if (1) fails: a `ExprShape`-based recursive translator covering
   a deliberately narrow, named whitelist — `Value` (int/float/string/bool literals via
   `(|Int32|_|)`-style patterns on `Expr.Value`), `Var`, `Lambda`, `Let`, `IfThenElse`, and `Call` nodes
   whose `MethodInfo` matches a short table of known arithmetic/comparison operators (obtained via
   technique 3 above, not manual reflection). Explicitly not attempting full coverage — anything outside
   the whitelist should raise a clear "unsupported node" error, not silently mis-translate, so a KILL
   is honest rather than masked by wrong output that happens to typecheck by accident.

**What Round 1 must record:** which translator (if either) produced text that reparsed and typechecked
cleanly for the trivial case; the exact failure mode if Unquote's output didn't reparse as-is (this is
new, useful information regardless of which way it comes out); which translator Round 2 will use.

If neither translator produces reparseable text even for `1 + 2`: stop, do not attempt Round 2, and
record the KILL per the hypothesis's own pre-registered threshold.

## Round 2 — capability claim: real partial evaluation, not data-driven templating

**Scenario, chosen to be the smallest genuine instance of staged compilation, not a Myriad-domain toy
dressed up as one** — the classic power-function specialization, well-understood enough that a wrong
result is unambiguous, and small enough that "unrolled, no residual recursion" is checkable by eye as
well as mechanically:

```fsharp
[<ReflectedDefinition>]
let rec power (n: int) (x: float) : float =
    if n = 0 then 1.0 else x * power (n - 1) x
```

**The general implementation is not a strawman:** this is the standard textbook partial-evaluation
example precisely because it has a real, meaningful specialized form (no runtime branch, no runtime
recursion, a fixed-length multiplication chain) that a template-driven generator could not produce
without already knowing `n` — matching Q001's own "must test against something a syntax-only
alternative genuinely can't do" discipline, applied here to "a naive data-driven generator genuinely
can't do this specific transformation without a partial evaluator."

**Partial-evaluation procedure** (adapting the "expand" pattern named in the recon section above, built
and verified fresh against this quartet's own pinned packages, not copied unverified):

1. Retrieve `power`'s `Expr` via `Expr.TryGetReflectedDefinition` on its `MethodInfo`.
2. Fix `N = 4` as the static, compile-time-known argument (chosen as the smallest N that still makes
   "unrolled vs. recursive" visually and mechanically unambiguous — `N = 0` or `1` would be too
   degenerate to prove anything).
3. Substitute the `Var` for `n` with `Expr.Value(4)` (`ExprShape`-based generic substitution, not a
   hand-matched case for every node type).
4. Repeatedly constant-fold the resulting tree: reduce `IfThenElse` when its condition is a fully-known
   `Value` comparison (`4 = 0` folds to `false` first pass), and when a recursive `Call` back to `power`
   is encountered with a now-known first argument, replace that call node with `power`'s own body again
   (technique 1, re-fetched), substituting the new known `n`. Repeat until no recursive `Call` node
   remains — bounded by construction, since `N` is a fixed, known-in-advance literal (4 iterations,
   not an open-ended fixpoint).
5. The result should be an `Expr` equivalent to `fun x -> x * (x * (x * (x * 1.0)))` with no reference
   to `power` or `n` remaining.
6. Translate that `Expr` to source text via whichever translator Round 1 selected, splice as
   `let power4 (x: float) = <generated text>` into the same virtual-project harness, and
   `checker.ParseAndCheckFileInProject`.

**What Round 2 must record:**
- Zero diagnostics on the spliced text.
- **Correctness:** evaluate the generated `power4` for at least two concrete `x` values (e.g. `2.0`,
  `3.5`) and confirm the results match calling the original `power 4 x` directly — not just "it
  typechecks."
- **Structural difference, the actual capability claim:** confirm by inspecting the spliced text itself
  that it contains no recursive call and no reference to `n` — a literal, fixed-length multiplication
  chain. This is the concrete evidence for "specialized, not just re-templated data," the distinction
  the hypothesis's SHIP threshold turns on.
- Exact wording of any diagnostic if it fails, and which step (substitution, folding, or translation) is
  implicated.

If Round 2's constant-folding/inlining loop does not terminate cleanly at exactly 4 unrolled
multiplications, or produces an `Expr` shape the Round 1 translator can't handle (a realistic risk —
folding can introduce node shapes, like a chain of nested `Let`s from naive substitution, that the
trivial Round 1 case never exercised): report this precisely rather than special-casing the translator
to paper over it. That would be exactly the kind of narrow-subset REVISE result the hypothesis's own
pre-registered thresholds already anticipate, not a failure to hide.

## Round 3 — cost sanity check (single sample, honestly labeled)

Time Round 2's full sequence (`TryGetReflectedDefinition` → substitute → fold-to-fixpoint → translate →
splice → typecheck) with `System.Diagnostics.Stopwatch`, reported as one sample, consistent with every
prior quartet's own caveat about not running repeated trials. No comparison baseline is pre-registered
as required here (unlike Q006/Q008's live-edit-cost claims) — this quartet's SHIP threshold is about
mechanism and capability, not keystroke-latency practicality, so cost is recorded for the record, not
gated on.

## Reproduction

```
dotnet new console -lang F# -o q014-spike
cd q014-spike
dotnet add package FSharp.Compiler.Service --version 43.9.101
# Add Unquote only if Round 1 step 1 is attempted:
dotnet add package Unquote
# Program.fs per round, see 02-results.md for the actual output of each
dotnet run
```

No MSBuild, no `Myriad.Sdk`, no real `.fsproj` beyond the throwaway host's own — same deliberate
scoping as Q001/Q003/Q010: the question under test is what the compiler-and-quotation-hosting layer can
do, not the MSBuild integration layer.

## Explicit instruction to whoever executes this (do not silently deviate)

- Do not edit this file or `00-hypothesis.md` once execution starts. If reality diverges (Unquote's
  `decompile` signature differs from what's described here, `ExprShape` substitution behaves
  differently than expected, the fold loop doesn't terminate the way step 4 predicts), report the
  correction honestly in `02-results.md` — do not quietly patch the design to match what was built.
- If Round 1 fails for both translators: still record which specific node/text shape broke each one,
  per the hypothesis's KILL threshold's own request for a "significant, general finding," not just a
  bare pass/fail.
- If Round 2's fold loop produces a node shape Round 1's translator can't handle: report this as a
  concrete example of the "narrow, named whitelist" boundary the hypothesis's REVISE threshold
  anticipates, including the specific `Expr` shape that broke it.
