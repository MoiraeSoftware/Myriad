# Q025-typed-expr-interpreter / Movement 1 — Hypothesis

**Status:** PLANNED. Pre-registered only — not yet built. Awaiting go-ahead.
**Date:** 2026-07-17

**Question:** Can a generator get real generation-time *computation* — not just generation-time
*data* (Q003) and not a `MethodInfo` pointer to code the host already knows the shape of (Q014,
Q015) — by hand-walking FCS's own resolved, post-typecheck `FSharpExpr` tree and interpreting it
directly (reflection-invoking already-compiled reference-assembly methods for anything the walk
bottoms out on), with **no `FsiEvaluationSession` anywhere in the harness**? And does interpreting
the *typed tree the checker already produced* — rather than re-evaluating separately-hosted source
text the way FSI does — sidestep the specific failure mode Q015's review found (FSI handing back a
pointer to host-compiled code instead of doing the work itself)?

This is the general-purpose "does the compiler's own typed output support direct interpretation"
question, informed by inspecting `FSharp.Compiler.PortaCode` (added as an adjacent working
directory 2026-07-17) — not a port of that project, which is dead (last commit 2020-11-23, pinned
to `FSharp.Compiler.Service 38.0.0`, the old `FSharp.Compiler.SourceCodeServices` namespace, none
of it directly usable against this repo's `43.9.101` pin) — but a genuinely new mechanism this
repo has never tried, prompted by seeing that a `FSharpExpr`-interpreting approach is one FCS
consumer (Fabulous's on-device live update, DiffSharp's symbolic shape execution) has actually
shipped and depended on for years, independent of Myriad.

## Why this is the right next spike

Composes and sidesteps several already-established findings without re-testing any of them:

- **Q014/Q015's own review named the exact gap this attacks.** Q015's adversarial review found
  that hosting `FsiEvaluationSession` did not itself perform the partial-evaluation computation —
  it compiled a plugin and handed back a `MethodInfo` via a one-line quotation-destructure that
  never executed anything; 100% of the actual logic remained host-compiled code identical to
  Q014's. The review's first-named follow-up was "make FSI do real computation." This hypothesis
  takes a different path to the same goal: don't host a second compiler subsystem (FSI) at all —
  interpret the *typed tree the outer `FSharpChecker` already produced* directly. If this works,
  it reaches Q015's unmet goal (real generation-time computation, not a compile-time-literal
  pointer) by a mechanism that never needed FSI in the first place.
- **Q010 already proved same-in-progress-compilation typed visibility is achievable** (reentrant
  `DocumentSource.Custom`, inside Myriad's own CLI-hosted checker). That result gives a generator
  access to another declaration's resolved `FSharpExpr`. What Q010 never did is *interpret* that
  expression's body — it only used the reentrant callback to synthesize virtual file text from
  already-typechecked symbol information, never walked or evaluated an expression tree. This
  hypothesis is the natural next step for consuming what Q010's mechanism makes available.
- **Directly confirmed against the current pin, not assumed from PortaCode's stale API:**
  `FSharp.Compiler.Symbols.FSharpExprPatterns` exists in the pinned `43.9.101` package
  (`FSharp.Compiler.Service.xml`, checked directly) with active patterns
  (`|Const|_|`, `|NewRecord|_|`, `|Call|_|`, `|Application|_|`, `|Let|_|`, `|IfThenElse|_|`, and
  ~25 more) that map almost one-to-one onto PortaCode's `DExpr` union cases in `CodeModel.fs` —
  strong evidence the *shape* of the mechanism PortaCode used in 2018-era FCS still exists in the
  current API surface, even though the exact member names have moved (PortaCode's
  `FSharpCheckFileResults.PartialAssemblyContents.ImplementationFiles` is now the current pin's
  `FSharpCheckFileResults.ImplementationFile: FSharpImplementationFileContents option`, confirmed
  directly against the same XML doc). Nothing about this is a copy-paste port; it is a genuinely
  fresh build against the current pin, informed by a working prior-art shape.

## Novelty gate

No prior quartet in this repo has interpreted an `FSharpExpr` body. Q001/Q002 used typed access
only for *shape* resolution (field types, attribute values, cross-file type identity) — never
walked or evaluated an expression's *body*. Q003/Q014/Q015 all route generation-time computation
through `FsiEvaluationSession`, a wholly different mechanism (a second hosted compiler-service
subsystem evaluating separately-supplied source text) from walking the *first* subsystem's own
already-resolved output. Q016-19's reflection-forwarding quartets reflect over already-*compiled*
members (real `MethodInfo`s from a built DLL), never over a checker's typed-but-uncompiled
expression tree. This is genuinely new ground, not a re-tread of any of the above.

## Contradiction gate

Does not contradict Q014 or Q015 — it doesn't re-litigate whether FSI performs real computation
(it doesn't, per Q015's review); it tests a different mechanism for the same unmet goal. Does not
contradict Q006's structural wall — no type provider is involved, so the "can only resolve
already-compiled, already-referenced types" restriction doesn't apply; this walks a typed
expression tree the *checker* already resolved for a file in the current compilation, the same
category of access Q010 already proved works (Myriad's own hosted-checker route, not a TP). Does
not contradict Q018's ALC/file-lock finding — reflection-invoke here targets ordinary, already-
compiled *reference* assemblies (the normal, unregenerated case), never a satellite DLL being
concurrently rebuilt, so `Assembly.LoadFrom`'s file-lock problem is out of scope by construction.

## Validity preconditions

- `FSharp.Compiler.Service` pinned to `43.9.101`, matching every prior quartet. No PortaCode code
  is vendored or assumed compatible; the expression walk is hand-written fresh against
  `FSharpExprPatterns` on the current pin, each pattern used confirmed to exist in that package's
  XML doc before being relied on (not assumed from reading PortaCode's `FromCompilerService.fs`).
- The interpreted expression must come from a real `FSharpCheckFileResults.ImplementationFile`
  produced by actually checking a file (`keepAssemblyContents = true` or the current pin's
  equivalent), not a hand-built `FSharpExpr` or a quotation — the whole point is interpreting what
  the checker itself resolved, not a strawman construction that guarantees success.
- No `FsiEvaluationSession` anywhere in the harness — if the design finds itself reaching for FSI
  to handle a case the interpreter can't, that is a KILL/REVISE signal for *this* mechanism, not a
  reason to fall back to Q014/Q015's already-tested one.
- Must demonstrate the interpreted result driving a real difference in generated output shape
  (matching Q007's own bar for "beyond primitive plumbing") — not just "the interpreter ran and
  returned a value."
- Must record which `FSharpExprPatterns` cases the walk actually needed to cover for the tested
  shape, and name (not silently ignore) which cases were left unimplemented — this repo's own
  standing risk pattern (Q001, Q014) is a spike that quietly narrows its own claim's generality
  without saying so.

## Cheapest falsifier

Before building anything richer: check one small file with the current FCS pin (`keepAssemblyContents
= true`), pull the resolved `FSharpExpr` for a single top-level `let` binding whose body is a
`NewRecord` construction over a record type from an already-compiled reference assembly (e.g.
`{ Name = "a"; Count = 1 }` for a `Name: string; Count: int` record), hand-write the minimal
recursive walk needed to evaluate just `|Const|_|` and `|NewRecord|_|` (reflection-constructing the
record via `FSharpValue.MakeRecord` or the record type's constructor), and confirm the interpreter
produces a runtime value equal to what actually constructing that record directly would produce —
with zero `FsiEvaluationSession`, zero `Reflection.Emit`, anywhere in the harness. If the current
pin's `FSharpExprPatterns` surface doesn't expose enough structure to do even this (e.g. a pattern
PortaCode relied on in 2018 turns out to be gone or insufficient in `43.9.101`), or if the resolved
`FSharpExpr` for a value from `keepAssemblyContents` mode doesn't actually carry the structure this
depends on, the idea dies here, cheaply, before any work goes into a richer expression shape.

## Pre-registered decision thresholds

- **SHIP:** the cheapest falsifier passes; a genuinely richer shape (at least one `Call` to a real
  already-compiled library function, reflection-invoked, plus at least one control-flow node —
  `IfThenElse` or `Let`) interprets correctly and produces a result that could plausibly drive
  generated-member shape; and no `FsiEvaluationSession`/`Reflection.Emit` was needed anywhere.
- **REVISE:** the mechanism works correctly for the tested shapes but the walk needed to bail out
  to FSI, `Reflection.Emit`, or an unimplemented-case exception for a case that would realistically
  appear in a plugin author's real generator logic (e.g. pattern matching, recursion, generic
  calls) — meaning a production interpreter would need to reimplement a materially large fraction
  of F#'s semantics, real but narrower than "generation-time computation, solved by interpretation
  alone."
- **NULL:** the interpreted result could have been obtained just as easily by an already-available
  Myriad mechanism with no interpretation needed — e.g. the tested case reduces to plain reflection
  over an already-compiled reference assembly (the same technique Q016-19 already use), meaning
  interpreting the *typed AST specifically*, as opposed to just reflecting over compiled output,
  doesn't add real value for that case. The same shape of null result Q001 got for typed input
  access on a structural-echo generator.
- **KILL:** the current pin's public `FSharpExpr`/`FSharpExprPatterns` surface doesn't expose
  enough structure to build even the cheapest falsifier (a needed pattern is absent, internal-only,
  or the resolved tree under `keepAssemblyContents` lacks the shape assumed), or reflection-invoke
  against a typed-tree-resolved member reliably fails for reasons intrinsic to the mechanism (not
  just an unhandled expression shape). This would be a significant, general finding: it would mean
  PortaCode's approach, real and shipped as it is elsewhere, does not carry over to this repo's
  current FCS pin without materially more infrastructure than a single quartet can build.
