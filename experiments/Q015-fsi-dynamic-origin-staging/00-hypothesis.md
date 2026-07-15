# Q015-fsi-dynamic-origin-staging / Movement 1 — Hypothesis

**Status:** CLOSED. **SHIP, narrowly scoped** — see `03-review.md`. All four pre-registered SHIP
conjuncts met and independently reproduced at a third `n` value, closing Q014's specific "no FSI ran"
gap for real — but FSI performs none of the actual staging (it hands back a `MethodInfo`; all
partial-evaluation logic is host code identical to Q014), and the origin is dynamic-*body*/
static-*interface* only.
**Date:** 2026-07-15
**Repo under test:** this repo, Thread 1 lineage. Direct follow-up to `Q014-fsi-staged-compilation`'s
own adversarial review, follow-up 1: "actually host `FsiEvaluationSession` and evaluate the general
implementation and/or its config from a string not known at host-compile-time, then reify — that is
the claim the [Q014] hypothesis's title makes and the one this spike did not test."

**Procedural note, not part of the science:** Movement 4 for this quartet also runs on Opus, same
resourcing reason as Q014.

**Question:** Q014 proved a compiler-built quotation can be specialized and reified into real,
executing F# source — but both the general implementation (`power`) and its specializing config
(`n = 4`) were source-level literals in the same host program, so no staging boundary was actually
crossed and no `FsiEvaluationSession` ever ran, despite the hypothesis's own title and SHIP threshold
naming FSI-time evaluation as load-bearing. Can a real `FsiEvaluationSession` evaluate source text for
*both* the general recursive implementation and its specializing integer config, where neither is known
to the host program at its own compile time (read from a separate plugin file and an environment
variable at runtime, not written as literals in the host's own `.fs`/`.fsx` source) — and can the
resulting quotation still be specialized (reusing Q014's own `unroll`/translate/splice/typecheck/
compile/execute pipeline, unchanged) into real, correctly-executing spliced source?

## The claim

Two separable claims, kept apart the same way Q001/Q010/Q014 did:

1. **Mechanism claim:** a `MethodInfo` for a function defined only inside a live `FsiEvaluationSession`
   (never referenced by name anywhere in the host's own compiled source, since the host cannot know
   that name at its own compile time) can be obtained by having the *FSI session itself* quote and
   destructure a call to its own just-defined function, returning the `MethodInfo` back across the
   FSI/host boundary as a plain `System.Reflection.MethodInfo` value — and `Expr.TryGetReflectedDefinition`
   then works on that host-side `MethodInfo` exactly as it did on Q014's compile-time one.
2. **Capability claim:** with both the general implementation's source and its specializing config read
   from outside the host's own compiled source at runtime, the full Q014 pipeline (substitute → unroll →
   `decompile` → splice → `ParseAndCheckFileInProject` → `checker.Compile` → load → invoke) still
   produces correct, executing, structurally-unrolled output — closing the specific gap named in Q014's
   review without needing a new mechanism for the reification half, only a new mechanism for getting the
   quotation out of a live FSI session in the first place.

**Deliberately not attempted here, named so the review doesn't have to guess whether it was missed or
scoped out:** Q014's review also named (2) generalizing `unroll` past `power`'s one hand-matched shape,
and (4) whether `TryGetReflectedDefinition`-style reflection can reach a general implementation living
in the compilation *currently being built* rather than an already-compiled FSI/plugin assembly (a
Q006-shaped wall). Both remain open. This quartet is scoped tightly to the FSI/dynamic-origin gap alone
— Q014's review named it as the single most direct next step, and stacking multiple open questions into
one spike is exactly what this repo's own discipline (Q010's Round 4, Q014's own tight scoping) argues
against.

## Why this is the right next spike

Composes two already-validated, independent pillars without re-litigating either: Q003 proved FSI and
`FSharpChecker` coexist in one process and that FSI-evaluated F#-typed *data* crosses cleanly (no ALC
isolation, the same configuration reused here). Q014 proved the reification pipeline (quotation →
specialize → text → splice → typecheck → compile → execute) works correctly once a quotation is in
hand. Neither quartet tested obtaining that quotation from something FSI evaluated whose source the
host never saw at its own compile time — the literal gap between "FSI evaluates data" (Q003) and
"FSI evaluates the *thing later reified*" (never tested until now, since Q014 substituted
`[<ReflectedDefinition>]` for this step without anyone flagging it before review).

## Novelty gate

Not covered by any closed quartet's verdict. Q003 never reified anything from FSI back into spliced
source (its own review named this as the outstanding "build the full loop" gap, which Q014 also failed
to close per its own review). Q014 never hosted FSI. This is the first quartet in either thread to
combine both.

## Contradiction gate

Does not contradict any prior verdict; it directly targets the specific gap Q014's review identified in
Q014's own claim, without reopening Q014's verdict itself (Q014 stays CLOSED/REVISE — this is new
evidence, not a re-litigation). Depends on: FSI/`FSharpChecker` coexistence with no ALC isolation
(validated, Q003); `[<ReflectedDefinition>]` + `TryGetReflectedDefinition` working on a `MethodInfo`
(validated, Q014, but only ever tried on a host-compile-time `MethodInfo` — this quartet is the first
test of whether it also works on a `MethodInfo` obtained from a *live FSI session's* dynamic assembly,
a materially different provenance not exercised before); the translate/splice/typecheck/compile/execute
pipeline (validated, Q014, reused unchanged).

## Validity preconditions

- FCS pinned to `43.9.101`; `Unquote` at whatever version Q014 resolved (`7.0.1`), for direct
  comparability.
- The plugin source (the general implementation) must live in a file the host's own `.fs` never
  contains a copy or paraphrase of — grep the host source for the function's name as a build-time check,
  not just an assertion in prose.
- The specializing config (`n`) must come from an environment variable or command-line argument, not a
  literal anywhere in the host's compiled source, and must differ from Q014's `n = 4` so a passing run
  can't be explained by an accidentally-cached or hardcoded result.
- The `MethodInfo` obtained from FSI must be shown to genuinely originate from FSI's dynamic assembly,
  not the host's own — check `methodInfo.Module.Assembly` is not the host's entry assembly, printed and
  recorded, not assumed.
- Same as Q014: correctness must be checked by actually compiling and executing the spliced text (not
  just evaluating the quotation), reusing Q014's stronger check rather than regressing to the weaker one.
- AssemblyLoadContext isolation remains explicitly out of scope, inherited unresolved from Q003/Q014.

## Cheapest falsifier

Before attempting the full pipeline: can a `MethodInfo` for a function defined only inside a live
`FsiEvaluationSession` (no host-compile-time reference to it anywhere) be retrieved by the *session
itself* quoting a call to it and handing the `MethodInfo` back across the boundary — and does
`Expr.TryGetReflectedDefinition` on that `MethodInfo`, called from host code, return the expected body?
This isolates the one genuinely new mechanism (getting a reflectable, `ReflectedDefinition`-bearing
`MethodInfo` out of FSI) from the already-proven reification pipeline, exactly the falsifier-first
discipline every prior quartet in this repo has used.

## Pre-registered decision thresholds

- **SHIP:** the cheapest falsifier passes; the full pipeline, with both the general implementation and
  its config genuinely unavailable to the host at its own compile time, produces spliced source that
  typechecks with zero diagnostics; correctness is verified by compiling and executing the actual
  spliced text, matching the FSI-defined general implementation's own behavior (invoked via reflection
  on the FSI-obtained `MethodInfo`, not a host-side re-implementation); and the output is structurally
  unrolled (no residual reference to the general implementation), mirroring Q014's own capability
  threshold but now with the FSI/dynamic-origin gap actually closed.
- **REVISE:** the mechanism works but only under a narrower condition than described (e.g. the
  `MethodInfo` retrieval works but `TryGetReflectedDefinition` on an FSI-sourced `MethodInfo` returns
  `None` or a differently-shaped `Expr` than the host-compiled case, requiring a workaround not
  originally anticipated).
- **NULL:** the mechanism works end to end, but nothing about crossing the dynamic-origin boundary
  changed the result compared to Q014 — i.e. this would just be Q014 again with extra ceremony, no new
  capability demonstrated. (Judged unlikely going in: Q014's review explicitly named this boundary as
  the thing that makes the difference between a toy and a Myriad-relevant mechanism, but stated here so
  the review has a real bar to check against, not just an assumed positive.)
- **KILL:** the cheapest falsifier fails — `TryGetReflectedDefinition` does not work on a `MethodInfo`
  obtained from FSI's dynamic assembly (returns `None`, throws, or the two compiler-service subsystems
  interfere with each other in a way Q003 never exercised, since Q003 never retrieved a
  `ReflectedDefinition`-bearing method from FSI). This would be a real, general finding: it would mean
  `[<ReflectedDefinition>]` metadata specifically does not survive FSI's own compilation path the way it
  does the host's ordinary compilation, bounding this entire approach regardless of how the
  `MethodInfo` is retrieved.
