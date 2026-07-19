# Q027 — Movement 1: Hypothesize

## Question

`Q010-prefix-stratified-generation`'s own adversarial review (Objection 1) named a gap this whole
reentrant-`DocumentSource.Custom` sub-line has carried, unresolved, through every quartet since:
when a later file's callback (e.g. file C's) reentrantly calls
`checker.ParseAndCheckFileInProject` on an earlier file (A or B) **from inside** the outer
`ParseAndCheckProject` call that is still on the stack, is that reentrant call doing genuinely
fresh, contended work — the strong "mid-compilation," "while a check is already in flight" framing
this lineage's write-ups have repeatedly used — or is it landing on state FCS's own internal,
`SourceFiles`-order walk had **already computed** before it ever asked the `DocumentSource.Custom`
callback for file C's source text at all, making the "reentrant" call a cheap cache hit rather than
forced fresh work?

Concretely: **does the reentrant call's latency look like a cold single-file check, or like an
already-warm repeat check on the same checker?**

## Novelty

This is not a new idea — it is a **named, repeatedly-inherited, never-attempted** follow-up:

- `Q010`'s own review (Objection 1): "the 359ms/126ms results are consistent with real reentrant
  success, but... equally consistent with a more benign shape: `ParseAndCheckProject` requesting
  each file's source in dependency order, such that by the time [the later file]'s callback fires,
  FCS's own machinery has already effectively finished with [the earlier files]... Worth a cheap,
  targeted follow-up: instrument the reentrant call itself... before leaning on 'in flight' language
  again." Named as Follow-up 1 in that same review.
- `Q021`'s review restates the identical caveat, unresolved, for the live-edit-loop case.
- `Q023`'s review: "objection 3 [Q010's silent-bypass footgun]... Do not cite this quartet for the
  stronger claim without the follow-up instrumentation objection 1 names" — restated again.
- `Q024`'s and `Q026`'s reviews both explicitly flag this as "inherited from Q010, unresolved" and
  add no new instrumentation.

No quartet in five attempts has actually built the timing instrumentation Q010's own review named
as cheap. This quartet does exactly that, isolated from every other quantity these prior quartets
measured (cross-generator composition, scale-vs-N, real-generator invocation) so the result is not
confounded by anything else.

## Contradiction check

- **Cannot reverse any prior SHIP.** Q010, Q021, Q024, Q026 all required only correctness (no hang,
  zero diagnostics, symbol-verified resolution) for their SHIP thresholds — none of their
  pre-registrations required proving genuine mid-flight contention. Whatever this quartet finds, it
  sharpens the *framing* those write-ups used, not the verdicts themselves.
- **Does not contradict Q023/Q024's cost-model finding** (linear-in-successors cost via
  `ParseAndCheckProject` on independent files). That result is about cost as a function of *project
  position*, holding the reentrant-call question fixed; this quartet holds project position fixed
  (a 3-file toy, no scale sweep) and isolates the reentrant-call's own latency character instead.
  Different axis, same underlying mechanism.
- **Does not contradict Q026's finding** that the composition mechanism works with real generators.
  This quartet uses hand-typed content (deliberately, see preconditions below) precisely so
  generator-invocation cost (Fantomas formatting, `LensesGenerator.Generate`) cannot confound the
  FCS-internal timing signal being isolated.

## Validity preconditions

1. **Must isolate FCS's own internal caching/scheduling behavior from generator-invocation cost.**
   Q026's real-generator harness cannot answer this question cleanly: `LensesGenerator.Generate` +
   Fantomas `CodeFormatter.FormatASTAsync` take real, variable wall-clock time unrelated to FCS's
   own check scheduling, which would swamp the signal. This quartet therefore uses **hand-typed**
   virtual file content (matching Q010's own original approach), not real generator output — a
   deliberate, disclosed return to Q010-style stand-ins for this one question, justified because the
   question is about FCS's checker behavior, not about generator composition (already settled by
   Q026).
2. **Files must carry genuine typecheck weight**, not `let x = 5`-level padding — `BACKLOG.md` item
   4's named methodology complaint, already fixed once in Q023/Q024. Reuse Q023's own
   `mkPrefixSrc` template verbatim (generic record + `Map`/`List` pipeline + explicit recursion,
   `experiments/Q023-scale-cost-reentrant-callback/artifacts/q023-spike/Program.fs:24-42`) rather
   than re-deriving a weighted-file shape from scratch.
3. **The comparison must be a fair apples-to-apples one**: "cold" means a *freshly created*
   `FSharpChecker` instance that has never seen the file's content before, called directly
   (`ParseAndCheckFileInProject` with explicit source text, no `DocumentSource.Custom` — bypassing
   the callback entirely is fine here since there is nothing to reenter on a checker used exactly
   once); "warm" means the *same* checker instance used for the reentrant call, called again
   immediately after the outer `ParseAndCheckProject` has fully returned (so unambiguously idle,
   nothing else in flight); "reentrant" is the timed call made *from inside* file C's
   `DocumentSource.Custom` callback, while the outer `ParseAndCheckProject` call is still
   unreturned on the async call stack.
4. **Every timing number is a single machine's single-process measurement** — matching this
   lineage's own standing cross-cutting caveat (`FINDINGS.md`) that no quartet has run repeated
   trials across separate machine states. This quartet takes multiple repeats *within* the
   cold/warm/reentrant categories (median reported), and runs the whole harness as multiple fresh
   `dotnet run` processes (not reused within one process — Q023's own disclosed mistake was reusing
   one process across conditions it should have kept separate; this quartet runs each repeat as an
   independent process invocation to avoid that specific trap).
5. **FCS pinned to `43.9.101`, `useTransparentCompiler = true`** — matching every quartet in this
   sub-line (Q010, Q021, Q023, Q024, Q026) for direct comparability.
6. **This does not attempt to resolve whether the outer `ParseAndCheckProject`'s own internal
   walk is literally concurrent/multi-threaded with the callback**, only whether the reentrant
   call's *observable cost* resembles a cache hit or a cold check. A result showing "reentrant ≈
   warm" would support (not conclusively prove, since FCS's internals are not directly observed)
   the benign "already resolved by the time C is requested" explanation; "reentrant ≈ cold" would
   support the stronger "genuinely fresh work at reentry" framing. Report exactly this scope, no
   further.

## Cheapest falsifier

Build the minimal 3-file toy, instrument the reentrant calls to A and B (made from inside C's
callback) with a `Stopwatch`, and compare their latency against a cold single-file check (fresh
checker, same content) and a warm repeat (same checker, post-outer-check). If the reentrant calls
already look nothing like either bound cleanly (e.g., wildly inconsistent across repeats), that
alone is worth reporting honestly as a NULL result for this specific falsifier, rather than forcing
a conclusion the data doesn't support — consistent with this repo's "null and negative results
reported with the same weight as positive ones" rule.
