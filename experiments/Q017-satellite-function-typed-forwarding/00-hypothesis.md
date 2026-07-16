# Q017-satellite-function-typed-forwarding / Movement 1 — Hypothesis

**Status:** RUNNING.
**Date:** 2026-07-16.
**Repo under test:** this repo, Thread 2 lineage. Direct follow-up to `Q016-satellite-dll-type-
provider`, promoted from that quartet's own adversarial review, follow-up 1 ("Highest priority... the
one open question both the executor and this review name, and the pre-registered review task
specifically required it").

**Question:** Q016 found that `sourceAssemblies = Assembly.LoadFrom`-at-construction plus
`invokeCode = fun args -> Expr.Call(realMethodInfo, args)` lets a generative provider reflection-forward
into a real Myriad-compiled satellite DLL — but every member it wrapped (`create`, `name`, `age`,
`email`) takes at most one non-primitive argument (a `Person`) and no function-typed argument. Does the
same mechanism, unchanged, forward a member whose signature includes **function-typed
(`FSharpFunc<_,_>`) parameters and two non-primitive arguments** — Myriad's own real `map` function,
already sitting compiled and unused in Q016's own `Generated.fs` the whole time
(`let map (mapname: string -> string) (mapage: int -> int) (record': Person) : Person`) — or does it
require new machinery, or fail outright?

## The claim

Two separable claims, same split every quartet in this file uses:

1. **Mechanism claim:** a `ProvidedMethod("Map", ...)` whose `ProvidedParameter` types are derived
   directly from the real `mapMi.GetParameters()` (i.e. `FSharpFunc<string,string>`,
   `FSharpFunc<int,int>`, `Person`, all reflected off the satellite's real compiled `MethodInfo`, the
   exact technique Q016's own attempt-4 fix established) and whose `invokeCode` is
   `fun args -> Expr.Call(mapMi, args)` can be constructed with no new API beyond what Q016 already used,
   and a `checker.Compile`'d consumer can pass **real F# lambda literals** at the call site
   (`P.Map((fun s -> s.ToUpper()), (fun i -> i + 1), person)`) that resolve to the correct
   `FSharpFunc`-typed arguments with zero type-mismatch diagnostics.
2. **Capability claim:** invoking the provided `Map` member through the whole pipeline (provider →
   generated IL → consumer code → runtime call) actually executes Myriad's real compiled `map` function
   — not a reimplementation — confirmed by a falsifier the trivial-echo objection from Q016's review
   cannot apply to: the lambdas passed are chosen so the output is **not** a fixed point of any
   plausible reimplementation shortcut (uppercasing a name and incrementing an age are trivial to get
   right two different ways, so the check instead compares the provider's result against a **direct
   reflection call to the same real `map` MethodInfo** with the same lambda values, the same
   two-independent-ways discipline every prior quartet in this lineage uses, not visual inspection).

**Deliberately not attempted here:** no change to Q016's `sourceAssemblies` registration, no new SDK
API, no attempt at Round 2's regeneration/file-lock question (that remains Q016's own open, unresolved
capability gap — this quartet is scoped purely to the generalization question Q016's review named).
Not testing `Lenses` or `DUCasesGenerator` output — `Fields`' own `map` function is the cheapest
available function-typed shape, already compiled, no new Myriad generator plumbing needed.

## Why this is the right next spike

Both Q016's executor (`02-results.md`'s own "what a review should press on") and its independent review
(`03-review.md`, objection 1, and the review's own "Follow-ups, prioritized" #1) named this exact gap as
the single most important open question left by Q016, and the review was specifically asked to close it
but could not, due to a disclosed tooling outage. It is cheap: `Generated.fs` already contains `map`,
compiled into the same `Satellite.dll` Q016 already builds — no new Myriad generator surface, no new
provider infrastructure, just one new `ProvidedMethod` and one new consumer test. It is the most direct
way to convert Q016's REVISE-with-an-open-generalization-question into either a confirmed general
mechanism or a specifically bounded one.

## Novelty gate

Not covered by any closed quartet's verdict:
- Q016 wrapped only primitive-and-single-record signatures (`string`, `int`, one `Person` argument at
  most). No quartet in this repo has reflection-forwarded a member with a function-typed parameter
  through a generative provider's `Expr.Call`, nor passed a real lambda literal across a provider
  boundary this way.
- Q003 hosted FSI and crossed compile-time-evaluated *data* into host code; Q014/Q015 reified
  computation into spliced *source*. Neither passes a **live lambda value** through a **provided
  method's own IL-emitted call**, which is what this quartet tests.

## Contradiction gate

Does not contradict any prior verdict. Depends on, and is consistent with:
- Q016's own found mechanism (`sourceAssemblies` + `Expr.Call(realMi, args)`) — this quartet changes
  nothing about that, only the shape of one additional member built the same way.
- Q016's review objection 1, which frames this precisely as an open hypothesis, not a settled fact:
  "the evidence is consistent with the trick being general... but 'general' is a hypothesis, not a
  result." This quartet either confirms or narrows that hypothesis, it does not need to argue against
  a prior finding.
- Q012/Q013 (generative types resolve only via `checker.Compile`) — inherited unchanged.

## Validity preconditions

Same as Q016's: FCS pinned to `43.9.101`; vendored `ProvidedTypes.fs`/`.fsi` copied forward unchanged;
provider genuinely generative (`isErased = false`); `map`'s source is Myriad's own real,
unmodified `FieldsGenerator` output (already verified in Q016 — `Generated.fs` carries Myriad's real
header comment); Windows file-lock concerns are out of scope for this quartet (Round 2's territory, not
retested here).

## Cheapest falsifier

Build one `ProvidedMethod("Map", ...)` on Q016's existing `SatelliteProvider`, wrapping the real `map`
`MethodInfo` reflected off the same `Satellite.dll` Q016 already produces, with `ProvidedParameter`
types taken directly from `mapMi.GetParameters()`. Compile one consumer that instantiates the same
`MyriadSatellite<...>` provided type Q016 already uses, calls `.Create("Ada", 42)`, then
`.Map((fun s -> s.ToUpper()), (fun i -> i + 1), person)`. Confirm: (a) zero compile diagnostics — no
`FSharpFunc`-related type mismatch; (b) the result agrees with a direct reflection call to the same real
`map` MethodInfo made independently, outside the provider, with the same lambda values.

## Pre-registered decision thresholds

- **SHIP:** the falsifier passes cleanly — zero type-mismatch diagnostics, and the provider's result
  agrees with the independent direct-reflection call. This would mean Q016's kernel (`sourceAssemblies`
  + reflected-`MethodInfo`-typed `ProvidedParameter`s + `Expr.Call`) is genuinely shape-general, not
  luck-shaped to primitive/single-record signatures, closing Q016's own review's top follow-up.
- **REVISE:** the falsifier passes but only after new scaffolding beyond what Q016 already used (e.g. a
  hand-built `FSharpFunc` conversion thunk, a different `invokeCode` construction, or
  `ConvertSourceExprToTarget`/`ConvertSourceTypeToTarget` become newly necessary here even though they
  actively broke Q016's attempt 2) — meaning the mechanism generalizes only with extra work, narrowing
  "general kernel" to "general with per-shape scaffolding."
- **NULL:** the mechanism works but demonstrates nothing beyond what Q016 already showed — judged
  unlikely going in (a function-typed argument is a qualitatively different signature shape than
  anything Q016 wrapped), but recorded as a possible outcome the review should check for.
- **KILL:** `Expr.Call` against a `MethodInfo` with a function-typed parameter is rejected by the SDK's
  IL-emission path, or a real F# lambda literal at the call site cannot be resolved to the required
  `FSharpFunc`-typed provided parameter at all. This would be a real, general finding: providers cannot
  reflection-forward Myriad generator output that takes function-typed arguments (ruling out, e.g., any
  future attempt to expose `Lenses`' getter/setter-function-typed members this way), bounding this whole
  approach's applicability to Myriad's own generator surface more narrowly than Q016 implied.
