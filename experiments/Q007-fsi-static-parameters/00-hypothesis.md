# Q007-fsi-static-parameters / Movement 1 — Hypothesis

**Status:** PLANNED. Pre-registered only — not yet built. Awaiting go-ahead.
**Date:** 2026-07-14

**Question:** F# restricts type-provider static arguments to a `[<Literal>]`-encodable set (string,
the numeric types, `char`, `bool`, `decimal`, enums) — a provider can declare a
`ProvidedStaticParameter` of any `System.Type` (`ProvidedTypes.fs:969`,
`parameterType: Type` is unconstrained at the SDK level), but the host compiler rejects anything
richer at the call site (confirmed empirically in Q006: `Invalid static argument to provided type.
Expected an argument of kind 'System.Type'`). Can a provider's own `DefineStaticParameters`
instantiation function — which receives a plain `obj[]` of already-compiler-approved literals,
entirely under the provider author's control — host `FsiEvaluationSession` internally and evaluate
a **string** static argument as real F# source, so the provider effectively receives an arbitrary
typed value (a record, a function, a list) instead of a bare literal? And does this survive being
invoked from *inside* the compiler's own live type-checking call stack, a nesting of
compiler-service hosts no prior quartet has tested?

This is explicitly the general-purpose type-provider question, independent of Myriad's own
configuration or generator surface — the user's framing for this line of inquiry ("excluding how
Myriad is configured, is there a way to advance type providers to be generally more useful").

## Why this is the right next spike

Composes two already-validated, independent pillars for the first time, rather than re-testing
either:

- **Q003** proved `FsiEvaluationSession` and `FSharpChecker` coexist in one process with no
  collision, and that FSI-evaluated F#-typed data (not just primitives) crosses cleanly into host
  code. But Q003's harness called FSI from its own top-level code — never from *inside* a live
  `ApplyStaticArguments` call that the compiler itself is in the middle of servicing.
- **Q006** proved a real, working generative provider using a **string** static parameter
  (`LensesByName<TypeName: string>`), invoked from a real on-disk consumer through
  `FSharpChecker.ParseAndCheckFileInProject`, with measured cold (1137ms) and live-edit re-check
  (47ms) costs. But Q006 treated the string as an opaque type-name lookup key — it never evaluated
  the string as code.

Neither quartet tested hosting a *second* compiler-service subsystem (FSI) from within a design-time
component while that component is itself being called synchronously by a *first* compiler-service
subsystem (the outer `FSharpChecker` doing the consumer's typechecking). That's a materially
different risk profile than either prior quartet's mechanism section assumed — reentrancy, shared
static state, and stack depth all become live questions in a way neither Q003 nor Q006 exercised.

## Novelty gate

Checked `FSharp.TypeProviders.SDK/src/ProvidedTypes.fs` directly for where static arguments are
received and validated: `ProvidedTypeDefinition.ApplyStaticArguments` (`ProvidedTypes.fs:1886-1891`)
receives `args: obj[]` — already-boxed values the *host compiler* chose to accept, not something
this SDK itself restricts (`ProvidedStaticParameter`'s constructor at line 969 takes an unconstrained
`parameterType: Type`). The literal-only restriction is therefore enforced upstream, by the F#
compiler's own static-argument grammar, before a provider ever sees the value — meaning the
provider-side instantiation function is unconstrained in what it does with a string once received.
No prior quartet exercised evaluating that string as code; Q006 used it as a lookup key, nothing
more. Not a re-tread.

## Contradiction gate

Does not contradict Q003 or Q006 — composes both without re-litigating either's verdict. Important
distinction to hold precisely, so this doesn't get conflated with Q006's finding: this does **not**
attempt to route around Q006's structural wall (type providers can only resolve already-compiled,
already-referenced types, never a type from the compilation currently in progress). FSI evaluation
here produces new **data** (a config value, a computed schema) at design time — it does not grant
visibility into sibling source declarations the way `Ast.fs`-style parsing would. Conflating "richer
static parameters" with "can now see the consumer's own source" would be a real error in this
quartet's own review; flagging it here so it doesn't happen by default.

## Validity preconditions

- `FSharp.Compiler.Service` pinned to `43.9.101`, matching every prior quartet.
- `ProvidedTypes.fs`/`.fsi` vendored from the same `FSharp.TypeProviders.SDK` commit Q006 used
  (`0a95768a2247daba80b24a2604f77f89fc88ff1f`), for direct comparability of any timing numbers.
- The evaluated value must be genuinely richer than a primitive — a record or a function — not an
  `int` or `bool` a provider could already get for free via an ordinary numeric static parameter.
  Evaluating `"1+2"` is only acceptable as the cheapest-falsifier smoke test below, never as the
  quartet's positive result.
- Must measure live-edit re-check cost with FSI evaluation in the loop, using the same
  cold-vs-live-edit methodology Q006 already established (`02-results.md`'s Round 1 table), not a
  vague "seems fine" — the standing engineering risk named when this idea was first proposed is
  that FSI-eval cost sits on every keystroke recheck, and that needs a real number, not an estimate.
- Must attempt a case where the richer value actually changes generated output shape (e.g. a
  function-valued config field that customizes a provided member's behavior, or a list-valued field
  driving a variable number of provided members) — a record whose fields are just read back
  losslessly wouldn't demonstrate anything a smarter string-encoding (CSV, a delimiter convention)
  couldn't already achieve without FSI at all.

## Cheapest falsifier

Before building anything richer: does `FsiEvaluationSession` even work correctly when created and
invoked *from inside* a `DefineStaticParameters` instantiation function, while that function is
itself running on the call stack of a real, outer `FSharpChecker.ParseAndCheckFileInProject` call
(mirroring Q006's harness exactly, so the nesting is real, not simulated)? Concretely: a provider
whose static parameter is the string `"1+2"`, whose instantiation function hosts FSI, evaluates it,
and uses the resulting `int` to decide how many trivial properties to generate — run through Q006's
same harness, same negative-control discipline (assert the count of generated properties matches
the FSI-evaluated result, not just that *something* got generated). If this deadlocks, throws, or
corrupts either compiler-service subsystem's state, the whole idea dies here, cheaply, before any
work goes into making the evaluated value richer.

## Pre-registered decision thresholds

- **SHIP:** the cheapest falsifier passes; a genuinely richer value (record or function) evaluated
  via FSI correctly drives a real change in generated-member shape; and the live-edit re-check cost
  with FSI evaluation in the loop is small enough (same order of magnitude as Q006's 47ms, not
  10x–100x worse) to be practical for interactive use without an opt-in/caching mode.
- **REVISE:** the mechanism works correctly but the live-edit cost is materially worse than Q006's
  baseline, meaning this should ship as an opt-in or explicitly-cached feature rather than Q006's
  "just works by default" result — real, narrower than pitched.
- **NULL:** FSI evaluation works, but the specific richer-value case tested could have been achieved
  almost as easily with a smarter string-encoding convention and no FSI at all (e.g. a
  semicolon-delimited literal a provider could parse directly) — meaning the "beyond literal
  expressiveness" claim doesn't clearly earn its complexity for that case, the same shape of null
  result Q001 got for typed input access on a structural-echo generator.
- **KILL:** the cheapest falsifier itself fails — hosting FSI from inside a live, compiler-invoked
  `ApplyStaticArguments` call deadlocks, throws, or corrupts state. This would be a significant,
  general finding worth recording plainly: it would mean nested compiler-service hosting (a design-
  time component that itself hosts more FCS-family machinery while being called by an outer host) is
  unsafe in this configuration, bounding not just this idea but any future one shaped like it.
