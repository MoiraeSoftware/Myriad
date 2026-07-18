# Q025-typed-expr-interpreter / Movement 4 — Adversarial review

## Reproduction status — read this first, it bounds everything below

I executed this quartet independently. No tooling outage was in effect: every `dotnet build` and
`dotnet run` I issued ran to completion. The claims below are from observations I made, not inherited
from the checked-in logs. I went past reproduction on three shapes the executor never built — a
generic function call, a nested record, and a three-argument call — each through a real `dotnet build`
of an augmented `RefLib` and a real checker-produced `FSharpExpr` fed to the unmodified `interpret`
function, in a scratch copy of the harness so the committed artifacts were left untouched.

What I verified by executing:

- **Both pre-registered rounds reproduce byte-for-byte.** A clean `dotnet build RefLib -c Release`,
  `dotnet build Harness -c Release`, `dotnet run --project Harness -c Release` printed `ROUND 1: PASS`,
  `ROUND 2: PASS`, `Summary: Round1=true Round2=true`, and exited **0**. Round 1's
  `Object.Equals(interpreted, direct) = true`, Round 2's `interpreted 'result' = "big"` with the raw
  `Let(.. Call .. IfThenElse(Call ..))` tree, and the `mkMembers`/contrast shapes are all exactly as
  `02-results.md` and `run-logs/harness-run.txt` record. The numbers and the PASS/PASS are honest.
- **The interpreter source matches its description.** `artifacts/Harness/Program.fs` implements exactly
  six `FSharpExprPatterns` arms — `Const`, `NewRecord`, `Call`, `Let`, `Value`, `IfThenElse` — with an
  `| other ->` final arm that raises `NotImplementedException` carrying `other.Type` and the `%A` of the
  expression (lines 180-182). The fallthrough is real and not swallowed. The `Call` arm discards
  `_typeArgs` and `_methodTypeArgs` (line 157) — hold that thought, it is the crux of my strongest
  finding below.
- **The no-FSI / no-`Reflection.Emit` precondition holds by inspection, not just by the harness's own
  self-report.** Grepping `Program.fs` for `FsiEvaluationSession|Reflection.Emit|Interactive.Shell`
  returns only the line-5 comment and the line-324 print string, both asserting absence. No
  construction, no import. This is the hypothesis's own validity precondition and it is met.

## The strongest independent finding: a generic call does NOT hit the promised safety net

`01-design.md` (lines 67-73) and `02-results.md` (§"Deliberately not implemented", lines 134-139)
both make the same explicit claim: the shapes left unimplemented — and the list in both docs names
**"generic method calls with unresolved type parameters"** among them — "falls through to a named
`NotImplementedException` carrying the unmatched expression's `.Type`, never silently swallowed." The
design frames this as the safety net that makes an unhandled shape "fail loudly and specifically rather
than silently," and cites it as protection against this repo's own standing risk (Q001/Q014 quietly
narrowing scope).

**That claim is false for generic calls, and I disproved it by running one.** I added
`let identity (x: 'a) : 'a = x` to `RefLib.Ops`, checked `let g = RefLib.Ops.identity 42`, and fed the
checker-resolved expression for `g` to the unmodified `interpret`. The result:

```
raw expr:
Call
  (None, val identity, [], [type Microsoft.FSharp.Core.int], [],
   [Const (42, type Microsoft.FSharp.Core.int)])
PROBE generic Call: interpret THREW System.InvalidOperationException:
  Late bound operations cannot be performed on types or methods for which
  ContainsGenericParameters is true.
```

A generic call is **not** an unmatched shape. It matches `FSharpExprPatterns.Call` like any other call,
so it never reaches the `| other ->` arm and never produces the promised `NotImplementedException`.
Instead `resolveMethod` returns the *open* generic method definition (`GetMethod` with no type
substitution), and `mi.Invoke(null, args)` throws a raw reflection `InvalidOperationException` deep
inside the `Call` arm. The design's mental model — "unsupported feature = unmatched pattern =
`NotImplementedException`" — conflates a missing *expression pattern* with a missing *semantic feature*.
Generic calls are the case where those two come apart, and they are the exact case both frozen docs
named as covered by the fallthrough.

Two things sharpen this:

1. **The failure is loud but not specific**, which is precisely the property the design promised.
   "Late bound operations cannot be performed on types or methods for which ContainsGenericParameters is
   true" gives a plugin author zero signal that the cause is a generic call in interpreted user source.
   The `NotImplementedException` the docs promise would at least print the offending expression. So the
   safety-net claim is not merely imprecise; the one-line guarantee it offers ("fails specifically")
   is defeated for a case it explicitly lists.
2. **The information to fix it was in hand and thrown away.** The raw expr shows the method type
   argument `[type Microsoft.FSharp.Core.int]` sitting in the fourth tuple position — `methodTypeArgs` —
   which the `Call` arm binds to `_methodTypeArgs` and discards. A production interpreter would have to
   `mi.MakeGenericMethod(...)` from exactly that list before invoking. There is no `MakeGenericMethod`
   anywhere in `Program.fs` (grep-confirmed). This is not a deep obstacle, but it is real
   reimplementation work the current harness does not do and mis-describes.

This does not sink the SHIP conjuncts (they never required generic support — see the threshold check
below), but it is a directly falsified statement in a frozen results doc, of exactly the class this
review discipline exists to catch, and it should be struck or corrected.

## The two positive generalizations: nested records and multi-arg calls both hold

To be fair to the mechanism, I also built the two shapes the review brief flagged as untested soft
spots, and both generalized cleanly:

- **Nested `NewRecord`.** `type Inner = { Tag: string }`, `type Outer = { Label: string; Body: Inner }`,
  and `let o : RefLib.Outer = { Label = "x"; Body = { Tag = "t" } }`. The checker resolves the field
  value to a nested `NewRecord`, and `interpret` returned `{ Label = "x"; Body = { Tag = "t" } }` typed
  `RefLib.Outer`. The `argExprs |> List.map (interpret env)` recursion in the `NewRecord` arm genuinely
  generalizes past the primitive-field case Round 1 tested — the inner record is reflection-constructed
  by the same arm one level down. This closes the review brief's soft-spot (a) as a genuine positive:
  `NewRecord` was not only tested for primitive fields.
- **Three-argument call.** `let add3 a b c = a + b + c`, checked as `RefLib.Ops.add3 1 2 3`. The
  curried source compiles to a single flattened 3-parameter static method, the checker emits one `Call`
  with three `argExprs`, and `mi.Invoke(null, [|1;2;3|])` returned `6`. F#'s curried-arity flattening —
  the exact brittleness Q017's review found poisons a *forwarding* provider — is a non-issue here on the
  *consuming* side, because the interpreter reads the already-flattened compiled `Call` shape rather than
  reconstructing a `ProvidedParameter` list from source. Worth noting the asymmetry: Q025 reads compiled
  arity, Q017 had to predict it.

So the mechanism is more robust than the two hand-picked fixtures alone show for the *record-construction*
and *non-generic-call* axes, and less robust than the docs claim for the *generic-call* axis. Both
corrections matter; only the second is an overclaim.

## Pre-registered thresholds, checked conjunct by conjunct

**SHIP** (`00-hypothesis.md` lines 117-119): "the cheapest falsifier passes; a genuinely richer shape
(at least one `Call` to a real already-compiled library function, reflection-invoked, plus at least one
control-flow node — `IfThenElse` or `Let`) interprets correctly and produces a result that could
plausibly drive generated-member shape; and no `FsiEvaluationSession`/`Reflection.Emit` was needed
anywhere."

- Cheapest falsifier passes — **met.** Round 1 reproduced, `Object.Equals = true`.
- Genuinely richer shape (≥1 real reflection-invoked `Call` + ≥1 control-flow node) interprets correctly
  and could drive shape — **met.** Round 2 has two real `Call`s (`bump`, `isBig`, one feeding the other),
  a `Let` chain, a `Value` lookup, and an `IfThenElse`, evaluates to `"big"`, and feeds `mkMembers` to a
  differing member-name list. Reproduced.
- No FSI / `Reflection.Emit` anywhere — **met**, grep-confirmed independently.

All three SHIP conjuncts are literally met and independently reproduced.

**REVISE** (lines 120-125): "the mechanism works correctly for the tested shapes **but the walk needed to
bail out** to FSI, `Reflection.Emit`, or an unimplemented-case exception **for a case that would
realistically appear in a plugin author's real generator logic** (e.g. pattern matching, recursion,
generic calls)." The trigger is conditioned on the walk *needing to bail out within the tested shapes*.
The two pre-registered rounds did not contain such a case, so the walk never bailed out during the
executor's own run, and REVISE did not fire on the pre-registration as written. My generic-call probe is
outside the two pre-registered rounds — it demonstrates the REVISE *boundary* is real and closer than the
docs imply, but it is not one of "the tested shapes," so it does not retroactively convert the run into a
REVISE. This is the same situation as Q019 ("SHIP by the letter of the pre-registration"): the caveat
travels with the verdict rather than flipping it.

**NULL** (lines 126-131): "the interpreted result could have been obtained just as easily by an
already-available Myriad mechanism with no interpretation needed — e.g. the tested case reduces to plain
reflection over an already-compiled reference assembly." Does not fire. Round 2's result is not obtainable
by reflecting over compiled output: `Sample2.fs`'s `result` is never compiled by anything in this harness
(the checker only parse-and-checks it), and the interpreted value comes from walking the *typed tree the
checker produced for uncompiled source* and reflection-invoking the reference functions the tree points
at. That is materially more than "reflect over a member that already exists in a built DLL" — the member
`result` exists in no DLL. NULL is out.

**KILL** (lines 132-138): the pin's `FSharpExpr`/`FSharpExprPatterns` surface lacking the structure to
build even the falsifier. Does not fire — every needed pattern exists on `43.9.101` and
`keepAssemblyContents = true` populated `ImplementationFile` on the first try, confirmed in my run.

Verdict by the letter of the pre-registration: **SHIP**. Scoped, per below.

## Novelty and contradiction gates still hold given what was actually built

- **Novelty holds.** No prior quartet interpreted an `FSharpExpr` *body*. Q001/Q002 used typed access
  for shape only; Q003/Q014/Q015 routed generation-time computation through `FsiEvaluationSession`;
  Q016-19 reflected over already-*compiled* members. Q025 walks and evaluates the checker's own resolved
  expression tree with no FSI. Confirmed against the actual source: this is genuinely the first
  expression-body interpretation in the lineage.
- **Contradiction gate holds.** No FSI is hosted, so this does not re-litigate Q014/Q015. No type
  provider is involved, so Q006's same-compilation wall does not apply (and indeed the interpreted value
  comes from a file in the current parse-and-check, the Q010-category access). No `Assembly.LoadFrom` of
  a concurrently-rebuilt satellite DLL, so Q018's file-lock finding is out of scope. All three
  as claimed.

## Where the result is narrower than the SHIP label sounds

Reading the SHIP verdict without the scope attached would overstate it in three ways:

1. **It is a mechanism proof over six patterns and two fixtures, not "generation-time computation,
   solved."** The fixtures were written to hit exactly the six implemented arms with nothing left over.
   The moment a plugin author's real generator logic reaches a generic call (shown above to fail badly),
   a `Lambda`/`Application` (any higher-order use, or interpreting a *function's* body rather than a
   value binding — explicitly out of scope), DU pattern matching (`NewUnionCase`/`UnionCaseGet` — i.e.
   any of Myriad's own union-based generators), or recursion (`LetRec`), the interpreter either throws
   the loud-but-vague reflection error or the named `NotImplementedException`. The hypothesis's own
   REVISE wording ("a production interpreter would need to reimplement a materially large fraction of
   F#'s semantics") is the correct characterization of the gap between this spike and a usable tool. The
   spike proves the *approach is viable for a small closed set of shapes*, which is a real and honestly
   scoped result, not "generators can now compute at generation time."

2. **The assembly-identity story is still an artifact of this harness's execution order, and I did not
   remove that artifact.** The results doc's own "what a review should press on" flags the one test it
   could not reach: moving `Assembly.LoadFrom` to run *after* `Harness`'s own `RefLib.Widget` reference
   has already forced its `ProjectReference` copy to load — the ordering the executor's own mechanism
   explanation predicts *would* produce a `Type`-identity mismatch (and, because `Object.Equals` on an
   F# record is structural, would surface as a silent `false` in Round 1, not a visible error). I chose
   not to build that reordering, and I want to be explicit about the trade rather than bury it: it is an
   *infrastructure* question about `Assembly.LoadFrom` load-context identity, not a question about the
   interpreter mechanism or any SHIP conjunct, and I judged my execution budget better spent proving or
   breaking the interpreter's *generalization* (the "genuinely richer shape" conjunct), which is central
   to SHIP. So the identity claim remains, correctly, an untested-at-the-boundary caveat — the executor's
   account of it is honest and self-limiting, and a real Myriad generator would not enjoy this harness's
   convenient "`LoadFrom` runs first by construction" ordering. Treat "assembly identity resolved here"
   as harness-specific, exactly as the results doc already asks.

3. **`mkMembers` meets the Q007/Q014 bar, and it is a low bar honestly labeled.** `mkMembers` is a bare
   `string -> string list` match (`"big" -> ["Detail"; "Summary"]`, else `["Summary"]`), and it does
   demonstrate the interpreted value *selecting* between two differently-shaped outputs rather than a
   hardcoded branch — the contrast print makes that concrete. This is the same "list/count drives shape"
   stand-in Q007's design and Q014 used, not a weaker one invented here; I confirmed that against the
   design's own citation. But it is a stand-in: nothing here is a real `IMyriadGenerator` or a provided
   type, and "could plausibly drive generated-member shape" is doing real work in the SHIP wording. The
   result is "interpretation produced a value that a generator could branch on," not "a generator did."

## Verdict

**SHIP, scoped — and correct the frozen docs' "generic calls fall through to `NotImplementedException`"
claim.**

By the letter of the pre-registration, all three SHIP conjuncts are met and independently reproduced:
the cheapest falsifier passes, a genuinely richer `Call`+`Let`+`IfThenElse` shape interprets correctly
and drives a differing member-name list, and no `FsiEvaluationSession` or `Reflection.Emit` is present
anywhere (grep- and source-confirmed, not merely self-reported). KILL, NULL, and REVISE did not fire on
the pre-registered rounds. This is genuinely new ground — the first `FSharpExpr`-body interpretation in
either research thread — and it reaches Q015's unmet "real generation-time computation, not a
`MethodInfo` pointer" goal by a mechanism that never needed FSI, which is a clean, positive, reproduced
result worth stating plainly.

What travels with the SHIP, and matters more than the label:

- **The failure-mode safety-net claim is wrong as written.** Both `01-design.md` and `02-results.md`
  list generic calls among the shapes that "fall through to a named `NotImplementedException`." They do
  not: a generic call matches the `Call` arm and dies with a raw `InvalidOperationException` from
  `mi.Invoke` on an open generic method, because the `Call` arm discards `methodTypeArgs` and never
  calls `MakeGenericMethod`. I reproduced this directly. The statement should be struck or downgraded to
  "generic calls match the `Call` arm and fail with a raw reflection error, not the named fallthrough —
  supporting them needs `MakeGenericMethod` from the discarded `methodTypeArgs`." This is a write-up
  accuracy defect, not a result failure, but it is exactly the overclaim class this discipline exists to
  catch, and it happens to falsify the one property (loud *and specific*) the design leaned on.
- **The reach is six patterns over two fixtures.** Nested records and multi-arg non-generic calls do
  generalize (I verified both). Generic calls, higher-order functions, DU matching, and recursion do
  not, and the hypothesis's own REVISE wording already names that as the ceiling. Cite this as "direct
  `FSharpExpr` interpretation is viable for a small closed set of shapes with no FSI," not as
  "generation-time computation solved."
- **Assembly identity and `mkMembers` are honestly scoped caveats already**, unchanged by my run: the
  identity result is harness-execution-order-specific (the deepest test remains unbuilt, by my choice,
  for stated reasons), and `mkMembers` is a legitimate but minimal stand-in at the same bar Q007/Q014
  used.

The clean, unqualified win: a hand-written interpreter walking a real, checker-produced `FSharpExpr`
(never a hand-built expr or a quotation) correctly reflection-constructs records — including nested
ones — and reflection-invokes already-compiled reference functions across a `Let`/`Call`/`IfThenElse`
chain, driving a differing generated-member-name list, with zero `FsiEvaluationSession` and zero
`Reflection.Emit`. That is real, new to this lineage, and reproduced. Its reach is just much smaller
than "generators can compute at generation time," and one of its own stated safety guarantees does not
hold for a case both docs claimed it covered.

## Follow-ups, prioritized

1. **Strike or downgrade the "generic calls fall through to `NotImplementedException`" claim** in
   `01-design.md` and `02-results.md`, per the reproduced probe above. If generic support is ever
   wanted, the fix is `mi.MakeGenericMethod` fed from the `methodTypeArgs` currently bound to
   `_methodTypeArgs` in the `Call` arm — small, but not free, and currently mis-described as impossible-
   by-design.
2. **Interpret a *function* body, not just a value binding** — the `Lambda`/`Application` case the
   hypothesis scoped out. This is the shape any higher-order generator logic needs and is the honest
   next boundary between "viable for closed shapes" and "a real interpreter."
3. **A DU / pattern-match fixture** (`NewUnionCase`/`UnionCaseGet`/`DecisionTree`), since Myriad's own
   union-based generators are the obvious real target and none of the six implemented arms touch them.
4. **Build the untested assembly-identity reordering** (move `Assembly.LoadFrom` to run after the
   `ProjectReference` copy is forced to load) if this mechanism is ever moved toward a real generator,
   where the harness's convenient module-init ordering will not hold. Lower priority than 1-3 because it
   is infrastructural, not about the interpreter itself.
