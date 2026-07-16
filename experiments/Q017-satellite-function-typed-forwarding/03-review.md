# Q017-satellite-function-typed-forwarding / Movement 4 — Adversarial review

## Reproduction status — read this first, it bounds everything below

Unlike Q016's own review, I *was* able to execute this quartet independently. The tooling outage that
blocked Q016's reviewer was not in effect: every `dotnet build`/`dotnet run`/`dotnet fsi` I issued ran.
So the pass/fail claims below are from observations I made, not inherited from saved logs.

What I verified by executing:

- **The saved logs correspond to exactly the checked-in code.** The executor's original scratch build
  survives intact at the hard-coded job path (`…/jobs/f0b85ddf/tmp/q016-satellite-dll-spike/`). `diff`
  reports its `SatelliteProvider.fs`, `Harness/Program.fs`, and `SampleLib/Generated.fs` are all
  byte-identical to the copies under `Q017-.../artifacts/`. So the logs and the checked-in source are
  the same artifact.
- **Round 3 reproduces exactly as claimed.** Rebuilt `SatelliteTP.Runtime` (pulls in `.DesignTime`) and
  `Harness` from the checked-in source, ran `dotnet Harness.dll round3`: compile exitCode=0 errors=0,
  provider-mediated `Map` returns `name="ADA" age=43`, the independent direct-reflection `map` call
  returns `name="ADA" age=43`, agreement true on both fields. Round 1 regression also reran PASS
  (`name="Ada" age=42`, agreement true). The executor's PASS is real.
- **The SDK-internals narrative for press-on #1 is grounded, and it resolves *against* "surprising".**
  I read the vendored `ProvidedTypes.fs`'s own type-conversion path directly (see objection 1). It does
  not special-case `FSharpFunc`; the PASS was predictable from that code.

I then went past reproduction and built three additional shapes the executor did not test (objection 2).
Those extra members and their satellite functions were added to the scratch build, exercised, and then
**reverted** — the scratch build and `artifacts/` are back byte-identical to the checked-in state, and
Round 3 was re-run PASS from the restored source to confirm I left nothing broken. The extra-shape
results below are therefore reproducible only by re-applying the diff I describe, not from the current
checked-in tree; I kept the tree matching 02-results deliberately.

## Strongest objections

1. **The PASS is real but it was not a discovery — it is a re-derivation of what Q016's attempt-4 design
   already entailed, and the vendored SDK source says so directly.** This is the executor's own press-on
   #1, and it resolves against "surprising". `convType` (`ProvidedTypes.fs:9352-9384`) converts a
   provided member's parameter/return types with no `FSharpFunc` special-case anywhere. A
   non-generic-definition generic type takes the branch at `:9365-9369`: convert the generic *definition*
   (`FSharpFunc<_,_>`, an ordinary FSharp.Core type present in both the source and target assembly sets
   because the consumer references FSharp.Core), recurse `convType` over each type argument, then
   `MakeGenericType`. `FSharpFunc<string,string>` is handled by the exact same code as `list<int>` or
   `Nullable<bool>`. The three `FSharpFunc`-related hits elsewhere in the file (`:8969-8972` the
   `OptimizedClosures.FSharpFunc` arity dispatch, `:14456`/`:14469` target-side `Expr.Lambda`
   construction) are in IL/quotation emission, not the parameter-type-conversion path, and none of them
   fire for a member that merely *declares* an `FSharpFunc`-typed parameter and forwards it. So Q016's
   attempt-4 decision — derive every `ProvidedParameter`/return type directly from the real
   `MethodInfo`'s own reflection so its identity already matches what the target expects — is exactly why
   this works: the forward table finds each reflected type unchanged. Q017's contribution is not new
   machinery; it is turning Q016's review objection-1 hypothesis ("consistent with general, but general
   is a hypothesis") into a verified fact across more shapes. That is worth having, but the SHIP should
   not be read as "a new capability was found."

2. **"Function-typed parameters work" carries no more weight than "reference-typed arguments work",
   because the provider never touches the function value — and I confirmed this generalizes to the
   harder directions the executor named but did not test.** This is press-on #2. The consumer's
   `(fun (s: string) -> s.ToUpperInvariant())` is compiled by FCS into a closure class implementing
   `FSharpFunc<string,string>` *in the consumer assembly*, and passed as an ordinary argument into the
   forwarded `call`. The provider's design-time code never constructs, adapts, introspects, or emits a
   closure; it declares a static parameter type and emits `Expr.Call(mapMi, args)`. From the SDK's
   perspective an `FSharpFunc`-typed argument is an object reference like any other. To test whether the
   *opposite* direction (a function value flowing OUT, the case that actually matters for a
   Lenses-shaped getter/setter provider) is any harder, I added two members to the satellite and wrapped
   them with the identical kernel:
   - **`adder (record': Person) : (int -> int)`** — a genuine `FSharpFunc<int,int>` *return*. Provider
     member `Adder`, consumer `let f : int -> int = P.Adder(person) in f 100`. Compile errors=0;
     provider-mediated `f 100 = 142`; independent direct-reflection `adder(person) |> (fun f -> f 100) =
     142`; agree. **Function values flow OUT with zero new scaffolding.**
   - **`transform (f: Person -> Person) (record': Person) : Person`** — an `FSharpFunc<Person,Person>`
     whose element type is the real record, not a primitive, plus a *different* non-identity lambda
     (`fun p -> P.Create(P.GetName(p) + "!", P.GetAge(p) * 10)`). Compile errors=0; result
     `name="Ada!" age=420`, exactly as expected. **Non-primitive `FSharpFunc` element types flow through
     too**, consistent with `convType` recursing on type arguments.
   Both pass. So the mechanism generalizes further than the single Map case Q017 pre-registered. But the
   reason it generalizes is precisely objection 1: it is opaque-object forwarding in every direction, and
   "function-typed" is not a property the kernel does anything special about. This *strengthens* the SHIP
   direction while *deflating* the significance of the word "function-typed" in the headline.

3. **The kernel is brittle in a way 02-results never surfaces, and I hit it by accident — this is the
   single most important thing the executor's results omit.** My first `adder` attempt was
   `let adder (record': Person) : (int -> int) = fun x -> x + record'.age`. F#'s compiler *flattened*
   that curried function-returning-function into a two-parameter method `adder(Person, int) : int` — at
   the CLR/reflection level there was no `FSharpFunc` return at all. My provider built one
   `ProvidedParameter` (from `GetParameters().[0]`) for a method the real `MethodInfo` says has two
   parameters, so `Expr.Call(adderMi, args)` threw **"Incorrect number of arguments (Parameter 'args')"**
   — and that single malformed member **poisoned the entire provided type**: `Create`, `GetName`,
   `GetAge`, `Map`, and `Transform` all became "does not define the field, constructor or member" in the
   same compile. There is no per-member isolation; one bad forward takes down everything. This matters
   concretely for the Lenses follow-up the executor invokes: whether a Myriad-generated function-returning
   member exposes a real `FSharpFunc` return depends on F#'s arity-flattening, which is *not* visible
   from the F# source signature and which the reflection-forwarding provider consumes as-is. I only got a
   genuine `FSharpFunc` return by defeating the flattener (`id (fun x -> …)`, forcing arity-1). A real
   generator author wiring `Lenses` getters this way would have to inspect the *compiled* arity of every
   member and would get a catastrophic, whole-type failure on any mismatch — not the narrow per-member
   error the phrase "the mechanism generalizes" implies.

4. **The distinguishing-lambda falsifier (Q017's answer to Q016 objection 2) is adequate but not because
   uppercase+increment is special — it is adequate because the independent check reconstructs the value a
   *different* way, and I confirmed that holds under a different lambda pair.** This is press-on #3. The
   uppercase/increment choice on its own only rules out an identity echo; a reimplementation that
   correctly applied the two functions would still agree. What actually discriminates forwarding from
   reimplementation here is the same thing that discriminated it in Q016: `invokeCode = fun args ->
   Expr.Call(mapMi, args)` emits a direct `call` into the loaded `MethodInfo` (source fact), and the
   independent check builds its `FSharpFunc` values via `FromConverter` and invokes the real `map`
   reflectively, a genuinely separate path. My Round-4b used a third, structurally different lambda
   (append "!", multiply by 10, constructing a *new* record inside the lambda) and it agreed too. So the
   agreement is robust, not coincidental to one arithmetic choice — but the load-bearing evidence remains
   the `Expr.Call` source fact plus path-independence, not the specific arithmetic, exactly as in Q016.

## Smaller points, checked

- **No regression to Round 1, confirmed by execution, not just by the guard.** The `if mapMi <> null`
  guard mirrors `emailMi`'s, and I reran Round 1 after every rebuild: still PASS, `name="Ada" age=42`.
  Consumers that never reference `Map`/`Adder`/`Transform` are unaffected — *until* a malformed member is
  added, at which point objection 3's whole-type poisoning would hit them too. The guard protects against
  a *missing* method, not against a *shape-mismatched* one.
- **KILL did not trigger, and that is the clean win worth stating.** The design's named risk — that
  `Expr.Call` against a `MethodInfo` with `FSharpFunc`-typed parameters is rejected by the SDK's
  IL-emission path, or that a lambda literal cannot resolve to the provided `FSharpFunc` parameter — did
  not materialize, for parameters *or* returns *or* record-typed element types. The emitted consumer IL
  resolves every forwarded call.
- **NULL did not trigger.** Forwarding function-typed parameters, an `FSharpFunc` return, and a
  record-typed function element is materially more signature surface than anything Q016 wrapped
  (`create : string -> int -> Person` and three primitive-returning getters). "Nothing beyond Q016" does
  not hold.
- **No new API was introduced, verified by diffing the provider.** The added `Map` member uses only
  `ProvidedMethod`/`ProvidedParameter`/`Expr.Call`/`GetParameters()`/`ReturnType` — the identical
  surface Q016's `Create`/`GetName`/`GetAge` already used. `ConvertSourceExprToTarget`/
  `ConvertSourceTypeToTarget` (which broke Q016's attempts 2-3) are absent, as the design promised. My
  extra `Adder`/`Transform` members likewise needed no new API. So the REVISE threshold's trigger ("passes
  only after new scaffolding") is not met.
- **Standing caveat, unchanged since Q006:** all of this is `FSharpChecker`-as-library, never a real IDE
  host. Not retested here.
- **Round 2's file-lock problem is untouched**, exactly as the hypothesis scoped it. This quartet says
  nothing new about that; Q016's REVISE on that axis stands.

## Verdict

**SHIP**, matching the pre-registered thresholds read literally. The cheapest falsifier (Round 3) passes
cleanly: zero type-mismatch diagnostics and the provider's result agrees field-for-field with the
independent direct-reflection call, on a signature with two `FSharpFunc<_,_>` parameters plus a record.
No new scaffolding beyond Q016's kernel was required, so REVISE (whose sole trigger is "passes only after
new scaffolding") is out. KILL is out — the IL-emission path accepted every function-typed shape I threw
at it. NULL is out — this is materially more signature surface than Q016 wrapped. By the hypothesis's own
words, this "means Q016's kernel is genuinely shape-general, not luck-shaped to primitive/single-record
signatures," which is the SHIP condition.

Three things travel with the SHIP and matter more than the label:

- **The SHIP is a confirmation, not a discovery (objection 1).** The vendored `convType` has no
  `FSharpFunc` special-case; the PASS was entailed by Q016's attempt-4 "derive types from real
  reflection" decision. Q017 converts Q016 review objection-1's hypothesis into a verified fact, and I
  extended that fact to `FSharpFunc` *returns* and record-typed function *elements* (objection 2), which
  the executor did not test. Cite Q017 as "the kernel is shape-general, confirmed across four shapes
  including function-out," not as "a new function-passing capability was found."

- **"Function-typed" is not load-bearing (objection 2).** The provider forwards function values as opaque
  object references in both directions and never manipulates a closure. The win is exactly as general as
  "any reference-typed argument forwards," which is genuinely general — but the word "function" should
  not be read as evidence the provider can *do* anything with functions.

- **The kernel is brittle to compiled-arity surprises, and F#'s currying-flattening is a live trap for
  the Lenses follow-up this result is meant to unblock (objection 3).** A source-level
  function-returning-function usually compiles to a flat multi-arg method, not an `FSharpFunc` return; a
  `ProvidedParameter` list that disagrees with the real `MethodInfo`'s arity throws and poisons the whole
  provided type, not just the offending member. Any real generator wiring must inspect *compiled* arity,
  not source signatures.

## Follow-ups, prioritized

1. **Before building a Lenses-shaped forwarding provider, check what the real `Lenses` generator emits at
   the CLR level.** Objection 3 shows the source signature is not a reliable guide: F# flattens curried
   returns, so a `Lens` getter/setter that looks like `'T -> 'Field` in source may or may not be an
   `FSharpFunc`-returning method after compilation. Determine the compiled arity of `Lenses` output
   before assuming the reflection-forwarding kernel exposes usable function values from it. This is the
   concrete next spike the Q016→Q017 line points at.
2. **Add arity-mismatch defense to the kernel, or document the whole-type-poisoning failure mode
   loudly.** The `if mapMi <> null` guard handles a *missing* method but not a *shape-mismatched* one; a
   single bad forward is catastrophic to the entire provided type. A real generator-driven version needs
   either a per-member try/guard that skips (and warns about) members whose declared arity disagrees with
   the reflected `MethodInfo`, or an explicit up-front assertion, so one surprise does not silently take
   down every other member.
3. **Curried arity > 2 and mixed function/primitive interleavings.** I tested arity-2 curried
   (`transform`) and a genuine arity-1 `FSharpFunc` return (`adder`). A three-plus-argument curried
   member with functions interleaved among primitives is the next shape to confirm, since arity flattening
   interacts with it in exactly the way that bit me in objection 3.
4. **Real-IDE host test (standing caveat since Q006).** Whether FSAC/Ionide reproduces this pass, rather
   than `FSharpChecker`-as-library. Unchanged from Q016.
