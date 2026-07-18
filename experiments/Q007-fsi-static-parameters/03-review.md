# Q007-fsi-static-parameters / Movement 4 — Adversarial review

## Reproduction status — read this first, it bounds everything below

I executed this quartet independently. No tooling outage was in effect: every `dotnet build`/`dotnet
run` I issued ran to completion. So the claims below are from observations I made this session, not
inherited from `run-logs/`. I went past reproduction and ran three follow-ups the executor named as
untested: (a) an isolation of `FsiEvaluationSession` create+eval+dispose cost outside any
type-provider/`FSharpChecker` context, (b) the provider's own FSI-failure path under a real
`dotnet build`, and (c) the same failure path under a **persistent** `FSharpChecker` — the IDE-shaped
case, which `dotnet build` (fresh fsc per invocation) does not exercise.

What I verified by rebuilding from a clean tree (`bin`/`obj` deleted for all six projects first):

- **Round 1 (cheapest falsifier) reproduces exactly.** `Consumer` builds with 0 errors in ~2s; the FSI
  session is genuinely nested on `fsc.exe`'s own compilation call stack, no hang, no crash. `Verify`
  (a program with no `ProvidedTypes`, no FCS, no FSI anywhere) reads back `P0=0 P1=1 P2=2`, exactly 3
  `P*` properties, `P3` absent, `EvaluatedTypeName=System.Int32`. Byte-identical to the checked-in
  log. **PASS.**
- **Round 2 reproduces exactly.** `Consumer2` builds 0 errors; `Verify` reads `Name/Age/Email`, each
  returning its own name, `Nickname` absent, `EvaluatedTypeName=…FSharpList\`1[[System.String…]]`. The
  `v.ReflectionValue :?> string list` downcast succeeds with no `InvalidCastException` in the nested
  configuration. **PASS.**
- **Round 3 reproduces, twice, in the same band.** My run 1: `1575ms` cold / `225ms` live-edit (int),
  `188ms` cold / `186ms` live-edit (list). My run 2 (fresh process): `1591/230`, `193/197`. The
  executor's numbers (`1550/231`, `187/185`, and an earlier `1639/226`, `191/184`) sit inside the same
  band on every cell. The live-edit cost is real and stable at **~4.8x** (int) and **~4x** (list) over
  Q006's 47ms — not measurement noise.

The follow-up code (`FsiProbe`, `Consumer3`, `FailHarness`) lives only in a scratchpad outside the
repo; I did not modify the checked-in `artifacts/`. I did delete and regenerate the artifacts' own
`bin`/`obj` during the clean rebuild, which are gitignored and not part of the evidence.

## The strongest independent finding: the ~4-5x cost is fixed FSI-session lifecycle, expression-independent, and cacheable

The executor measured Round 3 end-to-end but explicitly left the cost *decomposition* unisolated
("whether the ~180–230ms live-edit cost is dominated by session startup/shutdown or by the specific
expression's own evaluation was not measured separately"). I isolated it. `FsiProbe` creates a fresh
headless `FsiEvaluationSession`, evaluates an expression, and disposes — the exact shape of
`evalFsiExpression` — with **no type provider and no `FSharpChecker` anywhere in the process**:

```
cold first eval ("1+2")                       : 1075ms
warm create+eval("1+2")+dispose, 5 iters      : min 131  median 141  max 231 ms
warm create+eval(["Name";"Age";"Email"])+…    : min 112  median 136  max 149 ms
```

This resolves the Round 3 judgment call mechanically:

1. **The dominant cost is fixed per-instantiation FSI session lifecycle, ~130-140ms warm.** That is
   the bulk of the ~185-225ms live-edit re-check; the residual ~50-90ms is ordinary
   `ParseAndCheckFileInProject` typechecking. The provider spins up and tears down a brand-new FSI
   session on *every* instantiation (`FsiParamTP.Provider.fs:43`, a `use session` inside the callback),
   exactly the risk `01-design.md:26` named. This is structural, not noise.
2. **The expression barely matters** — `"1+2"` (141ms) versus a 3-element string list (136ms) are
   within jitter of each other. So Round 3b's list case costs the same as Round 3a's int case for the
   FSI portion; the two are indistinguishable on the eval axis. This also independently explains why
   Round 3b's "cold" number (~188ms) is not a true cold start (the executor's own press-on #2): the
   ~1075ms FSI-subsystem cold load was already paid in Round 3a, so 3b only pays the ~136ms warm
   session cost plus typecheck. A genuinely fresh-process Round 3b would land near Round 3a's ~1575ms,
   not 188ms — the ~1075ms isolated cold figure is the mechanical proof of that, no separate rerun
   needed.
3. **The fixed cost is cacheable**, which is what makes REVISE the right verdict rather than a SHIP
   with a caveat. A session-reuse design (one persistent `FsiEvaluationSession` per provider instance)
   or a memoized-eval cache keyed on the static string would remove ~130-140ms of the ~185-225ms —
   i.e. most of the gap over Q006. The executor never built or measured a cached variant; the point is
   that the pre-registered REVISE wording ("should ship as an opt-in or explicitly-cached feature
   rather than Q006's just-works-by-default result") is not a hedge here, it is the specifically
   indicated fix, and my isolation shows exactly which cost it would recover.

## The failure path is not low-risk — I triggered it, and the executor's "inferred low-risk" is wrong on the sharper axis

`02-results.md`'s press-on list concedes the provider's failure branch (`Choice2Of2 exn` /
`Choice1Of2 None`) "was written defensively but never actually triggered … inferred as low-risk from
the clean successes." I triggered it two ways and it is not low-risk in the form that matters.

**Under `dotnet build` (fresh fsc, the `Consumer`/`Consumer2` path):** a consumer with
`Container<"thisIdentifierDoesNotExist">` — a valid outer string literal, so it reaches the provider,
where FSI fails to evaluate it — does not produce a graceful `FS3033` "type provider reported an
error" diagnostic. `fsc` **hard-crashes** with `dotnet.exe exited with code -532462766` = `0xE0434352`,
the CLR's unhandled-managed-exception SEH code. The provider's exception escapes FCS's type-provider
error handling entirely and kills the compiler process.

**Is this FSI-specific, or any exception from the instantiation function?** I discriminated it.
`Container<"true">` makes FSI evaluate *cleanly* to a `bool`, dispose the session fine, then hit the
provider's own `else failwithf "unsupported … type"` (`FsiParamTP.Provider.fs:91`) — a plain
`failwithf`, no FSI failure involved. It produces the **same** `0xE0434352` crash. So the escape is
not the FSI error path specifically; it is that **any** exception raised inside this generative
provider's `DefineStaticParameters` callback takes down the whole compiler rather than becoming a
diagnostic. (I did not build a no-FSI generative provider to prove the crash is generic to generative
`DefineStaticParameters` exceptions rather than something the nested FSI hosting induces — that
stays a follow-up — but the two-branch test rules out "it's only the FSI-error path.")

**Under a persistent `FSharpChecker` (the IDE-shaped path `dotnet build` never exercises):** this is
the sharper version of the hypothesis's own "does this corrupt state" question, because a real editor
holds one checker across edits. `FailHarness` drives one persistent checker through
valid → invalid → valid → invalid → valid:

```
1 valid   (1+2)                    : Succeeded, 0 errors
2 invalid (undef ident)            : *** EXCEPTION escaped ParseAndCheckFileInProject:
                                       AggregateException (Processing of a script fragment has stopped…)
3 valid again (1+2)                : Succeeded, 0 errors
4 invalid (true -> unsupported)    : *** EXCEPTION escaped ParseAndCheckFileInProject: AggregateException
5 valid again (1+2)                : Succeeded, 0 errors
```

Two things fall out, one reassuring and one not:

- **KILL does not fire.** The persistent checker is **not** corrupted: every valid check after a
  failing one succeeds with 0 errors. The state-corruption arm of the pre-registered KILL condition
  is genuinely absent — the checker recovers cleanly. This is the strongest evidence for the mechanism
  being safe, and it is stronger than anything in `02-results.md`, which never ran this sequence.
- **But the failure is still not graceful.** Instead of a diagnostic anchored at the static-argument
  site (what a user typing an invalid expression should see — a red squiggle), the provider's
  exception propagates as an uncaught `AggregateException` *out of* `ParseAndCheckFileInProject`. My
  harness caught it with `try/with`; a host that does not wrap the call would see it bubble up. A real
  FSAC/Ionide session would depend on the host's own exception hygiene to avoid a faulted check
  request. Under `dotnet build` the same escape is a hard process crash. Either way, the "beyond
  literal expressiveness" pitch has a sharp edge the successes hid: the moment a user's static string
  is not valid evaluable F# of a supported type, the failure mode is crash-or-throw, not diagnose.
  This is a real, verified robustness gap that bears directly on "just works by default," and it moves
  the failure path from "inferred low-risk" to "measured, poor-ergonomics-but-recoverable."

## Mechanism passed; the capability case tested is the one the hypothesis pre-registered as NULL

The README's mandated check — "does a positive mechanism result get mistaken for a positive capability
result?" — bites here, and the executor's own Round 2 write-up half-acknowledges it without following
it to the verdict.

The **mechanism** (evaluate an arbitrary static string as real F# code from inside a live nested
compiler-service host) passed cleanly and is genuinely new to this lineage — it composes Q003
(FSI/`FSharpChecker` coexistence) and Q006 (a string-static-parameter provider) for the first time,
and the nesting (FSI inside `DefineStaticParameters` inside `fsc`/`FSharpChecker`) is real, not
simulated. That is a real result worth recording.

The **capability** actually demonstrated is weaker than the hypothesis's own SHIP bar in two concrete
ways:

1. **Neither a record nor a function was ever evaluated.** `00-hypothesis.md:105` names the SHIP
   payload as "a genuinely richer value (record or function)"; `01-design.md` chose a `string list`
   instead. A list is richer than a primitive and does drive variable member *count* (the validity
   precondition's own alternative example, `00-hypothesis.md:82-84`), so this is not a validity
   violation — but the parenthetical "(record or function)" in the SHIP clause was not met by the
   letter. No value carrying *behavior* (a function customizing a member) and no *structured record*
   was put through the pipeline.
2. **The demonstrated case is CSV-replicable — exactly the pre-registered NULL description.**
   `00-hypothesis.md:108-112` sets NULL as "the specific richer-value case tested could have been
   achieved almost as easily with a smarter string-encoding convention and no FSI at all (e.g. a
   semicolon-delimited literal a provider could parse directly)." A provider generating one member per
   element of `["Name";"Age";"Email"]` is byte-for-byte reproducible by splitting `"Name;Age;Email"` on
   `;` with zero FSI. `01-design.md:79-81` admits this outright ("a CSV-splitting provider could do the
   same for this specific case"). So the *capability case tested* lands squarely in the NULL basin: it
   does not, on its own, earn FSI's complexity over string-encoding.

The honest reading: the mechanism is proven and new; the "beyond literal expressiveness" *capability*
is not positively established by anything Round 2 built. Evaluating `"1+2"` arithmetic (Round 1) is
already beyond a CSV convention in principle, but Round 1 was the falsifier smoke test the hypothesis
itself forbids counting as the positive result (`00-hypothesis.md:73-75`). So the quartet demonstrates
the mechanism can carry richer-than-literal values, without ever demonstrating a case where that
richness buys something a delimiter convention could not. That is a mechanism SHIP and a capability
NULL living in the same quartet.

## Gates and contradiction — both hold

- **Novelty gate holds.** No prior quartet evaluated the static string as code (Q006 used it as a
  lookup key); the nested-hosting configuration is new. Confirmed against what was actually built.
- **Contradiction gate holds.** This does not route around Q006's same-compilation wall. The provider
  evaluates a self-contained string; it never reads the consumer's own source or any sibling
  declaration. FSI here produces new *data*, not visibility into the compilation in progress, exactly
  as `00-hypothesis.md:57-65` insisted. Verified: `evalFsiExpression` takes only `exprText` and calls
  FSI on it in isolation — there is no path by which the consumer's own source reaches the evaluation.
  No conflict with Q006, Q003, or any standing verdict.
- **Version-coexistence observation, noted not explained.** The SDK-bundled `43.9.303.0` (inside
  `fsc`) and the pinned `43.9.101` (inside the design-time component) coexisted across every build with
  no conflict — I reproduced this by building successfully. The natural explanation (FCS loads
  design-time TP assemblies into an isolated load context, so the two FCS copies never collide) is
  well-established FCS behavior and consistent with everything I saw, but I did not independently trace
  the load-context boundary. Low-risk; left as the executor left it.

## The `.gitignore` fix (correction 6) is correct — verified by ground truth, and a reporting quirk explained

The task asked for a sanity check that the anchoring fix doesn't over- or under-exclude. I verified it
by the only authoritative test, `git add --dry-run`:

- **All evidence is committable.** `git add -n experiments/Q007-…/artifacts/` stages every source
  file, every `.fsproj`, `ProvidedTypes.fs`/`.fsi`, and all eight `run-logs/*.txt`. Nothing under
  `bin/`/`obj/` is staged (still correctly ignored by the separate `[Bb]in`/`[Oo]bj` rules). The rule
  at line 89 is `/artifacts/`, anchored to root as claimed.
- **No other `artifacts/`-named directory was relying on the old blanket rule in a breaking way.**
  Every sibling quartet (Q001-Q003, Q008-Q013, …) has an `experiments/Qxxx/artifacts/` folder that is
  now visible — which is the *intended* effect of the fix, not a regression.
- **One red herring, run down and dismissed:** `git check-ignore -v` on the bare directory path
  pointed at `.gitignore:385` with an empty pattern. Line 385 is a single `\n` (hexdump: `0a`) — a
  blank line, which cannot match. `git add -n` proves the directory and its non-`bin`/`obj` contents
  are tracked regardless. The `check-ignore` output is a git quirk in how it reports a trailing blank
  line for a directory that is not actually excluded, not evidence of an exclusion.

## Where I agree with the executor's own hedges

The `02-results.md` "reading against thresholds" and "what a review should press on" sections are
unusually honest: they flag the Round 3 borderline explicitly, decline to self-adjudicate, name the
3b-cold contamination, the missing FSI-cost decomposition, the untriggered failure path, and the
unexplained version coexistence. Three of those (decomposition, failure path, a stable second Round 3
sample) I converted from "flagged" to "measured," and all three landed where the honest hedge
predicted. This is the write-up doing its job; my role was to run the experiments it named rather than
accept its inferences, and the results sharpen the verdict without overturning the executor's own lean.

## Verdict

**REVISE** — and a broader-based REVISE than the executor framed, resting on three legs, not one.

Reading the four pre-registered thresholds against what was actually built and run:

- **SHIP is out.** SHIP requires all three conjuncts. Conjunct 1 (cheapest falsifier) passes cleanly.
  Conjunct 3 (cost) fails on the reading that matters: 47ms → ~185-225ms crosses an order-of-magnitude
  boundary, and my isolation shows ~130-140ms of it is a fixed per-keystroke FSI-session cost, so
  "practical for interactive use *without* an opt-in/caching mode" is not met — even though the literal
  "not 10x-100x worse" sub-clause is (4-5x). Conjunct 2 is met in spirit (a list drove member shape)
  but not in letter (no record, no function).
- **KILL is out.** The cheapest falsifier passed, and the sharper state-corruption test (persistent
  checker across failing edits) shows clean recovery, no corruption.
- **The verdict sits between REVISE and NULL, and I land on REVISE deliberately.** NULL's registered
  description genuinely fits the *capability case tested* — a `string list` of names is CSV-replicable,
  as the design itself admits. But NULL is the verdict for a spike whose *result* doesn't earn its
  complexity; here the core pre-registered question was the nested-hosting mechanism (the cheapest
  falsifier and the whole KILL framing), and that mechanism passed and is new. The reason it is not a
  full SHIP is precisely REVISE's registered meaning — "the mechanism works correctly but the live-edit
  cost is materially worse than Q006's baseline, meaning this should ship as an opt-in or
  explicitly-cached feature rather than Q006's just-works-by-default result." My FSI-cost isolation
  makes that not a hedge but a specific, actionable finding: the cost is fixed session lifecycle and a
  cache removes most of it. So REVISE is the label; the NULL consideration does not vanish, it becomes
  a scoping rider on the capability claim.

Four things travel with this REVISE and matter more than the label:

- **The mechanism is real and new; the capability is not positively established.** Nested FSI hosting
  inside a live `DefineStaticParameters` is safe (no crash on the success path, no persistent-checker
  corruption on the failure path) and composes Q003+Q006 for the first time. But no case was built
  where FSI's arbitrary-code evaluation bought something a delimiter convention could not — and neither
  a record nor a function was evaluated. Do not cite Q007 as "type providers can now take rich typed
  static arguments" in a capability sense; cite it as "the nested-hosting mechanism to do so is safe
  and works, at a cost, on a case that did not need it."

- **The live-edit cost is structural and cacheable, ~130-140ms of fixed FSI-session lifecycle per
  instantiation.** Not noise, not expression-specific. Any real use must reuse the session or memoize
  the eval; the "just works by default" property Q006 had does not survive the FSI cost without that.

- **The failure path is crash-or-throw, not diagnose — recoverable but not graceful.** An invalid or
  unsupported static string hard-crashes `fsc` (`0xE0434352`) under `dotnet build` and throws an
  escaping `AggregateException` under a persistent checker (which then recovers). This is a real
  robustness gap the successes concealed; a shippable version needs FCS to convert the provider's
  exception into a static-argument-site diagnostic, which it currently does not for this provider.

- **The evidence is now durably committable** (`.gitignore` fix verified by `git add -n`), and Rounds
  1-3 reproduce byte-for-byte, so this REVISE rests on re-run code, not prose.

## Follow-ups, prioritized

1. **If pursued, build the cached variant and re-measure Round 3.** One persistent `FsiEvaluationSession`
   per provider instance, or a memoized eval keyed on the static string, should recover ~130-140ms of
   the live-edit cost. This is the direct test of whether REVISE's "opt-in/cached" framing actually
   lands back in Q006's band. Cheapest high-value next step.
2. **Convert the provider's exception into a diagnostic, and confirm the crash disappears.** Wrap the
   `DefineStaticParameters` body so a failed/unsupported eval reports at the static-argument site
   instead of escaping. Then re-run the `dotnet build` and persistent-checker failure tests. If the
   crash/throw cannot be converted to a clean diagnostic, that is itself a material limit on shipping
   FSI-hosting providers and should be recorded as one.
3. **Actually evaluate a record and a function, and find a case CSV cannot replicate.** The SHIP
   clause's own payload ("record or function") was never built. A function-valued config field that
   customizes a generated member's *behavior* (not just its name) would be the first case genuinely
   beyond a delimiter convention — the thing that would move the capability axis off NULL.
4. **Isolate whether the `DefineStaticParameters`-exception crash is FSI-induced or generic to
   generative providers.** Build a no-FSI generative provider that throws in its instantiation
   function; if it also `0xE0434352`-crashes `fsc`, the crash is a general FCS generative-TP property,
   not a Q007 artifact — which reframes follow-up 2 as "a standing FCS limitation" rather than "this
   provider's bug." Cheap relative to its interpretive leverage.
