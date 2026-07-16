# Q016-satellite-dll-type-provider / Movement 4 — Adversarial review

## Reproduction status — read this first, it bounds everything below

I could not independently re-execute this quartet. For the entire review window the environment's
command-safety classifier was unavailable for *effectful* commands ("claude-sonnet-5 is temporarily
unavailable, so auto mode cannot determine the safety of…"): read-only commands (`grep`, `diff`,
`find`, all Read/Glob/Grep tooling) ran, but every `dotnet build`/`dotnet run` was refused, across both
Bash and PowerShell, on more than a dozen spaced retries. So the pre-registered mandate for this
review — rerun Round 1 at a *different* parameter, ideally with a materially different signature, and
reproduce Round 2's lock independently — was not runnable. I am not going to paper over that: the
pass/fail claims in this review are inherited from the saved logs and from source I read in full, not
from an observation I made myself.

What I *was* able to verify without executing anything, which is more than nothing:

- **The saved logs correspond to exactly this code.** The executor's original scratch build survives
  intact at the hard-coded job path (`…/jobs/f0b85ddf/tmp/q016-satellite-dll-spike/`), and
  `diff` reports its `SatelliteProvider.fs` is byte-identical to
  `artifacts/SatelliteTP.DesignTime/SatelliteProvider.fs`. `Satellite.dll` and a compiled
  `Harness.dll` are still present. So `run-logs/round1-run.txt` and `round2-run.txt` were produced by
  the code that is checked in, not a since-edited variant.
- **The SDK-internals narrative is grounded, not invented.** Every line reference the four-attempt
  history cites resolves in the vendored SDK: `RegisterGeneratedTargetAssembly` at
  `ProvidedTypes.fs:16372`, the `sourceAssemblies` constructor at `ProvidedTypes.fsi:459/477`,
  `ConvertSourceTypeToTarget`/`ConvertSourceExprToTarget` present. The attempt log is a faithful
  account of real API surface.
- **The realism claim in §"what a review should press on" #2 is checkable from source and checks out.**
  `src/Myriad.Sdk/build/Myriad.Sdk.targets` runs generation as a fresh `<Exec>` per input
  (`MyriadSdkGenerateCode`, line 227), gated `Condition="'$(DesignTimeBuild)' != 'true'"` (line 162),
  with the design-time-build target left empty (line 230). Myriad's own generation is a short-lived
  process per build; it never holds a lock across edits.

Treat the verdict as: the mechanism *as implemented in the checked-in source* does what §Round 1 says,
and the objections below are source-grounded. The one thing this review was specifically asked to add —
a fresh, differently-shaped Round 1 — is **not** delivered, and stays the top follow-up.

## Four strongest objections

1. **"The mechanism generalizes" is not shown, and the provider as written *cannot* generalize without
   new code — a sharper statement than 02-results' "only one shape tested."** `createTypeUncached`
   hard-codes `moduleType.GetMethod("create")`, `"name"`, `"age"`, and (conditionally) `"email"`
   (`SatelliteProvider.fs:141-149,184`). Any other record — different field names, `Lenses`,
   `DUCasesGenerator` — is not merely untested; it produces members this provider never looks for, so
   it requires editing the provider. The genuinely shape-independent kernel is real and small (the
   `sourceAssemblies` source-registration trick, plus `ProvidedParameter`/return types derived directly
   from the target `MethodInfo`'s own reflection and `Expr.Call(mi, args)` — the deliberate fix in the
   attempt-4 comment, which is sound), but the strongest *signature* it was ever exercised on is
   `create : string -> int -> Person`. Every wrapped member takes at most **one** non-primitive
   argument (a `Person`). The one materially different shape is sitting unwrapped in the same
   `Generated.fs` the whole time: `map (mapname: string -> string) (mapage: int -> int) (record': Person)
   : Person` — two *function-typed* (`FSharpFunc<_,_>`) parameters plus a record, returning a record.
   Whether `Expr.Call` forwards `FSharpFunc`-typed arguments through the SDK's automatic
   source-to-target conversion is exactly the untested question, and it is the one I was asked to
   settle and couldn't. 02-results is honest that generalization is unproven; I'd go further — the
   evidence is consistent with the trick being general, but the four-attempt history shows how much of
   getting here was incantation-finding, so "general" is a hypothesis, not a result. Two clarifications
   in the executor's favour, to keep the objection honest: Round 2 *did* exercise a 3-arg `create` and
   three getters returning primitives rather than the record (`GetName`/`GetAge`/`GetEmail`), so
   "returns something other than the record type" and "3+ args" are already covered — the genuinely
   open case is specifically the multi-non-primitive / function-typed argument.

2. **Round 1's "two independent ways" is weaker evidence than the phrase implies, because `Fields`
   getters are trivial identity echoes.** `name`/`age` are `let name (x: Person) = x.name` /
   `x.age`. Both pre-registered checks — readback of the compiled consumer's `readName`/`readAge`, and
   `directSatelliteCall` — ultimately return `"Ada"`/`42`, and so would a provider that *reimplemented*
   `fun p -> p.name` instead of forwarding. The load-bearing proof that this genuinely calls Myriad's
   compiled code rather than reimplementing it is the *source* fact that `invokeCode = fun args ->
   Expr.Call(realMethodInfo, args)` emits a direct `call` into the loaded `MethodInfo` — not the runtime
   agreement, which for an identity echo cannot discriminate the two. This is the same NULL-adjacent
   trap Q001 named for pure structural-echo generators, and it means the mechanism claim rests on code
   inspection at least as much as on the "verified two independent ways" runtime result the falsifier
   pre-registered. A stronger falsifier would forward a member whose output is *not* a trivial echo of
   its input, so the runtime value itself distinguishes forwarding from reimplementation. (`create`
   composes a record, which is marginally better, but its check runs through the same two
   trivially-agreeing paths.)

3. **The Round 2 FAIL lands precisely on the type provider's *only* differentiator over an ordinary
   `<ProjectReference>`, which makes it more damaging than 02-results frames it, and the "is the
   long-lived process realistic?" question resolves against the idea either way.** For the cross-project
   case, `BACKLOG.md` item 15 concedes in its own text that a TP "is barely pulling its weight over an
   ordinary `<ProjectReference>` — its actual differentiator is dynamic re-exposure … refreshed live
   via `Invalidate()` + `FileSystemWatcher`." Round 2 shows that exact live-re-exposure path is what
   breaks: `Assembly.LoadFrom` (required, per attempts 1/3, for the source-identity match Round 1 needs)
   holds the file open, the separate `SatelliteBuilder` process cannot overwrite `Satellite.dll`
   (`Could not open file for writing`), so the file never changes, so the `FileSystemWatcher`-driven
   `Invalidate()` never even fires and step 5 correctly still sees the v1 member set. Now the realism
   fork the executor raised (§press-on #2): (a) a *live design-time/IDE host* genuinely stays alive
   across edits, so the long-lived process is the *correct* model for the scenario the provider is
   for — and in that model the lock is real and fatal; (b) Myriad's *own* generation is a fresh
   `<Exec>` per build (confirmed above) that never holds a lock — but in that world you also get no
   live re-exposure and don't need the provider at all, a rebuilt `<ProjectReference>` already does it.
   Both branches remove the provider's reason to exist for this case. 02-results reports the lock
   honestly but does not connect it to "this is the sole thing the TP was buying us"; that connection
   is the most important finding here.

4. **REVISE is the correct label, but it is a *weaker* REVISE than the pre-registered threshold's
   wording assumes, because the "workaround that narrows the claim" is not shown to exist.** The
   registered REVISE reads: falsifier passes but regeneration hits the lock "requiring a workaround
   (e.g. `Assembly.Load(bytes)` instead of `LoadFrom`, per Q011's lesson) that narrows the claim." But
   this quartet's own attempts 1 and 3 establish that `Load(bytes)` — the very workaround the threshold
   names — cannot satisfy the source/target identity match the stronger real-external-type claim needs.
   So the threshold's own escape hatch is ruled out on the record. The only remaining candidate named
   is a collectible `AssemblyLoadContext` unloaded before rebuild, which 02-results itself flags may
   just relocate the tension (a live `ProvidedTypesContext` holding references into the ALC could block
   collection). The honest state is therefore: **Round 1's mechanism ships; Round 2's capability is
   blocked by a genuine two-requirement tension with no demonstrated resolution** — not "works with a
   specific loading discipline." Any citation of this quartet as REVISE must carry that the repair path
   is unproven, not merely "narrower than hoped."

## Smaller points, checked

- **The blast-radius bullet is accurate; if anything its framing should be tightened, not softened.**
  `LoadHelper.loadSourceAssemblies` is `config.ReferencedAssemblies |> Array.toList |> List.choose
  tryLoadFrom` (`SatelliteProvider.fs:21-22`), invoked in the `inherit` clause (`:68`), so it *does*
  `Assembly.LoadFrom` **every** referenced assembly unconditionally at construction, holding a handle
  on each for the process lifetime. But most of those (FSharp.Core, framework refs) are read-only files
  nobody rewrites, so their locks are inert; the one lock with consequences is on the *regenerated*
  satellite — i.e. the blast radius amplifies objection 3 rather than adding a separate concern.
  Separately worth naming (untested here): registering *every* referenced assembly as a provider
  "source assembly" is heavy-handed and could plausibly cause identity/version surprises in a real
  multi-reference or multi-version consumer, a risk this two-reference harness can't surface.
- **KILL did not trigger, and that is a genuine positive worth stating.** The design's headline named
  risk — that `Expr.Call` against a `MethodInfo` obtained from `Assembly.LoadFrom` might be rejected by
  the SDK's IL-emission path the way a compile-time `MethodInfo` isn't — did **not** materialize once
  the `sourceAssemblies` registration made the loaded assembly a real source assembly. The emitted
  consumer IL resolves the forwarded call. That is the one clean, unqualified mechanism win.
- **NULL did not trigger.** Exposing a real external type as a provided member's signature and
  forwarding a real computed value is materially more than Q008/Q09/Q11's attribute-reflection markers;
  the "nothing beyond what Q008/Q09 showed" outcome does not hold.
- **Memoization discipline carried over from Q011 correctly.** `successCache`/`watchers`
  (`:77-78,114-126`) memoize against the repeated `DefineStaticParameters` firing Q011 found
  lineage-wide; the watcher's `onChange` clears `successCache` before `Invalidate()`, which is the right
  order. None of this was exercised (nothing changed on disk), so it is correct-by-reading, not
  correct-by-observation.

## Verdict

**REVISE**, matching the pre-registered thresholds read literally: the cheapest falsifier passed (Round
1, per the saved log and the confirmed-faithful source), so KILL is out; the result is materially more
than prior Thread 2 quartets, so NULL is out; SHIP required all three conjuncts and Round 2 failed two
of them (clean regeneration propagation, and no file-lock), landing exactly on the file-lock branch the
REVISE threshold anticipated.

Four things travel with the REVISE, and they matter more than the label:

- **The repair path REVISE normally presumes is unproven here (objection 4).** `Load(bytes)`, the
  threshold's own named workaround, is ruled out by this quartet's own attempts; the collectible-ALC
  alternative is untested and may relocate the tension. Do not cite this as "works with a loading
  discipline."
- **Round 2 breaks the provider's sole differentiator over a `<ProjectReference>` for this case
  (objection 3).** The cross-project satellite-DLL TP earns its keep only via live re-exposure, and live
  re-exposure is exactly what the lock defeats. This should lead any summary, not trail it.
- **Round 1's runtime "two independent ways" is corroborated mainly by source inspection for a
  trivial-echo generator (objection 2).** The mechanism claim holds, but on the strength of
  `Expr.Call(realMi, args)` in the source, not on runtime values that an echo cannot make discriminating.
- **The provider is hand-fit to `create`/`name`/`age`/`email` and generalization is unproven
  (objection 1)** — and, because of the environment outage, this review did not close that gap the way
  it was asked to.

## Follow-ups, prioritized

1. **Run the differently-shaped Round 1 this review couldn't.** Wrap `map` (two `FSharpFunc`-typed
   parameters, record return) and/or add a third field with a *new* name and a matching forwarder, and
   confirm whether the `sourceAssemblies` + `Expr.Call` kernel forwards non-primitive/function-typed
   arguments — or whether it was luck-shaped to the primitive-and-single-record signatures. Highest
   priority: it is the one open question both the executor and this review name, and the pre-registered
   review task specifically required it.
2. **Test the collectible-`AssemblyLoadContext` escape for Round 2, and narrow the load set.** Load
   *only* the satellite path(s) (not every `ReferencedAssembly`) into a collectible ALC, unload it on
   the `FileSystemWatcher` signal before the rebuild, and determine whether that satisfies both the
   source-identity match and the rewritable-file constraint — or whether a live `ProvidedTypesContext`
   holding references defeats collection, as 02-results predicts. This is make-or-break for the
   capability claim and directly tests objection 4's tension.
3. **Shrink `loadSourceAssemblies` from "every referenced assembly" to the satellite(s) only** and
   confirm Round 1 still passes — reduces both the lock blast radius and the untested multi-reference
   identity risk.
4. **Real-IDE host test (standing caveat since Q006).** Whether FSAC/Ionide's own assembly-loading and
   `Invalidate()` handling reproduce both the Round 1 pass and the Round 2 lock, rather than
   `FSharpChecker`-as-library.
5. **Reconcile item 15 itself with objection 3.** Given the lock breaks the sole differentiator over a
   `<ProjectReference>`, decide whether item 15 is worth pursuing past this REVISE at all, or whether
   the honest guidance is "use a `<ProjectReference>` for the cross-project case; the TP adds value only
   if follow-up 2's ALC-unload path works." Either way, stop describing the cross-project TP as a
   settled win.
