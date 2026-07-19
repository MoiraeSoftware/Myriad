# Q026-real-generator-reentrant-composition / Movement 4 — Adversarial review

Independently rebuilt (`Myriad.Core`, `Myriad.Plugins`, `Q026.Bridge`, `ReentrantJsonGenerator`,
`Harness`, all Release) and reran the executor's `Harness` from a clean build. It reproduces
byte-for-byte: Round 1 emits the real `PersonLenses` module; Round 2 reports both callback sentinels
`true`, zero project-wide/`fileB`/`fileC` errors, both symbol-resolution checks confirmed, exit 0.
Stress-ran it 12 times back-to-back: 12/12 clean, zero failures. The pre-registered result is real
and stable. I then went past reproduction with a second `VariantHarness` (added under `artifacts/`,
same pins) that reuses the *same built* `ReentrantJsonGenerator.dll` and `Myriad.Plugins.dll` via the
same reflection path, but drives them over two record shapes the executor never tried and instruments
the ALC caveat `02-results.md` left untested. Findings below; two are new.

## Four strongest objections

1. **The news is the invocation mechanism, not the second generator — and a shape the executor never
   ran shows the second generator is toy-grade, producing genuinely broken output that only the
   in-process typed check catches.** `ReentrantJsonGenerator`'s typed-inspection core
   (`collectEntities`/`collectMfvs`/`getterReturnTypeName`) is copied verbatim from Q010's already-
   SHIPped stand-in; what is actually new over Q010 is (a) it is a real `IMyriadGenerator` discovered
   by `MyriadGeneratorAttribute` reflection and invoked through the interface (confirmed line-by-line
   in `Harness/Program.fs:101-119` and `ReentrantJsonGenerator.fs:51-131` — `Activator.CreateInstance`
   + `:?> IMyriadGenerator` + `GeneratorContext.Create` + `.Generate`, no hand-inlined logic and no
   direct constructor call), and (b) its emission is data-driven off the discovered bindings rather
   than Q010's hardcoded `synthesizePersonJson`. My **variant A** (a three-field record
   `{ Name; Age; Email }`, mixed types, different order) proves (b) generalizes: with zero code
   changes the generator discovered and emitted a correct three-field serializer, all 3 references
   resolved to the real generated `PersonLenses.fs`, zero errors — so the `DisplayName = "PersonLenses"`
   / binding-discovery approach is **not** brittle to the specific two-field case, which was a real
   open question. But my **variant B** (a field whose type is another record, `{ Name; Age; Home: Address }`)
   shows the emission logic is still a spike toy: the reentrant inspection correctly surfaced the real
   nested `Address` type and all 3 symbols resolved to the real file (the *mechanism* generalizes), yet
   the generator's naive `if ret.EndsWith "string" then "%s" else "%d"` type-sniffing emitted `%d` for
   the `Address` field and produced a `PersonJson.fs` that fails to typecheck — 1 deterministic
   `Error` ("The type 'Address' is not compatible with any of the types byte,int16,...", stable 10/10
   runs of variant B alone). This is not a defect in the composition mechanism; it is evidence for
   scoping the SHIP: what travels is "two real `IMyriadGenerator`s compose via reentrant typed query,"
   not "`ReentrantJsonGenerator` is a working serializer." One incidental point in favour of the whole
   in-process-hosting thesis: the downstream generator's bug surfaced immediately as a real typed
   diagnostic on the generated virtual file, at composition time, in the same check — exactly the
   feedback loop the architecture line is chasing.

2. **NEW: the `Q026Bridge` static side-channel does not survive being reused for a second composition
   in the same process — it races ~50% of the time — which is concrete evidence that it breaks under
   precisely the persistent/multi-composition host condition item 18/22's vision requires.** `02-results.md`'s
   own precondition-5 discussion flags the static-mutable side-channel as a spike-only stand-in in the
   abstract. I made it concrete. Running variant A alone: 10/10 clean. Variant B alone: 10/10 stable
   (deterministic type error from objection 1, never a crash). The executor's single-composition
   `Harness`: 12/12 clean. But running A **and** B in one process, sharing the process-global
   `Q026Bridge.Files` dictionary and reusing the same virtual `fileB`/`fileC` paths across two separate
   `FSharpChecker` instances, crashes ~50% of fresh processes (4 crash / 4 OK in one sweep of 8, then
   confirmed A-alone and B-alone never crash in 10 each) with `KeyNotFoundException` on
   `Q026Bridge.Files.[fileA]` at `ReentrantJsonGenerator.fs:68`. Root cause: the generator, running
   inside `fileC`'s `DocumentSource.Custom` source callback, reads `Files.[fileA]` assuming `fileA`'s
   source callback already fired and cached — but FCS's `DocumentSource` contract makes **no ordering
   guarantee** about when it requests each file's source text (parsing is independent of typecheck
   order and may reorder/parallelize), and the side-channel is a single unkeyed process-global with no
   per-composition isolation. The single-composition path the executor tested happens never to lose
   this race across 12+ runs because there is only one composition and its `fileA` is populated before
   `fileC`'s generator runs in practice — but "in practice, once" is not "guaranteed." The load-bearing
   consequence: any real host that runs more than one composition in a long-lived process (the FSAC-
   sidecar / persistent-`FSharpChecker` host that is the actual deeper ask behind item 18/22) cannot
   use this static side-channel as built. This sharpens `02-results.md`'s honest-but-abstract caveat
   into a demonstrated failure mode, and reinforces the frozen docs' own point that `GeneratorContext`
   would have to actually carry the checker/options handle (or a per-composition scope) for real.

3. **The reentrancy caveat inherited from Q010 (Objection 1: sentinel proves the callback fired, not
   that it fired against genuinely in-flight rather than already-warm state) is real, unresolved, and
   my variants do not touch it.** `02-results.md` names this inheritance explicitly and adds no new
   instrumentation; neither did I — the variant harness reuses the identical mechanism. So the "in
   flight," "mid-check" framing remains, as in Q010, one the measurements cannot distinguish from "the
   reentrant `ParseAndCheckFileInProject(fileB)` lands on content whose source callback already fired
   and whose typecheck is warm." Structurally the outer `ParseAndCheckProject` is on the stack when the
   generator runs (it is inside that call's own callback), so the checker is not literally idle; but
   whether the reentrant query contends with unfinished work or hits warm caches is not shown either
   way. Do not cite Q026 for the strong in-flight-contention claim. A cheap follow-up (timing the first
   reentrant call vs a warm re-call) would settle it, as Q010's review already noted — it was not done
   in either quartet.

4. **"Invoked the same way Myriad's real CLI invokes a generator" is fair for the discovery-and-
   interface path but elides two things beyond the one disclosed simplification.** The disclosed gap
   (plain `Assembly.LoadFrom` instead of `McMaster.NETCore.Plugins.PluginLoader`'s isolated ALC) is
   real, and I now have concrete evidence on both sides of it (see below). Two smaller, undisclosed
   deviations from `src/Myriad/Program.fs`'s actual `runGenerator` (read directly, lines 226-261): the
   harness does **not** gate on `instance.ValidInputExtensions |> Seq.contains (extension)` before
   calling `.Generate`, and does not take the `:? IMyriadGeneratorWithDiagnostics` branch. Neither
   changes this result (both generators declare `.fs`; neither implements the diagnostics interface),
   so the parity claim holds in substance — but strictly the harness runs a narrowed `runGenerator`,
   not a byte-identical one. Separately I confirmed the central scope claim by reading `Program.fs` in
   full: it is a single `[<EntryPoint>]`, `PluginLoader` + `Activator` + write-files + exit, with **no
   `FSharpChecker` anywhere** and no persistent state — so "this does not show Myriad's real CLI can
   host this today" is accurate, not hedging.

## On the ALC caveat, now with evidence rather than speculation

`02-results.md` correctly said the plain-`Assembly.LoadFrom` approach "cannot surface [the ALC static-
sharing] failure mode one way or the other." I instrumented it. Under the default load context: exactly
**one** `Q026.Bridge` assembly is loaded after `Assembly.LoadFrom` of the generator DLL, and
`obj.ReferenceEquals(genBridgeAsm, harnessBridgeAsm) = true` — the generator's `Q026.Bridge` dependency
resolves *by simple name* to the copy the harness already loaded, which is exactly **why** the static
side-channel works here. That also confirms the caveat's other half: this sharing is a property of
default-context by-name resolution, which an isolated per-plugin ALC (`Program.fs` sets
`config.PreferSharedTypes <- true`, which governs which types unify across the boundary) would **not**
automatically guarantee. So the caveat is not just "untested" — it is "works here for a concrete,
identified reason that an isolated ALC would remove." This overlaps `BACKLOG.md` item 2 and remains its
territory, but it is no longer a black box.

## Verdict

**SHIP, scoped** — consistent with the pre-registration, which required both rounds to pass at Q010's
evidentiary bar (sentinel-confirmed non-bypass, zero `Error` diagnostics on the two generated files,
two independent symbol-resolution checks). They did, and independently reproduced 12/12. None of
KILL/REVISE's pre-registered triggers fired: Round 1 did not throw, Round 2's failure modes
(assembly-identity mismatch; real output shape breaking the copied inspection code) did not occur.
The genuinely new thing over Q010 is the real invocation mechanism — a real, reflection-discovered,
interface-invoked second `IMyriadGenerator` reentrant-querying a real, Fantomas-formatted, real-
`LensesGenerator`-produced file — and my review strengthens rather than undercuts it: the mechanism
generalizes cleanly to a shape (three fields, mixed types, reordered) the executor never ran.

Scope precisely, because five things travel with the ship and two are sharper than `02-results.md`
left them:

- **What is proven is the composition/invocation mechanism, not that `ReentrantJsonGenerator` is a
  real serializer.** Variant B: the reentrant typed inspection correctly surfaces a real nested-record
  field type and all symbols resolve, but the generator's toy `%s`/`%d` sniffing then emits code that
  does not typecheck. The typed-inspection logic itself is Q010's, unchanged; do not read this quartet
  as proving new generation logic.
- **The static `Q026Bridge` side-channel is demonstrably not reusable for more than one composition in
  a process (objection 2).** It is a spike-only stand-in that races (~50% crash) under exactly the
  persistent/multi-composition condition the deeper item 18/22 vision needs. A real integration must
  widen `GeneratorContext` (or add a per-composition scope) to carry the checker/options handle; the
  global mutable cannot be that.
- **"Invoked the same way the real CLI does" holds for discovery + `Activator` + interface + context,
  with the disclosed `Assembly.LoadFrom`-vs-`PluginLoader` gap now concretely characterized** (single
  shared `Q026.Bridge` by-name resolution is why it works; an isolated ALC would not guarantee it), plus
  two immaterial narrowings (no `ValidInputExtensions` gate, no diagnostics branch).
- **The in-flight-vs-warm reentrancy question is inherited from Q010 unresolved (objection 3).** Do not
  cite Q026 for genuine mid-flight contention.
- **This is one `ParseAndCheckProject` call, once, in a standalone process** — not Myriad's real CLI
  (which has no `FSharpChecker`, verified), not a watcher, not an FSAC session. The composition works
  *hosted*; turning Myriad into that host is the unattempted architecture change, as the frozen docs
  already say.

No factual error was found in the frozen `00`/`01`/`02` docs; their self-flagged caveats are accurate,
and my new evidence makes two of them concrete rather than contradicting them.

## Follow-ups, if the frontier keeps moving (for `BACKLOG.md`)

1. Replace the process-global static `Q026Bridge` with a per-composition scope (widen `GeneratorContext`
   with an optional checker/options handle, mirroring `IMyriadGeneratorWithDiagnostics`'s opt-in
   pattern) and re-run the two-composition-in-one-process case that crashes today — the minimum change
   before any persistent host could compose more than one generation.
2. Give `ReentrantJsonGenerator` (or its successor) real type-directed emission so variant B typechecks,
   or, better, treat variant B as the standing test that a *real* composed generator must handle nested
   and non-primitive field types, not just two primitives.
3. Settle objection 3 with the cheap timing instrumentation Q010's own review already named (first
   reentrant call latency vs warm re-call), before any write-up leans on "in flight" language.
4. If ALC isolation is ever reintroduced (item 2), re-test the side-channel under a real isolated
   `PluginLoader` with `PreferSharedTypes` — my evidence predicts the by-name sharing that makes it
   work today would no longer be guaranteed.
