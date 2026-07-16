# Q016-satellite-dll-type-provider / Movement 3 — Execute + write up

**Status:** DONE. Both rounds built and run to completion, 2026-07-16. Round 1 (cheapest falsifier)
**PASS**. Round 2 (regeneration) **FAIL**, specifically on the named Windows file-lock risk, exactly the
outcome the design's own REVISE threshold anticipated as a real possibility, not a surprise found only
after the fact. Final SHIP/REVISE/NULL/KILL verdict is deferred to `03-review.md`; this file records
what actually happened, including four real dead ends before the mechanism worked, reported in full
rather than smoothed over.

**Environment:** .NET SDK 9.0.310 on Windows 11. `FSharp.Compiler.Service` `43.9.101` (pinned).
`ProvidedTypes.fs`/`.fsi` copied forward from `Q012-compiler-behavior-probe/artifacts/vendor/` (same
pinned `FSharp.TypeProviders.SDK` commit `0a95768a2247daba80b24a2604f77f89fc88ff1f` every Thread 2
quartet uses). `src/Myriad` and `src/Myriad.Plugins` built once from this repo's own source, `dotnet
build -c Release`, no modifications to either. Scratch build under
`$CLAUDE_JOB_DIR/tmp/q016-satellite-dll-spike/`; durable source and run logs copied to
`experiments/Q016-satellite-dll-type-provider/artifacts/`.

---

## Read this first: the mechanism took four real attempts, not one — reported in full

The design's own "Deviations expected" section flagged two specific risks going in (whether `Expr.Call`
against a dynamically-loaded `MethodInfo` would be accepted by the SDK's IL-emission path, and whether
`FileSystemWatcher` timing would be meaningful). What actually went wrong was different and more
fundamental — getting a generative provider to expose a **real type from a dynamically-loaded external
assembly** as a provided member's parameter/return type, at all, hit three dead ends before the fourth
one worked. None of Q006/Q008/Q009/Q011/Q012 needed this: their providers all use `typeof<string>`/
`typeof<bool>`/`typeof<int>` for provided-member types and pure literal-quotation `invokeCode`
(`<@@ value @@>`) — never a real external type, never a real `Expr.Call` into an externally-loaded
method. This quartet is the first in the lineage to attempt it, which is exactly why `BACKLOG.md`'s own
Q009 follow-up (4) named "real per-field data accessors, not verification-only markers" as still open.

**Attempt 1 — bare `Assembly.Load(bytes)`, used only for the provider's own internal reflection.**
Rejected: `The design-time type 'SampleNs.Person' utilized by a type provider was not found in the
target reference assembly set`. Fix: the consumer must reference the satellite DLL directly
(`-r:Satellite.dll`), not only the provider's own runtime assembly — this actually matches the real
cross-project shape (project B would reference project A's compiled output too), so this "fix" is also
a correction to the design, not a workaround.

**Attempt 2 — `RegisterGeneratedTargetAssembly`** (`ProvidedTypes.fs:16372`, "Register that a given file
is a provided generated target assembly, e.g. an assembly produced by an external code generation
tool" — reads as exactly the intended mechanism for this shape). Fixed the reference-set error, but
every `ProvidedParameter`/`ProvidedMethod` built from its types (a `TargetTypeDefinition` wrapper,
confirmed by printing `.GetType().FullName`) was then rejected by the SDK's own invokeCode machinery:
`Type mismatch when building 'args' ... Expected 'tgt type System.String', but received type
'System.String'`. Declaring the `ProvidedParameter` with a converted target type
(`ConvertSourceTypeToTarget`) did not fix it. Applying `ConvertSourceExprToTarget` to the invokeCode
`args` themselves (attempt 2b) produced a different failure, `unknown parameter/field`, at IL-emission
time — tracing into `ProvidedTypes.fs` showed this replaces the SDK's own tracked argument `Var`s with
fresh, untracked ones. **Conclusion, confirmed by reading the SDK's own `convCodeToTgt` (`ProvidedTypes.fs:9559`):
invokeCode is meant to be authored entirely in *source* terms; the whole returned expression is
converted to target form automatically, once, by the SDK afterward — not assembled from target-side
pieces directly.**

**Attempt 3 — plain source `Assembly.Load(bytes)`, no target registration at all.** Failed identically
to attempt 2 (`not found in design-time assembly set`), which is what revealed the real mechanism: FCS
itself, independent of anything this provider's code does, auto-registers every `config
.ReferencedAssemblies` entry as a **target** assembly the moment a consumer references it (here, via
`-r:Satellite.dll`). So a genuine **source**-side counterpart was required regardless of what
`loadSatellite` itself did — and manually loading bytes a second time never produces one, because the
SDK's source-assembly table is populated only from the constructor's own `sourceAssemblies` parameter
and its *static* transitive assembly-reference closure, never from ad-hoc runtime `Assembly.Load` calls.

**Attempt 4 — the one that works.** `TypeProviderForNamespaces`'s `sourceAssemblies` constructor
parameter (`ProvidedTypes.fsi:445`) is evaluated once, at provider construction, and
`TypeProviderConfig.ReferencedAssemblies` (every path the *consumer* passes via `-r:`) is already
available at that same moment — before any static parameter is ever applied. Loading every one of those
paths via `Assembly.LoadFrom` and passing them as `sourceAssemblies` means Satellite.dll, once a
consumer references it, is a real, correctly-identity-matched source assembly by the time
`DefineStaticParameters`'s instantiation function runs. `loadSatellite` itself then also uses
`Assembly.LoadFrom` (not `Load(bytes)`) so it returns the *identical, runtime-cached* `Assembly` object
the constructor already loaded for that path, not a second unregistered instance. Every
`ProvidedParameter`/`ProvidedMethod`/`Expr.Call` in the file then uses ordinary source-side reflection
objects (`createMi.GetParameters()`, `nameMi.ReturnType`, etc. — plain `RuntimeType`, confirmed by the
same `GetType().FullName` check), and the SDK's own automatic source-to-target conversion handles the
rest. Full history kept as comments in `artifacts/SatelliteTP.DesignTime/SatelliteProvider.fs`.

**A fifth, much smaller issue along the way:** `[ for p in config.ReferencedAssemblies do try yield
Assembly.LoadFrom p with _ -> () ]` — a `try/with` directly inside a list comprehension — failed to
compile (`FS0193: ... did not contain the val 'ValLinkagePartialKey(EnumerateTryWith)'`) at the pinned
`FSharp.Core 4.7.2` for this design-time project. Fixed by extracting a plain `tryLoadFrom : string ->
Assembly option` helper function and using `List.choose`. Unrelated to type-provider mechanics, just a
real compatibility snag between a newer F# language construct and an old pinned `FSharp.Core`.

---

## Round 1 — cheapest falsifier

`SatelliteTP.Provided.MyriadSatellite<Satellite.dll path, "SampleFields.Person", "SampleNs.Person">`,
instantiated in a `checker.Compile`'d consumer (`module Consumer1 ... let person = P.Create("Ada",
42) ... let readName : string = P.GetName(person) ... let readAge : int = P.GetAge(person)`), compiled
clean:

```
--- Round 1 compile ---
  compile: 1359-1415ms exitCode=0 errors=0 outExists=true
  readback via compiled consumer: name="Ada" age=42
  direct reflection on Satellite.dll (outside provider): name="Ada" age=42
  agreement: name true, age true
ROUND 1 verdict: PASS
```

Both pre-registered independent checks agree: the value read back from the harness's *compiled
consumer output* (via reflection on the DLL `checker.Compile` produced) matches a call made *directly*
against `Satellite.dll` by the harness, entirely outside the provider. Since `P.Create`/`P.GetName`/
`P.GetAge`'s `invokeCode` is `Expr.Call(realMethodInfo, args)` — a direct IL `call` instruction to
Myriad's own compiled `create`/`name`/`age` functions, not a hand-written reimplementation — this
confirms the mechanism claim: **a generative provider can carry Myriad's real generated behavior**, not
a synthetic stand-in, reflection-forwarded into the consumer's own compiled IL. `Generated.fs` was
inspected and confirmed to carry Myriad's own real header comment (`//   This code was generated by
myriad.`), satisfying the validity precondition that the payload be real, unmodified Myriad output.

Reproduced across three separate runs (initial pass, a rerun after the Round 2 detour, a final
clean run captured to `artifacts/run-logs/round1-run.txt`) — same result each time.

---

## Round 2 — regeneration, `Invalidate()`, and the named Windows file-lock risk

```
-- step 1: cold check, v1 satellite (2 fields) already loaded from Round 1 in this process --
  v1 sanity compile errors=0
-- step 2: REAL EDIT to Person.fs (add email field), rerun Myriad's real CLI unmodified --
  Myriad CLI rerun: exit=0 in ~410-420ms
  Generated.fs now contains an email getter: true
-- step 3: recompile satellite to the SAME path (overwrite), while this process still holds v1 loaded --
  satellite rebuild subprocess: exit=1
  Error: A problem occurred writing the binary 'Satellite.dll': Could not open file for writing
         (binary mode): Satellite.dll
  file-lock exception observed: true
-- step 5: SAME checker instance, check a NEW consumer referencing GetEmail --
  compile: 241ms exitCode=1 errors=2
    Error: The member or object constructor 'Create' takes 2 argument(s) but is here given 3.
    Error: The type 'P' does not define the field, constructor or member 'GetEmail'.
ROUND 2 verdict: FAIL (email-getter-generated=true no-lock-exception=false post-regen-resolves=false)
```

**Step 2 succeeded exactly as designed:** editing `Person.fs` to add an `email: string` field and
rerunning Myriad's real, unmodified CLI produced a real `Generated.fs` with a genuine `email` getter —
confirming Myriad's own generator correctly regenerates from a changed source, independent of anything
provider-specific.

**Step 3 is where the named risk materialized, not a hypothetical.** The satellite rebuild subprocess
(a genuinely separate `dotnet SatelliteBuilder.dll` process, mirroring a real second `dotnet build`)
failed to write `Satellite.dll` because the Harness process still held it open — **directly caused by
Attempt 4's own fix**: `Assembly.LoadFrom`, required for the source/target identity match Round 1
needed, holds a file handle for the process's lifetime, exactly the behavior Q011 first documented and
exactly the risk `BACKLOG.md` item 15 named as real and untested ("the satellite DLL will be rewritten
by Myriad while a live host may still hold it loaded"). Because the file was never actually overwritten,
step 5's failure (2 errors, `Create` still expects 2 args, `GetEmail` doesn't exist) is not a separate
regression — it's the correct, honest downstream consequence of step 3 never having produced a v2
DLL. The `FileSystemWatcher`-driven `Invalidate()` mechanism itself was never actually exercised, since
nothing ever changed on disk for it to observe.

**This is a genuine structural tension, not a bug to patch in five minutes:** the same `Assembly
.LoadFrom`-based source-assembly registration that makes Round 1's real-type reflection-forwarding work
at all is the same mechanism that reintroduces Q011's file-lock problem. Q011 avoided the lock by using
`Assembly.Load(bytes)` — but attempts 1 and 3 above already showed `Load(bytes)` cannot satisfy the
SDK's source/target identity-matching requirement for this quartet's stronger claim (real external types
as provided-member signatures, not just attribute-value reflection). Untested within this quartet:
whether loading into a collectible `System.Runtime.Loader.AssemblyLoadContext` and unloading it before
the rebuild would resolve both constraints at once — a concrete, named next step, not attempted here.

---

## Round 3 — cost (single-sample, same convention as prior quartets)

```
Myriad CLI rerun (real subprocess, dotnet startup included):     ~410-420ms
Satellite compile (checker.Compile, Person.fs + Generated.fs):   ~1.35-1.40s  (includes dotnet startup)
Consumer compile (provider resolution + typecheck, cold):        ~1.35-1.42s
Consumer compile (same checker, subsequent scenario):              ~240-250ms
```

Not directly comparable to Q006's 47ms/1137ms or Q008's 19-32ms/1161ms bands: this pipeline does
genuinely more work (a real external CLI subprocess plus a real `checker.Compile` of two real source
files, not a single in-process type-check), and every number here is a single sample from one machine,
per this file's own cross-cutting caveat — no repeated-trial distribution was collected.

---

## What was and wasn't proven, stated plainly

- **Proven (mechanism claim, Round 1):** a generative type provider can `Assembly.LoadFrom` a satellite
  DLL that Myriad's real, unmodified CLI produced and compiled (via `checker.Compile`, mirroring a real
  project's `<Compile>` list: source file then its Myriad-generated sibling), and construct provided
  members whose `invokeCode` is a direct `Expr.Call` into Myriad's own real compiled methods — verified
  two independent ways, not visual inspection. This is the first quartet in the Thread 2 lineage to
  expose a **real external type** as a provided member's signature and forward a **real computed
  value** (not a literal marker) through it.
- **Not proven / boundaries the review should weigh directly:**
  - **Only one generator (`Fields`) and one shape (two-field record, three static forwarded methods)
    was tested.** Whether the mechanism generalizes to `Lenses` or `DUCasesGenerator`'s different output
    shapes (e.g. lens getter/setter pairs, DU pattern matches) is untested. The claim is real for this
    one generator, not yet shown to generalize past it.
  - **Round 2 is a clean, reproducible FAIL, not a REVISE-and-patch.** The regeneration/`Invalidate()`
    capability claim did not ship. The mechanism that makes Round 1 work (`Assembly.LoadFrom`-based
    source registration) structurally conflicts with the file-overwrite Round 2 needs — this is not a
    small bug, it's a real tension between two requirements this SDK's public API doesn't obviously let
    you satisfy simultaneously.
  - **`sourceAssemblies = LoadHelper.loadSourceAssemblies config` `Assembly.LoadFrom`s *every* path in
    `config.ReferencedAssemblies`, unconditionally, at construction time** — not just Satellite.dll.
    For any real consumer project, this means every one of that project's own referenced assemblies
    gets file-locked by this provider for the process's lifetime, not only the one satellite DLL Round 2
    happened to test. This is a real, broader blast-radius concern the design did not anticipate and
    this results file did not measure (how many assemblies, what the aggregate lock footprint looks
    like on a real multi-reference project) — named here for the review to weigh, not quantified.
  - As every prior Thread 2 quartet: this ran against `FSharpChecker`/`checker.Compile` as a library,
    never a real Ionide/VS/Rider session. Whether a live IDE host's own assembly-loading/locking behavior
    differs from this harness's is untested.
  - The four-attempt mechanism discovery (documented above and in the provider source's own comments)
    depended on reading `ProvidedTypes.fs`'s internals directly (the `ProvidedTypesContext`
    source/target table architecture, `convCodeToTgt`, `RegisterGeneratedTargetAssembly`'s
    implementation) — this is real, working code, but it is not a documented, stable public contract of
    the SDK; a future `FSharp.TypeProviders.SDK` version could change these internals without notice.

## What a review should press on, beyond what's flagged above

1. **Is the Round 1 mechanism actually robust, or narrowly hand-fit to this one shape?** The four-attempt
   history above is honest about how much trial-and-error went into finding a working incantation. A
   review should try at least one variant (e.g. a method with more than one non-primitive argument, or a
   provided member returning a *list* of records) to see whether the `sourceAssemblies`-registration
   trick generalizes or was luck-shaped to `create`/`name`/`age`'s exact signature shapes.
2. **Independently confirm the file-lock finding isn't an artifact of this harness's own process
   structure.** The Harness process never exits between Round 1 and Round 2's rebuild attempt — is that
   realistic (a live IDE/design-time-build host genuinely stays alive across edits, so this is arguably
   the *right* test), or does it overstate the risk relative to how Myriad's actual MSBuild pre-build
   step would invoke things (a fresh CLI process per build, per `DEVNOTES.md`)?
3. **Weigh whether Round 1 PASS + Round 2 FAIL nets out to REVISE or something more specific** — per this
   quartet's own pre-registered thresholds, a REVISE requires "a workaround that narrows the claim." Is
   "load into a collectible `AssemblyLoadContext`, unload before rebuild" (named above, not attempted) a
   real, checkable next step, or does it just relocate the same tension (unloading a collectible ALC
   that a live `ProvidedTypesContext` may still hold references into could hit its own problems, per the
   general "AssemblyLoadContext identity mismatch" risk `BACKLOG.md` already names for a related idea)?
