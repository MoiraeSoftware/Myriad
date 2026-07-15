# Q012-compiler-behavior-probe / Movement 3 — Execute + write up

**Status:** DONE. Rounds 1 and 2 built and run; the design's "all three shapes fail" branch was hit,
so its mandated positive control was run; Round 3 (confirm against Q011's real artifacts) was then
reached and run to completion, 2026-07-15. KILL condition **not** triggered (invocation counts stable
across repeated runs, no crash/hang). Final SHIP/REVISE/NULL verdict is deferred to `03-review.md`;
this file records what actually ran, including two design-vs-reality corrections found by running.

**Environment:** .NET SDK 9.0.310 on Windows 11 (net8.0 harness; provider TFMs `netstandard2.0;net8.0`;
DummyAsm `netstandard2.0`). `FSharp.Compiler.Service` `43.9.101` (pinned). `ProvidedTypes.fs`/`.fsi`
vendored by file copy from `FSharp.TypeProviders.SDK` commit
`0a95768a2247daba80b24a2604f77f89fc88ff1f` into `artifacts/vendor/` (single shared copy, referenced by
every design-time project). Provider projects pin `FSharp.Core` `4.7.2`; the harness resolves
`FSharp.Core` `10.1.302` transitively via FCS (benign `NU1608`, same as Q011).

**Scratch layout:** `C:\Users\Dave\tp-compiler-behavior-probe-spike\`. Everything durable is copied to
`experiments/Q012-compiler-behavior-probe/artifacts/` — every provider pair, DummyAsm, the harness,
the vendored `ProvidedTypes`, and `run-logs/` holding the captured output this write-up cites plus the
first-invocation stack trace. This quartet exists partly to demonstrate the credibility-gap fix, so
the artifacts are complete and re-runnable, not just described.

---

## Read this first: two corrections found by running, not assumed

### Correction 1 — the design's premise that shape (a) SUCCEEDS under `ParseAndCheckFileInProject` was wrong; it fails too

`01-design.md` (Round 1) states shape (a) "is expected to *succeed* via this API, giving a clean
success-path baseline to compare shapes (b)/(c) against." It does not. `ProbeSimple` — one static
parameter, no I/O, the minimal Q008/Q009-`ClientTP`-shaped generative provider — **fails** under
`ParseAndCheckFileInProject` with the identical "couldn't find type ... in assembly `tmpXXXXXX`"
diagnostic Q011 documented. This collapsed the isolation matrix to "all three shapes fail," which the
design explicitly pre-registered as the branch requiring an immediate positive control before
concluding (Round 2 text: "If all three fail ... warranting an immediate sanity re-check against a
provider already known to work"). That control was run (see Round 2b), and it is what actually
isolated the factor. Reported here rather than smoothed over.

### Correction 2 — the provided-member readback channel can only report the invocation count of a *successful compile* (=1), not the 2 firings the failing API produces

The design's load-bearing novelty over Q011's `MiniTP` was exposing invocation count via a **provided
member read back from compiled output**, not file logging. That channel works — `Tag`,
`InvocationCount`, `CallLog` all read back correctly from the compiled `checker.Compile` output (see
Round 1). But it can only ever report state captured during a *successful* compile, and generative
types only compile successfully under `checker.Compile`, which fires the instantiation function
**once**. So the provided member reads `InvocationCount = 1`. The "more than one invocation" finding
comes from the **file-log channel** (Q011's `MiniTP` technique), which records **2** firings during
the *failing* `ParseAndCheckFileInProject` call — precisely the path where no compiled type ever
exists to carry a provided member. Both channels were therefore needed; neither alone tells the whole
story. This is a real ceiling of the "expose via provided members" idea, stated plainly.

---

## Round 1 — falsifier: does the probe mechanism itself work?

`ProbeSimple.Provided.Probe<Tag: string>`, generative, memoized (Q011 correction 1), instrumented to
(1) increment a per-instance counter, (2) capture `StackTrace(true)` + first `FSharp.Compiler.*`
frame, (3) append a file-log line, (4) expose `InvocationCount`/`CallLog`/`Tag` as provided members.

**Checked `Probe<"A">` via `checker.ParseAndCheckFileInProject`** (the API under test), then separately
via `checker.Compile`, reading the provided members back from the compiled output in-process by
reflection. Raw result (`run-logs/run-all.txt`):

```
[ParseAndCheckFileInProject] 1276ms  aborted=false
    ParseAndCheck: 4 error diagnostic(s)
      DIAG: The type provider 'ProbeSimpleImplementation.ProbeProvider' reported an error: couldn't
            find type 'ProbeSimple.Provided.T' in assembly 'tmpi1caqf, Version=0.0.0.0, ...'
    design-time invocations during ParseAndCheck (file log): 2
      LOG: 1|13:15:49.047|4|createType|FSharp.Compiler.TypeProviders+ProvidedType.ApplyStaticArguments
      LOG: 2|13:15:49.263|4|createType|FSharp.Compiler.TypeProviders+ProvidedType.ApplyStaticArguments
[checker.Compile] 652ms  exitCode=0  outDllExists=true
    Compile: 0 error diagnostic(s) (CLEAN/RESOLVED)
    design-time invocations during Compile (file log): 1
    READBACK from compiled output: Tag="A" InvocationCount=1
    READBACK CallLog:
      1|13:15:49.749|4|createType|FSharp.Compiler.TypeProviders+ProvidedType.ApplyStaticArguments
```

**Falsifier verdict: PASS (both halves).**
1. **More than one invocation, confirmed via the instrument:** 2 firings, 216ms apart, for one logical
   check under `ParseAndCheckFileInProject`. Replicates Q011's `MiniTP` finding through the reusable
   channel. (The provided-member `InvocationCount` reads 1, per correction 2 — the file log is the
   channel that shows the 2.)
2. **The captured stack contains recognizable `FSharp.Compiler.*` frames — not `NONE`.** The first FCS
   frame is `FSharp.Compiler.TypeProviders+ProvidedType.ApplyStaticArguments`. The full captured stack
   (`artifacts/run-logs/ProbeSimple.first-invocation.stack.txt`) shows the entire FCS call chain into
   the provider, e.g.:

   ```
   at FSharp.Compiler.TypeProviders.ProvidedType.ApplyStaticArguments(...) TypeProviders.fs:line 424
   at FSharp.Compiler.TypeProviders.TryApplyProvidedType(...)              TypeProviders.fs:line 1384
   at FSharp.Compiler.Tainted`1.PApplyWithProvider(...)                    tainted.fs:line 132
   at FSharp.Compiler.CheckExpressions.TcProvidedTypeAppToStaticConstantArgs(...) CheckExpressions.fs:line 4954
   at FSharp.Compiler.CheckDeclarations.EstablishTypeDefinitionCores.TcTyconDefnCore_TryAsGenerateDeclaration(...) CheckDeclarations.fs:line 2997
   ```

   So the probe's core value proposition — revealing *where in FCS* the instantiation is driven from,
   not merely *that* it happened more than once — holds. This is a real, in-process window into the
   host's own call pattern, delivered exactly as the mechanism claim proposed. The KILL/REVISE fallback
   (empty/uninformative frames) did not occur.

The `TcTyconDefnCore_TryAsGenerateDeclaration` frame is itself the first clue to the isolation below:
the instantiation is driven from FCS's **type-definition** path (`type T = Probe<...>`), which is the
only legal way to name a generative provided type.

---

## Round 2 — the isolation matrix

All three probe shapes checked under the **same** `checker.ParseAndCheckFileInProject` (the API under
test) and, as a control, the same `checker.Compile`, one `FSharpChecker` instance, same process
(`Harness -- matrix`). Consumer text per shape: `type T = <ProviderType><static args>; let _t = T()`.

| shape | static params | inst. I/O | **ParseAndCheckFileInProject** | checker.Compile | PC firings |
|---|---|---|---|---|---|
| ProbeSimple      | 1 | no  | **FAIL** — couldn't find type in `tmpXXXXXX` | exit 0, 0 err | 2 |
| ProbeTwoParams   | 2 | no  | **FAIL** — couldn't find type in `tmpXXXXXX` | exit 0, 0 err | 2 |
| ProbeTwoParamsIO | 2 | yes | **FAIL** — couldn't find type in `tmpXXXXXX` | exit 0, 0 err | 2 |

Every shape fails identically under the API under test and succeeds identically under `checker.Compile`.
The failure diagnostic is byte-for-byte the same shape as Q011's regression, naming a self-consistent
memoized temp assembly (`tmpbu14a1`, `tmpnf5lnu`, etc.) in each case. Determinism confirmed by running
the whole matrix twice: 2 PC firings / 1 Compile firing / same success-failure split every shape, both
runs (`run-logs`, and the two-run determinism check cited below) — KILL not triggered.

**Direct mapping to the pre-registered thresholds:** this is the design's own NULL pattern for the
*named* factors — "all of them fail identically regardless of shape ... which would mean Q011's
regression isn't explained by parameter count or instantiation-time I/O at all, and must lie
elsewhere." **Neither static-parameter count nor instantiation-time I/O is the isolating factor.**
Both were falsified as candidates in one run: the simplest possible shape (a) already fails.

Because all three failed, the design's Round 2 text mandates a positive control before concluding.
That is Round 2b, and it found the "elsewhere."

---

## Round 2b — the mandated positive control: it is *generative vs. erased*, not a broken API

The design names "a provider already known to work" as the control. Q009's `ClientTP` has no saved
artifact to rebuild, so a stronger, buildable control was used: an **erasing** provided type
(`isErased = true`) with the *identical* single static parameter, *identical* instrumentation, and
*identical* `type T = ...<"A">` usage, added to the `ProbeSimple` provider as `ProbeErased`. If the
erased type resolves under `ParseAndCheckFileInProject` where the generative one does not, the axis is
generative-vs-erased, and the API is not broken for providers in general. Result (`run-sharpen.txt`):

| script (all under `ParseAndCheckFileInProject`) | outcome |
|---|---|
| `type T = Probe<"A">` (generative, definition form)          | **FAIL** — couldn't find type in `tmpXXXXXX` |
| `let _t = Probe<"A">()` (generative, expression form)         | **FAIL** — *"A direct reference to the generated type 'Probe' is not permitted. Instead, use a type definition"* |
| `type T = ProbeErased<"A">` (erased, definition form)         | **RESOLVED — 0 errors** |
| `let _t = ProbeErased<"A">()` (erased, expression form)       | FAIL — benign unrelated cause ("No constructors are available"; erased+static-args needs the alias too — a test artifact, not evidence) |

**This is the isolation.** The erased provided type, checked the identical way, **resolves cleanly**
under `ParseAndCheckFileInProject`. The generative one does not. So:

- `ParseAndCheckFileInProject` is **not** broken for type providers in this environment — it resolves
  erased providers with zero diagnostics.
- It specifically **fails to materialize generative (`isErased = false`) provided types** to a form
  name resolution can bind. The provider builds the type into a `ProvidedAssembly` (temp DLL named in
  the diagnostic), but the diagnostics-only checking path never compiles/loads that assembly's types,
  so the subsequent lookup of `ProbeSimple.Provided.T` in `tmpXXXXXX` finds nothing. `checker.Compile`
  performs the real backend emit and the type exists.
- The generative expression-form row is a bonus finding: a generative provided type **cannot** be used
  in expression position at all (FCS rejects it outright and demands `type Alias = <path>`). The only
  legal usage is the definition form — the exact form that fails under the diagnostics-only API. There
  is no generative usage shape that resolves under `ParseAndCheckFileInProject` here.

This positively characterizes Q011 correction 2's open question: Q011's `SchemaProvider` is
generative, so it was never going to resolve under `ParseAndCheckFileInProject`, independent of its two
parameters or its file I/O. The fast incremental checking API cannot see any generative provided type
in this environment; only a full compile emits it.

---

## Round 3 — confirm against Q011's REAL providers

Q011's actual `SchemaTP.DesignTime/SchemaProvider.fs` and `ClientTP.DesignTime/ClientProvider.fs` were
copied verbatim from `Q011-consumer-driven-contracts/artifacts/`, with only a small `Q012Instr`
file-logging module and one `Q012Instr.log "createType"` call added (the same instrumentation as the
probes). No enforcement logic was rewritten. Both built clean and were checked under both APIs
(`Harness -- round3`, output in `run-logs/run-round3.txt`):

| real provider | consumer | **ParseAndCheckFileInProject** | checker.Compile | PC firings |
|---|---|---|---|---|
| `SchemaTP.Provided.Schema<"Name:v1;Age:v2", "">` | empty known-clients (guaranteed success path, Q011 step 1 shape) | **FAIL** — couldn't find type `SchemaTP.Provided.S` in `tmpynp0rz` | 621ms, exit 0, 0 err | 2 |
| `ClientTP.Provided.Client<schemaCarrier.dll, "Name;Age">` | reads `FieldProvenanceAttribute` from a schema carrier built above | **FAIL** — couldn't find type `ClientTP.Provided.Dep` in `tmp54zyif` | 214ms, exit 0, 0 err | 2 |

Both real Q011 providers behave **identically to the minimal probes**: fail under
`ParseAndCheckFileInProject` with the exact "couldn't find type ... in assembly `tmpXXXXXX`" diagnostic
and 2 firings; succeed under `checker.Compile` with 0 diagnostics and 1 firing. The `ClientTP` Compile
even ran the full reversed-arrow mechanism end to end — it read the schema carrier's provenance
attributes and generated `Name`/`Age` — confirming the failure is purely the diagnostics-only API's
non-materialization of the generative type, nothing in the provider's own logic. This upgrades the
finding from "isolated in a toy analog" to "confirmed against the actual artifacts Q011's mystery
originated in."

---

## Timing summary (single-sample, same convention as prior quartets)

```
                                     ParseAndCheckFileInProject   checker.Compile
ProbeSimple      (cold, first check)          1276 ms                652 ms
ProbeTwoParams                                 235 ms                166 ms
ProbeTwoParamsIO                               247 ms                204 ms
SchemaTP  (real, cold in round3 process)      (fail path)            621 ms
ClientTP  (real)                              (fail path)            214 ms
```

The `ParseAndCheckFileInProject` numbers are wall-time for a call that **fails** to resolve the type
(it still does full parse+check work and fires the instantiation twice), not a meaningful "resolution
cost." The `checker.Compile` numbers are full-compile times, the same measurement Q011 reported
(106–201ms warm there; comparable band here). Neither is Q008/Q009's claimed 15–57ms incremental
band — see below.

Determinism (KILL check): matrix run twice back-to-back, PC firings = 2 and Compile firings = 1 for
every shape both times, timings within ~10ms warm; no non-determinism, no crash, no hang.

---

## What was and wasn't proven, stated plainly

- **Proven (mechanism claim, Round 1):** a generative provider can instrument its own instantiation and
  expose invocation count / call log as real provided members read back from compiled output (`Tag`,
  `InvocationCount`, `CallLog` all round-tripped). The captured `StackTrace` contains recognizable
  `FSharp.Compiler.*` frames revealing the FCS call chain (`ApplyStaticArguments` ←
  `TryApplyProvidedType` ← `TcProvidedTypeAppToStaticConstantArgs` ←
  `TcTyconDefnCore_TryAsGenerateDeclaration`), so the probe is a genuine window into *where in FCS*, not
  just *that*. Two boundaries surfaced honestly: the provided-member channel can only report a
  successful-compile snapshot (count 1), and the "more than one invocation" evidence therefore comes
  from the file-log channel (count 2 on the failing API).
- **Proven (capability claim, Rounds 2/2b/3), but on a different axis than pre-registered:** the factor
  that governs whether a generative provided type resolves under `ParseAndCheckFileInProject` is
  **generative-vs-erased**, not static-parameter count and not instantiation-time I/O — both of the
  latter were falsified as candidates (the simplest shape already fails; adding a param or I/O changes
  nothing). An erased provided type, checked identically, resolves with zero diagnostics; every
  generative shape fails. Confirmed against both of Q011's real providers, not only the toy analogs.
  This gives Q011 correction 2 a named, reproduced root cause: the diagnostics-only checking API does
  not emit/load a generative provider's `ProvidedAssembly`, so the generated type is never found; only
  a full `checker.Compile` does.
- **Not proven / boundaries for the review:**
  - Strict reading of the pre-registered thresholds: the SHIP condition was worded around a factor that
    fails "on (b) and/or (c) but not (a)"; the actual result is all-of-(a)(b)(c)-fail, which is the
    pre-registered **NULL** pattern for the *named* factors. The isolation that *did* succeed
    (generative-vs-erased) was not among the pre-enumerated candidates — it came from the design's own
    mandated all-fail positive control. Whether "NULL on the two named factors, but the mystery
    positively resolved by the mandated control and confirmed on the real artifact" reads as SHIP or as
    an enriched NULL is the reviewer's call, not this file's.
  - **Direct contradiction with Q008/Q009's reported numbers, unresolvable here.** Q008/Q009 report
    generative-provider success-path resolution via `ParseAndCheckFileInProject` at 19–57ms. In this
    reproducible, artifact-backed environment, *no* generative provided type resolves via that API. I
    cannot re-run Q008/Q009 to reconcile this — they saved no artifacts (the exact credibility gap this
    quartet was partly created to address). Either their harness resolved something that did not require
    materializing the generative assembly, or their providers/usage differed in a way their prose does
    not capture. Stated as an open contradiction, not guessed away.
  - As every prior Thread-2 quartet: this ran against `FSharpChecker` as a library, not a live
    Ionide/VS/Rider session with FSAC's caching on top. Whether a real IDE host materializes generative
    provided types differently than the bare `ParseAndCheckFileInProject` API is untested.
  - Root cause is characterized at the level of "diagnostics-only checking does not emit the generative
    `ProvidedAssembly`" (evidenced by the erased control resolving and the `tmpXXXXXX` assembly being
    named but empty to name resolution). The precise FCS code path that would need to change to emit it
    on the checking path was not located in FCS source — out of scope for a probe spike.

- **Corrections surfaced, not patched into the frozen files:** two, detailed above — (1) the design's
  expectation that shape (a) succeeds under `ParseAndCheckFileInProject` was wrong (it fails, collapsing
  the matrix to the all-fail branch and triggering the mandated positive control); (2) the
  provided-member readback channel reports only a successful-compile snapshot (count 1), so the
  multi-invocation evidence comes from the file-log channel (count 2), and both channels were needed.

---

## Reproduction

See `artifacts/README.md` for the full build/run sequence. In short, from the scratch dir (vendored
`ProvidedTypes` copied from the pinned commit): build each `*.Runtime` project (each triggers its
`DesignTime` build and colocates the design-time DLL), then
`dotnet run --project Harness -- all` (Round 1 + matrix), `-- sharpen` (Round 2b positive control),
`-- round3` (real Q011 providers). No Myriad `Myriad.Sdk` wiring — independent of Myriad by design,
same as every Thread 2 quartet.
