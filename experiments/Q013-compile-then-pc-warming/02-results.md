# Q013-compile-then-pc-warming / Movement 3 — Execute + write up

**Status:** DONE. Round 1 (the falsifier) built and run, twice, 2026-07-15. Result is a clean,
deterministic **NULL**: a prior `checker.Compile` of a generative scenario does **not** warm a
subsequent `ParseAndCheckFileInProject` (PC) of the same scenario. Per `01-design.md`'s explicit
instruction ("If Round 1 comes back NULL, stop there ... do not force Round 2/3 to happen anyway"),
Rounds 2 and 3 were **not** run. KILL condition not triggered (perfectly deterministic across 3
in-process repeats and a second full process run; no crash, no hang). Final SHIP/REVISE/NULL/KILL
verdict is deferred to `03-review.md`; this file records what actually ran, with two design-vs-reality
corrections found by running.

**Environment:** .NET SDK 9.0.310 on Windows 11 (net8.0 harness; provider TFMs `netstandard2.0;net8.0`).
`FSharp.Compiler.Service` `43.9.101` (pinned). `ProvidedTypes.fs`/`.fsi` vendored by file copy from
`FSharp.TypeProviders.SDK` commit `0a95768a2247daba80b24a2604f77f89fc88ff1f` into `artifacts/vendor/`.
Provider projects pin `FSharp.Core` `4.7.2`; the harness resolves `FSharp.Core` `10.1.302` transitively
via FCS (benign `NU1608`, same as Q011/Q012).

**Scratch layout:** `C:\Users\Dave\q013-warm-spike\` (outside the Myriad repo, so the repo's
`Directory.Build.props`/`global.json` don't leak into the provider builds — mirroring Q012's own
out-of-repo scratch). Everything durable is copied to `experiments/Q013-compile-then-pc-warming/
artifacts/`: the harness, the verbatim-copied provider sources and vendored `ProvidedTypes`, and
`run-logs/` holding the two captured runs this write-up cites.

---

## Read this first: two corrections found by running, not assumed

### Correction 1 — "same scenario ... literally identical ... same project options" is not achievable while also reusing Q012's helpers verbatim; the two are in tension, and I chose verbatim reuse

`00-hypothesis.md` (validity preconditions) requires the PC scenario to be "literally identical: same
virtual file path, same source text, same project options" as what `Compile` ran, **and** requires
constructing it "via the same helper Q012 used ... not a hand-rebuilt approximation." These two
requirements conflict, because Q012's own `compile` and `parseAndCheck` helpers do **not** use the
same project construction as each other:

- Q012's `compile` builds a real `.fs` → library through `checker.Compile` with `fsc.exe`-style args
  (`-o out.dll --target:library -r:<runtime.dll> consumer.fs`).
- Q012's `parseAndCheck` builds a `.fsx` **script**, gets options via `GetProjectOptionsFromScript`,
  and runs `ParseAndCheckFileInProject`.

They are different file paths, different source-file kinds (`.fs` vs `.fsx`), and different project
option objects — by construction, in Q012 already. I resolved the tension the way the design's
"reuse infrastructure (do not rebuild from scratch)" section and its NULL-stop instruction most
strongly point: **reuse Q012's two helpers verbatim in structure**, parameterized only by the
`FSharpChecker` instance and the static-argument tag, and hold the **generative type instantiation**
(`ProbeSimple.Provided.Probe<"TAG">`) identical between the Compile and the PC. So "same scenario"
here means the identical generative provided-type instantiation, checked via the exact two APIs Q012
used, with the ordering (Compile-then-PC) and the shared checker instance as the only things varied
from Q012. This is the apples-to-apples comparison with Q012 that the design's Round-1 text calls for.

What this does **not** test, and a stricter reading of the precondition would: running PC over the
*exact same project object* that `Compile` just built (e.g. a non-script `FSharpProjectOptions` for the
same `.fs`, PC'd in place). That is a different, narrower experiment; it is noted as a boundary, not
silently folded in. Given a NULL result even in the more permissive same-checker-same-instantiation
form, the stricter form is unlikely to flip to positive, but it is genuinely untested.

### Correction 2 — the warm PC names a *fresh, different* temp assembly than the preceding Compile, every time

Not a design error, but a load-bearing observation the design didn't anticipate. On each warm trial,
the PC that runs *immediately after* a successful same-checker `Compile` of the identical
instantiation still fails with `couldn't find type 'ProbeSimple.Provided.T' in assembly 'tmpXXXXXX'`,
and the `tmpXXXXXX` name is **different on every PC call** (`tmp0blrck`, `tmpfhkgpt`, `tmp1zlzjg`) and
different from the cold-control PCs' temp assemblies. This is direct evidence for *why* there is no
warming: the PC path constructs a **fresh `ProvidedAssembly`** on each call (the provider's memoized
`createType` fires 2× per PC, minting a new temp-assembly identity) and reuses nothing the prior
`Compile` produced. The prior `Compile`'s materialized generative type does not live anywhere the
subsequent PC's name resolution looks. Nothing crosses the API boundary.

---

## Round 1 — falsifier: same-checker compile-then-PC on the toy `ProbeSimple` generative shape

Per repeat, a **fresh `FSharpChecker`** instance and **fresh tag strings**:

1. **Cold control** (`cold-r1{a,b,c}`): fresh checker, `ParseAndCheckFileInProject` only, no prior
   `Compile`. Expected to reproduce Q012's cold-fail in this process.
2. **Warm trial** (`warm-r1{a,b,c}`): fresh checker, `checker.Compile` of the instantiation, then —
   on the **same checker instance** — `ParseAndCheckFileInProject` of the identical instantiation.

Raw output: `artifacts/run-logs/run-round1.txt` (and an independent second process run,
`run-round1-rerun.txt`, byte-identical verdict).

| repeat | cold PC (no prior Compile) | warm Compile | warm PC (same checker, after Compile) |
|---|---|---|---|
| r1a | **FAIL** 1516ms — couldn't find type in `tmprzxqyh` | exit 0, 719ms, 1 firing | **FAIL** 310ms — couldn't find type in `tmp0blrck`, 2 firings |
| r1b | **FAIL** 140ms — couldn't find type in `tmpsf5yrl` | exit 0, 280ms, 1 firing | **FAIL** 236ms — couldn't find type in `tmpfhkgpt`, 2 firings |
| r1c | **FAIL** 285ms — couldn't find type in `tmptk4epy` | exit 0, 311ms, 1 firing | **FAIL** 282ms — couldn't find type in `tmp1zlzjg`, 2 firings |

The warm PC diagnostic is byte-for-byte the same shape Q012 documented as the cold-fail:

```
The type provider 'ProbeSimpleImplementation.ProbeProvider' reported an error: couldn't find type
'ProbeSimple.Provided.T' in assembly 'tmp0blrck, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null'
```

**Falsifier verdict: NULL.** Compile-then-PC ordering makes no difference. PC fails identically after a
prior same-instance `Compile` of the identical scenario, with the same "couldn't find type"
diagnostic, the same 2 design-time firings, in all three repeats. The Compile half succeeds every time
(exit 0, 1 firing), confirming the scenario itself is a real, materializable generative type — it is
specifically PC that can't see it, warmed or cold. This is exactly the NULL pattern
`00-hypothesis.md` pre-registered: "PC still fails identically ... after a prior `Compile` of the
identical scenario, same checker instance, repeated 3+ times."

### Determinism (KILL check)

- 3 in-process repeats: identical outcome (cold FAIL / Compile exit 0 / warm PC FAIL), identical firing
  counts (Compile 1, PC 2), identical diagnostic shape.
- A second, independent process run (`run-round1-rerun.txt`): same summary line —
  `cold [false;false;false]`, `warm [false;false;false]`, verdict NULL.

No non-determinism, no crash, no hang. KILL not triggered.

### Incidental corroboration against process-global warming (weak, not a substitute for Round 2)

The cold controls for r1b and r1c run *after* r1a's (and r1b's) successful `Compile` calls have already
executed in the same process (on different checker instances, with different tags). They still fail.
This is weak incidental evidence that a prior `Compile` of a *different* scenario doesn't warm a later
PC via any process-global state either. It is **not** the Round 2 test the design specifies (which
would Compile and PC the *same* scenario across two instances) — that was gated behind a positive Round
1 and was correctly not run. Reported only as an observation, not as a Round 2 result.

---

## Rounds 2 and 3 — not run, by design

`01-design.md`: "If Round 1 shows a positive effect ... run Round 2 ... and Round 3." and "If Round 1
is NULL (no effect), stop there per the design's explicit instruction — do not force Round 2/3, write
up the NULL result fully." Round 1 is NULL, so Round 2 (cross-instance) and Round 3 (retrofit onto
Q011's real `SchemaTP`/`ClientTP`) were not executed. The `SchemaTP.*`/`ClientTP.*` sources are still
saved under `artifacts/` (copied verbatim from Q012) so a future quartet can run Round 3 directly if
the question is reopened, but no Round 3 numbers are claimed here.

The harness's `all` mode contains the (unexercised) Round 2/3 code paths; they are only invoked when
Round 1 returns positive, so running `-- all` on this result prints the NULL summary and the line
"Round 1 was NOT positive -> stopping per design."

---

## What this means for the standing contradiction

The whole reason Q013 exists: Q008 claims a generative provided type resolves via
`ParseAndCheckFileInProject` (real generated members read back, ~1161ms cold); Q012 found,
reproducibly, that PC *never* resolves a generative provided type, only `Compile` does. Q012's review
named compile-then-PC warming as the single cheapest plausible reconciliation, because Q008 saved no
source and its harness ordering is unknown.

This result **removes that reconciliation.** Compile-then-PC ordering, same checker instance, identical
generative scenario, does not make PC resolve the type — deterministically, across repeats, on the
exact `ProbeSimple` shape Q012 proved fails cold. Per `00-hypothesis.md`'s NULL threshold, this
"hardens the Q008/Q09-vs-Q012 contradiction and makes retroactive reconstruction of Q008/Q09's actual
harness the clear next priority, no longer optional." The charitable "their harness must have compiled
something first" explanation is now specifically tested and falsified for the same-scenario,
same-checker form; if some warming mechanism explains Q008, it is not this one.

---

## What was and wasn't proven, stated plainly

- **Proven:** a prior same-checker-instance `checker.Compile` of the identical generative instantiation
  does not cause a subsequent `ParseAndCheckFileInProject` to resolve that generative provided type.
  Deterministic across 3 in-process repeats and a second process run. The Compile succeeds each time
  (so the type is genuinely materializable); PC fails each time with Q012's exact "couldn't find type
  in tmpXXXXXX" diagnostic; and each PC mints a fresh temp assembly, reusing nothing from the Compile.
- **Not tested (boundaries for the review):**
  - The stricter "PC over the exact same project object `Compile` built" form (Correction 1). A NULL in
    the more permissive form makes a positive there unlikely, but it is genuinely untested.
  - Cross-instance / process-global same-scenario warming (Round 2) — gated behind a positive Round 1,
    correctly not run. Only the weak incidental corroboration above touches it, with a *different*
    scenario, not the same one.
  - Round 3 against Q011's real `SchemaTP`/`ClientTP` — not run, because the toy shape (the simplest
    case, and the one the design says is "the whole ballgame") already shows no effect.
  - `TransparentCompiler` was not enabled (default `FSharpChecker.Create`, same as Q012). Whether the
    newer compiler's caching behaves differently across the Compile/PC boundary is untested; Q012's
    finding it reconciles was also measured on the default compiler, so this stays apples-to-apples.
  - As every Thread-2 quartet: bare `FSharpChecker` as a library, not a live Ionide/VS/Rider host.
- **Corrections surfaced, not patched into the frozen files:** two, detailed above — (1) the
  precondition "literally identical project options" is in tension with "reuse Q012's helpers
  verbatim," resolved toward verbatim reuse with the identical generative instantiation, the stricter
  variant noted as untested; (2) the warm PC mints a fresh temp assembly per call and reuses nothing
  from the prior Compile, which is the mechanistic reason there is no warming.

---

## Reproduction

See `artifacts/README.md`. In short, from the out-of-repo scratch copy: build
`ProbeSimple.Runtime/ProbeSimple.Runtime.fsproj -c Release` (triggers its DesignTime build) and
`Harness/Harness.fsproj -c Release`, then
`Q013_ROOT=<scratch> dotnet run --project Harness/Harness.fsproj -c Release --no-build -- round1`.
No Myriad `Myriad.Sdk` wiring — independent of Myriad by design, same as every Thread 2 quartet.
