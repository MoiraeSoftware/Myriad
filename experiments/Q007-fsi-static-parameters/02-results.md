# Q007-fsi-static-parameters / Movement 3 — Execute + write up

**Status:** DONE. All three rounds built and run to completion, 2026-07-18. Kill condition **not**
triggered (Round 1 passed cleanly). Final SHIP/REVISE/NULL/KILL verdict is deferred to
`03-review.md`; this file records what actually happened, with real numbers from code that was
actually run.

**Location:** `artifacts/` in this quartet folder (not a scratch dir outside the repo, unlike
Q003/Q006 — see the corrections section for why that mattered).

**Environment:** .NET SDK 9.0.310 host (`dotnet build`/`dotnet run`), `FSharp.Compiler.Service`
`43.9.101` pinned for both `FsiParamTP.DesignTime` and `Harness` (matches every prior quartet).
`ProvidedTypes.fs`/`.fsi` copied verbatim from `Q016-satellite-dll-type-provider/artifacts/`
(SDK commit `0a95768a2247daba80b24a2604f77f89fc88ff1f`, unchanged). `FsiParamTP.Runtime`/
`.DesignTime` target `netstandard2.0;net8.0`; `Consumer`/`Consumer2`/`Verify`/`Harness` target
`net8.0`. The .NET SDK's own bundled `FSharp.Compiler.Service.dll` (the one `fsc.exe` itself
loads when `dotnet build` runs) is version `43.9.303.0` — a **different** version from the
`43.9.101` our design-time component separately references. Both versions were active in the
same process throughout Rounds 1–2 with no observed crash or conflict (see "What a review should
press on").

---

## Round 1 — cheapest falsifier: FSI hosted inside a live `DefineStaticParameters` call

`FsiParamTP.Provided.Container<Expr: string>`'s instantiation function creates a fresh, headless
`FsiEvaluationSession` (stdin/stdout/stderr redirected to a `StringWriter`, `collectible = false`)
and calls `.EvalExpressionNonThrowing(exprText)` **synchronously inside the callback**
`DefineStaticParameters` hands the outer compiler. `Consumer.fs`:

```fsharp
module Consumer
type T = FsiParamTP.Provided.Container<"1+2">
let p0 = T.P0
let p1 = T.P1
let p2 = T.P2
let evaluatedTypeName = T.EvaluatedTypeName
```

Built with a real `dotnet build Consumer/Consumer.fsproj -c Release` — i.e. the nesting under
test (FSI hosted inside a design-time component, itself called from inside `fsc.exe`'s own live
compilation of `Consumer.fs`) is the literal, unmodified `dotnet build` path, not a simulated one.

**Result: build succeeded, 0 errors, in ~2 seconds — no hang, no exception, no crash.** This is
the cheapest falsifier passing: `FsiEvaluationSession.Create` and `.EvalExpressionNonThrowing` ran
correctly to completion while genuinely nested on `fsc.exe`'s own compilation call stack.

**Independent reflection-only verification** (`Verify/Program.fs` — no `ProvidedTypes.fs`, no
`FSharp.Compiler.Service`, no `FsiEvaluationSession` anywhere in this program), against the built
`Consumer.dll`:

```
===== ROUND 1 VERIFY =====
  P0 = 0 (expect 0) : OK
  P1 = 1 (expect 1) : OK
  P2 = 2 (expect 2) : OK
  P3 absent, as expected (negative control OK)
  exact P* property count: 3 (expect 3)
  EvaluatedTypeName = System.Int32
ROUND 1 VERIFY verdict: PASS
```

The count (3) and values (0, 1, 2) match the FSI-evaluated `1+2 = 3` exactly, not a hardcoded
stand-in — confirmed by the negative control (`P3` does not exist; if generation were driven by
anything other than the real evaluated int, this would either be absent for the wrong reason or
present when it shouldn't be). **Round 1: PASS. Kill condition not triggered.**

---

## Round 2 — richer value driving generated-member shape

Same mechanism, `Expr = "[\"Name\"; \"Age\"; \"Email\"]"` (a `string list`, not a primitive).
`Consumer2.fs`:

```fsharp
module Consumer2
type T = FsiParamTP.Provided.Container<"[\"Name\"; \"Age\"; \"Email\"]">
let a = T.Name
let b = T.Age
let c = T.Email
let evaluatedTypeName = T.EvaluatedTypeName
```

Built with `dotnet build Consumer2/Consumer2.fsproj -c Release` — succeeded, 0 errors.
Independent reflection verification:

```
===== ROUND 2 VERIFY =====
  Name = Name (expect Name) : OK
  Age = Age (expect Age) : OK
  Email = Email (expect Email) : OK
  Nickname absent, as expected (negative control OK)
  EvaluatedTypeName = Microsoft.FSharp.Collections.FSharpList`1[[System.String, ...]]
ROUND 2 VERIFY verdict: PASS
```

Three provided members exist, named after the FSI-evaluated list's own content (`Name`, `Age`,
`Email`), each returning its own name string; `Nickname` (not in the list) is absent. The
generated-member **shape** — which members exist, how many — was driven by evaluating the
static-argument string as code, not by a fixed schema.

**The downcast the design flagged as the one place Q003's favorable finding could plausibly fail
differently:** `v.ReflectionValue :?> string list` succeeded with **no `InvalidCastException`**,
inside this specific nested-hosting shape (FSI invoked from inside a live
`DefineStaticParameters` call, itself invoked by `fsc.exe`'s own compilation) — confirming, not
just assuming, that Q003's FSI/host `FSharp.Core`-identity finding survives this nesting. **Round
2: PASS both axes** (mechanism and capability).

---

## Round 3 — live-edit re-check cost

`Harness/Program.fs` builds a real, hand-built (non-script) `FSharpProjectOptions` — the same
shape as Q008's `Harness` (`--noframework`, explicit `-r:` to the net8.0 ref pack, the harness's
own resolved `FSharp.Core`, and `FsiParamTP.Runtime.dll`) — and drives
`FSharpChecker.ParseAndCheckFileInProject` with one persistent checker instance
(`FSharpChecker.Create(keepAssemblyContents = true)`), `Stopwatch`-timed, single samples per
`01-design.md`'s explicit instruction (not a benchmark).

```
===== ROUND 3a: int-expr ("1+2") cold vs live-edit =====
  cold check         : 1550ms   (0 diagnostics)
  live-edit re-check : 231ms    (0 diagnostics, Expr "1+2" -> "2+2", same checker, no rebuild)

===== ROUND 3b: string-list expr cold vs live-edit =====
  cold check         : 187ms    (0 diagnostics)
  live-edit re-check : 185ms    (0 diagnostics, last element "Email" -> "Phone", same checker)

Q006 baseline        : cold=1137ms live-edit=47ms
```

(This is the numbers from the final clean rebuild-and-rerun saved in `run-logs/harness-run.txt`; an
earlier same-day run from a from-scratch build landed at 1639ms/226ms and 191ms/184ms respectively
— close enough on both runs, without averaging or treating this as a benchmark, to trust the shape
of the result: both live-edit numbers cluster tightly together across runs, and Round 3a's cold
number is consistently the outlier.)

Both live-edit re-checks land at roughly **4–5x Q006's 47ms baseline** (231ms and 185ms
respectively), and Round 3a's cold check (~1550-1639ms) is in the same range as Q006's 1137ms cold
baseline. Round 3b's cold number (~187-191ms) is markedly lower than Round 3a's — see the
corrections section below for why that number specifically should not be read as a second true
cold start.

---

## Corrections to the design's assumptions, found while building

1. **`FsiEvaluationSession.Create`'s `collectible`/`legacyReferenceResolver` parameters are F#
   "optional-style" parameters** (compiled as `FSharpOption<'T>` in the IL signature, confirmed by
   reflecting on the method directly), but the **call site** passes bare values, not
   `Some`/`None` — `FsiEvaluationSession.Create(fsiConfig, argv, inStream, out, err, false)`, not
   `..., Some false, None)`. Passing `Some false` produces `error FS0001: expected bool but got
   bool option`. `01-design.md` didn't specify the exact call shape (reasonably, since it's an
   implementation detail); confirmed by a standalone probe script before writing the real
   provider, not discovered mid-build.
2. **`EvalExpressionNonThrowing`'s actual return shape**, confirmed directly by reflecting on the
   method and then by calling it: `Choice<FsiValue option, exn> * FSharpDiagnostic[]` — a tuple of
   the `Choice` and a separate diagnostics array, matching what Q015's results already found for
   `EvalInteractionNonThrowing`. `01-design.md:39`'s "`EvalExpressionNonThrowing` returns a
   `Choice<FsiValue option, exn>`" was correct about the `Choice` shape but silent on the paired
   diagnostics array; the real API needed `let result, diags = session.EvalExpressionNonThrowing(...)`.
3. **Generative type-alias naming.** `type T = FsiParamTP.Provided.Container<...>` written inside
   `module Consumer` compiles the real IL type as the **nested type `Consumer+T`**, not a
   top-level type named `Consumer`. `Verify/Program.fs`'s first version called
   `asm.GetType("Consumer")` and found zero of the expected properties (a clean, honest FAIL, not
   a silent false pass) — a direct reflection dump of `Consumer.dll`'s actual type list
   (`asm.GetTypes()`) found `Consumer+T` holding `P0`/`P1`/`P2`/`EvaluatedTypeName`, and
   `Verify` was corrected to look up `Consumer+T` / `Consumer2+T`.
4. **A real generation bug in the Harness's own Round 3b consumer-text builder**, caught by the
   harness's own diagnostics, not silently passed: the first version didn't escape the string
   list's embedded quotes for the *outer* static-argument string literal, producing invalid F#
   syntax in the generated consumer text (`Container<"["Name"; "Age"; "Email"]">` — the inner
   unescaped `"` closes the outer literal early). `ParseAndCheckFileInProject` correctly reported
   5 real parse errors rather than silently succeeding on garbage input; fixed by escaping each
   list element as `\"%s\"` before splicing (`Container<"[\"Name\"; \"Age\"; \"Email\"]">`,
   matching `Consumer2.fs`'s own working literal), then rerun to get the real numbers reported
   above.
5. **A repo-tree build-environment gap, not an FCS/TP finding, but load-bearing for actually
   building anything here:** the Myriad repo's root `Directory.Build.props` unconditionally sets
   `<TargetFrameworks>net9.0</TargetFrameworks>` (plural). Because MSBuild's SDK-style
   multi-targeting logic lets a discovered `TargetFrameworks` silently override each project's own
   explicit singular `<TargetFramework>net8.0</TargetFramework>`, every project under
   `artifacts/` restored/built against `net9.0` regardless of what its own `.fsproj` said, causing
   a restore/build target mismatch (`NETSDK1005`). Q003/Q006 never hit this because their scratch
   projects lived in an out-of-repo temp directory; Q008/Q016's artifacts, per their own
   `RECONSTRUCTION.md`, were also originally built outside the repo tree and only copied in
   afterward. Building **in place** under `experiments/Q007.../artifacts/` (as this task's
   instructions required) hit it directly. Fixed with a minimal, standard MSBuild idiom: an empty
   `artifacts/Directory.Build.props` stub, which stops MSBuild's upward search for that file
   before it reaches the repo root, isolating this quartet's projects from the repo's own
   packaging defaults. net8.0 was kept (rather than switching to net9.0) specifically so Round 3's
   numbers stay comparable to Q006/Q008's own net8.0 timings.
6. **A `.gitignore` bug that would have silently discarded everything in this section.** The
   repo's `.gitignore` had an unanchored `artifacts/` line (line 82, from a generic
   `gitignore.io` "dotnet core" template) that matches **any** directory named `artifacts`
   anywhere in the tree — including every `experiments/Qxxx/artifacts/` folder this repo's own
   methodology requires for a closed quartet. `git status`/`git check-ignore` confirmed this
   quartet's entire `artifacts/` tree was invisible to git before the fix. This is very likely the
   actual root cause of Q006's artifacts going missing outright and Q008/Q009 needing the
   `RECONSTRUCTION.md` recovery effort — neither of those write-ups identified *why* their
   artifacts never made it into the repo, and this explains it mechanically. Fixed by anchoring
   the rule to `/artifacts/` (root-only); confirmed via `git check-ignore` that
   `experiments/Q007.../artifacts/` is now visible while `bin/`/`obj/` under it remain correctly
   ignored by the separate, still-unanchored `[Bb]in/`/`[Oo]bj/` rules. This is a repo-hygiene fix,
   not an FSI/type-provider finding, but it's the reason this file's own artifacts are actually
   committable at all.

---

## Reading against the pre-registered thresholds

- **Cheapest falsifier (Round 1):** clean **PASS**. No hang, no exception, no corrupted state —
  `FsiEvaluationSession.Create`/`.EvalExpressionNonThrowing` ran correctly on the call stack of
  `fsc.exe`'s own live `ApplyStaticArguments`/`DefineStaticParameters` handling, with a real
  independently-reflected result matching the FSI-evaluated value exactly (not a stand-in).
- **Richer-value / capability claim (Round 2):** **PASS**. A genuinely richer value (`string
  list`, not a primitive) drove a real change in generated-member shape (member count and names),
  and the design's flagged highest-risk step — the `FsiValue.ReflectionValue :?> string list`
  downcast, inside this specific nested-hosting configuration — succeeded with no
  `InvalidCastException`.
- **Live-edit cost (Round 3):** genuinely borderline against the pre-registered wording, and I'd
  rather flag the ambiguity than round it to a confident verdict. `00-hypothesis.md`'s SHIP bar
  reads "small enough (same order of magnitude as Q006's 47ms, not 10x–100x worse)"; both
  measured live-edit numbers (~226-231ms, ~184-185ms across two runs) are ~4–5x Q006's 47ms — comfortably under the
  10x–100x REVISE-trigger wording taken literally, but well past what "same order of magnitude"
  usually means colloquially (typically read as within ~3x). The fact that **both** the int and
  list cases land in the same 4–5x band, and that Round 3a's cold number is *also* elevated
  relative to Q006's cold baseline, points to a real, structural fixed cost (a brand-new
  `FsiEvaluationSession` spun up on every single instantiation, exactly the risk
  `01-design.md:26` names) rather than measurement noise. My own honest read leans toward
  REVISE's framing ("works correctly but materially worse than Q006's baseline, should ship as an
  opt-in/cached feature rather than by default") over a confident SHIP, but this is a judgment
  call the pre-registered thresholds don't cleanly resolve on their own, and it's exactly what
  `03-review.md` exists to adjudicate rather than something I should settle here.
- **KILL:** did not trigger. Round 1 passed outright; Rounds 2–3 were run per the design's
  instruction that they only proceed if Round 1 passes.

---

## What a review should press on

- **Single-sample timing, no repeated trials by design** — explicitly named as a sanity check, not
  a benchmark, per `01-design.md`'s own instruction. One incidental second run happened here only
  as a side effect of a full clean-rebuild reproducibility pass (not a designed multi-sample
  measurement), and it landed close to the first run on every number — reassuring, but this is
  still two data points, not a real distribution; don't read the closeness as statistical
  confidence.
- **Round 3b's "cold" check is not a true cold start.** By the time it runs, the *same* checker
  instance has already serviced two prior `ParseAndCheckFileInProject` calls from Round 3a (its
  cold check and its live-edit re-check) in the same process — meaning the type-provider host,
  JIT warm-up, and FCS's own internal caches are already warm. Its ~187-191ms is not directly
  comparable to Q006's 1137ms "true cold" baseline or even to Round 3a's own ~1550-1639ms
  true-cold number in the same run. `01-design.md`'s Round 3 step 3 ("repeat the same cold/live-edit pair
  for the Round 2 list-valued case") doesn't specify whether "cold" there means a fresh process —
  I read it as reusing the same persistent checker throughout Round 3 (matching the design's
  repeated emphasis on "same checker instance"), which is a defensible reading but leaves 3b's
  cold number contaminated by 3a's warm-up. A rerun isolating 3b into its own fresh process would
  give a genuinely comparable cold number.
- **The live-edit-cost verdict above is a judgment call, not a fact** — I've stated my own read
  (leaning REVISE) plainly rather than picking whichever framing sounds better, but a review
  should decide deliberately rather than deferring to my read, especially since the 10x/3x
  threshold ambiguity is a real gap in `00-hypothesis.md`'s own wording, not something I can
  resolve by re-reading it more carefully.
- **The FSI-cost decomposition was never isolated.** Round 3 measures end-to-end
  `ParseAndCheckFileInProject` cost, not `FsiEvaluationSession` construction/teardown in
  isolation. A fresh session is created on *every* instantiation per the design
  (`01-design.md:23-24`); whether the ~180–230ms live-edit cost is dominated by session
  startup/shutdown (fixed per-instantiation overhead) or by the specific expression's own
  evaluation was not measured separately. A follow-up timing just `FsiEvaluationSession.Create`
  and `.Dispose` in a loop, outside any type-provider context, would isolate this cleanly.
- **The nested-hosting version-mismatch finding is observational, not explained.** Two different
  versions of `FSharp.Compiler.Service` (the SDK's own bundled `43.9.303.0` inside `fsc.exe`, and
  this quartet's pinned `43.9.101` inside the design-time component) coexisted in the same
  process across every build and check performed, with no crash or conflict observed. That's a
  real, positive fact, but I did not confirm *why* — the natural explanation (FCS loads
  design-time type-provider assemblies into an isolated `AssemblyLoadContext`, so the two FCS
  copies never actually collide) was not independently verified here; a review with more time
  could confirm the actual isolation mechanism rather than accepting "it didn't crash" as
  sufficient.
- **Only one shape per round was tested** — a single arithmetic expression (`"1+2"`), a single
  3-element string list. No stress test of a much larger or more structurally complex FSI
  expression (nested records, functions), and critically: **the provider's own failure path was
  never exercised.** `evalFsiExpression`'s `Choice2Of2 exn` branch (a genuine FSI evaluation
  failure, as opposed to an outer-compiler parse error like the escaping bug in correction 4
  above, which never reached the provider at all) was written defensively but never actually
  triggered by any round in this write-up. Whether a real FSI-evaluation failure *inside* the
  nested host corrupts the outer checker's subsequent checks — arguably the sharper version of
  the hypothesis's own "does this corrupt state" question — is inferred as low-risk from the
  clean successes here, not itself falsified.
- **The `.gitignore` fix (correction 6) touches a shared repo file, not just this quartet's own
  folder.** It's minimal and precisely scoped (one line, anchored, with no effect on the
  `[Bb]in/`/`[Oo]bj/` rules verified separately still to apply), but a review should confirm it
  doesn't unexpectedly surface any *other* pre-existing `artifacts/`-named directory elsewhere in
  the repo that was relying on the old blanket exclusion.

---

## Reproduction

```
cd experiments/Q007-fsi-static-parameters/artifacts
dotnet build FsiParamTP.Runtime/FsiParamTP.Runtime.fsproj -c Release   # builds DesignTime too
dotnet build Consumer/Consumer.fsproj -c Release                       # Round 1
dotnet build Consumer2/Consumer2.fsproj -c Release                     # Round 2
dotnet build Verify/Verify.fsproj -c Release
dotnet Verify/bin/Release/net8.0/Verify.dll both \
    Consumer/bin/Release/net8.0/Consumer.dll \
    Consumer2/bin/Release/net8.0/Consumer2.dll                         # independent reflection check
dotnet build Harness/Harness.fsproj -c Release
dotnet Harness/bin/Release/net8.0/Harness.dll                           # Round 3 timing
```

Layout: `ProvidedTypes.fs`/`.fsi` (vendored, shared), `FsiParamTP.Runtime/` (TPRTC),
`FsiParamTP.DesignTime/` (`FsiParamTP.Provider.fs` — the one provider, `Container<Expr>`, hosting
FSI internally), `Consumer/` + `Consumer2/` (Round 1/2 real on-disk consumers, each a separate
`dotnet build`), `Verify/` (independent reflection-only checker, no FCS/ProvidedTypes anywhere),
`Harness/` (Round 3 `FSharpChecker` timing), `Directory.Build.props` (empty stub, see correction
5), `run-logs/` (real captured output for every build and run above, byte-for-byte what's quoted
in this file).
