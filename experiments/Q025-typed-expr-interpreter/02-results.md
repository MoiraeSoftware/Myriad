# Q025-typed-expr-interpreter / Movement 3 — Results

**Status:** DONE. Both pre-registered rounds ran and passed against `00-hypothesis.md`'s
pre-registered thresholds. See `03-review.md` (separate, independently dispatched) for the
adversarial pass.

## Summary

Built `RefLib` (a tiny classlib, built first with an ordinary `dotnet build`, standing in for an
already-compiled reference assembly) and `Harness` (a console project referencing `FSharp.Compiler.
Service 43.9.101` and `RefLib.fsproj`). The harness builds a real, hand-built, non-script
`FSharpProjectOptions` (manual `ProjectFileName`/`SourceFiles`/`OtherOptions`, `-r:RefLib.dll` plus
the net9.0 ref pack and `FSharp.Core`), uses `FSharpChecker.Create(keepAssemblyContents = true)`, and
calls `ParseAndCheckFileInProject` against two real on-disk files, `Sample.fs` and `Sample2.fs`
(exact content from `01-design.md`). From each result's `ImplementationFile.Declarations`, it finds
the named top-level `let` binding's bound `FSharpExpr` and hand-interprets it with a recursive
`interpret` function matching `FSharp.Compiler.Symbols.FSharpExprPatterns`: `Const`, `NewRecord`,
`Call`, `Let`, `Value`, `IfThenElse` — exactly the six cases `01-design.md` named, no more. Every
other shape falls through to a `NotImplementedException` naming the unmatched expression's `.Type`.

**Both rounds passed on the first real attempt against the running code**, with no API-shape
correction needed anywhere in the interpreter or the entity/type-resolution path — see "Corrections
to design assumptions" below for the honest account of what *didn't* need fixing, since the design
doc explicitly asked this to be reported rather than assumed.

Full source: `artifacts/RefLib/`, `artifacts/Harness/`. Raw run output: `artifacts/run-logs/`.

## Round 1 — cheapest falsifier: PASS

`Sample.fs`:
```fsharp
module Sample
let w : RefLib.Widget = { Name = "a"; Count = 1 }
```

`checker.ParseAndCheckFileInProject` on this file (0 diagnostics) produced a populated
`ImplementationFile` (`keepAssemblyContents = true` worked as assumed, first try — see below).
`findBinding "w"` walked `Declarations` (one `Entity` wrapping the implicit `Sample` module, one
`MemberOrFunctionOrValue` inside it for `w`) and returned the bound `FSharpExpr`, which matched
`FSharpExprPatterns.NewRecord` directly, exactly as `01-design.md` predicted for this fixture.

`interpret Map.empty wExpr` reflection-constructed the record via `FSharpValue.MakeRecord` against a
runtime `System.Type` resolved as `entity.FullName` (`"RefLib.Widget"`) looked up directly via
`Assembly.GetType` on the loaded reference DLL — no indirection needed beyond that one call.
Compared against `{ RefLib.Name = "a"; Count = 1 }` constructed directly in the host program:

```
interpreted value = { Name = "a"
  Count = 1 }
direct value      = { Name = "a"
  Count = 1 }
Object.Equals(interpreted, direct) = true
ROUND 1: PASS
```

**An anticipated infrastructure risk was checked directly and found not to reproduce — reported
honestly rather than left as an untested "fix," per this repo's own standing risk of a plausible-
sounding correction that doesn't actually hold (see Q019's own struck correction 3).** Before the
first run, it seemed like a real risk that `Harness` needs a compile-time `ProjectReference` to
`RefLib.fsproj` (so `let direct : RefLib.Widget = { ... }` can even be written), and MSBuild
satisfies that by *copying* `RefLib.dll` into `Harness`'s own output directory — a second, distinct
physical file from `artifacts/RefLib/bin/Release/net9.0/RefLib.dll`, the path the interpreter's own
`Assembly.LoadFrom` originally pointed at. Two loaded copies of an identically-named assembly can in
principle carry distinct CLR `Type` identities, which would fail `Object.Equals` for a reason that
has nothing to do with the interpreter. Pre-emptively, `Assembly.LoadFrom` was pointed at the copy
sitting next to `Harness.dll` (`AppContext.BaseDirectory + "RefLib.dll"`) instead, and Round 1
passed.

**Deliberately re-tested afterward, rather than trusting the explanation:** `refLibDll` was reverted
to the original `artifacts/RefLib/bin/Release/net9.0/RefLib.dll` path and the harness rebuilt and
rerun. `Object.Equals(interpreted, direct)` **still returned `true`** — the anticipated mismatch does
not reproduce. The actual mechanism (confirmed by this test, not assumed): the top-level `let
refLibAsm = Assembly.LoadFrom refLibDll` binding executes at module-init time, before `main` calls
`round1()`, which is the first place `Harness`'s own compiled code actually touches `RefLib.Widget`
via its `ProjectReference`. By the time the CLR needs to resolve the compile-time-referenced
`RefLib` assembly, one of that name is already loaded into the default `AssemblyLoadContext` (from
whichever path `Assembly.LoadFrom` used), and the CLR's default resolution reuses the already-loaded
assembly by identity rather than loading a second copy from a different path. **Execution order, not
which physical copy is targeted, is what actually matters here** — if `Assembly.LoadFrom` ran *after*
some other code path had already forced the ProjectReference copy to load, a genuine mismatch might
appear; that ordering was not tested (this quartet's actual `refLibAsm` binding happens to run first
either way, by construction). The code still targets the `AppContext.BaseDirectory` copy — it is a
sound, unnecessary-but-harmless choice, not a load-bearing fix — and the `01-design.md`-anticipated
"real trial and error against the API" for this step turned out to be a trial-and-error dead end,
not a live bug. This is reported here as a correction to this results file's own first-pass
narrative, caught by re-testing the claim rather than by review.

## Round 2 — Let/Call/IfThenElse chain: PASS

`Sample2.fs`:
```fsharp
module Sample2
let result =
    let n = 7
    let bumped = RefLib.Ops.bump n
    if RefLib.Ops.isBig bumped then "big" else "small"
```

The checker resolved `result`'s bound expression to exactly the shape `01-design.md` predicted:
`Let(n = Const 7, Let(bumped = Call(bump, [Value n]), IfThenElse(Call(isBig, [Value bumped]),
Const "big", Const "small")))`. The raw `%A`-printed expression (see `run-logs/harness-run.txt`)
shows `Call`'s underlying case with six printed fields (`Call (None, val bump, [], [], [],
[Value val n])`); this is `FSharpExpr`'s own internal debug `ToString`, not the public active
pattern's arity — the actual `FSharpExprPatterns.Call` active pattern used in `interpret` is a
5-tuple (`objExprOpt, memberOrFunc, typeArgs, methodTypeArgs, argExprs`), exactly as
`01-design.md` specified, and it matched and destructured correctly with no changes.

`interpret Map.empty resultExpr` evaluated to `"big"` (`bump 7 = 17`, `isBig 17 = true`), matching
the expected value. `resolveMethod` (walking `mfv.DeclaringEntity` → `RefLib.Ops`'s compiled static
class → `GetMethod(mfv.CompiledName, Public ||| Static ||| NonPublic)`) resolved both `Ops.bump` and
`Ops.isBig` correctly on the first attempt — F# modules' standard compiled shape (a static class,
public static methods named by `CompiledName`) needed no binding-flags workaround beyond including
`Static` in the flags, and `MethodInfo.Invoke(null, args)` (no instance receiver) worked directly.

The interpreted `"big"` was then fed into a stand-in codegen function, `mkMembers`, demonstrating
the validity precondition that the result must drive a real difference in generated-output shape:

```
mkMembers("big") = ["Detail"; "Summary"]
contrast — mkMembers(other branch) = ["Summary"]
shapes differ between branches = true
ROUND 2: PASS
```

`mkMembers` is a plain function, not a real Myriad generator or type provider, matching the design's
own explicitly-stated bar ("a plain function matching Q007 Round 2's same 'list/count drives shape'
bar is sufficient evidence for this quartet's narrower claim").

## What `FSharpExprPatterns` cases were used, and what was deliberately left out

**Used, all six, exactly as `01-design.md` named — no more, no fewer:**
`Const`, `NewRecord`, `Call`, `Let`, `Value`, `IfThenElse`.

**Deliberately not implemented** (falls through to a named `NotImplementedException` carrying the
unmatched expression's `.Type`, never silently swallowed): `Lambda`/`Application` (function values —
`result` was made a value binding, not a function, specifically to avoid needing these, per
`01-design.md`'s own scoping note), `NewUnionCase`/`UnionCaseGet` (DU pattern matching), `LetRec`
(recursion), generic method calls with unresolved type parameters, `TryFinally`/`TryWith`,
`Sequential` beyond a simple `Let`-chain, quotations-in-quotations. Neither fixture's real,
checker-resolved expression tree needed any of these — confirmed directly from the printed raw
expression in `run-logs/harness-run.txt`, not assumed.

## Corrections to design assumptions discovered while building

Contrary to the design doc's expectation ("Expect real iteration against the FCS 43.9.101 Symbols
API... that's expected engineering work"), **the `FSharpExprPatterns` API surface and the
entity/type-resolution path matched `01-design.md`'s sketch exactly on the first working attempt**:

- `keepAssemblyContents = true` populated `ImplementationFile` (`Some`, not `None`) on the very first
  run, for both `Sample.fs` and `Sample2.fs`.
- `FSharpEntity.FullName` (e.g. `"RefLib.Widget"`, `"RefLib.Ops"`) resolved directly via
  `Assembly.GetType` on the reference DLL with no namespace/name reassembly needed (contrast with
  Q019's correction 2, where `SynComponentInfo` needed namespace and name stitched back together
  from two separate AST fields — that was a *syntax*-tree (`Fantomas.FCS.Syntax`) quirk, not present
  here since this quartet works entirely against the *typed*, post-check `FSharpEntity`/`FSharpType`
  surface, which already carries the full name as one property).
- `FSharpMemberOrFunctionOrValue.DeclaringEntity` + `.CompiledName`, reflected against the module's
  compiled static-class shape with `BindingFlags.Public ||| BindingFlags.Static ||| BindingFlags.
  NonPublic`, resolved both `Ops.bump` and `Ops.isBig` with no further BindingFlags tuning and no
  module-vs-type resolution quirk beyond the ordinary "F# modules compile to a static class" fact.
- `FSharpExprPatterns.Call`'s active-pattern arity (5-tuple) matched the design's sketch exactly.
- `FSharpExprPatterns.Let`'s active-pattern arity (`(bindingVal, bindingExpr, debugPoint), body`)
  matched the design's sketch exactly, including the `DebugPointAtBinding` third element.

No `FSharpExprPatterns`/entity-resolution correction was needed at all. The one place this quartet's
narrative did need correcting was self-inflicted and infrastructural, not API-shape: an anticipated
`Assembly.LoadFrom` path-identity risk (two physical copies of `RefLib.dll`) that, on direct
re-testing, turned out not to be real — see Round 1 above for the full account, including why it
didn't reproduce and what would actually have to be true for it to.

## No `FsiEvaluationSession` anywhere — checked, not assumed

`grep -rn "FsiEvaluationSession\|Reflection.Emit" artifacts/Harness/Program.fs artifacts/RefLib/
RefLib.fs` finds two hits, both inside comments/print strings noting its absence (`Program.fs:5` and
`Program.fs:324`), zero actual construction or import of `FSharp.Compiler.Interactive.Shell` or
`System.Reflection.Emit` anywhere in either file. This is the hypothesis's own validity precondition
and Q014's original sin (per its review) was believing this without checking — checked here
directly, not assumed.

## What this does and doesn't show

**Shown:** a hand-written interpreter walking a real, checker-produced `FSharpExpr` (never a
hand-built expr or a quotation) correctly reflection-constructs a record and reflection-invokes two
already-compiled reference-assembly functions across a `Let`/`Call`/`IfThenElse` chain, with the
interpreted result concretely driving a different generated-member-name list depending on which
branch is taken — all with zero `FsiEvaluationSession` and zero `Reflection.Emit`.

**Not shown:** anything requiring `Lambda`/`Application` (interpreting a *function's* body, as
opposed to a value binding whose body happens to call functions — explicitly out of scope per the
hypothesis's own scoping), pattern matching over a DU (`NewUnionCase`/`UnionCaseGet`), recursion
(`LetRec`), generic method calls, or any case list `Sequential`/`TryWith`/`TryFinally` chain. Nothing
about how this interpreter would behave against a materially large fraction of real F# — this
quartet exercised exactly the two rounds `01-design.md` pre-registered, no more.

## What a review should press on

- **Representativeness of the two fixtures.** `Sample.fs`/`Sample2.fs` were written specifically to
  hit exactly the six implemented patterns with nothing else — a real plugin author's generator
  logic would almost certainly hit `Lambda`/`Application` (any higher-order use), pattern matching
  over a DU (any non-record attributed shape, e.g. Myriad's own union-based generators), or
  recursion, all pre-registered as out of scope and left unimplemented. The SHIP verdict's own
  wording anticipates this ("a production interpreter would need to reimplement a materially large
  fraction of F#'s semantics" is the named REVISE trigger) — a reviewer should judge whether six
  hand-picked patterns clearing two hand-picked fixtures constitutes "genuinely richer shape" per the
  SHIP threshold, or whether it's a narrower mechanism proof than that phrasing implies.
- **Whether the assembly-identity story generalizes.** This quartet found, by directly re-testing
  rather than trusting a first-pass explanation, that which physical copy of `RefLib.dll`
  `Assembly.LoadFrom` targets doesn't matter here — only that it runs before any other code path
  forces the `ProjectReference` copy to load. That ordering happens to hold by construction in this
  harness (`refLibAsm` is a top-level binding, `direct`'s construction is inside a function called
  later from `main`); a real Myriad generator wouldn't have this convenient ordering guarantee, and
  Q016-18's whole file-lock saga shows how much harder cross-copy assembly identity gets once a
  satellite DLL is being concurrently rebuilt. A reviewer should treat "assembly identity resolution
  worked here" as an artifact of this harness's specific execution order, not a general result.
- **The stand-in `mkMembers` function is not a real generator or type provider.** It is a bare
  `string -> string list` match, deliberately meeting the same minimal bar Q007/Q014 used. A
  reviewer should independently confirm this is the same bar those quartets actually used (not a
  weaker one invented for this write-up) and consider whether "could plausibly drive shape" is being
  read too generously here.
- **`Object.Equals` on an F# record uses structural equality** (compiler-generated
  `IStructuralEquatable`), so a genuine `Type`-identity mismatch would show up as `false` for a
  reason invisible in the printed `%A` output (which would print identical-looking values from two
  different `Type` objects). This quartet already tested the one path-reordering it could reach
  without restructuring the harness (reverting `Assembly.LoadFrom`'s target path, § Round 1) and
  found no mismatch. It did **not** test the deeper claim — moving the `Assembly.LoadFrom` call to
  run *after* `Harness`'s own `RefLib.Widget` reference has already forced its `ProjectReference`
  copy to load — which is the one ordering that this write-up's own mechanism explanation predicts
  *would* produce a real mismatch. A reviewer with time to spare could construct that ordering
  directly as the strongest independent check of the explanation given here.
- **No `dotnet fsi` involved anywhere**, deliberately (per `01-design.md`'s own reproduction
  commands and Q001's artifacts README noting fsi's own FCS-version clash risk) — both `RefLib` and
  `Harness` are compiled console/classlib projects, never `.fsx` scripts.

## Reproduction

```
cd experiments/Q025-typed-expr-interpreter/artifacts
dotnet build RefLib -c Release
dotnet build Harness -c Release
dotnet run --project Harness -c Release
```

Raw logs from an actual run of the above: `artifacts/run-logs/build-reflib.txt`,
`artifacts/run-logs/build-harness.txt`, `artifacts/run-logs/harness-run.txt`. Reproducing prints
`ROUND 1: PASS`, `ROUND 2: PASS`, and exits 0.
