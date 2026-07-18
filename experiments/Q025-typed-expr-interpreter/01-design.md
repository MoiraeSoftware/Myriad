# Q025-typed-expr-interpreter / Movement 2 — Design

**Status:** NOT YET EXECUTED.
**Location:** scratch console project under `artifacts/` in this quartet folder (not part of the
committed repo). No type provider, no `ProvidedTypes.fs`, no `FsiEvaluationSession` anywhere —
deliberately simpler infrastructure than Q007/Q016-19, since the whole point is that this mechanism
needs only `FSharpChecker` + reflection. `FSharp.Compiler.Service` pinned to `43.9.101`.

## Fixture: an already-compiled reference assembly

`RefLib/RefLib.fs`, built first with an ordinary `dotnet build` into `RefLib.dll` — this stands in
for "a reference assembly already on disk," never touched by the checker that later parses
`Sample.fs`:

```fsharp
namespace RefLib
type Widget = { Name: string; Count: int }
module Ops =
    let bump (x: int) = x + 10
    let isBig (n: int) = n > 15
```

## Harness shape

Same non-script `FSharpProjectOptions` pattern as Q008/Q010/Q025's sibling quartets: manual
`ProjectFileName`/`SourceFiles`/`OtherOptions` (`-r:RefLib.dll` plus the ref-pack/`FSharp.Core`
references), `FSharpChecker.Create(keepAssemblyContents = true)`,
`ParseAndCheckFileInProject` on a real `Sample.fs`/`Sample2.fs` on disk. `keepAssemblyContents = true`
is required — it is the flag that makes `FSharpCheckFileResults.ImplementationFile` populate at all
(a `FSharpImplementationFileContents option`); confirm this directly (report if it's `None` when the
flag is set, since that would be a KILL-relevant fact, not a design assumption to wave past).

From `ImplementationFile.Value.Declarations`, walk to the `FSharpImplementationFileDeclaration.Entity`
/ `.MemberOrFunctionOrValue` node for the top-level `let` binding under test, and take its bound
expression (the `FSharpExpr`) as the interpreter's input — never a hand-built `FSharpExpr` or a
quotation, per the hypothesis's validity preconditions.

## The interpreter

A hand-written recursive function `interpret (env: Map<string, obj>) (expr: FSharpExpr) : obj`,
matching against `FSharp.Compiler.Symbols.FSharpExprPatterns` active patterns. Only the patterns
actually needed for the two rounds below are implemented; every other case falls through to a named
`NotImplementedException` carrying the unmatched expression's `.Type` and a short description, so an
unhandled shape fails loudly and specifically rather than silently — required by the hypothesis's own
standing risk note (Q001/Q014's pattern of quietly narrowing scope).

Patterns needed:
- `FSharpExprPatterns.Const(value, _ty)` → return `value` as-is.
- `FSharpExprPatterns.NewRecord(recordType, argExprs)` → interpret each `argExprs` element, then
  reflection-construct: resolve the record's actual runtime `System.Type` (via
  `recordType.TypeDefinition.CompiledName`/`.Namespace` looked up through `Assembly.LoadFrom` on the
  reference DLL — record the exact resolution steps used, this is likely to need real trial and
  error against the API rather than working first try), then
  `FSharpValue.MakeRecord(runtimeType, valuesArray)`.
- `FSharpExprPatterns.Call(objExprOpt, memberOrFunc, typeArgs, methodTypeArgs, argExprs)` → interpret
  `argExprs`, resolve `memberOrFunc` to a real `System.Reflection.MethodInfo` (via
  `memberOrFunc.DeclaringEntity`/`.CompiledName` reflected against the loaded reference assembly —
  again, record the actual resolution path used, not an assumed one-liner), reflection-invoke
  (`MethodInfo.Invoke(null, argsArray)` for the `Ops.bump`/`Ops.isBig` module-function case, no
  instance receiver).
- `FSharpExprPatterns.Let((bindingVal, bindingExpr, _), bodyExpr)` → interpret `bindingExpr`, extend
  `env` with the bound name → value, interpret `bodyExpr` under the extended `env`.
- `FSharpExprPatterns.Value(v)` → look up `v.LogicalName` (or `.DisplayName`) in `env`.
- `FSharpExprPatterns.IfThenElse(condExpr, thenExpr, elseExpr)` → interpret `condExpr` as `bool`,
  branch.

Explicitly **not implemented**, named up front rather than discovered by accident: `Lambda`/
`Application` (function values), `NewUnionCase`/`UnionCaseGet` (DU pattern matching), `LetRec`
(recursion), generic method calls with unresolved type parameters, `TryFinally`/`TryWith`, `Sequential`
beyond what a simple `Let`-chain covers, quotations-in-quotations. If either round's real expression
tree needs one of these unexpectedly, that is exactly the finding the hypothesis's REVISE threshold is
watching for — report it, don't quietly work around it by hand-simplifying the source fixture to avoid
the shape.

## Round 1 — cheapest falsifier

`Sample.fs`:
```fsharp
module Sample
let w : RefLib.Widget = { Name = "a"; Count = 1 }
```

Interpret `w`'s bound expression (expected: a bare `NewRecord`). Compare
`interpret Map.empty wExpr` against directly evaluating `{ RefLib.Name = "a"; Count = 1 }` in the host
program (same values, `Object.Equals`, and print both for visual confirmation). If the current pin's
`FSharpExprPatterns.NewRecord` doesn't expose enough structure (e.g. `recordType` isn't resolvable to
a loadable runtime `System.Type` the way assumed above), or `ImplementationFile` doesn't carry the
expected shape at all, stop here — this is the pre-registered KILL condition.

## Round 2 — richer shape (Call + control flow + real effect on generated shape)

`Sample2.fs`:
```fsharp
module Sample2
let result =
    let n = 7
    let bumped = RefLib.Ops.bump n
    if RefLib.Ops.isBig bumped then "big" else "small"
```

`result`'s bound expression is `Let(n = Const 7, Let(bumped = Call(bump, [Value n]), IfThenElse(Call
(isBig, [Value bumped]), Const "big", Const "small")))` — covers `Let`, `Call` (twice, one feeding the
other), `Value`, and `IfThenElse` in one small tree, without needing `Lambda`/`Application` (deliberately
avoided by making `result` a value binding, not a function, per the note in `00-hypothesis.md`'s own
scoping — interpreting a *function's* body would require argument binding via `Lambda`/`Application`,
explicitly out of scope for this quartet).

Interpret it and confirm the result equals `"big"` (since `bump 7 = 17 > 15`). Then, to satisfy the
hypothesis's validity precondition that the interpreted result must drive "a real difference in
generated output shape": feed the interpreted string into a small stand-in codegen function
(`mkMembers: string -> string list`, e.g. `"big" -> ["Detail"; "Summary"]`, `"small" -> ["Summary"]`)
and print the resulting member-name list — demonstrating the *result of interpretation*, not a
hardcoded branch never actually exercised, choosing how many members a hypothetical generator would
emit. This does not require building a real type provider or Myriad generator; a plain function
matching Q007 Round 2's same "list/count drives shape" bar is sufficient evidence for this quartet's
narrower claim (interpretation works and produces a value that *could* drive shape), same standard
Q014's design used for "reified source that could plausibly compile," not a full production pipeline.

## What `02-results.md` must record explicitly

- Every `FSharpExprPatterns` case the walk actually matched for these two rounds (list them), and the
  explicit list of cases named above as unimplemented — do not let this shrink silently.
- Whether `keepAssemblyContents = true` produced a populated `ImplementationFile` on the first try, and
  the actual API path used to go from `FSharpEntity`/`FSharpType` back to a loadable runtime
  `System.Type` for reflection (this is the step most likely to need iteration against the real API
  rather than working as sketched here).
- Whether reflection-invoking `Ops.bump`/`Ops.isBig` needed anything beyond a direct `MethodInfo.Invoke`
  (e.g. binding-flags issues, module-vs-type resolution quirks specific to F# modules' compiled shape).
- No `FsiEvaluationSession` was created anywhere in the harness — confirm this explicitly as a checked
  fact, not an assumption, since it is the hypothesis's own validity precondition and Q014's original
  sin (per its review) was believing this without checking.

## Reproduction

```
dotnet new classlib -lang F# -o artifacts/RefLib
dotnet build artifacts/RefLib
dotnet new console  -lang F# -o artifacts/Harness
dotnet add artifacts/Harness package FSharp.Compiler.Service --version 43.9.101
# Sample.fs / Sample2.fs written to disk as real files the harness parses by path
dotnet run --project artifacts/Harness
```

## Explicit instruction to whoever executes this (do not silently deviate)

- Do not edit this file or `00-hypothesis.md` once execution starts. If an assumed API shape (e.g.
  `FSharpExprPatterns.NewRecord`'s exact argument tuple, or how to get from `FSharpType` to a
  reflectable `System.Type`) turns out wrong, report the correction honestly in `02-results.md`.
- If Round 1 fails outright (KILL), still report which specific piece failed (pattern absent from the
  pin vs. structure not present under `keepAssemblyContents` vs. something else) rather than a bare
  "didn't work" — this repo's own methodology treats a precise KILL as valuable as a SHIP.
- Do not quietly simplify `Sample2.fs`'s shape to dodge an unimplemented pattern if one shows up
  unexpectedly (e.g. if `if`/`then`/`else` on primitives compiles to something other than a plain
  `IfThenElse` under this pin) — report the mismatch as a finding.
