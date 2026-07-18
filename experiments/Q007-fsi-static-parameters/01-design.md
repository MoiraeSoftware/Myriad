# Q007-fsi-static-parameters / Movement 2 — Design

**Status:** NOT YET EXECUTED.
**Location:** scratch multi-project solution under `artifacts/` in this quartet folder (not part of
the committed repo). Same convention as Q006/Q008/Q016: a `DesignTime` project (vendors
`ProvidedTypes.fs`/`.fsi` from `FSharp.TypeProviders.SDK` commit `0a95768a2247daba80b24a2604f77f89fc88ff1f`,
the same commit Q006 used), a `Runtime` project (the assembly the generative type's members get
remapped into via `assemblyReplacementMap`, same pattern as Q008's `SchemaTP.Runtime`), a `Consumer`
project that references the DesignTime assembly as a `TypeProviderAssembly`, and a `Harness` console
project that drives `FSharpChecker` directly against a **real, non-script `FSharpProjectOptions`**
built by hand (`ProjectFileName`/`SourceFiles`/`OtherOptions` with explicit `-r:` references), the
same shape Q008's `Harness/Program.fs` uses — deliberately not `GetProjectOptionsFromScript`, since
Q012/Q013 found that route fails to resolve generative provided types on the success path for an
unrelated reason. `FSharp.Compiler.Service` pinned to `43.9.101` throughout.

## Provider under test: `FsiParamTP.Provided.Container<Expr: string>`

Generative (`isErased = false`), one string static parameter named `Expr`. Its
`DefineStaticParameters` instantiation function:

```fsharp
let createType typeName (exprText: string) =
    use session = // FsiEvaluationSession.Create(...), stdin/stdout redirected to StringWriter,
                  // collectible = false (match Q003's finding that FSI/FSharpChecker coexist
                  // cleanly; collectibility was never Q003's variable under test)
    let value, errors = session.EvalExpressionNonThrowing(exprText)
    // build a ProvidedTypeDefinition from whatever `value` turns out to be — see Round 1 vs Round 2
    ...
```

This function is called by the *outer* `FSharpChecker`/`fsc` compilation machinery while it is
mid-way through resolving the consumer's type reference — i.e., `FsiEvaluationSession.Create` and
`.EvalExpressionNonThrowing` run **on the call stack of** the outer checker's own `ApplyStaticArguments`
call. That nesting is the thing no prior quartet has exercised and is the actual subject of the
cheapest falsifier below, independent of which round's payload is used.

## Round 1 — cheapest falsifier (nesting safety + primitive correctness)

`Expr = "1+2"`. `EvalExpressionNonThrowing` returns a `Choice<FsiValue option, exn>`; on success,
`.ReflectionValue :?> int` gives `3`. The instantiation function generates exactly `N` static
`ProvidedProperty`s (`P0`..`P(N-1)`, `int`, `getterCode = fun _ -> <@@ i @@>`) where `N` is that
evaluated int.

`Consumer.fs`:
```fsharp
module Consumer
type T = FsiParamTP.Provided.Container<"1+2">
let p0 = T.P0
let p1 = T.P1
let p2 = T.P2
```

Build the whole solution with a real `dotnet build` (matching Q008's `SchemaTP`/`ClientTP` pipeline,
not `checker.Compile` in-process) to get a real `Consumer.dll`. A separate, independent reflection-only
program (no `ProvidedTypes.fs`, no FCS — mirrors Q008's `AttrCheck`) loads `Consumer.dll` and confirms:
exactly 3 properties named `P0`, `P1`, `P2` exist, and reading each back gives `0`, `1`, `2`.

**Record:** did `EvalExpressionNonThrowing` return cleanly (no hang, no exception, no corrupted state
in the *outer* checker for subsequent operations)? Did the reflected property count/values match the
FSI-evaluated `3` exactly (not a hardcoded stand-in)? If this deadlocks, throws unrecoverably, or the
outer `dotnet build` process hangs, stop — this is the KILL condition, record it as-is per the
hypothesis's pre-registered threshold, do not attempt Round 2.

## Round 2 — richer value driving generated-member shape

`Expr = "[\"Name\"; \"Age\"; \"Email\"]"` (an `string list`, genuinely richer than a primitive per the
hypothesis's own validity precondition). `.ReflectionValue :?> (string list)` (boxed as `obj`, downcast
via the FSI-returned `FsiValue.ReflectionType` matching `typeof<string list>` — record whether this
downcast needs any special handling, since FSI's `FSharp.Core` identity is the same question Q003
already answered favorably but never tested inside this specific nested-hosting shape).

The instantiation function generates one static `ProvidedProperty` per list element, named after the
list's string content (`Name: string`, `Age: string`, `Email: string`, `getterCode` returning the
literal element text) — i.e., the *shape* of the generated type (which members exist, how many) is
driven by evaluating the static-argument string as code, not by a fixed schema. This is the
"list-valued field driving a variable number of provided members" case the hypothesis names as the
bar for "beyond primitive plumbing," and it could not be replicated by a smarter string-encoding
convention parsed without FSI only in the trivial sense (a CSV-splitting provider could do the same
for *this* specific case) — Round 2's write-up must say so honestly rather than oversell it; the
real claim under test is the nesting-safety + arbitrary-code-evaluation mechanism, not that this
particular list-shape case is inherently beyond string-parsing.

`Consumer2.fs`:
```fsharp
module Consumer2
type T = FsiParamTP.Provided.Container<"[\"Name\"; \"Age\"; \"Email\"]">
let a = T.Name
let b = T.Age
let c = T.Email
```

Same build-then-independently-reflect verification as Round 1: confirm exactly 3 properties named
`Name`/`Age`/`Email` exist and each returns its own name string.

## Round 3 — live-edit re-check cost (the standing engineering question)

Using the **same Harness-built, non-script `FSharpProjectOptions`** pattern as Q008's `Harness`
(manual `OtherOptions` with `-r:` to the ref pack, `FSharp.Core`, and `FsiParamTP.Runtime.dll` — the
consumer never needs `SchemaAsm`-style path-based loading since the provider's own static string
carries the payload), measure with `FSharpChecker.ParseAndCheckFileInProject`, matching Q006's exact
cold-vs-live-edit methodology (`02-results.md`'s Round 1 table):

1. Cold check of `Consumer.fs` (`Expr = "1+2"`), single sample, `Stopwatch`-timed.
2. Edit the static argument in place (`"1+2"` → `"2+2"`) and re-check with the **same** checker
   instance, single sample, timed — this is the number that matters: it is the cost of re-running
   `EvalExpressionNonThrowing` (a fresh FSI session per instantiation, per the design above) on every
   keystroke-triggered recheck, not just the cost of ordinary typechecking.
3. Repeat the same cold/live-edit pair for the Round 2 list-valued case, since list evaluation may
   cost differently than a single arithmetic expression.

Report both numbers plainly against Q006's own 1137ms cold / 47ms live-edit baseline — do not average
or extrapolate, single samples, consistent with this repo's own standing convention (Q010 Round 3,
Q021).

## Reproduction

```
dotnet new classlib -lang F# -o artifacts/FsiParamTP.Runtime
dotnet new classlib -lang F# -o artifacts/FsiParamTP.DesignTime
dotnet new console  -lang F# -o artifacts/Harness
# ProvidedTypes.fs/.fsi copied from Q006's vendored copy (same SDK commit)
# each .fsproj pins FSharp.Compiler.Service / FSharp.TypeProviders.SDK-equivalent to 43.9.101
dotnet build artifacts/FsiParamTP.Runtime
dotnet build artifacts/FsiParamTP.DesignTime
dotnet build artifacts/Consumer   # references DesignTime via TypeProviderAssembly
dotnet run --project artifacts/AttrCheck -- <path-to-Consumer.dll>   # independent reflection check
dotnet run --project artifacts/Harness                                # Round 3 timing
```

## Explicit instruction to whoever executes this (do not silently deviate)

- Do not edit this file or `00-hypothesis.md` once execution starts. If `EvalExpressionNonThrowing`'s
  actual signature/behavior differs from what's assumed here (e.g. it needs stdin/stdout redirection
  set up a specific way to avoid hanging, or `FsiEvaluationSession.Create` needs specific args to run
  headless), report the correction honestly in `02-results.md`.
- If Round 1 hangs or throws unrecoverably: stop, do not build Round 2 or 3, record the KILL verdict
  as pre-registered, and name exactly what failed (deadlock vs. exception vs. state corruption —
  these are different findings).
- Round 3's timing is a single-sample sanity check, not a benchmark — say so explicitly in
  `02-results.md`, do not imply statistical rigor that wasn't done.
- Name explicitly, in `02-results.md`, whether the downcast from `FsiValue.ReflectionValue` to the
  host's own `string list`/`int` type worked without an `InvalidCastException` — this is the one
  place Q003's favorable finding could plausibly fail differently inside this specific nested-hosting
  shape, and it must be checked, not assumed from Q003's result.
