# Q006-myriad-as-type-provider / Movement 2 — Design

**Status:** IN PROGRESS.
**Location:** scratch console/library projects under a temp working directory, not part of any
committed repo — same precedent as Q001–Q003 (no `artifacts/` subfolder checked in; this file plus
`02-results.md` are the durable record).
**Pins:** `FSharp.Compiler.Service` `43.9.101` (matches `Myriad/paket.lock`, same as every prior
quartet) for the harness; `FSharp.TypeProviders.SDK`'s `ProvidedTypes.fs`/`ProvidedTypes.fsi`
vendored by file copy from `FSharp.TypeProviders.SDK` at commit
`0a95768a2247daba80b24a2604f77f89fc88ff1f` (the SDK's own documented usage pattern — design-time
components embed a copy of these files, they aren't consumed as a compiled dependency);
`FSharp.Core` `4.7.2` (matches the SDK's own `paket.dependencies` pin) for the provider projects.

## Round 1 — falsifier: does provider visibility actually work with zero build of the consumer?

This tests the entire claimed payoff before any Myriad-specific code is written, using the SDK's
own stock example verbatim (`docs/providing-types.md:85-119`, "Your First Generative Type
Provider") so a failure here is diagnostic about the SDK/hosting mechanism, not about anything this
quartet built.

1. `TrivialTP.DesignTime` and `TrivialTP.Runtime` projects (the split the SDK's own
   `examples/BasicProvider.DesignTime` / `.Runtime` use), containing the doc's
   `GenerativeSchema<Count>` provider unmodified. Build once with `dotnet build`.
2. A harness program hosts `FSharpChecker` (real files on disk this time, not
   `DocumentSource.Custom` — the thing under test is provider hosting, not virtual-file mechanics
   already proven in Q001) with `FSharpProjectOptions` referencing the built `TrivialTP.Runtime.dll`
   (which per the SDK's `TypeProviderAssembly` attribute convention also carries or points at the
   design-time component). One consumer source file:
   ```fsharp
   type S = TrivialTP.Provided.GenerativeSchema<Count = 3>
   let v = S.Property2
   ```
3. Call `checker.ParseAndCheckFileInProject` on the consumer file. Assert zero diagnostics and that
   `Property2`'s symbol use resolves to a real provided member with type `int` —
   **without ever running `dotnet build` on a project containing the consumer file.**
4. The live-edit test: using the *same* `FSharpChecker` instance (so any caching behaves as it
   would in a real host across keystrokes), rewrite the consumer file's text in place to
   `Count = 6` and `S.Property5`, bump the file's version/timestamp the way `DocumentSource.Custom`
   or a real editor buffer would, and re-run `ParseAndCheckFileInProject`. Assert `Property5` now
   resolves and that this required no rebuild of anything — only a re-check, which is exactly what
   Ionide/FSAC does on every keystroke. This is the actual mechanism behind "live in the IDE," a
   closer proxy to a real editor session than Q001–Q005's harnesses attempted, per this quartet's
   validity preconditions.

Kill condition for the whole quartet, pre-registered: if step 3 or step 4 fails (diagnostics appear
where none are expected, or the symbol doesn't resolve, or the live-edit re-check doesn't pick up
the change without an explicit rebuild step), stop — Round 2 and 3 would be testing a port of
something that doesn't clear the bar this quartet exists to test.

## Round 2 — falsifier: can the Lenses shape be expressed as generative members at all?

Before porting `LensesGenerator.fs`'s real field-iteration logic, confirm the target shape is
buildable. A lens in Myriad's output is a value `let street = (getter, setter)` — a 2-tuple of
functions. Add a second provided type to the same `TrivialTP` design-time assembly (co-located for
build convenience; logically independent of Round 1's test):

```fsharp
type LensDemo = TrivialTP.Provided.LensPoC<TypeName = "unused">
let (getter, setter) = LensDemo.NameLens
```

implemented as a static `ProvidedProperty` whose `getterCode` returns a quotation constructing a
tuple of two function values (`FSharpFunc`-shaped), for one hardcoded field on a hardcoded record,
to isolate "can the SDK express this shape" from "can we drive it off real field data." Confirm it
typechecks (Round 1's harness, zero diagnostics) and — new relative to every prior quartet, which
only ever checked typechecking — confirm it is **runtime-correct**: compile a small consumer
program (real `dotnet build` + `dotnet run` this one time, since runtime behavior can't be observed
through `FSharpChecker` alone) that calls the getter and setter and checks the round-trip value.

## Round 3 — port the real generator

Reimplement `LensesGenerator.fs`'s actual record-lens case (`LensesGenerator.fs:19-52`,
`createLensForRecordField`) as a generative provider, `LensesTP.Provided.Lenses<'T>`, taking the
target record type as a **static parameter of type `System.Type`** (`typeof<Person>`) — not a
source-attribute the way Myriad's real `[<Lenses>]` works, since a type provider has no access to
sibling source declarations in the consumer's file the way `Ast.fs` does; it only sees what's
already compiled and referenceable. This is a real, load-bearing design deviation from how Myriad
generators are invoked today, not an implementation detail — call it out explicitly in
`02-results.md`, don't bury it.

- Enumerate fields via `Microsoft.FSharp.Reflection.FSharpType.GetRecordFields(recordType)` (the
  reflection-based record introspection already used elsewhere in this codebase's ecosystem,
  confirmed available with zero FCS dependency).
- For each field, generate one static `ProvidedProperty` returning a `(getter * setter)` tuple,
  matching `createLensForRecordField`'s actual getter/setter shape (`x -> x.Field`,
  `fun x value -> { x with Field = value }` for the non-aetherStyle default) via quotations built
  with `Expr.Lambda`/reflection-based record field access (`FSharpValue.GetRecordField`/an
  `UncheckedQuotations`-style field-get if plain reflection calls don't splice cleanly into a
  quotation — resolve this concretely while building, don't assume from the docs).
- Test target: a `Person = { Name: string; Age: int }` record compiled into its own small reference
  assembly (so it's a genuinely separate compiled type, matching how a real consumer's record would
  already exist before applying the provider).
- Verify against Round 1's harness (zero diagnostics, live-edit-without-rebuild if a static
  parameter changes) **and** Round 2's runtime-correctness check (get/set actually round-trip a
  real `Person` value correctly) — both axes, since a provider that typechecks but computes the
  wrong lens is a worse outcome than one that fails to compile.

## Reproduction

```
mkdir tp-spike && cd tp-spike
# vendor ProvidedTypes.fs/.fsi from FSharp.TypeProviders.SDK@0a95768
dotnet new classlib -lang F# -o TrivialTP.Runtime
dotnet new classlib -lang F# -o TrivialTP.DesignTime   # references ProvidedTypes.fs + Runtime
dotnet new console -lang F# -o Harness                 # FSharp.Compiler.Service 43.9.101
# Round 1/2/3 program bodies as described above; see 02-results.md for what actually ran
dotnet build TrivialTP.DesignTime TrivialTP.Runtime
dotnet run --project Harness
```

No MSBuild integration with Myriad's own `Myriad.Sdk` targets — deliberate, matching Q001's same
choice, since the question under test is whether the type-provider mechanism itself delivers live
visibility, not whether Myriad's existing MSBuild pipeline can be wired to it (that would be a
follow-on quartet if this one ships).
