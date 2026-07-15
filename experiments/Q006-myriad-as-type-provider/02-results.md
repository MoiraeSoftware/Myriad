# Q006-myriad-as-type-provider / Movement 3 — Execute + write up

**Status:** DONE. All three rounds built and run to completion, 2026-07-14. Kill condition **not**
triggered (Round 1 passed). Final SHIP/REVISE/NULL verdict is deferred to `03-review.md`; this file
records what actually ran.

**Environment:** .NET SDK 9.0.310 host, provider TFMs `netstandard2.0;net8.0`, harness `net8.0`.
`FSharp.Compiler.Service` `43.9.101` (pinned, matches `Myriad/paket.lock`).
`ProvidedTypes.fs`/`.fsi` vendored by file copy from `FSharp.TypeProviders.SDK` @
`0a95768a2247daba80b24a2604f77f89fc88ff1f`. Provider projects pin `FSharp.Core` `4.7.2` (SDK's own
pin); FCS resolved `FSharp.Core` `10.1.301` for the harness (NU1608 warning, benign — the provider
loads with its own FSharp.Core, the standard TP hosting arrangement).

Three projects, matching `01-design.md`'s reproduction sketch: `TrivialTP.Runtime` (TPRTC, carries
`[<assembly:TypeProviderAssembly("TrivialTP.DesignTime.dll")>]`), `TrivialTP.DesignTime` (TPDTC,
embeds a copy of the runtime source + `ProvidedTypes.fs` + the provider), `Harness` (FCS host). The
`IsFSharpDesignTimeProvider=true` project reference placed `TrivialTP.DesignTime.dll` directly
alongside `TrivialTP.Runtime.dll` in the runtime output folder, so FCS's default probing finds it
with no `typeproviders/fsharpNN` subfolder needed.

---

## Round 1 — falsifier: provider visibility with zero build of the consumer

The SDK's stock generative example (`docs/providing-types.md:85-119`) transplanted verbatim as
`TrivialTP.Provided.GenerativeSchema<Count>`. The harness hosts one `FSharpChecker` and drives a
real on-disk consumer `.fs` through `ParseAndCheckFileInProject` with in-memory `SourceText` and a
bumped version on each edit — the exact API and buffer semantics FSAC/Ionide use per keystroke.
**No `dotnet build` is ever run on any project containing the consumer file** (no `Consumer.fsproj`
exists; the harness passes a synthetic project name to FCS).

| step | consumer text | check time | result |
|---|---|---|---|
| initial | `GenerativeSchema<Count=3>`, `S.Property2` | 1137ms (cold) | 0 diagnostics; `Property2 : int` |
| negative control | `Count=3`, `S.Property5` | 92ms | **error, as required** (see below) |
| live-edit, same checker | `Count=6`, `S.Property5` | 47ms | 0 diagnostics; `Property5 : int` |

Negative-control diagnostic (proves the provider is genuinely instantiated per static argument, not
rubber-stamping):

```
Error (3,16): The type 'S' does not define the field, constructor or member 'Property5'.
```

At `Count=3`, `Property5` does not exist; after the live edit to `Count=6` the *same* checker
re-instantiates the generative type and `Property5` resolves as `int` — a 47ms re-check, no rebuild
of anything. This is the actual "live in the IDE" mechanism, and the negative control is what makes
the pass trustworthy: the symbol does not resolve for free, it appears exactly when the static
argument says it should.

**Round 1: PASS.** Kill condition not triggered.

One skepticism note carried into later rounds: `FSharpCheckFileResults.GetAllUsesOfAllSymbolsInFile`
will report an *inferred* type for a `let`-bound name via error recovery even when the provider
member it came from failed to resolve (observed directly in the Round 3 first attempt below). So
every round gates PASS on **diagnostics**, never on "a symbol resolved."

---

## Round 2 — falsifier: can the lens (getter, setter) tuple be a generative member?

Second provider `TrivialTP.Provided.LensPoC<TypeName>` in the same design-time assembly. One static
property `NameLens` for a hardcoded field on a hardcoded record `Point = { Name: string; X: int }`
(the record lives in the runtime assembly; the design-time copy is remapped to it by
`assemblyReplacementMap`). The getter body is a plain quotation literal:

```fsharp
getterCode = fun _ ->
    <@@ ( (fun (p: Point) -> p.Name),
          (fun (p: Point) (v: string) -> { p with Name = v }) ) @@>
```

**Typecheck (harness, 141ms, 0 diagnostics):**

```
getter inferred type: Point -> string
setter inferred type: Point -> string -> Point
```

**Runtime correctness** (the one round that needs a real compile+run — `R2Runtime`, `dotnet run`):

```
getter p        = alice                    (expect alice)
setter p "bob"  = { Name = "bob"; X = 1 }  (expect { Name = "bob"; X = 1 })
ROUND 2 RUNTIME: PASS
```

Getter reads the field; setter returns an immutable copy with `Name` replaced and `X` preserved; the
original `p` is unchanged. The record copy-update `{ p with Name = v }` (a `NewRecord` node) spliced
into a *generative* member with no trouble — **no `UncheckedQuotations` needed**, contrary to the
design's flagged risk. **Round 2: PASS both axes.**

---

## Round 3 — port `LensesGenerator.createLensForRecordField`

Same getter/setter shape as `LensesGenerator.fs:19-52` (`x -> x.Field`,
`fun x value -> { x with Field = value }`, non-`aetherStyle` argument order), one `(getter, setter)`
static property per record field, fields enumerated by
`Microsoft.FSharp.Reflection.FSharpType.GetRecordFields`. Because the field names and types are only
known by reflection at design time, the getter/setter bodies are built with the explicit quotation
combinators rather than literals:

```fsharp
getterCode = fun _ ->
    let xg = Var("x", recordType)
    let getterLam = Expr.Lambda(xg, Expr.PropertyGet(Expr.Var xg, field))
    let xs, vv = Var("x", recordType), Var("value", fieldType)
    let newArgs = [ for f in fields -> if f = field then Expr.Var vv
                                       else Expr.PropertyGet(Expr.Var xs, f) ]
    let setterLam = Expr.Lambda(xs, Expr.Lambda(vv, Expr.NewRecord(recordType, newArgs)))
    Expr.NewTuple [getterLam; setterLam]
```

These plain checked `Expr.*` nodes (`PropertyGet`, `NewRecord`, `NewTuple`, `Lambda`) spliced
correctly. `UncheckedQuotations` was not required here either. Function/tuple types were built via
`ProvidedTypeBuilder.MakeGenericType(typedefof<int->int>, ...)` + `FSharpType.MakeTupleType`, per
`technical-notes.md`'s guidance to prefer `ProvidedTypeBuilder` over raw `Type.MakeGenericType`.

### The load-bearing design correction: `typeof<Person>` static parameter does not exist

`01-design.md:76-77` specifies the provider takes `typeof<Person>` — "a **static parameter of type
`System.Type`**." Built exactly that first (`Lenses<RecordType : System.Type>`,
`type PersonLenses = ...Lenses<RecordType = typeof<TrivialTP.Helpers.Person>>`). The compiler
rejects it:

```
Error (2,58): Invalid static argument to provided type. Expected an argument of kind 'System.Type'.
Error (3,32): The type 'PersonLenses' does not define the field, constructor or member 'Name'.
Error (4,30): The type 'PersonLenses' does not define the field, constructor or member 'Age'.
```

F# type-provider static arguments must be literal constants (the `[<Literal>]`-encodable set: the
numeric types, `char`, `bool`, `string`, `decimal`, enums). `System.Type` is not among them, so
`typeof<Person>` cannot be passed as a static argument at all — the SDK will happily *declare* a
`typeof<System.Type>` static parameter, but no consumer can ever supply one. This is a real,
unavoidable design-vs-reality gap, not an implementation detail.

(Note the error-recovery trap flagged in Round 1 firing here: the harness still printed
`nameG inferred type: Person -> string` for this failing attempt, purely from the consumer's
`let n : string = nameG p` annotation. The four `Error` diagnostics are the truth; the "resolved"
symbol type is inference noise. Gating on diagnostics is what keeps this a FAIL.)

**Correction applied** (recorded here, not by editing the frozen design): name the record type with
a **string** static parameter and resolve it by reflection over `config.ReferencedAssemblies`
(`Assembly.LoadFrom` + `GetType`). Everything downstream — field enumeration, quotation
construction — is byte-for-byte the same. Provider becomes
`Lenses­ByName<TypeName : string>`, consumer:

```fsharp
type PersonLenses = LensesTP.Provided.LensesByName<TypeName = "TrivialTP.Helpers.Person">
```

This sharpens, rather than invents, the deviation the design already called out (`01-design.md:76`):
a type provider has no access to sibling source declarations the way `Ast.fs` does; the target
record must **already be compiled in a referenced assembly** before the provider can see it. Myriad's
real `[<Lenses>]` reads the attributed record straight out of the same file's syntax tree — the
provider cannot. This bounds which Myriad generators the approach can reach.

### Results (corrected by-name provider)

Test record `Person = { Name: string; Age: int }`, compiled into the runtime assembly (a genuinely
separate compiled type resolved by reflection, not the design-time copy).

**Typecheck (harness, 75ms, 0 diagnostics):**

```
nameG : Person -> string        nameS : Person -> string -> Person
ageG  : Person -> int           ageS  : Person -> int -> Person
```

Negative control (proves the provider enumerates Person's *actual* fields):

```
Error (3,25): The type 'PersonLenses' does not define the field, constructor or member 'Nickname'.
```

**Runtime correctness** (`R3Runtime`, `dotnet run`):

```
nameG p        = alice                     (expect alice)
ageG p         = 30                        (expect 30)
nameS p "bob"  = { Name = "bob"; Age = 30 }
ageS p 99      = { Name = "alice"; Age = 99 }
original p     = { Name = "alice"; Age = 30 }   (unchanged)
ROUND 3 RUNTIME: PASS
```

All four lenses round-trip: getters read, setters produce the correctly-updated immutable copy, the
source value is untouched. **Round 3: PASS both axes** (with the static-parameter correction above).

---

## What was and wasn't proven, stated plainly

- **Proven:** a generative F# type provider hosting `LensesGenerator`'s real getter/setter logic
  typechecks and is runtime-correct, and — via the exact FCS API and in-memory-buffer semantics a
  real editor uses — appears and updates on re-check with **no `dotnet build` of the consumer**. The
  live-edit re-check was 47ms. This is a materially closer proxy to a live editor than Q001–Q005's
  virtual-file harnesses.
- **Not proven (boundary for the review):** this ran against `FSharpChecker` (the host *library*),
  not a literal Ionide/VS/Rider editor session. The validity precondition allowed "at minimum
  `dotnet fsi`/`fsc` loading the provider"; the FCS check API is arguably stronger than that, but it
  is still not a keystroke-level test inside a running editor with FSAC's caching and invalidation
  layers on top. Whether that gap keeps this at SHIP or drops it to REVISE is `03-review.md`'s call.
- **Design corrections surfaced (not patched silently):** (1) `typeof<Person>` as a `System.Type`
  static parameter is impossible — static arguments must be literal constants; replaced with a
  string type-name + reflection over referenced assemblies. (2) `UncheckedQuotations` was *not*
  needed for either the literal-quotation lens (Round 2) or the reflection-built lens (Round 3),
  contrary to the design's flagged risk — plain `Expr.PropertyGet`/`NewRecord`/`NewTuple`/`Lambda`
  spliced cleanly into generative members.
- **Structural boundary (per the hypothesis's requirement to name it):** the provider only reaches
  record types that are *already compiled in a referenced assembly*. Myriad's `[<Lenses>]` reads the
  attributed record from the same source file's untyped AST; a type provider structurally cannot see
  sibling source declarations. So this reaches the "generate lenses for a type that already exists in
  a referenced library" shape, not Myriad's actual same-file "annotate this record I'm writing now"
  workflow — the single biggest generalization limit of the result.

---

## Reproduction

From the scratch dir `tp-spike` (vendored `ProvidedTypes.fs`/`.fsi` already present):

```
# build the provider (Runtime ref triggers DesignTime build + colocates the TPDTC)
dotnet build TrivialTP.Runtime/TrivialTP.Runtime.fsproj -c Release

# Round 1 + typecheck of Rounds 2/3 (no consumer build; FCS-only visibility test)
dotnet build Harness/Harness.fsproj -c Release
dotnet run --project Harness/Harness.fsproj -c Release --no-build all
#   argv: "all" | "r2" | "r3"  (each also runs Round 1 first)

# runtime-correctness of Rounds 2 and 3 (the only real compile+run of a consumer)
dotnet run --project R2Runtime/R2Runtime.fsproj -c Release
dotnet run --project R3Runtime/R3Runtime.fsproj -c Release
```

Scratch layout: `TrivialTP.Runtime/` (TPRTC + `Point`/`Person` records), `TrivialTP.DesignTime/`
(`TrivialTP.Provider.fs` — all four providers: `GenerativeSchema`, `LensPoC`, the failed
`Lenses<System.Type>`, the working `LensesByName<string>`), `Harness/Program.fs` (FCS host + three
rounds), `R2Runtime/`, `R3Runtime/`. Per Q001–Q003 precedent, only this write-up is durable; the
scratch projects are not committed.
