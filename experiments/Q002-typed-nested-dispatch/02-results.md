# Q002-typed-nested-dispatch / Movement 3 — Execute + write up

**Status:** DONE, 2026-07-14. One bug hit and fixed during the run — recorded below rather than
silently corrected, per the standard set in Q001.

## Round A — cheapest falsifier: PASS

```
HomeAddress field type (as seen from Person.fs): Address.Address
HasTypeDefinition: true
Resolved TypeDefinition.DisplayName: Address
Resolved TypeDefinition.DeclarationLocation.FileName: C:\virt\Address.fs
Resolution reached a DIFFERENT virtual file than Person.fs: true (expected true)
Resolved cross-file entity carries the Fields attribute: true (expected true)
```

Cross-file typed resolution works exactly as hoped: from `Person.fs`'s checked results, the
`HomeAddress` field's type resolves to the *actual* `Address` entity declared in the separate
virtual file `Address.fs`, and that entity's attribute list is readable directly. No cross-file
search, no path-walking, one property access (`FieldType.TypeDefinition`).

## Round B — generator build: PASS, after one self-inflicted bug

First attempt failed with two typecheck errors in the generated code:

```
(11,17)-(11,42) typecheck error Type mismatch. Expecting a 'string -> string -> 'a -> string' but given a 'string -> string -> string'
(11,65)-(11,81) typecheck error This expression was expected to have type 'string' but here has type 'Address.Address -> string'
```

Cause: the generator's own string template emitted `Address.describe x.HomeAddress` as a bare
sprintf argument — F# parsed that as two separate curried arguments (`Address.describe` and
`x.HomeAddress`) instead of one applied call. Needed `(Address.describe x.HomeAddress)`. This is
exactly the class of bug Myriad's current AST-construction approach (building `SynExpr` nodes
directly rather than string-templating) is structurally immune to — worth noting honestly since
this spike's generator used string templates for simplicity, the way Q001's did, not the
AST-builder approach Myriad itself uses. Fixed and reran; final generated output:

```fsharp
namespace rec TestDescribe

module Address =
    open Address
    let describe (x: Address) : string =
        sprintf "Street=%s; City=%s" (sprintf "%O" x.Street) (sprintf "%O" x.City)

module Person =
    open Person
    let describe (x: Person) : string =
        sprintf "Name=%s; HomeAddress=%s" (sprintf "%O" x.Name) (Address.describe x.HomeAddress)
```

`Generated.fs` diagnostics: `[||]` — typechecks cleanly, zero disk writes, `Person.describe`
correctly recurses into `Address.describe` for the nested field and formats the primitive field
directly. **Round B PASS: the capability claim holds for this case.**

## Round C — syntax-only comparison, built for real (addendum, same day)

The first pass at this quartet reasoned Round C from source rather than building it — flagged in
`03-review.md` as the weakest part of the close-out. Built it for real afterward:
`artifacts/round-c-syntax-resolver/`, using Myriad's own parsing and attribute-matching code
(`typeNameMatches`, `hasAttribute`, `extractRecords`, etc.), copied verbatim from
`src/Myriad.Core/Ast.fs` and credited inline, not a reimplementation shaped to be easy to beat.

**Scenario 1 (unambiguous, mirrors Round A/B's setup):** one `Address` record, one file. The
naive simple-name search finds exactly one candidate and correctly reports it as
Fields-attributed. 197ms elapsed (cold Fantomas parse of two small files — no per-lookup cost
beyond that, so wall-clock-wise this is actually *cheaper* than Q001's typed cold-start, ~937ms;
speed was never the weak point of syntax-only, see the finding below).

**Scenario 2 (adversarial, the real test): FAILS, measured, not hypothesized.** Two different
modules (`Domain1`, `Domain2`) each declare a record named `Address` — only `Domain1`'s carries
the `Fields` attribute. `Person.fs` does `open Domain1`, so the *only* correct answer is
`Domain1.Address`. Running the identical naive resolver twice, changing nothing but the order the
three files were handed to it:

```
File order [Domain1; Domain2; Person]: picks Domain1.fs -> Fields-attributed: true   (correct)
File order [Domain2; Domain1; Person]: picks Domain2.fs -> Fields-attributed: false  (WRONG)
```

Same input, same field, same `open` declaration — a different answer, entirely because of file
list order, which has nothing to do with F#'s actual scoping rules. A generator built on this
naive resolver would silently emit the wrong branch (primitive formatting instead of nested
dispatch, or vice versa) depending on build-order happenstance, with no error raised anywhere.

**Correction to the original reasoning:** the "materially more machinery" claim was originally
framed around engineering *cost* (no caching, reimplemented per plugin). The measured result shows
the sharper problem is **correctness**, not cost — parsing is actually cheap (197ms, no worse than
the typed path's cold start). What's missing is real scope/`open` resolution, which is not a
caching problem, it's reimplementing a meaningful slice of the compiler's name binder. Fixing
*this specific* scenario (respect `open Domain1`, prefer it over an unopened module) is plausible
without huge effort; F#'s full shadowing/precedence rules (multiple opens, module aliases, nested
modules, the `global` qualifier) are the part that would keep growing. That refinement was not
built — noted as a limitation of this addendum, not asserted as proven.

## An additional finding, not in the original pre-registration

The capability demonstrated here depends on **two** things together, not typed access alone:
typed field resolution, *and* having the whole project's files loaded into one
`FSharpProjectOptions` at once (this spike's in-process host, validated architecturally in Q001).
Myriad's current plugin API invokes a generator once per input file
(`GeneratorContext.InputFilename: string`, singular) — so even a hypothetical "typed-aware"
generator bolted onto Myriad's *current* invocation model, without also changing that model to a
whole-project view, would not gain this capability. The two Q001 pillars (typed access,
in-process whole-project hosting) are not separately optional for this class of generator; both
are load-bearing.
