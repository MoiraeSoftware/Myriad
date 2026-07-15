# Q002-typed-nested-dispatch / Movement 4 — Adversarial review

## Three strongest objections

1. **RESOLVED (addendum, same day) — Round C was built for real.** Using Myriad's own
   `Ast.typeNameMatches`/`hasAttribute`/`extractRecords` (copied verbatim, not a reimplementation
   shaped to be easy to beat), a naive cross-file resolver gets the unambiguous case right, then
   fails the adversarial one (two same-named records in different modules) in a way that's
   measured, not argued: the identical resolver, given the identical three files in a different
   *order*, flips between the correct answer and a silently wrong one. That's a real correctness
   bug class, not a cost argument — the original framing ("materially more machinery" as an
   engineering-cost claim) undersold it. Parsing itself was cheap (197ms, cheaper than the typed
   path's cold start). What's missing is real `open`/scope resolution, not caching. See
   `02-results.md`'s Round C addendum. This objection no longer stands as written; folded into the
   verdict below.

2. **One level of nesting, two files, a synthetic example.** Real nested-generation cases (a
   three-level JSON tree, a DU case whose field is itself a DU, a diamond where two sibling
   fields reference the same nested type) weren't tried. Nothing here tests what happens with a
   genuine cycle (A references B references A) — F# forbids that for non-recursive record/DU
   definitions without `and`, but a generator needs to handle the case gracefully rather than
   stack-overflow on a recursive `describe` call, and this was never exercised.

3. **The generator itself used string templates, the same simplification Q001 used, not Myriad's
   real AST-builder approach** (`SynExpr`/`SynModuleDecl` construction via `Fantomas.FCS.Syntax`,
   the way `FieldsGenerator.fs`/`LensesGenerator.fs`/`DUCasesGenerator.fs` actually work). The bug
   hit and fixed in Round B — a missing paren turning one applied call into two curried arguments
   — is exactly the class of error Myriad's AST-builder approach is structurally immune to, since
   you can't accidentally construct a syntactically-wrong `SynExpr` the way you can a wrong string.
   This quartet's win is entirely on the typed-**input** side. Typed/safe **output** construction —
   one of the pillars named as untested back in Q001 — is still completely untested, and this
   round's own bug is a small, concrete demonstration of why it would matter.

## Verdict

**SHIP the capability claim — now measured, not just argued.** For the specific class of generator
that needs to know whether one field's type is itself another attributed type — nested
serializers, nested lenses, nested validators, recursive structural generators — typed access
resolves this correctly across files with a single property access, and the current Myriad plugin
API has no comparable built-in facility (verified from source: `GeneratorContext`/
`GeneratorHelpers.generateModules` both scope to one file). The syntax-only alternative was built,
not just described, and it fails in a way a generator author could easily ship without noticing:
same input, different file-processing order, silently different (wrong) output. This is the first
positive (non-null) capability result in this line of spikes, and it directly closes the gap Q001
left open — with real evidence behind both halves of the comparison now, not one.

Scope it precisely, though, because two things travel with the "ship":

- **It required both Q001 pillars together, not typed access alone.** The capability depends on
  the whole-project in-process hosting model as much as on typed field resolution — bolting typed
  lookups onto Myriad's *current* per-file invocation model (`GeneratorContext.InputFilename`,
  singular) would not reproduce this result without also changing that model to a whole-project
  view. That's a real architecture change to Myriad's plugin invocation, not a drop-in enhancement,
  and its cost hasn't been scoped by this quartet.
- **Output-side safety is still untested**, and this round's own bug (unparenthesized nested call)
  is a small live example of the exact failure class a typed/AST-safe output layer would prevent.

Next spike this motivates, if the frontier keeps moving: pivot to the still-untouched pillar —
typed output construction (quotations or an AST-builder DSL) instead of string templates, using
this same nested-dispatch generator as the test case so the "no more paren bugs" claim gets
checked the same rigorous way this quartet now checked the input side. A smaller follow-on, if
useful: try the "respect the caller's `open` list" fix hinted at in the Round C addendum, to see
whether it's a small patch or the start of reimplementing real scope resolution.
