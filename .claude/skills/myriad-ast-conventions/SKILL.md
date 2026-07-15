---
name: myriad-ast-conventions
description: AST-construction conventions for Myriad's built-in generators (Fantomas.FCS.Syntax vs FSharp.Compiler.Service boundary, GeneratorHelpers idioms, config lookup pattern). Apply automatically when writing or reviewing code in src/Myriad.Plugins or src/Myriad.Core.
user-invocable: false
---

Background knowledge for working on Myriad's code generators. The failure mode that matters
here isn't "code doesn't compile" — it's a generator that produces F# which compiles but is
subtly wrong (bad range, wrong case order, silently non-idempotent output). Consistency with
existing generators is the main defense against that.

## Two AST worlds, don't mix them up

- **Reading input**: `Myriad.Core.Ast` (`src/Myriad.Core/Ast.fs`), built on
  `FSharp.Compiler.Service`, parses the user's source file and extracts typed declarations
  (`Ast.extractDU`, `Ast.extractTypeDefn`, `Ast.hasAttribute<'A>`, `Ast.getAttribute<'A>`).
- **Writing output**: `Fantomas.FCS.Syntax` (`SynExpr`, `SynPat`, `SynModuleDecl`,
  `SynTypeDefn`, …) is used to *build* the generated AST, which Fantomas.Core then pretty-prints
  back to source text. Don't hand-construct output as raw strings — build it as `SynExpr`/
  `SynModuleDecl` nodes like the existing generators do, so formatting stays consistent and
  Fantomas can reformat it safely.

## Shared idioms — use `GeneratorHelpers`, don't re-derive them

`src/Myriad.Plugins/GeneratorHelpers.fs` exists specifically because three generators used to
duplicate the same destructuring and pipeline code (see the commit that extracted
`getCaseIdent` to eliminate a 5-fold `SynUnionCase` destructure — that's the shape of bug this
file prevents). Before writing a new AST helper, check whether one of these already covers it:

- `getCaseIdent` — extract the `Ident` from a `SynUnionCase`.
- `resolveCaseIdent` — fully-qualify a DU case name when `RequireQualifiedAccess` applies.
- `createTypedNamedParen` — build a `(name: typ)` parenthesised typed pattern.
- `parseInputAst` — parse the generator's input file.
- `filterByAttribute<'A>` — keep only namespaced types decorated with a given attribute.
- `generateModules<'Attr>` — the standard parse → extract → filter → build-per-type pipeline
  used by `DUCasesGenerator` and `FieldsGenerator`.

If a new generator needs a variant of one of these, extend the helper rather than copy-pasting
it into the new generator file — that's exactly the duplication this module was created to
avoid.

## Range handling

Synthetic AST nodes (ones with no real source position, because they're generated) use
`range0` from `Fantomas.FCS.Text.Range`. Don't invent positions or reuse the input file's
ranges for generated nodes.

## Config lookup pattern

Attributes carry a `configGroup: string`. `Generator.getConfigFromAttribute<'a>` resolves that
string against the `configGetter` (backed by the relevant `myriad.toml` section) into a
`(string * obj) seq`. Read individual values with `GeneratorConfig.tryGet<'T>` or
`GeneratorConfig.getOrDefault<'T>` — don't pattern-match the seq directly, the helpers already
handle the missing-key case.

## Naming convention

New generator files are `<Name>Generator.fs`, with an `internal module Create<Name>Module` for
the AST-building logic and a public `[<MyriadGenerator("configKey")>] type <Name>Generator()`
for the registration. See [[new-generator]] for the full scaffold.
