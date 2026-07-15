# myriad-fcs-typed-codegen / Movement 3 — Execute + write up

**Status:** DONE, all four rounds run to completion, 2026-07-14.

## Round 1 — mechanism + alias resolution

Both files typechecked with zero diagnostics. `Person.Age`, declared as alias `Id`, printed via
plain `.Format(ctx)` as `A.Id` (alias NOT resolved by default — a correction to the initial API
sketch, which had assumed it would be). Only `.StripAbbreviations().Format(ctx)` resolved it to
`System.Int32`. `A.fs` never existed on disk at any point (virtual path `C:\virt\A.fs`).

**Result: mechanism claim round-1 PASS**, with one correction to the working assumption
(resolution requires an explicit `StripAbbreviations()` call, it is not automatic).

## Round 2 — incrementality and staleness

| step | version bump | result |
|---|---|---|
| cold check A | v0 | 937ms, OK |
| cold check B | v0 | 149ms, OK |
| re-check both unchanged | same/bumped | 0-1ms, correct |
| mutate B only, re-check | B bumped | 3ms, correct |
| **break A (remove field B reads), re-check B, B's version UNCHANGED** | — | **0ms, WRONG (stale success)** |
| break A, re-check B, **B's version bumped too** (content identical) | — | **0ms, STILL WRONG** |
| break A, `checker.InvalidateConfiguration(opts)`, re-check B | — | **80ms, CORRECT** (real error surfaced) |

**Result: neither party's version-bump parameter propagates a dependency's breaking change.**
`InvalidateConfiguration` is the only thing that worked, and it wasn't in the original API sketch —
this is a genuine, easy-to-miss correctness trap the pre-registration didn't anticipate.

## Round 3 — scaling, BackgroundCompiler vs TransparentCompiler

| checker | padCount | initial warm | recheck after break | needed explicit invalidate? |
|---|---|---|---|---|
| BackgroundCompiler | 2 | 1010ms | 88ms | yes (required) |
| BackgroundCompiler | 60 | 203ms | 54ms | yes (required) |
| TransparentCompiler | 2 | 711ms | **5ms** | **no — auto-detected** |
| TransparentCompiler | 2 | 154ms | 4ms | yes (also correct) |
| TransparentCompiler | 60 | 652ms | 7ms | yes (also correct) |

TransparentCompiler correctly caught the break **without any explicit invalidation call** —
apparently content/snapshot-keyed rather than relying on caller-tracked dependency invalidation.
Cost stayed low and flat from N=2 to N=60 padding files, where BackgroundCompiler's did not show
clean growth either — largely because the padding files were too cheap to stress the "recompute
everything preceding the target file" cost a source-level review (separately, via an independent
Opus-model agent reading `BackgroundCompiler.fs`/`IncrementalBuild.fs`) predicted for that path.
**This round did not decisively confirm or refute the BackgroundCompiler O(n) prediction** — the
padding files were too cheap. It did decisively show TransparentCompiler removing the manual
invalidation requirement, which the source-review agent could not have told us without the
code actually being run.

Follow-up maturity check (separate agent, sourced from FSAC's `Parser.fs`/`CompilerServiceInterface.fs`
and FCS release notes): TransparentCompiler is opt-in in FSAC, default `false`, and both FCS and
FSAC still label it "experimental" as of the 43.9.x line. Known open issues are narrow
(signature/implementation diagnostic mismatches, one flaky fuzz test) and don't touch the path
exercised here. Verdict: usable for a prototype, not yet something Ionide itself defaults to.

## Round 4 — Fields generator port

Source: `Example.Person = { Name: string; Age: Id; Nick: string option }`, `Id = int` alias,
`[<Fields("fields")>]` on `Person` (a locally-declared attribute type standing in for Myriad's real
one — attribute-argument plumbing wasn't ported, only entity-level discovery).

Generated output (typechecked with zero diagnostics, never written to disk):

```fsharp
namespace rec TestFields

module Person =
    open Example

    let Name (x : Person) = x.Name
    let Age (x : Person) = x.Age
    let Nick (x : Person) = x.Nick

    let create (name : string) (age : Id) (nick : string option) : Person =
        { Name = name; Age = age; Nick = nick }

    let map (mapName : string -> string) (mapAge : Id -> Id) (mapNick : string option -> string option) (record': Person) =
      { record' with
          Name = mapName record'.Name
          Age = mapAge record'.Age
          Nick = mapNick record'.Nick }
```

This matches the shape Myriad's real `Create.createRecordModule` produces (accessor per field,
`create`, `map`), modulo naming-convention edge cases (e.g. `Ast.Ident.asCamelCase`'s exact
behavior on unusual identifiers) not re-implemented.

**The direct comparison that was the point of this round:**

```
Age field type via typed access, alias-preserving format: Id
Age field type via typed access, alias-STRIPPED format:   System.Int32
```

The generator used the alias-preserving format (`Id`) — because that is what a correct, idiomatic
generator should emit here. That is **exactly** what Myriad's current syntax-echo approach already
produces for this generator, since Myriad's `createCreate`/`createMap` just re-emit the source
`SynType` verbatim (`Id`), and that's always syntactically valid given the same `open`. Typed
access was available, correct, and unused by the better choice — for this generator, resolving
the alias would have made the output *worse* (less idiomatic), not better.

**Result: capability claim round-4 — NULL for this generator.** Typed access did not change or
improve the output for Fields. The mechanism worked end to end (real generator, real typecheck,
zero disk writes), but the motivating "typed beats syntax" advantage did not manifest here.
