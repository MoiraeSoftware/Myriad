# Q002-typed-nested-dispatch / Movement 1 — Hypothesis

**Status:** PLANNED. Pre-registered only — not yet built. Movement 1 is the free gate; this spike
should not be run until this hypothesis, its novelty/contradiction check, and its cheapest
falsifier are worth the effort. Awaiting go-ahead.
**Date:** 2026-07-14

**Question:** Q001 tested whether typed access changes anything for a generator Myriad already
ships (Fields) and got a clean null: it doesn't, because Fields is pure structural echo and
syntax-echo is already correct for that. Q001's review named the actual open claim: typed access
should matter for a generator that needs **real semantic reasoning**, not just field names and
arities. Does it? Specifically: can a generator correctly detect that one field's type is itself
*another* Myriad-attributed type (so it needs nested/recursive generation) using typed symbol
resolution — something a syntax-only generator cannot do without re-implementing a chunk of name
resolution itself?

## The claim

Given:

```fsharp
[<Fields("fields")>]
type Address = { Street: string; City: string }

[<Fields("fields")>]
type Person = { Name: string; HomeAddress: Address }
```

A generator dispatching per-field on "is this a primitive or another attributed type" (the shape
every nested serializer, nested lens-composition, or nested validator generator needs) can, via
the typed tree, resolve `HomeAddress`'s `FSharpType` to the actual `Address` `FSharpEntity` and
check `.Attributes` directly — one call, no cross-file search. A syntax-only generator sees only
the token `Address` in `SynField`'s `SynType`; to answer "is `Address` itself attributed" it would
have to locate `Address`'s declaration (possibly in another file), parse its attribute list, and do
this for every field of every type it processes — effectively hand-rolling the symbol resolution
the typed tree gives for free. Myriad's `Ast` module (checked as part of this pre-registration)
does not currently do any cross-declaration lookup at all; every existing generator (`Fields`,
`Lenses`, `DUCases`) restricts itself to information local to the one type being processed. **This
is precisely the case Q001 didn't test and where the mechanism validated in Q001 should earn its
keep — or fail to.**

## Novelty gate

Checked `src/Myriad.Plugins/FieldsGenerator.fs`, `LensesGenerator.fs`, `DUCasesGenerator.fs`
directly (not from memory): all three re-emit each field's `SynType` verbatim and never inspect
what that type resolves to. `DUCasesGenerator` goes further — it reduces every case with fields to
a single `hasFields: bool` and wildcards the pattern, discarding field-type information entirely.
No existing Myriad generator, built-in or found in the README's external-plugins list, does
cross-type attribute lookup. Not a re-tread.

## Contradiction gate

Does not contradict Q001's verdict — it directly extends the open question Q001's review left
unresolved ("typed access matters for generators needing real semantic reasoning, untested"),
rather than re-asserting the falsified claim ("typed access matters for structural-echo
generators").

## Validity preconditions

- Must run against FCS `43.9.101` (Myriad's pin), same as Q001, for the same reason: an
  architecture claim tested on a version Myriad doesn't use proves nothing about Myriad.
- The "nested" case must be genuinely cross-file (two virtual files, not two types in one file) —
  a same-file test would understate the syntax-only difficulty, since a syntax tool at least has
  the other declaration's AST in hand without a file-system search.
- Must attempt the syntax-only equivalent far enough to state concretely what it would require
  (even if not fully implemented), so the comparison isn't "typed access exists" vs a strawman
  that was never seriously attempted.

## Cheapest falsifier

Before building the full nested-dispatch generator: can `FSharpField.FieldType` even be resolved
to the *other* file's `FSharpEntity` when that file is virtual (`DocumentSource.Custom`) and part
of the same `FSharpProjectOptions.SourceFiles` array, the way Q001 already proved works for a
single dependency pair? If cross-file symbol resolution silently fails to reach a *second* virtual
file's entity (as opposed to just typechecking against it, which Q001 already showed works), the
capability claim dies immediately, cheaply, before any generator logic is written. Run this check
first.

## Pre-registered decision thresholds

- **SHIP (capability confirmed):** the generator correctly identifies `HomeAddress: Address` as
  needing nested dispatch (by resolving to `Address`'s `FSharpEntity` and finding the `Fields`
  attribute on it) and correctly falls back to primitive handling for `Street: string`, and doing
  the equivalent via syntax-only inspection is shown to require materially more machinery (explicit
  cross-file lookup Myriad's `Ast` module doesn't have today).
- **NULL (like Q001):** typed access resolves it, but a cheap syntax-only trick (e.g., a naming
  convention, or a registry the generator host already builds for other reasons) gets there almost
  as easily — meaning the capability claim still doesn't clearly earn its complexity cost for this
  case either.
- **KILL:** cross-file resolution to a second virtual file's entity doesn't work at all (falsifier
  above fails) — this would also partially undercut Q001's mechanism verdict, not just this
  spike's capability claim, since Q001 never actually tested resolving *into* a second file's
  entities, only typechecking *against* them.
