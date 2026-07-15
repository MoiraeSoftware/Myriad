# Q006-myriad-as-type-provider / Movement 1 — Hypothesis

**Status:** PLANNED. Pre-registered only — not yet built. Awaiting go-ahead.
**Date:** 2026-07-14

**Question:** Myriad's single biggest named engineering gap is that generated code is invisible to
the IDE until a real `dotnet build` runs (`BACKLOG.md`, known gap 1 — `MyriadSdkGenerateCode` is
gated `Condition="'$(DesignTimeBuild)' != 'true'"`, confirmed from
`src/Myriad.Sdk/build/Myriad.Sdk.targets`). Can one of Myriad's real, shipping generators be
reimplemented as a **generative** (`isErased = false`) F# type provider, using the
`FSharp.TypeProviders.SDK` (`ProvidedTypes.fs`, checked out at commit `0a95768`), so that its
output appears live in Ionide/FSAC with zero build step — reusing host-integration machinery that
Visual Studio, Rider, and Ionide already implement for `ITypeProvider`, rather than needing FSAC to
grow a bespoke hook for Myriad specifically? And where exactly does this stop working for the rest
of Myriad's generator surface?

## Why this is the right next spike

Every prior quartet (Q001–Q005) worked inside a private `FSharpChecker` harness — none of them ever
ran against a real host process, so none could actually test whether the design-time-invisibility
gap is fixable, only reason about it from source. This is the first quartet whose payoff is only
real if verified against an actual IDE (Ionide/FSAC), not a virtual project in a test harness. It's
also the first quartet whose mechanism is independent of Q001's TransparentCompiler pillar
entirely: type providers are a *native* FCS/compiler hosting protocol, not something built on the
in-process-FCS-hosting foundation Q001–Q005 constructed. If this works, it doesn't extend that
foundation, it sidesteps it — worth stating plainly since every prior quartet's mechanism section
assumed Q001's harness as a starting point and this one doesn't.

## Novelty gate

Checked `FSharp.TypeProviders.SDK/src/ProvidedTypes.fs` directly, not from memory:

- Generative (non-erased) providers build real IL types with full metadata and PDB support
  (`generatePdb` machinery from roughly line 10250 onward) via `ProvidedTypeDefinition(..., isErased
  = false)` plus a `ProvidedAssembly()` (`docs/providing-types.md:85-119`) — these are debuggable,
  browsable .NET types, not opaque erased shims.
- `ITypeProvider.Invalidate` is a real, implemented event (`ProvidedTypes.fs:16271-16287`) — the
  live-refresh signal the design-time-invisibility fix would need, already wired into every host
  that implements the `ITypeProvider` protocol.
- Member bodies are specified as F# quotations (`getterCode`/`invokeCode` taking `<@@ ... @@>`),
  not string templates — a different, independently mature answer to `BACKLOG.md` spike-shaped item
  3 ("typed/AST-safe output construction"), worth weighing against the `SynExpr`-builder option
  that item currently assumes is the only path.
- Checked `src/Myriad.Core/Ast.fs`: confirmed (again, as in Q004/Q005) zero use of
  `FSharpChecker`, `ITypeProvider`, or any FCS-hosting API anywhere in current Myriad. No overlap
  with any closed or planned quartet.

Also corrects an assumption from this session's own prior turn: it was claimed that "most of
Myriad's actual generator surface augments an existing user type in place," which would rule out
the type-provider approach structurally (providers can only add types under a synthetic namespace,
never inject members into a type the user wrote). Reading the three built-in generators directly
shows this was wrong for all of them: `LensesGenerator.fs:139-172` and `FieldsGenerator.fs:139-142`
both emit a **standalone nested module** (`SynModuleDecl.CreateNestedModule` under a fresh
`SynModuleOrNamespace.CreateNamespace`), not an augmentation of the original type — `PersonLenses`
sits alongside `Person`, it does not extend it. `DUCasesGenerator.fs` was not checked in this pass
and should not be assumed either way. This correction changes the shape of the spike: the
type-provider-shaped subset of Myriad's real generator surface may be larger than assumed, not a
rare exception.

## Contradiction gate

Does not contradict Q001–Q005. Q001's REVISE verdict (typed access alone doesn't earn its keep for
a structural-echo generator) is a caution worth carrying over by analogy, not a blocker: the claim
here isn't about typed *input* access at all, it's about live *output* visibility, an orthogonal
axis. Does not depend on or extend the FSI/comptime lineage (Q003 + backlog item 1) — a generative
type provider's members can be built from ordinary syntax-tree data the same way Myriad's
generators already work today; FSI evaluation is a separate, composable concern, not a
precondition.

## Validity preconditions

- Reproduce against the actual shipped `ProvidedTypes.fs` API from this checkout (commit
  `0a95768a2247daba80b24a2604f77f89fc88ff1f`), following the vendoring convention the SDK itself
  documents (`docs/technical-notes.md:30`: the design-time component "includes the
  ProvidedTypes.fs/fsi files from the type provider SDK") — not a hand-rolled `ITypeProvider`
  sketched from the docs alone.
- Must port a **real** Myriad generator's actual field-iteration and naming logic, not a toy
  provider invented to flatter the hypothesis. `LensesGenerator.fs`'s record-lens case is the
  concrete candidate: confirmed standalone-module output (see novelty gate), no cross-file/typed
  resolution dependency (unlike Q002's nested-dispatch case), and a genuinely representative
  generator, not a strawman chosen because it's trivial.
- The payoff claim ("appears in the IDE without a build") is only meaningfully tested against a
  real host — Ionide/FSAC in an actual editor session, or at minimum `dotnet fsi`/`fsc` loading the
  provider the way `docs/guide.md:468` describes real hosts doing it. A result confined to the
  private `FSharpChecker` harness Q001–Q005 used would not test the actual claim, only the
  mechanism's existence.
- Must explicitly scope out (not silently ignore) any Myriad generator shape that augments an
  existing user type rather than adding a standalone one — record it as a named boundary in the
  review, since it bounds how much of Myriad's real surface this result generalizes to.

## Cheapest falsifier

Two-stage, cheapest first:

1. **Does the SDK's own minimal generative-provider example (`docs/providing-types.md:85-119`)
   actually appear live in Ionide with no `dotnet build`, before any Myriad-specific work happens?**
   This tests the entire claimed payoff — IDE visibility without a build — using code that's
   already known to work against the SDK's own test suite. If it doesn't show up live in a real
   editor session (stale caching, FSAC not re-invoking the provider, etc.), the rest of this
   quartet is moot and nothing about Myriad needs to be touched to find that out.
2. **Can the Lenses shape — a getter/setter tuple exposed as a named value under a container —
   even be expressed as generative provided members?** Static properties returning function values
   are a plausible fit but untested here; confirm before porting `LensesGenerator.fs`'s full field
   loop and naming logic.

## Pre-registered decision thresholds

- **SHIP:** falsifier 1 passes (live IDE visibility confirmed against a real host, not just the
  test harness), the Lenses generator is faithfully ported and produces the same lens values a
  real user of the existing `[<Lenses>]` attribute would recognize, and it appears in Ionide
  without a build. Verdict would explicitly name the boundary: which shape of Myriad generator
  (standalone-module output) this reaches, and which shape (in-place augmentation, if any built-in
  or common third-party generator turns out to need it) it structurally cannot.
- **REVISE:** the mechanism works but the payoff is weaker than claimed — e.g. IDE visibility
  requires a project restart or explicit "reload" action rather than true live-as-you-type
  refresh, or works in Ionide but not in a tested second host. Still real, narrower than pitched.
- **NULL:** live IDE visibility works for the SDK's own trivial example but the specific shape
  Myriad generators need (dynamic field count driven by parsing an attributed record, not a fixed
  static-parameter count) hits an SDK limitation that makes the port impractical without
  substantial additional machinery — meaning the *idea* is sound but this generator class doesn't
  cleanly fit the SDK's model.
- **KILL:** falsifier 1 itself fails — a real host doesn't show generative provider types live
  without a build either, which would mean the entire premise (type providers already solve the
  IDE-visibility problem Myriad has) is false, and the gap named in `BACKLOG.md` is harder than a
  single quartet's scope regardless of which mechanism is used to attack it.
