# Q004-cross-assembly-typed-access / Movement 1 — Hypothesis

**Status:** PLANNED. Pre-registered only — not yet built. Awaiting go-ahead.
**Date:** 2026-07-14

**Question:** Every quartet so far (Q001/Q002) resolved typed information about types that have a
source-file `SynTypeDefn` somewhere in the virtual project — Q002's nested-dispatch case crossed
*files*, never crossed the boundary into a *compiled, source-absent* assembly. Can a generator use
`FSharpChecker`'s typed access to read the shape (fields, member signatures, F#-specific
classification like record/union/measure) of a type defined only in a **referenced compiled
assembly** — a NuGet package, or a separate project's output DLL — for which Myriad has no AST at
all? And if so, does that require FCS specifically, or does plain .NET reflection (plus
`Microsoft.FSharp.Reflection.FSharpType`) already answer the same question more cheaply, making the
Q001–Q003 typed-hosting foundation unnecessary for this particular capability?

## Why this is the right next spike

This is the one capability class none of Q001–Q003 or the current backlog touch: today's Myriad is
*structurally* incapable of reacting to a type it can't parse from source, because `Ast.fs` only
ever walks a `SynTypeDefn` obtained from parsing an attributed `.fs` file (confirmed by reading
`src/Myriad.Core/Ast.fs` directly — no `FSharpChecker`, no project-reference walking, no assembly
loading of user types anywhere in it; the module's only reflection use, `DynamicReflection` at
`Ast.fs:11`, is a dynamic-invocation shim used once, at `Ast.fs:404`, to load `Fantomas.FCS`
reflectively for a version-compat shim — unrelated to reading user-authored types). A generator
that wants to emit a mapper, validator, or serializer for a DTO defined in a shared contracts
assembly the user doesn't own the source of cannot be written against current Myriad at all, not
even badly. Every other quartet so far has been "typed access does the same job better or more
robustly than syntax"; this is the first candidate for "typed access does a job syntax cannot
attempt," which is the actual bar for "master of the universe"-grade capability rather than
incremental quality.

## Novelty gate

Checked `src/Myriad.Core/Ast.fs` in full: confirmed no cross-assembly or reflection-based
type-shape reading exists anywhere in current Myriad. Not covered by Q001 (single dependent virtual
file) or Q002 (cross-*file*, still all virtual source with `DocumentSource.Custom`, still all
carrying real `SynTypeDefn`s). Not in the current backlog list in `BACKLOG.md`. Not a re-tread.

## Contradiction gate

Does not contradict Q001–Q003's verdicts. It does put direct pressure on the framing behind all
three: Q001's REVISE verdict already established that "typed access" is not automatically valuable
just because it's typed — it has to be compared honestly against what a cheaper mechanism could do.
Here the cheaper mechanism isn't syntax (which structurally cannot do this at all) but **plain
reflection**, which is a live, serious alternative hypothesis for this specific case and must be
tested side by side, not waved off. If reflection alone solves this, the honest verdict is that
this capability needed neither Q001's in-process hosting nor its typed-tree access — it needed
`System.Reflection`, full stop, and could theoretically be added to Myriad today with none of the
FCS-hosting machinery.

## Validity preconditions

- FCS pinned to `43.9.101`, matching `Myriad/paket.lock`, same as every prior quartet.
- The referenced assembly must be a genuinely separate, independently compiled artifact (its own
  `dotnet build` producing a `.dll`, not another virtual in-memory file in the same
  `FSharpProjectOptions.SourceFiles` array) — otherwise this collapses into a restatement of Q002.
- Must test at least one case reflection is plausibly weak at, not only cases reflection handles
  fine (e.g., a record with a private/internal field, or an F# union with results that require
  SRTP-adjacent inference), so the FCS-vs-reflection comparison isn't decided on the easy case.
- Must actually attempt the reflection-only path with real code (not asserted from memory) before
  concluding FCS earns its keep — same discipline Q002 applied to the syntax-only alternative.

## Cheapest falsifier

Two falsifiers, cheapest first:

1. **Does reflection already solve this trivially?** Write a throwaway record type in a separate
   compiled DLL, load it with `System.Reflection.Assembly.LoadFrom`, and check whether
   `Microsoft.FSharp.Reflection.FSharpType.GetRecordFields` returns full field name/type
   information with no FCS involved at all. If yes for the easy case, this is the finding to build
   the rest of the spike around — the real question moves from "can Myriad see cross-assembly
   types" (probably yes, cheaply) to "is there any case where FCS's typed view gives something
   reflection's metadata view cannot."
2. **Can FCS resolve into a compiled, source-absent assembly at all?** Add a `-r:<path-to-dll>`
   style reference to `FSharpProjectOptions.OtherOptions` for a virtual project (extending Q001's
   harness) whose one virtual source file uses a type from that DLL as a record field, and check
   whether `FSharpField.FieldType` resolves to a usable `FSharpEntity` for the external type (not
   just an opaque/abbreviated type reference). If FCS only exposes name-level information for
   out-of-project entities, the FCS side of the capability claim dies immediately.

## Pre-registered decision thresholds

- **SHIP (capability confirmed, FCS earns its keep):** FCS resolves usable shape data for the
  compiled-assembly type on a case reflection handles poorly or not at all (e.g., needs
  SRTP-resolved member signatures, or F#-specific classification reflection's `FSharpType` module
  gets wrong/incomplete for the test case) — and a generator built on it produces correct output a
  reflection-only generator could not.
- **NULL (like Q001):** FCS resolves it, but reflection plus `FSharpType` gets equivalent
  information for every case tested, at a fraction of the hosting cost (no `FSharpChecker`, no
  virtual project, no `TransparentCompiler`) — meaning this capability should ship as a
  reflection-based Myriad feature, not a typed-FCS-hosting one, and the "master of the universe"
  framing for this specific idea was oversold.
- **KILL:** FCS cannot resolve into compiled, source-absent assemblies in any useful way (falsifier
  2 fails) — narrows the whole typed-hosting foundation's reach to source-visible types only,
  worth recording explicitly since it bounds every future capability claim built on Q001–Q003.
