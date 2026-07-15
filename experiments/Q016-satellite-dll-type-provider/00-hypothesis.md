# Q016-satellite-dll-type-provider / Movement 1 — Hypothesis

**Status:** RUNNING.
**Date:** 2026-07-16.
**Repo under test:** this repo, Thread 2 lineage (general type-provider headroom), but the payload
under test is Myriad's own real generator output for the first time in Thread 2 — every prior Thread 2
quartet (Q008/Q09/Q11/Q12/Q13) used synthetic verification-only providers, never Myriad's own code.
Promotes `BACKLOG.md` item 15 ("Type provider subsuming a Myriad-compiled satellite DLL — cross-project
case").

**Question:** Can a generative type provider (`isErased = false`) re-expose members that are *really
Myriad's own compiled generator output* — not hand-built `ProvidedTypes` members, not a synthetic
stand-in — by `Assembly.LoadFrom`-ing a satellite DLL that Myriad's real, unmodified CLI (`src/Myriad`)
produced and that was then compiled via `checker.Compile`, and does this work end-to-end for Myriad's
**cross-project** usage shape (attributed type in project A, consumer wants generated members in project
B which references A), fully sidestepping Q006's same-compilation wall?

## The claim

Two separable claims, kept apart the same way every prior quartet in this file has:

1. **Mechanism claim:** a generative provider's `DefineStaticParameters` instantiation function can
   `Assembly.LoadFrom` (or `Load(bytes)`, per Q011's file-lock lesson) an externally-produced satellite
   DLL and construct `ProvidedMethod`/`ProvidedProperty` members whose `invokeCode` reflection-forwards
   to the *real* compiled members on that DLL — not reimplementing their logic, just wrapping them — and
   invoking a provided member through the whole compiler pipeline (provider → generated IL → consumer
   code → runtime call) actually executes Myriad's real generated code and returns Myriad's real answer.
2. **Capability claim:** the satellite DLL is not a synthetic stand-in built to flatter the hypothesis —
   it is produced by literally running `src/Myriad`'s own CLI, unmodified, against a real attributed
   record (`[<Generator.Fields "fields">]`), taking its actual `FieldsGenerator` output text and
   compiling it together with the original attributed source. If the satellite DLL is regenerated (the
   record gets a new field, Myriad is rerun, the DLL is recompiled to the same path) and the provider's
   `Invalidate()` is called, a live-checking consumer picks up the new member set — with no
   Windows file-lock exception from the just-overwritten DLL, a named, real risk in `BACKLOG.md` item 15
   that has never been tested.

**Deliberately not attempted here, named so the review doesn't have to guess whether it was missed or
scoped out:** this is explicitly the **cross-project** case, item 15, not item 16 (same-project
pre-build scratch-DLL preview). Item 16 requires racing Myriad's own pre-build step against MSBuild's
design-time build and was named in the backlog as strictly weaker than Q010's already-shipped mechanism
for that same target — not worth building until item 15's cheaper cross-project case is settled. Also
not attempted: resolving the provider via `ParseAndCheckFileInProject` (the fast incremental API). Q012
and Q013 already closed that question for generative providers in this pinned environment — it never
resolves one, full stop — so this quartet uses `checker.Compile` throughout, the same choice Q011/Q012
made, and does not re-litigate Q012/Q013's verdict.

## Why this is the right next spike

Directly executes backlog item 15, which is itself the more direct, lower-risk half of the two
follow-ups (15/16) added after Q006's REVISE — item 15 was explicitly named cheaper because it does not
fight Q006's wall at all (a satellite DLL loaded via `Assembly.LoadFrom` is, by construction, an
already-compiled, externally-referenced artifact the moment a second provider instantiation resolves
it — exactly the shape Q008/Q09/Q11 already proved type providers handle comfortably). It is also the
first Thread 2 quartet to use a **real** Myriad generator as the payload rather than a synthetic
verification marker, closing a gap none of Q008/Q09/Q11/Q12/Q13 touched: everything shipped so far in
Thread 2 proves the *provenance-checking* pattern works, never that a provider can carry Myriad's actual
generated behavior.

## Novelty gate

Not covered by any closed quartet's verdict:
- Q006 hand-built `ProvidedTypes` members from Myriad's *untyped AST* directly inside the provider and
  hit the same-compilation wall trying to reach a same-project type. This quartet does not touch
  Myriad's AST at all — it lets Myriad's own compiler-facing CLI run to completion and loads the
  *compiled* result, sidestepping the wall by construction rather than trying to out-clever it.
  `BACKLOG.md` item 14 (erased, self-parsing provider) is a third, distinct mechanism (parses source
  itself inside `ApplyStaticArguments`, no compile step, no wall-sidestep needed) — not spiked, not this
  quartet.
- Q008/Q09/Q11 emit synthetic verification-only markers (`ProvenanceOk`, `CheckedFieldCount`, version
  tags) — never real per-field data accessors, a gap `BACKLOG.md`'s Q009 follow-up (4) explicitly names
  as still open. This quartet is the first to close it, using Myriad's real generator rather than a
  hand-written stand-in.
- Q012/Q013 establish the generative-vs-erased resolution axis and the compile-then-PC non-warming
  result. This quartet does not re-test either; it inherits both as settled and uses `checker.Compile`
  from the start, consistent with what Q011/Q012 already found necessary for generative providers.

## Contradiction gate

Does not contradict any prior verdict. Depends on, and is consistent with:
- Q006's wall (generative providers cannot see the in-progress compilation) — this quartet's whole
  design point is that a satellite DLL is never part of the in-progress compilation by the time the
  provider resolves it, so the wall is structurally irrelevant here, not defeated.
- Q012/Q013 (generative types resolve only via `checker.Compile`, never `ParseAndCheckFileInProject`) —
  used as a given, not re-tested.
- Q011's `Assembly.Load(bytes)` vs `Assembly.LoadFrom(path)` file-locking lesson (`LoadFrom` was
  observed to hold a Windows file lock for the process lifetime, blocking a subsequent rebuild) — this
  quartet's Round 2 is specifically designed to test whether that same risk applies here, since it is a
  named, unresolved risk in `BACKLOG.md` item 15's own text ("the satellite DLL will be rewritten by
  Myriad while a live host may still hold it loaded — untested, and a real risk `Assembly.LoadFrom`'s
  file-locking behavior on Windows makes worth checking first").

## Validity preconditions

- FCS pinned to `43.9.101`, matching every prior FCS-hosting quartet in this repo.
- `ProvidedTypes.fs`/`.fsi` vendored from the same `FSharp.TypeProviders.SDK` commit
  (`0a95768a2247daba80b24a2604f77f89fc88ff1f`) already vendored into `Q012-compiler-behavior-probe/artifacts/vendor/`
  — copied forward, not re-fetched, for exact reproducibility with the pinned checkout this session's
  `FSharp.TypeProviders.SDK` working directory represents.
- The satellite DLL's source must come from `src/Myriad`'s own CLI (`Myriad.fsproj`/`Program.fs`),
  invoked as a real subprocess against a real attributed record — `Generator.Fields`
  (`src/Myriad.Plugins/FieldsGenerator.fs`), unmodified — not a hand-transcribed imitation of what Fields
  would produce. The generated `.fs` text must be inspected and shown to contain the real header comment
  Myriad's own `Generation.getHeaderedCode` emits, not just plausible-looking F#.
- The provider must be genuinely **generative** (`isErased = false`), matching every real Myriad
  generator's own irreversible, non-erased nature (Myriad writes real `.fs` files with real members that
  exist independent of any provider) and matching Q006/Q08/Q09/Q11/Q12's own choice for the same reason.
- Windows file-lock testing is meaningful in this environment specifically (this session runs on Windows
  11), so Round 2's file-lock/regeneration test is a real environment-appropriate check, not a
  platform-mismatched formality.

## Cheapest falsifier

Before attempting regeneration/`Invalidate()`: can a generative provider's `ApplyStaticArguments`
`Assembly.LoadFrom` (or `Load(bytes)`) an externally-produced satellite DLL — built from Myriad's real,
unmodified `FieldsGenerator` output — and construct one `ProvidedMethod` that reflection-forwards to one
real compiled member on that DLL, such that invoking the provided member through a `checker.Compile`'d
consumer returns the exact value Myriad's real generated code computes (checked two independent ways:
once by calling the compiled satellite DLL's method directly via reflection outside the provider, once
through the provider's generated IL) — end-to-end plumbing, no regeneration or invalidation yet. This
isolates the one genuinely new mechanism (a provider carrying real Myriad output rather than synthetic
markers) before spending anything on the regeneration/file-lock risk, which is real but secondary.

## Pre-registered decision thresholds

- **SHIP:** the cheapest falsifier passes (provided member's invocation genuinely executes Myriad's real
  compiled `FieldsGenerator` output, confirmed two independent ways, not visual inspection); AND
  regenerating the record (adding a field), rerunning Myriad's real CLI, recompiling the satellite DLL to
  the same path, and calling `Invalidate()` produces a live-checking consumer that sees the new member
  set with zero diagnostics; AND no file-lock exception occurs on the DLL overwrite. All three together
  would mean the cross-project satellite-DLL pattern genuinely works end to end with Myriad's real
  generator as the payload, not a synthetic one.
- **REVISE:** the cheapest falsifier passes, but regeneration/`Invalidate()` either doesn't propagate
  cleanly (stale members linger, a rebuild is needed) or hits the named Windows file-lock problem,
  requiring a workaround (e.g. `Assembly.Load(bytes)` instead of `LoadFrom`, per Q011's lesson) that
  narrows the claim to "works with a specific loading discipline, not `Assembly.LoadFrom` naively."
- **NULL:** the mechanism works end to end but demonstrates nothing beyond what Q008/Q09/Q11 already
  showed (i.e. loading an external DLL and reflecting over its custom attributes is not meaningfully
  different from reflection-forwarding its members) — judged unlikely going in, since real data
  accessors that execute real generator logic are a materially different claim from verification-only
  markers, but recorded as a real possible outcome the review should check for, not assumed away.
- **KILL:** the cheapest falsifier fails — the provider cannot construct a working reflection-forwarding
  member from an externally-loaded assembly (e.g. `ProvidedMethod`'s `invokeCode` quotation cannot
  legally close over a `MethodInfo` obtained from `Assembly.LoadFrom` the way it can over a
  compile-time-known one, or the emitted IL cannot resolve the forwarded call at runtime). This would be
  a real, general finding bounding every future idea in this space (items 15, 16, and the two-version
  schema-diff idea in `BACKLOG.md`, all of which depend on a provider forwarding into an externally
  loaded assembly's real members).
