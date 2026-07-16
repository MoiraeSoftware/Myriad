# Q019-erased-self-parsing-provider / Movement 1 — Hypothesis

**Status:** RUNNING.
**Date:** 2026-07-16
**Repo under test:** this repo, Thread 2 lineage (general type-provider headroom). Promotes
`BACKLOG.md` item 14 ("Erased, self-parsing design-time-only provider") to a quartet.

**Question:** Q006 proved a *generative* type provider hits a hard structural wall for Myriad's own
dominant usage pattern (`[<Lenses>]` on a record in the file currently being edited, in the project
currently being built): the provider can never resolve a type from the compilation in progress, only
one already compiled elsewhere. Every subsequent attempt to route around that wall (Q016/Q017/Q018's
satellite-DLL forwarding) stayed on the *generative* side and only ever reached the cross-project case,
because a generative provider's members must correspond to a real compiled backing type the SDK can
validate. Can an **erased** provider (`isErased = true`) sidestep the wall differently — not by finding
a way to reach the in-progress compilation, but by never asking the compiler to resolve anything at
design time at all? Specifically: give the provider a source file path as a static parameter, and have
it parse that file itself with `Fantomas.Core.CodeFormatter.ParseAsync` — via `Myriad.Core.Ast.fromFilename`
and `Myriad.Core.Ast.extractRecords`, Myriad's own actual shipped parsing code, referenced as a library,
not reimplemented — inside `DefineStaticParameters`'s instantiation function. Members are then named
directly from the record's syntactic field list (no type resolution needed for *that* half), and each
member's `invokeCode` erases to a plain runtime dynamic-reflection field read/write against whatever
object is actually passed in at the call site — not a real compiled type the SDK cross-checks, since an
erased type has no independent runtime representation to check against.

## The claim

Two separable claims, kept apart the way every quartet in this repo does:

1. **Design-time mechanism claim:** the provider can produce a type whose erased members are named
   from a source file's syntactic record-field list, resolving via `FSharpChecker` (both a cold check
   and a live-edit re-check with zero rebuild) with zero diagnostics — **even when the named record
   type does not exist, anywhere, compiled or not, at the moment the provider runs.** This is the
   specific claim that would demonstrate Q006's wall doesn't bind this shape: nothing here ever asks
   FCS to resolve `SampleNs.Person`, only to resolve the provider's own erased members, which are
   themselves the provider's invention.
2. **Runtime capability claim:** once a real object of the named record shape exists at the call site
   (compiled by ordinary means, with no relationship to the provider beyond having fields of matching
   names), invoking a provided erased member against it via dynamic reflection returns/sets the correct
   field value, matching a direct field access performed outside the provider. This is the "erasure to
   `obj` plus reflection" half that makes the design-time-only members actually do something at runtime,
   as opposed to being decorative.

**Deliberately not attempted here, named so the review doesn't have to guess whether it was missed or
scoped out:** this does not attempt Lenses' actual getter/setter-with-copy-and-update semantics (only
plain field read, and field write via `FSharpValue`-style reflection, not an immutable-copy update) —
that's a straightforward but separate follow-up once the self-parsing/erasure mechanism itself is
confirmed. It also does not attempt to make the design-time member *type* (not just name) reflect the
field's real declared type — every provided member here is typed `obj` at the design-time signature,
a real, named simplification (see validity preconditions).

## Why this is the right next spike

Every prior quartet that combined type providers with Myriad routed through a **generative** provider,
which by the SDK's own design must correspond to a real backing member the compiler can validate exists
(`Q006`, `Q016`, `Q017`, `Q018` all hit this in different ways — Q016/17's whole design is built around
finding a real `MethodInfo` to forward to). None has tried the other lever the SDK exposes: an *erased*
provider's members don't need a backing compiled type at all, which is exactly the property that would
let design-time visibility exist **before** the record itself is even written, let alone compiled — the
single biggest gap named against every generative attempt in this lineage (`FINDINGS.md`'s Thread 2
wall note: "a provider can never see a type from the compilation currently in progress"). This is also
the first quartet in either thread to make a type provider call into Myriad's own shipped parsing code
(`Myriad.Core.Ast`) rather than either reimplementing AST-walking logic from scratch (as Q002's
syntax-only resolver did) or forwarding into Myriad's *compiled generator output* (Q016-18) — literally
"Myriad inside a type provider," not "a type provider next to Myriad's output."

## Novelty gate

Not covered by any closed quartet's verdict. Q006 built a *generative* provider that needed the target
record already compiled elsewhere — the opposite of this quartet's whole point. Q016/Q017/Q018 forward
into a *compiled* satellite DLL that Myriad's real CLI already produced — this quartet's provider never
invokes Myriad's CLI or `checker.Compile` at all; it calls `Ast.fromFilename`/`Ast.extractRecords`
directly, in-process, inside the provider's own instantiation function. `BACKLOG.md` item 14 named this
mechanism but it was never built before this quartet.

## Contradiction gate

Does not contradict Q006's wall — it doesn't attempt to defeat the wall, it sidesteps needing to cross
it at all, since nothing here asks the compiler to resolve `SampleNs.Person`, only to typecheck the
provider's own self-invented erased members. Consistent with `FINDINGS.md`'s framing that the wall binds
specifically "any idea that needs same-project visibility [via asking the compiler]," not every possible
type-provider mechanism. Depends on, and does not re-litigate: `Ast.fromFilename`/`Ast.extractRecords`
being ordinary, already-shipped, already-tested Myriad code (no claim here about their correctness, only
about calling them from a new context); the SDK's own documented `isErased = true` path
(`ProvidedTypes.fsi:272,343`), never previously exercised end-to-end by any quartet in this repo (Q006
was deliberately generative, per its own design rationale); `DefineStaticParameters` firing more than
once and needing memoization (Q011's finding, inherited defensively here).

## Validity preconditions

- FCS pinned to `43.9.101`; `FSharp.TypeProviders.SDK`'s `ProvidedTypes.fs`/`.fsi` vendored from the same
  commit prior quartets used (`0a95768a2247daba80b24a2604f77f89fc88ff1f`), for direct comparability.
- The design-time project references the real, unmodified `Myriad.Core.dll` built from this repo's own
  `src/Myriad.Core` (Release config) — not a reimplementation or a copy-pasted subset of its parsing
  logic. If `Ast.fromFilename`/`Ast.extractRecords` turn out to be unusable from that context for a
  packaging reason (e.g. `Fantomas.Core`/`Fantomas.FCS`'s own transitive dependencies conflicting with
  the design-time host's), that is itself a real, reportable result, not a reason to quietly fall back to
  a hand-rolled parser — report it as a finding.
- The target record file must **not exist as a compiled type anywhere on any reference path** the first
  time the design-time member-resolution claim is tested — the whole point is proving this doesn't need
  to happen. The runtime capability claim is then tested separately, after compiling a real, independent
  program that defines a same-shaped record (not sharing an assembly with the provider's own test
  harness), to confirm the two claims are genuinely decoupled rather than accidentally relying on the
  provider process already having the type loaded.
- Live-edit re-check must use the same `FSharpChecker` instance across the edit (matching every prior
  quartet's live-edit methodology), and the edit itself must be a real field rename/addition written to
  disk via `FileSystemWatcher`-triggered `Invalidate()` — not a re-instantiation of a fresh provider
  process, which would prove nothing about live editing.
- Runtime correctness must be checked by actually compiling and running a consumer program (real
  `dotnet build`/`dotnet run` or `checker.Compile` + reflection-invoke, matching Q006 Round 2 and
  Q016/17's own methodology), not just asserting the quotation shape looks right.
- `DefineStaticParameters`'s instantiation function must be memoized by its static-argument tuple
  (file path, record name) — inherited defensively from Q011's finding, checked directly rather than
  assumed safe here.

## Cheapest falsifier

Before building the runtime-reflection half: can an erased `ProvidedTypeDefinition`, built entirely from
a call to `Myriad.Core.Ast.fromFilename`/`Ast.extractRecords` against a source file naming a record type
that **does not exist anywhere as a compiled or referenceable type**, produce members whose *names* the
compiler accepts (zero diagnostics on the members' existence and static-parameter application), when
checked via `FSharpChecker` exactly as Q006/Q016 checked their own providers? This isolates the one
genuinely new claim (design-time visibility with the target type provably absent from every reference
path) from the already-familiar erasure/reflection plumbing.

## Pre-registered decision thresholds

- **SHIP:** the cheapest falsifier passes; a live source edit (adding or renaming a field in the parsed
  file) is picked up via `FileSystemWatcher` + `Invalidate()` and the provider's members change to match,
  with no `dotnet build` of anything; and the runtime capability claim holds — a provided member,
  invoked against a real, independently-compiled object of the matching shape, reads/writes the correct
  field value via dynamic reflection, matching a direct field access performed outside the provider.
- **REVISE:** the design-time half works cleanly, but the runtime half has a real gap not anticipated
  above — e.g. the erased-to-`obj` signature makes overload resolution or a specific consumer shape
  (nested records, non-primitive field types) fail in a way that would need a different erasure target
  than plain `obj`, or reflection-based field access throws for a structural reason (e.g. F# record
  compiled field-backing-name mismatches, which do exist and aren't guaranteed to equal the source-level
  field name).
- **NULL:** the mechanism works end to end but demonstrates nothing beyond what a `JsonProvider`-style
  sample-file provider already does routinely in the wild — i.e., if this turns out to be a straight,
  unremarkable application of a well-known erased-provider pattern with nothing Myriad-specific or
  previously-unverified about it. Judged unlikely to be the *whole* verdict (calling into
  `Myriad.Core.Ast` specifically, and the explicit design-time/runtime decoupling test, are both new),
  but stated here as a real possible outcome rather than assumed away.
- **KILL:** the cheapest falsifier fails — either `Ast.fromFilename`/`Ast.extractRecords` cannot be
  called from inside a type provider's design-time host process at all (a packaging/dependency-isolation
  failure distinct from anything about erasure), or an erased provider's members are rejected by FCS
  when built from syntactic-only information even though no compiled backing type is being claimed. This
  would be a real, general finding: it would mean self-parsing inside a provider is not viable regardless
  of how carefully the erasure is set up, closing this entire route to Q006's IDE-invisibility gap, not
  just this quartet's specific attempt at it.
