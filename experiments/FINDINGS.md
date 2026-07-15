# Findings so far — what's real, what's not

Cross-quartet digest as of 2026-07-15, nine quartets in (Q001–Q009, six closed, three planned). This
is not a replacement for reading a closed quartet's own `03-review.md` — each is written to stand
alone — it's a synthesis for deciding what to do next without re-reading all nine. Two intertwined
but distinct threads share this quartet discipline; keep them separate, because they answer different
questions and one is Myriad-specific while the other explicitly isn't.

## Thread 1: should Myriad's own architecture change?

The question: could Myriad's current model (untyped-AST parsing → disk-written `.fs` files →
separate `dotnet build` re-typechecks them) be replaced by in-process, typed FCS hosting, and would
that actually buy real capability or just be novelty?

**Established, with real evidence:**

- In-process typed FCS hosting mechanically works, including a real correctness trap worth
  remembering on its own: `FSharpChecker`'s default `BackgroundCompiler` silently serves *stale*
  results across a broken cross-file dependency unless you explicitly call
  `InvalidateConfiguration`. The opt-in `useTransparentCompiler = true` path avoids this and was
  faster in every quartet that measured it (Q001).
- Typed access is not automatically an improvement over syntax. Ported onto the real `Fields`
  generator, it changed *nothing* — `Fields` is pure structural echo, and syntax-only was already
  correct. This NULL result matters as much as any SHIP in this file: it's the standing check
  against overclaiming everywhere else in this thread (Q001).
- Typed access *does* earn its keep for a harder, real class of problem: detecting that one field's
  type is itself another attributed type, resolved across files. A real syntax-only alternative was
  built (not a strawman) and found to flip between correct and silently-wrong depending only on file
  processing order — a genuine correctness bug class, not a convenience gap. But this required
  **both** pillars together — whole-project in-process hosting *and* typed field resolution — not
  typed access bolted onto Myriad's current per-file plugin model (Q002).
- FSI (`FsiEvaluationSession`, the mechanism type providers run on) can be hosted in the same
  process as `FSharpChecker` with no collision, and real F#-typed data — not just primitives —
  crosses into host code correctly (Q003). Untested: `AssemblyLoadContext` isolation, which is the
  configuration Myriad's actual plugin loader (`McMaster.NETCore.Plugins`) uses; the full
  evaluate→generate→splice→typecheck loop was never built.
- A real Myriad generator (`Lenses`) was ported onto the type-provider protocol instead of the
  FCS-hosting path, to attack Myriad's IDE-invisibility gap directly. The mechanism worked cleanly —
  47ms live re-check with zero rebuild — but hit a **structural wall, not an engineering gap**: type
  providers can only resolve types already compiled in a separately referenced assembly, never a
  type from the compilation currently in progress. Myriad's actual dominant usage (`[<Lenses>]` on a
  record in the file you're editing, in the project you're building) is exactly the case this rules
  out (Q006).

**Honest net position:** two SHIPs (Q002 fully, Q003 narrowly) prove the mechanism is sometimes
genuinely valuable; two REVISE/NULL results (Q001, Q006) prove it's easy to overclaim and that at
least one promising-looking route (type providers) hits a hard wall for Myriad's real usage pattern.
**Nothing has been built that would replace Myriad's current pipeline end to end**, and nothing in
`experiments/` has been merged into `src/`. Q004 (cross-assembly typed access) and Q005
(self-verifying generators — typecheck-before-emit) are the next pre-registered steps and neither has
been run.

## Thread 2: general F# type-provider headroom, independent of Myriad

A separate line, started from the question "excluding how Myriad is configured, can type providers
themselves be pushed further" — genuinely independent research using the same `FSharp.TypeProviders.SDK`
checkout, not gated on Thread 1's outcome either way.

**Established, with real evidence — a two-quartet SHIP streak, the strongest run in this whole file:**

- One generative provider can embed derivation provenance (a version tag) as a real custom
  attribute; a **second, independently-compiled** provider can read that attribute via plain
  reflection and refuse to generate when its own declared expectation disagrees — with a specific
  compiler diagnostic naming both versions, not a swallowed generic error. The conflict is caught
  live, on a source edit, through the same `FSharpChecker` instance, no rebuild, at 19–32ms (Q008).
- The same mechanism generalizes to **field-level** granularity: a client declaring a dependency on
  only a subset of a schema's fields stays clean when an unrelated field's provenance changes, and
  fails with a diagnostic naming the *specific* changed field(s) — including all of them at once in
  the multi-mismatch case — when a depended-on field changes. Re-check cost stayed flat from a
  3-field to a 12-field schema (Q009).

**Scoped precisely — what these two SHIPs are *not*:** neither result means "type-level supply-chain
verification is now possible." A CI script comparing version numbers already does that, more
crudely. What's proven is that the same check can move from CI-time to keystroke-time, with a
diagnostic cited at the exact consuming source line, by riding the type-provider protocol instead of
a side-band tool — a real, measured, but narrower claim than "verification through the type system"
suggests standing alone. Also unproven: both quartets generate verification-only marker members
(`ProvenanceOk`, `CheckedFieldCount`), never real per-field data accessors — the stronger argument
for why field-level grouping matters (that it's necessary for real data composition, not just
convenient) is plausible and consistent with Q006's lens-generation work, but no quartet has actually
combined the two to test it.

**Still open, pre-registered but not run:** Q007 — can a provider host FSI *inside* its own
static-parameter instantiation function, evaluating a string argument as real F# code to sidestep
the literal-only static-argument restriction, and does that survive being invoked from inside the
compiler's own live type-checking call stack (a nesting of compiler-service hosts no prior quartet
tested)? This is the best-supported unrun hypothesis in the whole backlog — it composes two already-
proven pillars (Q003's FSI hosting, Q006/Q008's working string-static-parameter pattern) rather than
needing new infrastructure.

**Still just brainstorming, not evidence — recorded in `BACKLOG.md`, none spiked:** minting new
`[<Measure>]` phantom types for zero-cost compile-time tagging (currency, tenant IDs — mechanism
plausible, unverified whether the language's `typ<unit>` syntax extends past numeric primitives);
`FileSystemWatcher`-driven `Invalidate()` (real, but less novel than first framed — sample-file
providers already do this); a provider reflecting on its own just-emitted IL mid-session (real risk
around `AssemblyLoadContext` identity, unverified); proof-carrying witness types via an external
SMT/theorem prover (viable only if narrowed to prove things about *external* artifacts, since a
provider can never see the compilation it's part of — the same wall Q006 hit); reachability-derived
session types via full protocol-composition model checking (the most ambitious idea on the list and
the least tractable soon — blocked on real state-space-explosion complexity, not an SDK gap);
property-tested/adversarial types (breaks build reproducibility as originally pitched; a
content-derived deterministic seed would fix that, not yet tried); optimization-as-static-parameter
(weakest idea recorded, a real unfixed structural flaw — the type itself changes shape when the
optimum moves, fighting the point of a stable interface).

## Cross-cutting limitations — true of nearly everything in this file, not quartet-specific

- **Every timing number anywhere in this lineage is a single sample**, not a distribution. The
  margins involved (order-of-magnitude gaps between measured numbers and the pre-registered REVISE
  thresholds) mean this hasn't mattered for any verdict yet, but no quartet has run repeated trials.
- **Every "works live in the IDE" claim has only ever been tested through `FSharpChecker` as a
  library, never a literal Ionide/FSAC/VS session.** This was judged disqualifying for Q006's
  specific claim (which was fundamentally about IDE-visible behavior) and judged real-but-secondary
  for Q008/Q009's narrower claims (which are fundamentally about diagnostic generation, a layer FCS
  owns directly). No quartet has closed this gap for any claim.
- **`TransparentCompiler` — the mechanism underpinning the entire in-process-hosting foundation — is
  still labeled experimental by both FCS and FSAC** as of the pinned version (`43.9.101`). Every
  result in Thread 1 inherits this caveat; Q005, if it ships, would be the first quartet to give a
  design-time-hosted compiler veto power over what gets emitted, which raises the stakes on this
  caveat rather than lowering them.
- **The generative-type-provider wall (Q006) is structural, not an engineering gap, and it bounds
  every idea in Thread 2 that routes through a type provider**, not just Q006 itself: a provider can
  never see a type from the compilation currently in progress, only already-compiled referenced
  code. Q008 and Q009 both live comfortably inside this wall (schema-package-vs-client-package is
  naturally a cross-compilation scenario); any future idea that needs same-project visibility will
  hit it again.
- **A specific, reproducible defect has now recurred twice unfixed**: the mismatch diagnostic in
  both Q008 and Q009 is reported twice (identical text, same location). Harmless to both quartets'
  pass/fail gating, real in an actual editor (two red squiggles), worth fixing once rather than
  re-discovering a third time.

## Starting the next session

Read this file for the digest, `experiments/README.md`'s Index for the one-line-per-quartet detail,
then `experiments/BACKLOG.md` for what's queued and why. If picking one next step: **Q007** is the
best-supported unrun hypothesis in the backlog (composes two already-proven pillars, doesn't need new
infrastructure). If continuing Thread 2's SHIP streak instead: **Q009's own follow-up 4** — combine
field-level provenance gating with Q006's real per-field accessor generation — is the one experiment
that would settle whether the ergonomic argument behind Q008/Q009 is actually load-bearing or just
plausible-sounding.
