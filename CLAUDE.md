# Myriad

F# code generator: parses attributed F# source (via `Fantomas.FCS.Syntax`), runs it through
plugin generators, emits new `.fs` files as an MSBuild pre-build step. See `README.md` for usage,
`DEVNOTES.md` for the MSBuild rebuild-caching mechanics, and
`.claude/skills/myriad-ast-conventions/SKILL.md` for AST-construction conventions when editing
`src/Myriad.Plugins`/`src/Myriad.Core`.

## Current R&D thread: is there a viable successor architecture, and does it earn its keep?

`experiments/` holds an active, evidence-gated exploration split across two intertwined but distinct
lines, both using the same quartet discipline:

1. **Myriad's own architecture** — whether Myriad's core model (untyped-AST parsing → disk-written
   `.fs` files → separate `dotnet build` re-typechecks them) could be replaced by in-process, typed
   FCS hosting (the way Fable already hosts FCS for transpilation), and whether that would actually
   deliver a real capability win or just be architecture novelty.
2. **General F# type-provider headroom** — a separate line using a second checkout,
   `FSharp.TypeProviders.SDK` (added as an extra working directory), asking what the type-provider
   protocol itself can do that the ecosystem isn't using, independent of Myriad's own configuration.

This is exploratory research, not a committed direction: nothing in `experiments/` has been merged
into `src/` or decided as the project's roadmap.

**Methodology:** `experiments/README.md` — a pre-registered hypothesis → design → results →
adversarial-review discipline (adapted from an ML-training experiment convention), enforced by
`dotnet fsi experiments/check-quartets.fsx`. The point is gating: a hypothesis that fails its own
pre-registration, or a spike whose result doesn't survive its own adversarial review, doesn't get
oversold. Read a quartet's `03-review.md` for the honest verdict, not just `02-results.md`.

**Status as of 2026-07-19 — thirty quartets (Q001–Q030), twenty-eight closed, two planned. Full
digest: `experiments/FINDINGS.md`** — read that first, it synthesizes both lines without requiring all
twenty-five `03-review.md`s as context. One-line summary: **nothing has been merged into `src/` from the
architecture-exploration track, and Myriad's real IDE-invisibility gap is narrowed but still not
solved** — twenty-seven quartets have mapped the space and, for the first time, actually closed part of
it: `Q022` hooked Myriad's own MSBuild codegen target into the design-time-build path and confirmed,
against a literal `fsautocomplete` process over real LSP (not `FSharpChecker`-as-library, a standing
gap this file had flagged since Q006), that it makes a generated member appear with zero `dotnet build`
— but only at project load/reload time, not during live source editing (REVISE; see below). Myriad's-
own-architecture line has eight scoped SHIPs (Q002, Q003, Q010, Q015, Q021, Q024, Q025, Q026) and proof that
overclaiming is easy even inside this repo's own discipline, in at least three distinct ways — not just
claiming more than was shown (Q001 NULL, Q006 REVISE, Q014 REVISE — each looked stronger before
adversarial review, and Q021's own results write-up had a secondary claim struck by review too) but also
claiming a negative more absolutely than the evidence supported (Q022's own results doc concluded no
in-session FSAC reload signal existed at all, until review found one) and reporting a single
unrepresentative worst-case condition as the general conclusion (`Q023`'s scale test always edited the
compilation-order *first* file — the maximum-successor, worst-case edit position — and its executor read
the resulting near-cold cost as "caching is largely absent"; review found cost is actually linear in the
number of files *after* the edit and collapses to a cache hit for a tail edit, REVISE). `Q024` then
generalized `Q023`'s corrected finding across scale (a 20-run position × N sweep, N=10 to 300) and
shipped it, scoped: the per-successor marginal cost shows no detectable drift with N, independently
reproduced, though the review knocked down an over-precise "essentially N-invariant" framing to the
better-supported "no detectable systematic drift" and flagged that this is a cost *model*
(`ParseAndCheckProject` on independent files), not a measured FSAC live-editing session — don't cite it
as "FSAC keystroke cost characterized." The general type-provider line shipped a
provenance-enforcement mechanism (Q008/Q09/Q11, reconstructed and reconfirmed after a real
credibility scare — read `FINDINGS.md`'s "gap in this file's own credibility" section before citing
any Thread 2 SHIP verdict) and then spent five more quartets (Q016–Q020) probing routes around the one
wall that keeps recurring: a type provider can never see a type from the compilation currently in
progress, only already-compiled referenced code (Q006). Every route since has hit its own real, narrower
ceiling: cross-project satellite-DLL forwarding into Myriad's own compiled output works but live
re-exposure has no in-process fix on Windows (Q016–18, settled — use `<ProjectReference>` for that
case); an erased provider that self-parses a source file sidesteps the wall entirely but only ever
delivers `obj`-typed member *names* for a parallel preview type, not Myriad's actual `[<Lenses>]` shape
made live (Q019, SHIP scoped); and a live diagnostics channel built on that same self-parsing trick had
its headline "two channels that cannot disagree" claim struck as tautological, with an
`FSharp.Analyzers.SDK` analyzer beating it outright in Ionide, the host most F# developers actually use
(Q020, SHIP scoped). Separately confirmed from an authoritative external source
(`fsharp/fslang-suggestions#864`, cited in `BACKLOG.md` item 9): F# has no Roslyn-source-generator
equivalent and the F# team's own stated answer is "use Myriad" — this repo's IDE-invisibility gap is not
a solved problem Myriad merely hasn't adopted, it's a real architectural gap the language itself has
left open.

Two more quartets closed 2026-07-18. `Q025` reached the goal Q015's own review named unmet (real
generation-time *computation*, not FSI handing back a `MethodInfo` pointer to already-host-compiled
code): a hand-written interpreter walking a real, checker-produced `FSharpExpr` tree directly (six
patterns, zero `FsiEvaluationSession`/`Reflection.Emit`) reflection-invokes already-compiled
reference-assembly functions and drives differing generated output — SHIP, scoped (a mechanism proof
over six patterns and two fixtures, not "generation-time computation solved"; review also caught a
falsified "generic calls fail loudly" claim in the frozen docs). `Q026` then closed `BACKLOG.md` item
22's own cheapest falsifier — the first quartet to reentrant-compose Myriad's *real, unmodified*
`LensesGenerator` (discovered via `MyriadGeneratorAttribute` reflection, exactly mirroring
`src/Myriad/Program.fs`'s own plugin-discovery code) with a second real `IMyriadGenerator`, rather
than the hand-typed stand-ins every prior reentrant quartet (Q010, Q021, Q023, Q024) used — SHIP,
scoped: the composition mechanism works and generalizes to an untested shape, but review found the
composed generator's own naive emission logic breaks on a nested field type (mechanism still correct,
generator toy) and, more consequentially, that the static side-channel standing in for a checker
handle (since `IMyriadGenerator.Generate` has no parameter to carry one) races on reuse across more
than one composition in the same process — a concrete, load-bearing finding for item 18/22's deeper
live-host vision, not just this quartet's own plumbing.

`Q027` then settled a question every quartet since `Q010` had inherited unresolved (Q010's own review
Objection 1): is a reentrant `DocumentSource.Custom` call — made from inside a later file's callback
while the outer `ParseAndCheckProject` is still unreturned — genuinely fresh work, or a cache hit
landing on already-resolved state? **REVISE.** The core conclusion holds and is strengthened: a warm
repeat costs 2-4ms while both reentrant calls cost tens to hundreds of ms, decisively ruling out the
cache-hit explanation. But the executor's own headline number (first reentrant call costs 4.2x *more*
than an isolated cold check) didn't survive review's own controls — ~91% of that gap turned out to be
an unrelated ~400-500ms one-time JIT/FCS-init tax that only the *first* typecheck in any process pays,
not something a "cold" baseline measured later in the same process ever pays; a direct, non-reentrant
first check on the same checker actually costs *more* (~929ms) than the reentrant one, since the outer
call had already begun building the shared snapshot. Warmed, both reentrant calls (17-48ms) land
exactly on the design's own pre-registered REVISE outcome. New lineage-wide caution: any single "first
check" timing this thread has ever reported carries the same tax — `BACKLOG.md` item 23 named the cheap
spot-check (whether `Q023`/`Q024`'s own cost-model numbers were affected).

**That spot-check ran the same week, as `Q028` — CLOSED, REVISE (2026-07-19).** Confirmed on an
independent rebuild of `Q023`'s own actual spike code (not just Q027's separate toy): `Q023`'s own `cold`
measurement does carry the same tax, a roughly fixed ~570-577ms one-time cost at both N=10 and N=300,
inflating `Q023`'s published `cold`-denominated ratios materially (`editOneRatio` at N=10 rises ~2.7x once
isolated by a warmup control). The correction is conservative — `editOne` is a *larger* fraction of a
tax-corrected `cold` than published, not smaller — and disturbs neither quartet's qualitative verdict
(real incremental caching under `TransparentCompiler`; cost linear in compilation-order successors).
REVISE rather than SHIP for one reason the review insisted on: the pre-registered hypothesis wrongly
claimed the correction reaches `Q024`'s own regression, which is fit in ms/successor, not on any ratio,
and publishes no `editOneRatio` column — `Q024`'s SHIP verdict is untouched; the correction lands only on
`Q023`'s own ratio/percentage prose. See `Q028-jit-tax-spotcheck/03-review.md`.

**The single most-named unresolved next step across the last several sessions has now closed too, as
`Q029` — SHIP, scoped (2026-07-19).** A real `fsautocomplete` 0.83.0 process, driven over hand-rolled
LSP (reusing Q022's own harness) against a genuinely-weighted, real multi-file F# project with an actual
compilation-order dependency chain, confirmed FSAC's own per-file incremental editing path reproduces
Q023/Q024's position-dependent, linear-in-successors cost curve: editing the last file settles in
~6-9ms, the first file in ~120ms (N=20) / ~243ms (N=40), ~6ms/successor, independently reproduced from a
clean rebuild. The load-bearing control is stronger than a number match — the raw LSP transcript shows
the `documentAnalyzed` cascade is strictly position-gated (exactly `successors+1` files re-analyzed, in
compilation order, verified by hand at three edit positions), decisively ruling out a position-blind
refresh or stale-cache reading, making this the best-controlled real-FSAC measurement in the lineage so
far. Review trimmed one framing overshoot (the identical correction Q024's own review already made for
the same claim): "N-invariant per-successor rate" overreaches on two N values — cite "no detectable
drift across N=20 and N=40" instead. One finding cuts toward the pessimistic Myriad reading: the tested
edit was value-only (exported signature untouched) yet FSAC still re-checked the whole tail — FSAC
invalidates on file content, not on whether the exported signature changed. Scoped: small N (≤40), one
linear dependency-chain topology that can't yet distinguish "FSAC invalidates by compilation order" from
"FSAC invalidates by true dependents." See `Q029-fsac-live-editing-cost/03-review.md`. The review's own
named next step, not yet spiked: a wide/shallow dependency shape (an early file only a few later files
actually reference) to test whether FSAC invalidates by true dependents or the whole compilation-order
suffix regardless — the one test that would tell whether Myriad's "attributed types sit early =
expensive" caveat is as bad as this linear-chain result implies, or softened by dependency-precise
invalidation.

**That named next step ran too, the same day, as `Q030` — CLOSED, SHIP (2026-07-19), a rare clean
review pass with no framing overshoot found.** A wide/shallow topology (N=30, an early "Hub" file
referenced by only 3 of the 27 later files, scattered among 24 unrelated ones) showed a value-only edit
to Hub re-analyzes **all 27 order-successors, not just the 3 true dependents** — the entire
compilation-order suffix — reproduced independently 3/3 reps and confirmed by hand against the raw LSP
transcript, with two controls (editing a file referenced by nothing still cascades to its full
order-suffix; pre-Hub files never re-fire across 10 edit cascades) ruling out both a
harness-solicited-request explanation and a blanket refresh. **This settles the order-vs-dependency
question `Q023`/`Q024`/`Q029` each named as their own top follow-up, on the unfavorable side for
Myriad**: an early attributed type pays the full order-suffix re-check cost regardless of how few files
truly reference it. The review went further than the executor and found this is not a fixable FSAC
limitation but F#'s own ordered-file compilation semantics — every file is checked against the
accumulated signature environment of all preceding files, so the compilation-order suffix genuinely *is*
the dependency set in F#'s model — making the pessimistic reading structural, not something a later FCS
release might relax. Overturns nothing: `Q029`'s cost curve stands, its x-axis is now confirmed to be
order-successors. See `Q030-fsac-dependency-precision/03-review.md`. With this and `Q029` both closed,
the FSAC-cost sub-line (`Q023`→`Q024`→`Q029`→`Q030`) is settled as far as this repo's tooling can
currently test it; the most direct remaining step if this thread continues toward an actual
persistent-host attempt is `Q026`'s own named engineering prerequisite — widening `GeneratorContext` to
carry a real checker/options handle — not another cost-model spike.

**Next steps, prioritized, with why:** `experiments/BACKLOG.md`. Split into spike-shaped hypotheses
(need a quartet — both Myriad-specific and general-type-provider ideas, kept in separate sections)
and known engineering gaps in current Myriad that were verified from source along the way but don't
need a spike to justify fixing. Design-time/IDE invisibility, long the biggest named gap, is now
**partially** closed rather than only mapped: every type-provider route (Q006, Q016–18, Q019) stops
short of it, and the direct MSBuild/DTB-hook route (item 8) has now been spiked as `Q022` — REVISE,
closing the gap at project load/reload time (confirmed against a real `fsautocomplete`/LSP session, the
first such test in this repo's history) but not during live source editing, since an ordinary edit to
the attributed source file never touches the `.fsproj` a reload is keyed on. **The gate removal was
applied for real on 2026-07-17** to the actual shared `src/Myriad.Sdk/build/Myriad.Sdk.targets` (Q022
itself only ever edited a scoped local copy) — re-verified directly against this repo's own test
project via a real DTB invocation (`-p:DesignTimeBuild=true -p:SkipCompilerExecution=true`): codegen
runs, the compiled `.dll`'s mtime never moves, and a repeat DTB call still correctly no-ops. Same
session, three more MSBuild/CLI engineering fixes shipped from `BACKLOG.md`'s known-gaps list, none of
them quartet-shaped: Myriad now runs once per project instead of once per file (a new `--manifest`
CLI mode, since the old per-file rebuild cache was already invalidating every file on any single
change — no real incrementality lost); the project-context TOML writer no longer depends on MSBuild's
implicit `;`-splitting of `Include` attributes to fake multi-line output (the same fragility class
this repo's git history already shows repeated fixes for); and a stray trailing `)` in `Myriad.Sdk.
targets`'s `OutputPath` (a 2022 refactor leftover) that had been silently defeating the up-to-date
check for `MyriadInlineGeneration` files, forcing regeneration on every build, is fixed. All four are
detailed in `BACKLOG.md`'s known-engineering-gaps section and `DEVNOTES.md`; committed as `793bc98`.
A second,
non-type-provider route was also opened in an earlier session: item 18 proposes FSAC itself hosting
Myriad as a live-editing sidecar (modeled loosely on rust-analyzer's out-of-process proc-macro
architecture), and its own cheapest-falsifier precursor question was spiked as `Q021`
— **SHIP, scoped**: the underlying reentrant-generation mechanism (`Q010`) survives a real persistent-
checker, multi-edit-cycle load pattern with no staleness, but whether an FSAC-hosted version would be
keystroke-cheap or pay a full-project-recheck cost on every edit was left genuinely open, pending a
scale test named as the then-single-highest-priority next step in `BACKLOG.md` item 18 and
`FINDINGS.md`. **That scale test ran as `Q023` — REVISE, with the corrected answer landing more
favorably for item 18 than the quartet's own first-pass conclusion:** the executor's own headline claim
("editOne tracks cold, caching is largely absent once anything changes") didn't survive review, which
found it was an artifact of the spike always editing the compilation-order *first* file — the
worst-case edit position, not a representative one. A position sweep, independently re-confirmed with a
durable checked-in artifact, showed cost is linear in the number of files *after* the edited one and
collapses to a no-op-repeat cache hit for a tail edit. The corrected finding: `TransparentCompiler`
genuinely skips the compilation-order prefix before an edit — real, working incremental caching, not
its absence — with one caveat that cuts back the other way for Myriad specifically: attributed domain
types often sit early in build order, close to the worst-case position actually measured. See
`Q023-scale-cost-reentrant-callback/03-review.md`. **`Q023`'s own review then asked whether that
positional model holds across scale, not just the one N=300 spot-check — spiked immediately as `Q024`,
SHIP scoped:** a 20-run sweep (5 positions × N ∈ {10,50,150,300}) found the per-successor marginal
cost is a clean line at every N with no detectable systematic drift in the slope as N grows,
independently reproduced. Review trimmed an over-precise "essentially N-invariant" claim down to "no
detectable drift" (5 points per fit can't support tighter than that) and flagged the more important
scope limit: this characterizes a cost *model* (`ParseAndCheckProject` on independent files), not a
real FSAC editing session — the review's own top follow-up is testing whether FSAC's actual incremental
per-file path reproduces the same curve. See `Q024-position-sweep-across-scale/03-review.md`.
Q020's own top follow-up — give `IMyriadGenerator` a real diagnostics API — is **done**, not just
designed: `IMyriadGeneratorWithDiagnostics`/`MyriadDiagnostic`/`DiagnosticSeverity` are built in
`src/Myriad.Core`, wired into the CLI, and covered by five new tests (all 58 tests in
`test/Myriad.IntegrationPluginTests` pass); see `experiments/BACKLOG.md`'s "Known engineering gaps"
section for the shipped shape.

**Starting a new session on this thread:** read `experiments/FINDINGS.md` for the synthesized
digest, then `experiments/README.md`'s Index table for a one-line-per-quartet summary, then
`experiments/BACKLOG.md` for what's queued. Each closed quartet's `03-review.md` is written to be
readable standalone — it names what was and wasn't proven without requiring the rest of the quartet
as context.
