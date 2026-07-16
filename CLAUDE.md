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

**Status as of 2026-07-16 — twenty quartets (Q001–Q020), seventeen closed, three planned. Full digest:
`experiments/FINDINGS.md`** — read that first, it synthesizes both lines without requiring all
seventeen `03-review.md`s as context. One-line summary: **nothing has been merged into `src/`, and
Myriad's real IDE-invisibility gap is still unsolved** — twenty quartets have mapped the space rather
than closed it. Myriad's-own-architecture line has four scoped SHIPs (Q002, Q003, Q010, Q015) and
proof that overclaiming is easy even inside this repo's own discipline (Q001 NULL, Q006 REVISE, Q014
REVISE — each looked stronger before adversarial review). The general type-provider line shipped a
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

**Next steps, prioritized, with why:** `experiments/BACKLOG.md`. Split into spike-shaped hypotheses
(need a quartet — both Myriad-specific and general-type-provider ideas, kept in separate sections)
and known engineering gaps in current Myriad that were verified from source along the way but don't
need a spike to justify fixing (design-time/IDE invisibility being the biggest one — generated code
doesn't appear in Ionide/FSAC until a real build, confirmed from
`src/Myriad.Sdk/build/Myriad.Sdk.targets`). Every type-provider route to that specific gap (Q006,
Q016–18, Q019) has now been tried and each stops short of it; the untested MSBuild/DTB-hook route named
in the backlog (item 8) remains the more direct candidate, and needs no type-provider machinery at all.
Q020's own top follow-up — give `IMyriadGenerator` a real diagnostics API, independent of any of this
session's type-provider work — is the other concrete, low-risk, currently-highest-value next
engineering task named in the backlog, not gated on any further spike.

**Starting a new session on this thread:** read `experiments/FINDINGS.md` for the synthesized
digest, then `experiments/README.md`'s Index table for a one-line-per-quartet summary, then
`experiments/BACKLOG.md` for what's queued. Each closed quartet's `03-review.md` is written to be
readable standalone — it names what was and wasn't proven without requiring the rest of the quartet
as context.
