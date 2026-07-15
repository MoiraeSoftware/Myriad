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

**Status as of 2026-07-15 — thirteen quartets (Q001–Q013), ten closed, three planned. Full digest:
`experiments/FINDINGS.md`** — read that first, it synthesizes both lines without requiring all
thirteen `03-review.md`s as context. One-line summary: Myriad's-own-architecture line has three SHIPs
(Q002, Q003, Q010) and two REVISE/NULLs (Q001, Q006), and nothing has been built that would replace
Myriad's current pipeline end to end; the general type-provider line shipped a provenance-enforcement
mechanism (Q008, Q009, Q011) plus one hard structural wall (Q006 — generative type providers can never
see a type from the compilation currently in progress, only already-compiled referenced code, which
rules out Myriad's own dominant same-file usage pattern specifically) — **but Q012 then found a
direct, reproducible contradiction with Q008's own claimed measurement (Q008 saved no source, so it
can't defend itself), and Q013 closed off the cheapest reconciling explanation with a NULL result, so
Q008/Q09's SHIP verdicts are now actively disputed, not just unverified.** Read `FINDINGS.md`'s "gap
in this file's own credibility" section before citing any Thread 2 SHIP verdict.

**Next steps, prioritized, with why:** `experiments/BACKLOG.md`. Split into spike-shaped hypotheses
(need a quartet — both Myriad-specific and general-type-provider ideas, kept in separate sections)
and known engineering gaps in current Myriad that were verified from source along the way but don't
need a spike to justify fixing (design-time/IDE invisibility being the biggest one — generated code
doesn't appear in Ionide/FSAC until a real build, confirmed from
`src/Myriad.Sdk/build/Myriad.Sdk.targets`; Q006 tested and REVISE'd the type-provider route to this
same gap, so the untested MSBuild/DTB-hook route named in the backlog is now the more direct
candidate).

**Starting a new session on this thread:** read `experiments/FINDINGS.md` for the synthesized
digest, then `experiments/README.md`'s Index table for a one-line-per-quartet summary, then
`experiments/BACKLOG.md` for what's queued. Each closed quartet's `03-review.md` is written to be
readable standalone — it names what was and wasn't proven without requiring the rest of the quartet
as context.
