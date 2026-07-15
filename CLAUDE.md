# Myriad

F# code generator: parses attributed F# source (via `Fantomas.FCS.Syntax`), runs it through
plugin generators, emits new `.fs` files as an MSBuild pre-build step. See `README.md` for usage,
`DEVNOTES.md` for the MSBuild rebuild-caching mechanics, and
`.claude/skills/myriad-ast-conventions/SKILL.md` for AST-construction conventions when editing
`src/Myriad.Plugins`/`src/Myriad.Core`.

## Current R&D thread: is there a viable successor architecture, and does it earn its keep?

`experiments/` holds an active, evidence-gated exploration of whether Myriad's core model
(untyped-AST parsing → disk-written `.fs` files → separate `dotnet build` re-typechecks them)
could be replaced by in-process, typed FCS hosting (the way Fable already hosts FCS for
transpilation) — and, separately, whether that would actually deliver a real capability win or
just be architecture novelty. This is exploratory research, not a committed direction: nothing in
`experiments/` has been merged into `src/` or decided as the project's roadmap.

**Methodology:** `experiments/README.md` — a pre-registered hypothesis → design → results →
adversarial-review discipline (adapted from an ML-training experiment convention), enforced by
`dotnet fsi experiments/check-quartets.fsx`. The point is gating: a hypothesis that fails its own
pre-registration, or a spike whose result doesn't survive its own adversarial review, doesn't get
oversold. Read a quartet's `03-review.md` for the honest verdict, not just `02-results.md`.

**Status as of 2026-07-14 — three quartets closed:**

- **Q001** (`experiments/Q001-fcs-typed-codegen/`): in-process typed hosting works — validated
  with real timing numbers, including a real correctness trap (`FSharpChecker`'s default
  `BackgroundCompiler` silently serves stale results across a broken dependency unless you
  manually call `InvalidateConfiguration`; the opt-in `useTransparentCompiler = true` path fixes
  this and was faster). Ported Myriad's real `Fields` generator onto the new model — it typechecked
  end to end, but typed access changed *nothing* for it: Fields is pure structural echo, and
  syntax-echo was already correct. Verdict: **REVISE** — foundation real, the "typed beats syntax"
  capability claim unproven for this generator class.
- **Q002** (`experiments/Q002-typed-nested-dispatch/`): tested the capability claim on a harder
  case — detecting that one field's type is itself another Myriad-attributed type, across files.
  Typed access resolved it correctly with one property access. A real syntax-only alternative was
  then built (using Myriad's own `Ast.fs` matching code, not a strawman) and found to fail
  concretely: the identical resolver flips between correct and silently-wrong answers depending
  only on file processing order. Verdict: **SHIP** the capability claim, narrowly — it required
  both Q001's pillars together (typed access *and* whole-project in-process hosting; Myriad's
  current per-file plugin invocation model wouldn't reproduce this from typed access alone).
- **Q003** (`experiments/Q003-fsi-comptime-eval/`): tested whether `FsiEvaluationSession` (FSI) —
  the mechanism type providers actually run on — can be hosted alongside `FSharpChecker` and hand
  real F#-typed data (not just primitives) to host code. It can, cleanly, in the simplest hosting
  configuration. Verdict: **SHIP the falsifier**, capability claim still open — untested under
  `AssemblyLoadContext` isolation (the configuration Myriad's real plugin loader actually uses),
  and the full evaluate→generate→splice→typecheck loop wasn't built yet.

**Next steps, prioritized, with why:** `experiments/BACKLOG.md`. Split into spike-shaped
hypotheses (need a quartet) and known engineering gaps in current Myriad that were verified from
source along the way but don't need a spike to justify fixing (design-time/IDE invisibility being
the biggest one — generated code doesn't appear in Ionide/FSAC until a real build, confirmed from
`src/Myriad.Sdk/build/Myriad.Sdk.targets`).

**Starting a new session on this thread:** read `experiments/README.md` for the methodology, the
Index table there for a one-line-per-quartet summary, then `experiments/BACKLOG.md` for what's
queued. Each closed quartet's `03-review.md` is written to be readable standalone — it names what
was and wasn't proven without requiring the rest of the quartet as context.
