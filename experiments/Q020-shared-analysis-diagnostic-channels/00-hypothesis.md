# Q020-shared-analysis-diagnostic-channels / Movement 1 — Hypothesis

**Status:** RUNNING.
**Date:** 2026-07-16
**Repo under test:** this repo, Thread 2 lineage. Promotes `BACKLOG.md` Round 5's first idea ("One
shared analysis, two diagnostic channels") to a quartet — a Fable-model brainstorm briefed on
`fsharp/fslang-suggestions#864` (F# has no Roslyn-source-generator equivalent; the F# team's own
answer is "use Myriad") and asked what a type provider could borrow from source generators' *design*
(precise diagnostic anchoring, a staged pipeline) since it structurally cannot borrow the *mechanism*
(a compiler-hosted hook FCS doesn't have).

**Question:** this repo's standing finding on type-provider diagnostics is purely negative — a
provider can only signal an error by throwing, and FCS pins the message to the whole static-argument
expression, never an arbitrary range (`BACKLOG.md`'s "structural ceiling on embedded-DSL diagnostics"
item). Separately, Myriad plugins today have **no diagnostics API at all**: `IMyriadGenerator.Generate:
GeneratorContext -> Output` (`src/Myriad.Core/Types.fs:39-41`) can only return generated output or
throw an exception the CLI prints as free text (`src/Myriad/Program.fs`, confirmed directly — no
canonical MSBuild-format diagnostic line is emitted anywhere today). Can one shared analysis function —
parsing a record's declaring file the way `Q019`'s provider already does, but producing a typed
diagnostic list (code, severity, message, range, optional associated member) instead of just a shape —
feed two independent emitters that can never disagree because they share one analysis pass: (1) a
Myriad-CLI-style emitter printing a canonical MSBuild diagnostic line anchored at the true
declaration-site range, and (2) an *erased* type provider (`Q019`'s shape) that never throws on a
member-level problem, instead stamping the affected provided member with `ProvidedProperty
.AddObsoleteAttribute(message, isError)` (confirmed present in the vendored SDK,
`ProvidedTypes.fsi:134`) so FCS surfaces the identical message live at every *use site* the developer
actually types — the one location a provider genuinely controls, and arguably more useful than the
declaration site since it's where the developer is looking while typing?

## The claims

Three separable claims, matching this repo's own discipline of not conflating mechanism and capability:

1. **Use-site anchoring claim (the load-bearing mechanism):** an erased provider's member stamped with
   `AddObsoleteAttribute(message, isError)` produces, via `FSharpChecker`, a diagnostic whose **message
   text matches verbatim** and whose **range is the use site** (where the consumer's code references the
   member), not the provider's own type-instantiation line — and `isError` genuinely controls
   `FSharpDiagnosticSeverity.Error` vs `.Warning`, not just cosmetic text.
2. **Synthetic-member claim:** a provided member whose name is not a normal identifier (contains a
   colon, spaces, or other characters requiring double-backtick access, `` `warning MYR012: ...` ``) is
   (a) a legal `ProvidedProperty` name FCS accepts without error, and (b) genuinely discoverable —
   checked via `FSharpCheckFileResults.GetDeclarationListInfo`, not just accessible if the user already
   knows the exact name — so it could plausibly surface in a real completion list for a diagnostic with
   no natural member to hang off.
3. **Shared-analysis capability claim:** given one concrete, non-contrived analysis condition — a
   record field whose declared type is itself another type defined in the *same* uncompiled file,
   directly reusing `Q019`'s own named open question about member typing — a single analysis function
   run once produces a diagnostic that both emitters render consistently: the Myriad-CLI emitter prints
   a canonical MSBuild line at the field's declaration range; the provider emits the identical message
   (byte-for-byte, modulo channel-specific wrapping) live at the use site. The two channels cannot
   disagree about *what* the problem is because neither computes it independently.

**Deliberately not attempted here, named so the review doesn't have to guess:** a real Myriad
plugin-interface change (`IMyriadGenerator` itself is untouched — this builds a standalone shared
analysis module and two standalone emitters, matching every prior quartet's "no MSBuild integration
with Myriad's own `Myriad.Sdk` targets" convention); a real Ionide/FSAC completion-list screenshot (the
standing `FSharpChecker`-as-library caveat, inherited unchanged); and a quick-fix/code-action UX for
either channel (out of scope, `ObsoleteAttribute`'s API surface doesn't offer one).

## Why this is the right next spike

It's the most concrete of Fable's two Round 5 ideas and the only one whose falsifier is symmetric with
its downside: even if the type-provider half doesn't pan out, the Myriad-CLI-side diagnostics API is a
real, independently useful capability Myriad has never had (a plugin can currently only generate or
throw — no way to say "this compiles, but here's a warning" at all). It directly extends `Q019`'s own
named follow-up (typing members against a field defined in the same uncompiled file) by using exactly
that scenario as the concrete diagnostic condition, rather than inventing a synthetic one.

## Novelty gate

Not covered by any closed quartet. `Q008`/`Q009`/`Q011` proved custom-attribute-carried metadata
survives into reflectable IL and drives a compiler-visible failure, but always as a **thrown** error at
the **static-argument** site — never a **non-fatal**, **use-site**-anchored diagnostic. `Q019` proved a
provider can self-parse and expose members with no compiled backing type, but never touched
diagnostics. `BACKLOG.md`'s own "structural ceiling on embedded-DSL diagnostics" item established the
throw-only/whole-argument-range limitation this quartet routes around, not defeats — the provider still
cannot choose an arbitrary range; it exploits the one range FCS already computes for it for free (a
member reference's own use-site range), which is a materially different mechanism from what that item
ruled out.

**A real competing alternative must be named, per Fable's own review of its own idea, not ignored:**
`FSharp.Analyzers.SDK` analyzers already run live inside FSAC today and *can* report a diagnostic at an
arbitrary range, including the declaration site directly — an analyzer calling the identical shared
analysis function this quartet builds would get true Roslyn-style anchoring in Ionide specifically, with
no type provider involved at all. This quartet does not attempt to build or compare against a real
analyzer (out of scope — a separate, cheap follow-up, not gating this quartet's own verdict), but its
`03-review.md` must weigh whether the TP channel's actual selling point survives that comparison, and
for which hosts (Ionide with analyzers enabled vs. Visual Studio, which has no F# analyzer host) it
remains the only live option.

## Contradiction gate

Does not contradict any prior verdict. Reuses `Q019`'s validated erased-provider/self-parsing mechanism
unchanged; extends it into diagnostics, a dimension `Q019` never tested. Consistent with `BACKLOG.md`'s
standing finding that a provider cannot anchor at an arbitrary range — this quartet doesn't dispute that,
it uses the one range (a use site) FCS computes independent of the provider's own choice.

## Validity preconditions

- Same pins as `Q019`: FCS `43.9.101`, `FSharp.TypeProviders.SDK` vendored from the same commit
  (`ProvidedTypes.fs`/`.fsi` copied from `Q019`'s own `artifacts/`, not re-fetched), `Fantomas.Core`
  `7.0.5`, `Myriad.Core.dll` built Release from this repo, unmodified.
- The use-site diagnostic's message must be checked **verbatim**, not "contains similar words" — copy
  the exact string passed to `AddObsoleteAttribute` and assert string equality against
  `diagnostic.Message`, allowing only for FCS's own known "This construct is deprecated. message" prefix
  (confirmed by inspection, not assumed, since `02-results.md` must report the exact wrapping observed).
- The range check must confirm the diagnostic's `(StartLine, StartColumn, EndLine, EndColumn)` matches
  the consumer file's own use-site token position, not the file/line of the `Fields<...>` static-parameter
  instantiation — printed and compared explicitly, not eyeballed.
- The synthetic-member claim must be checked against `GetDeclarationListInfo`, not merely that
  `checker.ParseAndCheckFileInProject` accepts a hand-written double-backtick reference to the name —
  discoverability and mere accessibility are different claims and must not be conflated.
- The shared-analysis claim's two emitters must call the **identical** analysis function (same module,
  same call, not two independently-written checks that happen to agree) — the whole point is structural
  non-disagreement, not accidental agreement.

## Cheapest falsifier

Before building anything else: stamp one erased provider member (reusing `Q019`'s provider directly)
with `AddObsoleteAttribute("Q020 test message", false)`, reference that member from a consumer file, and
check via `FSharpChecker` whether the diagnostic (a) contains the exact test message string, (b) is
anchored at the consumer's use-site range rather than the provider's instantiation line, and (c) has
`FSharpDiagnosticSeverity.Warning`. Then flip `isError` to `true` and confirm severity flips to `Error`
with no other change. This isolates the one genuinely uncertain mechanism (does `AddObsoleteAttribute`
actually anchor and control severity the way the SDK's doc comment implies, for an *erased* type
specifically) before building the synthetic-member or shared-analysis halves.

## Pre-registered decision thresholds

- **SHIP:** the cheapest falsifier passes (verbatim message, correct use-site range, correct severity
  both ways); the synthetic backtick-named member is both legal and genuinely discoverable via
  `GetDeclarationListInfo`; and the shared-analysis capability claim holds — one analysis call feeding
  two emitters that report the identical underlying condition, with the Myriad-CLI emitter's output
  matching MSBuild's documented canonical diagnostic format exactly (`origin(line,col,line,col):
  category code: text`, verified against Microsoft's own documented format, not assumed).
- **REVISE:** the mechanism works but only under a narrower condition than described — e.g. the message
  is wrapped or truncated in a way that loses information, the range is close but not exact (e.g. anchors
  the whole containing expression rather than the specific member-access token), or the synthetic member
  is accessible but does not appear in `GetDeclarationListInfo`'s results (accessible-but-not-discoverable
  is a real, materially weaker result than claimed).
- **NULL:** the mechanism works exactly as described, but the review judges `FSharp.Analyzers.SDK`
  already delivers the same or better result for the only host that matters in practice, making this
  quartet's contribution "a workaround for hosts that already have a better native option," not a real
  capability gain.
- **KILL:** the cheapest falsifier fails — `AddObsoleteAttribute` on an erased type's member doesn't
  anchor at the use site (e.g. it fires at the static-argument site instead, collapsing back into the
  already-known throw-only limitation), or severity control doesn't work, or the message is discarded
  entirely in favor of FCS's own generic "deprecated" text. This would be a real, general finding: it
  would mean the one erasure-compatible diagnostic-placement trick this quartet is built around doesn't
  actually escape the whole-argument-range ceiling `BACKLOG.md` already documented.
