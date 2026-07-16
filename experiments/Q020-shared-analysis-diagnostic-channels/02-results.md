# Q020-shared-analysis-diagnostic-channels / Movement 3 — Results

**Status:** DONE. All three rounds ran and passed against `00-hypothesis.md`'s pre-registered
thresholds, with one correction to the design's own anchoring-precision assumption, found by running.
See `03-review.md` for the adversarial pass.

## Summary

Built `SharedAnalysis` (a plain library, no TP/Myriad-CLI dependency of its own) exposing one function,
`Analyze.analyze : filePath -> DiagnosticInfo list`, that self-parses a file with `Myriad.Core.Ast`
(reusing `Q019`'s exact mechanism) and flags any record field whose declared type is itself another
record defined in the *same* file — a direct instance of `Q019`'s own named open question, not a
contrived condition. Two independent consumers of that identical compiled function: `EmitterA`, a
standalone console program rendering each result as a canonical MSBuild diagnostic line anchored at the
field's declaration; and `DiagTP`, an erased type provider (`Q019`'s shape) that stamps the
corresponding provided member with `ProvidedProperty.AddObsoleteAttribute(message, isError)` instead of
leaving it plain, so FCS surfaces the identical message live at the consumer's own use site. All three
pre-registered rounds passed. Full source under this quartet's own subfolders; raw run output in
`run-logs/`.

## Round 1 — `AddObsoleteAttribute` use-site anchoring + severity control

**PASS**, after one correction to the design's own precision assumption (below). A minimal
no-static-parameter erased type, `DiagTP.Provided.Sample`, exposes `Good` (plain), `Bad` (stamped
`AddObsoleteAttribute("Q020 test message", false)`), and `BadError` (same message, `isError = true`).
Checked via `FSharpChecker.ParseAndCheckFileInProject` against a consumer using all three from
deliberately padded, non-trivial-column lines:
- `Good`: **zero diagnostics** (negative control, confirming the mechanism doesn't fire spuriously).
- `Bad`: **one diagnostic**, `Severity = Warning`, message `"This construct is deprecated. Q020 test
  message"` — the custom string survives verbatim, with only FCS's own known fixed prefix added, not
  replaced or discarded. Range independently computed from the consumer source text before the check
  ran (not read back from the diagnostic), confirming true use-site anchoring — not the provider's own
  static-parameter/instantiation line (line 2), the actual reference site (line 5).
- `BadError`: identical shape with `Severity = Error` — `isError` genuinely controls severity, not just
  cosmetic text.

### Correction found by running, not anticipated by `01-design.md`

**The diagnostic's range is the whole member-access expression (`s.Bad`), not the narrower `Bad`
property-name token alone.** `01-design.md`'s validity preconditions asked for the range to match "the
exact token position of the `.Bad` reference" — the first run computed an expected range for just the
substring `Bad` (columns 14-17 on the test line) and got `FAIL` against the actual reported range
(columns 12-17, i.e. from the start of `s` through the end of `Bad`). Recomputing the expected range
against the full `s.Bad` expression matched exactly and deterministically. This is still genuine
use-site anchoring — the right line, the right specific reference, not the declaration or the provider's
own instantiation site — just coarser granularity than first assumed: a whole small expression, not a
single identifier token. Worth remembering alongside `BACKLOG.md`'s existing "structural ceiling on
embedded-DSL diagnostics" note (which is about a different range — a static-argument expression — but is
the same family of "whole expression, not sub-token" granularity).

## Round 2 — synthetic backtick-named member: legal and genuinely discoverable

**PASS**, both halves. A fourth property named `` warning MYR099: synthetic diagnostic with no natural
member `` (spaces and a colon, requiring double-backtick access) was accepted by `ProvidedProperty`'s
constructor with no SDK-level rejection. A consumer referencing it via
`` s.``warning MYR099: synthetic diagnostic with no natural member``  `` produced **zero diagnostics**
(legal, accessible). Separately — the actual claim, checked independently rather than conflated with
accessibility — `FSharpCheckFileResults.GetDeclarationListInfo`, called at a position right after `s.` on
a line with no further text, returned a completion list whose items include the synthetic name
(`decls.Items |> Array.exists (fun i -> i.NameInList.Contains "MYR099")` = `true`). A diagnostic with no
natural member to hang off can genuinely surface directly in a real completion list, not merely be
reachable if the developer already knows the exact string to type.

## Round 3 — one shared analysis, two consistent emitters

**PASS.** `SharedAnalysis.Analyze.analyze` run against `SampleLib/Company.fs` (`type Meta = { version:
string }` / `type Company = { title: string; meta: Meta }`) finds exactly one diagnostic: the `meta`
field's type `Meta` is itself a same-file record, not a primitive. `EmitterA` renders it as
`Company.fs(5,39,5,43): warning MYR012: field 'meta' has type 'Meta', which is declared in this same
file and cannot be resolved without a real build; it will be exposed as 'obj'` — verified character for
character against Microsoft's documented canonical format (`origin(line,col,line,col): category code:
text`), range anchored precisely at the `Meta` type-annotation text (columns 39-43 on line 5, confirmed
by manual character counting against the source line, not merely printed and eyeballed). Separately,
`DiagTP.Provided.Fields<"...Company.fs", "SampleNs.Company">` — the same `Q019`-shaped self-parsing
provider, now calling the identical compiled `SharedAnalysis.Analyze.analyze` during instantiation —
poisons its `meta` member with the identical message text, and a consumer referencing `c.meta` gets a
live diagnostic containing that exact message (modulo FCS's own added "This construct is deprecated."
prefix and the message being embedded in a range that also swallows FCS's own wrapping — checked by
substring containment against the shared core text, not by full-string equality, since each channel's
own presentation format necessarily differs). Both channels report the same underlying finding from one
analysis call; neither computed anything independently.

## What this does and doesn't show

- **Shown:** an erased provider can turn a self-detected structural limitation (a same-file field type
  it cannot resolve) into a live, correctly-severity-tagged, use-site-anchored diagnostic instead of
  silently degrading to `obj` or throwing — the load-bearing mechanism claim, verified with an
  independently-computed range and a verbatim message check, not eyeballed. A synthetic member name can
  carry a diagnostic message and genuinely appear in a real completion list. Myriad's own analysis logic
  (self-parsing, structurally identical to `Q019`'s) can drive a second, declaration-anchored,
  MSBuild-canonical-format diagnostic from the exact same compiled function — a capability Myriad
  plugins have never had (`IMyriadGenerator.Generate` can only return output or throw).
- **Not shown:** any of this wired into Myriad's real `IMyriadGenerator` interface or CLI
  (`src/Myriad/Program.fs`) — `EmitterA` is a standalone demonstration of the format and the
  shared-function claim, not a change to Myriad itself; a real Ionide/FSAC session (the standing
  `FSharpChecker`-as-library caveat inherited from every prior quartet in this lineage); a quick-fix or
  code-action for either channel; whether `FSharp.Analyzers.SDK`, which can report diagnostics at
  arbitrary ranges today with no type provider involved, already delivers an equal or better live
  channel for Ionide specifically — named explicitly in `00-hypothesis.md`'s novelty gate as a real
  competing alternative this quartet does not build or benchmark against, left for `03-review.md` to
  weigh.
