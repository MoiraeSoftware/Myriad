# Q020-shared-analysis-diagnostic-channels / Movement 4 — Adversarial review

## Reproduction status — read this first, it bounds everything below

I executed this quartet independently. The tooling outage that blocked Q016's reviewer was not in
effect: every `dotnet build`/`dotnet run` I issued ran, so the claims below are from observations I
made, not inherited from saved logs. I built `SharedAnalysis`, `EmitterA`, `DiagTP.DesignTime`,
`DiagTP.Runtime` (all clean, 0 warnings/0 errors), ran `Harness -- all` and `EmitterA` standalone, and
then went well past reproduction: I built a separate checker program and drove **eight** consumer
shapes the executor never tried, to test whether the obsolete-attribute anchoring generalizes.

What I verified by executing:

- **The premise is real, confirmed from source, not taken from the write-up.** `IMyriadGenerator`
  (`src/Myriad.Core/Types.fs:39-42`) is `abstract Generate : GeneratorContext -> Output`, and `Output`
  is `Ast | Source` — there is no diagnostic channel. `src/Myriad/Program.fs:167` either returns
  `Some (instance.Generate context)` or the catch-all at `:268` prints `OTHER: %A{error}` as free text.
  Myriad plugins genuinely have no way to say "this compiles, but here is a warning." The quartet did
  not overstate its own motivation.
- **All three rounds reproduce cell-for-cell.** Round 1: `Warning (5,12)-(5,17) FS0044` and
  `Error (6,12)-(6,22) FS0101`, both carrying `This construct is deprecated. Q020 test message`, Good
  clean, both ranges matching the independently-`locate`d `s.Bad`/`s.BadError` spans. Round 2: 0
  diagnostics on the backtick reference, `MYR099` present in `GetDeclarationListInfo`. Round 3: one live
  `FS0044` at `(5,4)-(5,10)` carrying the shared message, EmitterA line at `(5,39,5,43)`, `sameMessage`
  true. Identical to `run-logs/`.
- **The EmitterA declaration range is correct and I checked it independently of the harness.** Line 5 of
  `Company.fs` is `type Company = { title: string; meta: Meta }`; `Meta` occupies 0-based columns 38-42
  (verified by string index, not by reading the diagnostic back), so EmitterA's `(sc+1, ec+1)` render of
  `(5,39,5,43)` is right. Note this anchors at the **type annotation** `Meta` (col 39), not the field
  name `meta` (col 33) — the message says "field 'meta'" but the squiggle sits on the offending type.
  `02-results.md` discloses this ("anchored precisely at the `Meta` type-annotation text"), so it is not
  a hidden slip, but "anchored at the field's declaration" in the design's prose is loose: it is the
  field's *type*'s range.
- **`Myriad.Core.dll` was already built** at `bin/Release/net9.0/`. `01-design.md`'s reproduction block
  *does* list `dotnet build src/Myriad.Core -c Release` as step one, so there is no missing-step
  reproducibility gap of the kind the brief told me to watch for.

The three rounds are honest. What follows is about what they *mean*, where they stop, and whether the
type-provider half earns its place next to the build-time half.

## Generalization: the anchoring is more robust than the write-up's hedge, and also worse in the worst case

The executor tested exactly one shape: a bare `s.Bad` on a padded line. Its single "found by running"
correction is that the range covers `s.Bad`, not the `Bad` token, framed as "still genuine use-site
anchoring, just coarser." I drove eight more shapes through `ParseAndCheckFileInProject` against the
unmodified `DiagTP.Runtime.dll`. The result is a clean, consistent rule the write-up never states: **the
diagnostic anchors at the whole member-access AST node — receiver plus `.member` — never the containing
expression, and never just the member token.**

- `s.Bad |> ignore` → `(4,8)-(4,13)`: exactly `s.Bad`, **not** the whole pipe.
- `fun () -> s.Bad` → `(4,18)-(4,23)`: exactly `s.Bad`, the lambda body reference.
- `(s.Bad + s.Bad) * 2` → **two** warnings, `(4,9)-(4,14)` and `(4,17)-(4,22)`: each occurrence
  separately, not the arithmetic expression around them.
- `string (s.Bad)` → `(4,16)-(4,21)`: the argument occurrence.
- `if s.Bad > 0 then "<long branch>" else "b"` → `(4,11)-(4,16)`: **only** `s.Bad`, not the if/then/else.

This directly refutes the practical fear the brief asked me to test — that "for a longer or more complex
expression the squiggle becomes uselessly broad." For a long *containing* expression it does not: FCS
squiggles the member-access node and leaves the rest alone. So the executor's "coarser" framing is, if
anything, too pessimistic: for ordinary code the anchor is tight and precise.

Two shapes qualify that, and one is a genuine caveat the write-up misses:

- **Worst case, broad by the receiver, not the container.** `(mkS 1234 5678 9012).Bad` →
  `(4,8)-(4,32)`: the whole 24-character receiver expression is squiggled, because the member-access
  node *contains* its receiver. So the breadth is unbounded in the size of the *receiver*, not the
  surrounding expression. `s.Bad.ToString().Length` similarly spans `s.Bad.ToString`. In practice
  receivers are short identifiers and this rarely bites, but "just coarser, a whole small expression" is
  the best case, not the general one. The honest statement is: the anchor is the member-access node,
  tight when the receiver is an identifier, as wide as the receiver otherwise.
- **Use site means every *textual* reference, not every dataflow use.** `let getBad (x:S) = x.Bad`
  fired the warning at line 3 (the helper's *definition*), and `let v = getBad s` on line 5 produced
  **nothing**. This matters for the hypothesis's own selling sentence — "surfaces the identical message
  live at every use site the developer actually types … where the developer is looking while typing."
  It is every place the *provided member itself* is named in source. A one-line helper that touches the
  member hides the warning from every downstream caller. That is a real narrowing of "every use site,"
  and it is undisclosed.

Net: the load-bearing Round 1 mechanism is real and generalizes cleanly across expression contexts —
better than the write-up claims for the common case, with two honest caveats (unbounded-receiver
breadth, textual-not-dataflow) it should have named.

## Erased vs generative: untested, and not obviously safe to assume

The brief asked whether the mechanism depends on `isErased=true`. I did **not** test a generative
provided member — building a generative provider is a real lift, and I am flagging the gap rather than
papering it. Reasoning both ways: FS0044 fires whenever name resolution sees `System.ObsoleteAttribute`
on a member, with no documented dependence on erasure, so a generative member carrying the same
attribute *should* behave identically. But this repo's own Q012 found that generative provided *types*
do not resolve through `ParseAndCheckFileInProject` at all under a script configuration, and only the
Q008 reconstruction showed they resolve under a real hand-built `FSharpProjectOptions` (which is what
this harness uses). So "generative works the same" is plausible but not free of this lineage's known
generative-resolution weirdness. It is an untested generalization, not a settled one.

## Round 3's "cannot disagree" is tautological, and it is a mechanism result, not a capability result

This is the quartet's headline claim and its weakest. Read the actual check in `Harness.round3`:

```fsharp
live.Message.Contains emitted.Message
|| emitted.Message = (live.Message.Replace("This construct is deprecated. ", "").Trim())
```

`emitted.Message` is `d.Message` from `SharedAnalysis.Analyze.analyze`. `live.Message` is FCS's fixed
prefix prepended to *that same string*, because `DiagTP` passed the identical `d.Message` to
`AddObsoleteAttribute`. The two are the **same immutable string value** rendered through two channels.
The substring check will therefore always pass, and it *cannot* fail on substance divergence, because
there is no independent computation to diverge — both channels read the one pure function's output. The
looseness (substring rather than equality) is forced by FCS's prefix and is fine, but the "cannot
disagree" property is not a discovered capability: it is the trivial observation that two callers of the
same pure function get the same answer. It would hold identically for any two consumers of any pure
function, with or without a type provider.

There is one real (if unexercised) fragility: the pairing is `emitterDiags |> List.tryHead` against the
first live diagnostic matching a hard-coded substring. With a single finding this is exact; with
multiple findings the by-position/by-substring pairing could mis-associate an EmitterA line with the
wrong live diagnostic and still report "match." Not a bug here, but the check is not robust to the
multi-diagnostic case the shared-analysis framing is supposed to scale to.

This is the same trap Q019's review named for *its* Round 3: a mechanism/plumbing result wearing a
capability result's clothes. The genuine, non-trivial content of Q020 is entirely in **Round 1** (the
obsolete-attribute use-site channel — new to this repo's TP line) and to a lesser degree **Round 2**
(synthetic-member discoverability). Round 3 confirms the two channels are wired to one function; it does
not demonstrate a capability that needed proving.

## Capability or packaging: the two halves are un-entangled, and the more useful half needs none of the TP

The hypothesis's own strongest argument for the spike (`00-hypothesis.md` "Why this is the right next
spike") is that even if the TP half fails, the Myriad-CLI-side diagnostics API "is a real, independently
useful capability Myriad has never had." That framing is correct and it is exactly why the two halves
must be pulled apart, because the write-up leaves them co-located as if they were one result.

Reading the dependency edges: `EmitterA/Program.fs` opens **only** `SharedAnalysis`. `SharedAnalysis.fs`
depends on `Fantomas.FCS.Syntax` and `Myriad.Core` — no `ProvidedTypes`, no `DiagTP`, no TP machinery of
any kind. `DiagTP` depends on `SharedAnalysis`, not the reverse. So:

- A future Myriad user who wants **only** the build-time diagnostics API needs `SharedAnalysis` plus a
  plugin-interface change to return diagnostics. They need **zero** of the type-provider half — no
  `ProvidedTypes.fs`, no erased provider, no obsolete attribute, no `.DesignTime`/`.Runtime` split.
- Conversely the TP channel reuses the same analysis function but is otherwise a fully independent
  mechanism. These are two un-entangled capabilities that happen to share one pure function and one
  write-up, not one capability with two faces.

And the "build-time diagnostics API" is not actually built. `IMyriadGenerator` is untouched (correctly,
per every prior quartet's convention), and `EmitterA` is a 25-line standalone `printfn` that formats one
MSBuild line. What Q020 demonstrates on the build-time side is the *format* and that a shared pure
function can feed it — not an API Myriad plugins can call. The genuinely valuable outcome the hypothesis
gestures at (a diagnostics return on the plugin interface) remains entirely a follow-up, and it is the
half that would deliver the most value with the least of this quartet's own machinery.

(I did not re-fetch Microsoft's MSBuild-diagnostic-format doc to byte-check EmitterA's line; the
four-integer `origin(l,c,l,c): category code: text` form is a known-valid MSBuild diagnostic shape, and
whether it is character-perfect to the doc does not move the verdict, since EmitterA is a format demo,
not a Myriad integration.)

## The FSharp.Analyzers.SDK alternative: it dominates in the host most F# users run, and the TP's niche is narrow

`00-hypothesis.md` names `FSharp.Analyzers.SDK` as the real competitor and correctly declines to build
it, leaving the weighing to this review. My judgment, from repo knowledge and the established behavior of
the analyzer SDK:

An analyzer calling the identical `SharedAnalysis.analyze` runs live inside FSAC/Ionide and can report a
diagnostic at an **arbitrary** range — including the true declaration site (the `Meta` annotation
EmitterA points at), with chosen severity, no obsolete-attribute hack, and no synthetic backtick
members. For Ionide, which is the host most open-source F# developers actually use, the analyzer
**strictly dominates** the TP channel: it puts the squiggle where the problem is instead of being forced
to the consumer's use site, and it fires whether or not the developer adopts any preview provider.

The TP channel's use-site anchoring is, seen plainly, a *limitation reframed as a feature*: a type
provider can only influence the one range FCS computes for a reference to its own member, so it decorates
that. Where is it the *only* live option? A host that runs type providers live but hosts no F# analyzer:
Visual Studio's F# tooling. There, analyzers do not run and TPs do, so this obsolete-attribute channel is
genuinely the only way to get a live, non-fatal, source-anchored diagnostic. That is a real niche, and it
is why the result is not NULL. But it is narrow twice over: it requires (a) Visual Studio specifically,
and (b) that the developer has adopted the Q019-style erased `Fields<file, record>` preview provider
*and* is referencing the provided member — because the diagnostic only appears at a reference to a
*provided* member, never on the developer's own `[<Lenses>]`-attributed record. A VS user editing their
own record gets nothing from this channel. So the honest scope of the unique win is: Visual Studio users
of a not-yet-real erased preview provider. Everywhere an F# analyzer host exists, the analyzer is the
better tool and needs none of this.

## Round 2, checked: legal and discoverable as claimed, but of dubious value

The synthetic backtick member `` `warning MYR099: …` `` is accepted by `ProvidedProperty` and does
appear in `GetDeclarationListInfo` — both halves reproduced, and the discoverability check is genuinely
separate from mere accessibility, as the pre-registration demanded. The mechanism claim holds. Its
*utility* is questionable and the write-up does not weigh it: a fake member that shows up in completion
is a selectable, callable `int`-returning property (`getterCode = <@@ 0 @@>`). A user can pick it and get
a meaningless value, and it pollutes the completion list of the provided type. As a way to surface a
finding with "no natural member to hang off," it is strictly worse UX than an obsolete attribute on a
real member. It passed its pre-registered bar; it is not a mechanism anyone should ship as-is.

## Contradiction gate

No contradiction with a prior verdict. It reuses Q019's erased self-parsing shape unchanged and extends
it into diagnostics, a dimension Q019 never touched. It is consistent with `BACKLOG.md`'s standing
"structural ceiling" finding: it does not let a provider choose an arbitrary range; it decorates the one
range FCS computes for a member reference for free. The Round 1 obsolete-attribute use-site channel is
genuinely new ground for this repo's Thread-2 lineage — Q008/Q009/Q011 only ever produced **thrown**
errors at the **static-argument** site; this is the first non-fatal, use-site-anchored, severity-
controlled diagnostic in the lineage. That much is a clean, positive, reproduced result.

## Verdict

**SHIP, scoped — in the Q019/Q015 sense, with the caveats mattering more than the label.**

Reading the four pre-registered thresholds literally: the cheapest falsifier passed and I reproduced it
(verbatim message survives with only FCS's fixed "This construct is deprecated." prefix, correct
use-site range, severity flips Warning↔Error with `isError`), so **KILL is out** — the obsolete-
attribute trick does not collapse back to the static-argument site or discard the message. The synthetic
member is both legal and genuinely discoverable via `GetDeclarationListInfo`, and the shared-analysis
claim holds as written (one analysis call, two consumers reporting the same finding, EmitterA in the
MSBuild 4-tuple form). All three SHIP conjuncts are literally met and independently reproduced.
**REVISE's** registered range example — "anchors the whole containing expression rather than the member-
access token" — specifically did **not** occur: my `longexpr` test shows the containing expression is
*not* swallowed; the anchor is the member-access node, tighter than the REVISE trigger describes.
**NULL** does not fire cleanly either, because Visual Studio is a genuine, widely-used host where no F#
analyzer runs and this TP channel is the only live diagnostic option — so I cannot honestly say the
analyzer "delivers the same or better result for the only host that matters." By this repo's own
calibration (Q019 SHIP-scoped on a full three-round reproduction; Q018 REVISE only because a round
actually failed), a fully-reproducing three-round pass whose limits are about *reach*, not correctness,
is a scoped SHIP.

Five things travel with the SHIP and matter more than the word:

- **The only load-bearing new result is Round 1's obsolete-attribute use-site channel.** It is real,
  reproduced, generalizes across expression contexts (I tested eight shapes), and is new to this
  lineage. Cite *that* as the result.
- **Round 3's "cannot disagree" is tautological** — two callers of one pure function trivially agree; the
  `sameMessage` check is a substring test on the same shared string value and cannot fail on substance.
  It is a plumbing result, not a capability, the same trap Q019's Round 3 hit. Do not cite it as a
  capability win.
- **The two halves are un-entangled.** The build-time diagnostics half (`EmitterA`/`SharedAnalysis`)
  needs none of the type-provider machinery, and it is the half the hypothesis itself calls the more
  broadly useful one — yet it is only a `printfn` format demo, not a real `IMyriadGenerator` diagnostics
  API. The genuinely valuable Myriad-side outcome remains a follow-up that this quartet did not build and
  that requires nothing from the TP half.
- **The use-site anchor is the member-access node**, tight for an identifier receiver but as wide as the
  receiver when the receiver is a complex expression, and it fires at every *textual* reference — a
  helper that touches the member once hides the warning from all its callers. Both are honest narrowings
  of "surfaces at every use site the developer types" that the write-up should state.
- **For Ionide, an `FSharp.Analyzers.SDK` analyzer calling the same function strictly dominates** (true
  declaration-site anchoring, arbitrary range, no obsolete hack, no fake members, fires without a preview
  provider). The TP channel is the sole live option only in Visual Studio, and only for a developer who
  has adopted the Q019-style erased preview provider and references its members. That is the real,
  narrow niche — state it, don't let "live at the use site" imply more.

The clean, unqualified win worth stating: a type provider can turn a self-detected structural limitation
into a live, non-fatal, correctly-severity-tagged, use-site-anchored diagnostic carrying a verbatim
custom message, and it is the first such diagnostic in either thread — reproduced, and generalizing past
the one shape the executor tested. Its reach is just much narrower than "one shared analysis, two
channels" implies: one of the two channels is a tautology, the halves are independent, and an analyzer
beats the interesting channel everywhere an analyzer runs.

## Follow-ups, prioritized

1. **Build the actual `IMyriadGenerator` diagnostics API** — the half the hypothesis calls most useful,
   which needs none of the TP machinery here. A `Generate : GeneratorContext -> Output * DiagnosticInfo
   list` (or equivalent) plus `src/Myriad/Program.fs` emitting the canonical MSBuild line is the real,
   independently-useful capability; EmitterA is only its format demo.
2. **Build one real analyzer** calling the same `SharedAnalysis.analyze` and compare in Ionide directly,
   to make the "TP channel earns its keep only in VS" judgment observed rather than reasoned.
3. **Test a generative provided member** carrying the obsolete attribute, to close the erased-vs-
   generative gap this review left open (Q012's generative-resolution caveat makes it genuinely
   uncertain, not obvious).
4. **A real-IDE (Ionide/VS) host test** — the standing `FSharpChecker`-as-library caveat since Q006,
   unchanged. Whether VS actually shows this obsolete diagnostic live on the provided member, and whether
   the receiver-breadth behavior reads acceptably in an editor, is unobserved.
