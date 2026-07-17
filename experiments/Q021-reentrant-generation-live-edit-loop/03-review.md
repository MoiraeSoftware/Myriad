# Q021-reentrant-generation-live-edit-loop / Movement 4 — Adversarial review

## Reproduction status — read this first, it bounds everything below

I rebuilt and reran the spike independently (`dotnet run -c Release` in
`artifacts/round1-liveness/`). Every round reproduces:

- Round 1: `R1C1` int → `System.Int32`, `R1C2` string → `System.String`, both `entered=true`,
  `errs=0`. Pass.
- Round 2: `int → string → int64 → int` → `["System.Int32"; "System.String"; "System.Int64";
  "System.Int32"]`, exact match, zero diagnostics every cycle, the repeat-to-`int` cycle returns to
  `System.Int32` rather than sticking on `System.Int64`. Pass.
- Round 3: `decimal` with **no** version bump → `System.Decimal`, `entered=true`, fresh. Reproduced.
- Round 4: `ParseAndCheckFileInProject` with explicit placeholder text → `entered=false` both times,
  both calls return the stale `System.Decimal` from Round 3. The program's own logic even self-labels
  this `INCONCLUSIVE`, matching the prose. Reproduced.
- Round 5: two back-to-back `ParseAndCheckProject` calls, zero edit → `entered=true` both (8ms then
  4ms in my run). Reproduced.

One environmental note, not material to the verdict: the build emits `NU1608` because `FSharp.Core`
resolves to `9.0.303` while `FSharp.Compiler.Service 43.9.101` requests `9.0.101`. The FCS package
itself is the pinned `43.9.101` the preconditions require, so the pin that matters holds.

The results file is honest about what happened. The three objections below are about what Round 5
*means*, whether the "materially downgrades item 18" framing follows, and whether the pre-registered
SHIP bar was actually met. The primary liveness claim is real and I reproduced it; the secondary
conclusion built on Round 5 is overstated.

## Objection 1 (the load-bearing one): Round 5 does not prove explanation (b)

`02-results.md` states Round 5 "settles the question Round 3 left open: explanation (b) is correct" —
that `ParseAndCheckProject` "re-invokes `DocumentSource.Custom` for every file in the project on every
call, unconditionally... it is not incremental in any sense this harness could detect." The evidence
does not support that conclusion. What Round 5 shows is narrower: **the callback re-fires.** Callback
firing (`entered=true`) is not the same event as a downstream full re-typecheck, and conflating the
two is the error.

There is a third explanation the results file names as (b)'s definition but never actually rules out,
and it is the *documented* behavior of the compiler under test. `useTransparentCompiler = true` uses a
content-hash snapshot model. For a virtual file backed by `DocumentSource.Custom` there is no disk
mtime to stat — the *only* way FCS can learn the file's current content, and therefore the only way it
can decide whether its cached snapshot is still valid, is to call the callback. So on every
`ParseAndCheckProject` the callback *must* be consulted to obtain text to hash, after which FCS can
compare that hash to its cache and, if unchanged, return the prior typecheck result without redoing
the expensive work. Under this reading (call it (c)), `entered=true` on the no-edit repeat is exactly
what a *correctly caching* compiler produces, not evidence of an unconditional recompute. Round 5
cannot distinguish (b) from (c), because both predict `entered=true` on the repeat call.

The timings, which the results file leans on implicitly ("8ms then 3ms... awfully fast"), do not
discriminate either — and if anything they lean toward (c): call B (4ms in my run, 3ms in the
executor's) is *faster* than call A on the identical unchanged input, which is what a warm cache
skipping work looks like, not a fixed unconditional recompute. On a two-file toy where a from-scratch
check costs single-digit ms, the numbers are noise with respect to this question. A real
discriminator (hundreds of files, or first-call-vs-repeat latency on the reentrant inner check) was
not run.

This matters because the entire pessimistic half of the write-up rests on (b). If (c) is the truth —
and the compiler's own snapshot design says it is the more likely one — then unchanged files are
cache hits, keystroke cost may be perfectly acceptable, and the "materially less favorable mechanism"
conclusion evaporates. The honest statement is: **Round 5 proves the callback is consulted on every
call; it does not prove the typecheck is recomputed on every call, and the more probable reading given
`TransparentCompiler`'s content-hash model is that it is not.** The "unconditional full re-check"
claim should be struck to "the callback is re-consulted every call; whether that triggers real
recompute or a cache hit is unresolved by what was measured."

## Objection 2: "materially downgrades item 18's premise" does not follow

Item 18's cheapest-falsifier text (BACKLOG.md:488-492) is explicit about what it asked this quartet to
retire: "confirm Q010's reentrant-`DocumentSource.Custom` mechanism still produces correct
completion/typecheck results when the outer driving loop is an LSP `didChange`-shaped edit sequence...
if that holds, the remaining risk is purely FSAC integration plumbing." That is a **correctness**
premise. It says nothing about keystroke-time incremental cost. Rounds 1-2 confirmed exactly the
correctness premise item 18 named — cleanly, across four cycles including a return visit, with no
staleness. On item 18's own stated terms, its premise was *confirmed*, not downgraded.

The "materially less favorable... materially downgrades item 18" framing is therefore built on a cost
concern that (i) item 18 never claimed to have settled, so confirming-or-not is out of its scope, and
(ii) Q021 did not actually establish exists (Objection 1). Two layers of overstatement stacked.

There *is* a real, in-scope contribution to item 18 here, and the write-up buries it under the
overstated one: Round 4 shows the natural per-file incremental API — `ParseAndCheckFileInProject` with
explicit current source text, which is precisely what an LSP host has in hand on `didChange` — silently
bypasses the callback. That is a genuine constraint on item 18's "purely plumbing" claim: an FSAC
integration cannot just call the per-file API the way it normally would; it must route through a
whole-project check or otherwise solve the bypass. That *sharpens* the plumbing risk. It does not
downgrade the correctness premise. The correct framing is "Q021 confirms item 18's correctness premise
and adds a concrete plumbing constraint plus an open, untested cost question," not "materially
downgrades item 18's premise."

## Objection 3: the pre-registered SHIP bar was not met as written (it failed safe)

The frozen SHIP threshold has two conjuncts. The first — "every cycle... zero-diagnostic generated
text whose typed-resolved content matches that cycle's current prefix value" — is met cleanly, and I
reproduced it. The second is not: "the omitted-invalidation control cycle produces a *visibly
different* (stale or erroring) result, confirming the invalidation call is actually load-bearing." The
control cycle (Round 3) came back **FRESH and correct**, proving the version-bump discipline is *not*
load-bearing under `ParseAndCheckProject`. The conjunct that required the control to fail cannot be
satisfied, because its premise (that the discipline is necessary) was wrong.

This is worth stating plainly rather than smoothing over: the quartet did not meet its SHIP bar as
literally pre-registered. But it missed in the *safe* direction — the mechanism needed *less*
discipline than the hypothesis feared, and correctness never degraded on any cycle. The REVISE
threshold as written ("a single persistent instance serves stale results across at least one cycle
even with invalidation called correctly") did not fire — the persistent instance never served stale
results. KILL (callback hangs/throws/stops firing on reuse) did not fire — it fired correctly every
cycle. So the pre-registered thresholds do not cleanly classify this outcome; the verdict has to be
made off-map, and the honest reason it is off-map is that the hypothesis mis-predicted which mechanism
was doing the work. That is exactly the kind of "reported honestly, not patched into the frozen files"
correction this repo's discipline exists to surface, and the results file does surface it (Round 3's
"credit the wrong mechanism" warning, lifted verbatim from the design, turned out to apply to the
design's own assumption).

## Honesty and generalization checks (credit where due)

- **Unplanned rounds are labeled as such.** Round 4's and Round 5's headers both say "added after...
  not in the pre-registered design," and the source carries a "DEVIATION FROM DESIGN, discovered while
  running Round 3" comment. No retroactive claim that these were planned. Good. One small omission: the
  design's *own* optional Round 4 (a `BackgroundCompiler` comparison, explicitly a non-threshold
  stretch) was silently dropped in favor of the renumbered new Rounds 4-5; a one-line note that it was
  not run would have been cleaner, though skipping an explicitly-optional stretch is within bounds.
- **Round 4 is a genuinely useful reproduction, honestly framed.** It independently reproduces Q010's
  silent-bypass footgun (explicit source text on `ParseAndCheckFileInProject` bypasses
  `DocumentSource.Custom`, no error) in a fresh, unrelated harness, confirming it generalizes past
  Q010's original scenario. The write-up correctly reports it as a near-miss that settles nothing about
  (a)/(b) rather than dressing it up.
- **Scale is untested and flagged.** The results file's own caveat names the two-file toy limitation
  and ties it to BACKLOG item 4's scaling question. Correct. Objection 1 makes this worse than the
  write-up frames it: it is not merely "the cost is untested at scale," it is "the *mechanism* (b vs c)
  is unresolved *and* untested at scale" — and scale is precisely where (b) and (c) diverge by orders
  of magnitude, so a scale test would resolve both at once.
- **Single-sample timings are disclosed** per this repo's standing convention.

## Verdict

**SHIP, scoped — narrowly to the liveness/correctness claim, with the Round 5 conclusion struck.**

What ships: on a single persistent `FSharpChecker` (`useTransparentCompiler = true`) reused across
multiple sequential edit-then-recheck cycles, the reentrant `DocumentSource.Custom` mechanism Q010
proved for one check keeps producing correct, freshly-recomputed, zero-diagnostic results — the edited
prefix's current type is reflected every cycle, including a deliberate return to a previously-seen
value, with no staleness, no hang, no exception, and (an unanticipated bonus) no version-bump
discipline required under `ParseAndCheckProject`. That is the thing item 18 needed retired, and it is
retired. This directly extends Q001's finding that `TransparentCompiler` avoids the explicit-
`InvalidateConfiguration` staleness trap `BackgroundCompiler` needs, now confirmed to hold through the
reentrant callback path across a live edit sequence — a code path no prior quartet drove.

What does **not** ship, and must not be cited:

- **Round 5's "explanation (b) is correct / unconditional full re-check on every call" conclusion.**
  Round 5 proves the callback is re-consulted every call; it does not distinguish that from a cheap
  content-fetch feeding a cache hit, which is the more likely behavior given `TransparentCompiler`'s
  content-hash model (Objection 1). Cite Round 5 only as "the callback re-fires on every
  `ParseAndCheckProject`, mechanism-below-it unresolved."
- **The "materially downgrades item 18's premise" framing.** Item 18's premise was correctness under an
  edit loop and it was confirmed. The real, smaller contribution is a plumbing constraint (the per-file
  incremental API bypasses the callback) plus an open cost/mechanism question, neither of which
  downgrades the premise (Objection 2).

Not a REVISE: no correctness round failed, and the persistent checker never served a stale result —
the mechanism works, and works with less discipline than feared. The pre-registered SHIP bar's second
conjunct was unmet, but it failed safe and its failure is itself an honestly-reported finding, not a
defect in the mechanism. By this repo's calibration (Q018 was REVISE only because a round actually
failed on substance; Q019/Q020 shipped scoped with a secondary claim struck), a clean primary pass
with a struck secondary overclaim is a scoped SHIP. Not a KILL: nothing hung, threw, or stopped firing.

## Follow-ups, prioritized

1. **Resolve (b) vs (c), and do it with a scale test.** Build a project of a few hundred files behind
   the same reentrant callback, edit one, and measure `ParseAndCheckProject` cost on the unchanged
   remainder. If cost stays roughly flat with project size, (c) (caching) holds and the "less
   favorable mechanism" worry is dead; if it grows linearly, (b) holds and the cost concern is real.
   This is the single question the write-up wrongly claims to have closed, and the one item 18 actually
   turns on.
2. **Find whether any `ParseAndCheckFileInProject` calling pattern honors `DocumentSource.Custom`.**
   Round 4 showed only that the explicit-current-text path bypasses it. Test the `None`/callback-owned-
   text paths and any overloads — this is the API an FSAC host wants for keystroke-responsive single-
   file checks, and whether it can be made to drive the callback at all is the crux of item 18's
   plumbing risk.
3. **Concurrent / interleaved access** (carried over unmet from Q010 follow-up 4). A real LSP host
   serves overlapping requests; one outer check mid-reentrant-callback while another starts is still
   untested, and is closer to what FSAC would actually produce than this quartet's strictly sequential
   loop.
4. **Amend BACKLOG item 18** to record what Q021 actually established: correctness premise confirmed;
   new plumbing constraint (per-file incremental API bypasses the callback); cost/mechanism question
   open pending follow-up 1 — replacing any "materially downgraded" language, which the evidence does
   not support.
