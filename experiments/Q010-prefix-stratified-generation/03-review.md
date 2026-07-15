# Q010-prefix-stratified-generation / Movement 4 — Adversarial review

## Four strongest objections

1. **It's not established, from what was actually measured, whether Round 1/2's reentrant calls hit
   a checker that had already effectively resolved the A.fs/B.fs prefix through its own internal
   processing of the project in `SourceFiles` order, or whether they were genuinely interleaved with
   still-in-progress work on an unrelated file.** The hypothesis's own question frames the claim in
   the strong form — "while a check of a later file in that same project is already in flight" — and
   the 359ms/126ms results are consistent with real reentrant success, but they're equally consistent
   with a more benign shape: `ParseAndCheckProject` requesting each file's source in dependency order,
   such that by the time `Stratified.fs`'s callback fires, FCS's own machinery has already effectively
   finished with `A.fs`/`B.fs`, and the "reentrant" `ParseAndCheckFileInProject` calls inside the
   callback are landing on an idle checker and mostly hitting warm internal caches rather than
   contending with genuinely in-flight state. `02-results.md` reports aggregate outer-check timing,
   not per-call instrumentation fine-grained enough to distinguish these two stories, and the
   distinction matters: one proves the harder claim in the hypothesis's own title ("mid-compilation,"
   "in flight"), the other proves a real but easier claim ("a later virtual file can be computed from
   already-typed earlier ones via a reentrant call, in this file-ordering shape"). Doesn't change
   whether the pre-registered SHIP threshold was met (it doesn't require proving genuine mid-flight
   contention, only correct results with no hang/exception), but the write-up's own framing
   ("mechanism works... under both TransparentCompiler and BackgroundCompiler") reads as though the
   harder claim was tested. It wasn't, or at least wasn't shown to have been. Worth a cheap, targeted
   follow-up: instrument the reentrant call itself to detect whether it's a genuine fresh check
   (measurable first-call latency) or a cache hit, before leaning on "in flight" language again.

2. **The Round 3 cost result (reentrant 126ms beats staged 309ms, warm) is real as measured but its
   direction is partly a consequence of how "staged" was defined, not an inherent property of staging
   versus reentrancy.** Read `artifacts/round3-cost/Program.fs` directly: the staged path constructs
   **two separate `FSharpChecker` instances** (`checker1` for the A+B prefix, `checker2` for the final
   3-file project), which means `A.fs`/`B.fs` get fully re-typechecked from scratch by `checker2` even
   though `checker1` already checked them seconds earlier — genuinely redundant work with no shared
   cache between the two checker instances. The reentrant path uses **one** checker instance for the
   whole sequence, so its own outer project-level accounting of `A.fs`/`B.fs` likely reuses what the
   callback's reentrant calls already computed. This is a fair test of *the specific staged
   implementation the design asked for* (two independent checker/options pairs is what makes it
   "staged" rather than reentrant in the first place), and the design's own Round 3 spec is
   unambiguous about that shape — so this isn't an execution error. But the conclusion in
   `02-results.md` ("reentrancy is not a cost penalty... if anything the two-checker staging is the
   more expensive shape") should not be read as a general result about staged vs. reentrant
   generation strategies. A staged implementation that reuses one checker across both its stages (fully
   possible, just not what this design specified) would likely close most or all of this gap. Treat
   Round 3 as "reentrancy is at least not obviously more expensive than the most natural staged
   baseline," not as "reentrancy beats staging."

3. **The Round 1 "placeholder form" failure is more consequential than its placement in the write-up
   suggests, and deserves to travel forward as a named risk, not just a design correction.** Calling
   `ParseAndCheckFileInProject(filename, 0, <any explicit SourceText>, options)` on a file backed by
   `DocumentSource.Custom` silently uses the explicit text and never invokes the callback for that
   file — no exception, no diagnostic, no warning. In this quartet's own Round 1 table that produced a
   trivially-valid placeholder passing with 0 errors, which could easily be misread as "the generated
   file is fine" by anyone not specifically checking `reentrantEntered`. This is exactly the failure
   mode this repo's methodology exists to catch (a plausible, easy-to-make mistake — this quartet's own
   frozen design made it — that fails silently rather than loudly), and it's a real, sharp footgun for
   any future engineering built on this mechanism, not a footnote about one wrong API guess. Should be
   carried forward explicitly as a caveat: any production use of this pattern needs its own
   self-check (e.g., a sentinel value only the reentrant callback would produce) to detect this
   specific silent-bypass mode, because FCS gives no signal that it happened.

4. **The scenario tested is strictly one-directional (a later file depending on earlier ones,
   consistent with F#'s own file order) and this quartet does not, and structurally cannot, speak to
   mutual or circular cross-generator dependency.** `02-results.md` doesn't claim otherwise, but the
   hypothesis's framing ("supersedes `BACKLOG.md` item 10 as the preferred route") could be over-read
   as "item 10's iterative/fixpoint approach is now unnecessary." It isn't, for the case item 10 was
   actually named to solve if two generators need *each other's* output — no file ordering resolves
   that, by construction, since it isn't expressible as a prefix. This quartet settles the common,
   acyclic case (Myriad's realistic shape: a serializer generator consuming a lenses generator's
   already-generated output, never the reverse) cleanly. It leaves the mutual-dependency case exactly
   where it was.

## Verdict

**SHIP**, both pre-registered claims. The mechanism claim (Round 1) and capability claim (Round 2)
each met their frozen thresholds cleanly: no hang, no exception, no stack overflow, correct
alias-stripped typed derivation for the mechanism claim; zero diagnostics and two independent,
range-verified symbol-resolution confirmations (not visual inspection of emitted text) for the
capability claim. The capability claim in particular is a clean pass on the harder bar this repo has
applied since Q002 — a generated reference that merely *compiles* is a weaker result than one proven,
by `GetSymbolUseAtLocation` round-tripping to the exact declaration range in the second virtual file,
to have resolved to a real typed symbol. This quartet met that bar, not the weaker one.

Three corrections were reported honestly rather than patched quietly into the frozen files (the
design's literal Round 1 trigger doesn't fire the callback at this FCS pin; the `LensesGenerator`
stand-in's binding names and setter argument order were wrong in the design's sketch; two FCS API
names were guessed wrong) — exactly the kind of design-vs-reality divergence this repo's discipline
asks to be surfaced, not hidden, and it was.

Scope precisely, because four things travel with the ship:

- **Reentrancy was proven safe in the shape actually tested — a later file's callback referencing
  earlier files in `SourceFiles` order — not proven safe as "arbitrary reentrant calls into a checker
  with genuinely in-flight, contended state."** Objection 1 names the gap between what the
  hypothesis's own language claims and what the measurements can actually distinguish. Don't cite this
  quartet for the stronger claim without the follow-up instrumentation objection 1 names.
- **The Round 3 cost result is directionally real but shouldn't generalize past the specific staged
  baseline tested (two independent, non-cache-sharing checker instances).** A smarter staged
  implementation might close the gap; this quartet didn't test one.
- **The silent-bypass footgun (objection 3) is a real risk for anyone building on this mechanism and
  should be named explicitly in any follow-on design, not filed away as a one-line correction.**
- **This settles the acyclic, one-directional cross-generator case only.** `BACKLOG.md` item 10's
  iterative/fixpoint approach remains the only known route for mutual cross-generator dependency,
  which this quartet does not attempt and structurally cannot resolve via file ordering alone.

**Follow-ups, if the frontier keeps moving (recorded in `BACKLOG.md`):**

1. Instrument the reentrant call itself (first-call latency, or a counter inside FCS's own file-check
   entry point if accessible) to determine whether Round 1/2's reentrancy was landing on genuinely
   in-flight state or an already-idle checker — settles objection 1 either way.
2. Re-run Round 3 with a staged baseline that reuses one `FSharpChecker` instance across both its
   stages, to isolate whether reentrancy's apparent cost advantage survives a fairer staged
   comparison.
3. Scale past three files / two generators — a chain of four or five stratified files, each consuming
   the previous one's typed output, to check whether the mechanism's cost or correctness degrades with
   depth (untested in either direction here).
4. Test the mechanism under genuine concurrent access — two outer checks racing against the same
   checker instance, one of which is mid-reentrant-callback when the other starts — closer to what a
   real IDE host or a build with parallel MSBuild targets would actually produce, and a materially
   harder bar than anything this quartet exercised.
5. Build the self-check pattern named in objection 3 (a sentinel proving the reentrant callback
   actually fired) as a reusable guard, before any real generator is built on this mechanism.
