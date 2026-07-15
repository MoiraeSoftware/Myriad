# Q013-compile-then-pc-warming / Movement 4 — Adversarial review

## Verified directly before writing this review

- **Raw logs match the results table byte-for-byte.** Read `artifacts/run-logs/run-round1.txt` in
  full and cross-checked every cell of `02-results.md`'s Round 1 table against it: r1a cold FAIL
  1516ms `tmprzxqyh` (log:12-14), warm Compile exit 0 719ms 1 firing (log:18-20), warm PC FAIL 310ms
  `tmp0blrck` 2 firings (log:21-26); r1b cold 140ms `tmpsf5yrl`, Compile 280ms, warm PC 236ms
  `tmpfhkgpt`; r1c cold 285ms `tmptk4epy`, Compile 311ms, warm PC 282ms `tmp1zlzjg`. Every number,
  every temp-assembly name, and both firing counts (Compile 1, PC 2) appear in the log exactly as
  quoted. The verbatim diagnostic block quoted at `02-results.md:92-94` (`couldn't find type
  'ProbeSimple.Provided.T' in assembly 'tmp0blrck, ...'`) is `run-round1.txt:23` word for word.
- **The second process run is a genuine independent replicate, not a copy.** `run-round1-rerun.txt`
  has the same `[false;false;false]` / `[false;false;false]` / NULL summary but *different* temp
  names throughout (`tmpg2pqum`, `tmphaftwd`, `tmpdyxcw4`, `tmpydti0v`, `tmp1dpk3v`, `tmpqo0ozv`) and
  different timings (cold r1a 1272ms vs 1516ms) — i.e. it re-ran the code rather than reprinting the
  first log. The boolean verdict is stable across both; the temp identities are fresh each run.
- **The harness does what the design says.** Read `artifacts/Harness/Program.fs`. `warmTrial`
  (115-131) creates a fresh `FSharpChecker` per trial (`mkChecker`, 116), calls `compile` then
  `parseAndCheck` on that *same* instance (119, 125), in that order. `round1` (144-171) loops
  `r1a/r1b/r1c` with fresh tags and a fresh checker per repeat. `probeInstantiation` (102) and
  `probeConsumer` (104) both target `ProbeSimple.Provided.Probe<"TAG">` with the identical tag, so
  Compile and PC hit the same generative instantiation. The verdict logic (163-169) correctly maps
  all-fail → NULL, all-resolve → POSITIVE, mixed → KILL.
- **The shape under test is genuinely generative.** `artifacts/ProbeSimple.DesignTime/ProbeProvider.fs:78`
  and `:106` build `Probe` with `isErased = false` and a real `ProvidedAssembly`; the erased control
  (`:117`, `:123`) is `isErased = true`. This is Q012's exact minimal-difference probe, unmodified.
  `createType` (95-103) memoizes on `(typeName, tag)`, so within a single check the same
  `ProvidedAssembly` is returned on the second firing.
- **The cold control reproduces Q012's baseline in-process.** Cold PC fails with the identical
  "couldn't find type" diagnostic (log:14, 32, 50), confirming the harness reproduces Q012's cold-fail
  in this process — so the warm NULL is not an artifact of a broken or differently-pinned harness.
- **The Compile half genuinely resolves the type.** Every warm Compile is exit 0 with 1 firing
  (log:18-20, 36-38, 54-56), confirming the scenario is a real, materializable generative type that
  `Compile` can resolve. The asymmetry Q012 found (Compile resolves, PC does not) is reproduced, so
  the warming test is meaningful rather than testing an unbuildable type.

The write-up's framing does not outrun the evidence. This is a clean, honestly-reported negative.

## Strongest objections

1. **The NULL closes one named reconciliation, not "warming" in general — and the headline in
   `02-results.md` is slightly stronger than the body's own careful scoping.** Line 148 says the
   result "removes that reconciliation"; line 152-153 scopes it correctly to "the same-scenario,
   same-checker form ... if some warming mechanism explains Q008, it is not this one." The 152 version
   is the operative, defensible claim and should be the one cited. What was actually falsified is the
   single cheapest hypothesis Q012's review named: same-checker, same-scenario compile-then-PC. Three
   plausible warming shapes remain untested and are *not* logically ruled out by this NULL:
   (a) cross-instance / process-global warming (Round 2 — gated behind a positive Round 1 and so not
   run; but a same-instance null does not entail a process-global null, it only makes it less likely);
   (b) the stricter same-project-object PC form (Correction 1, below); (c) `TransparentCompiler`,
   which caches differently across API boundaries and was left at the default. The verdict is NULL,
   but it must be cited as "the named reconciliation is dead," not "warming is impossible."

2. **Correction 1's resolution is reasonable and does not corrupt the verdict, but it leaves a
   real, shared blind spot that the NULL does not close — and it is the same blind spot Q012 has.**
   Both Q012 and Q013 test PC exclusively via a `.fsx` script through
   `GetProjectOptionsFromScript` (`Program.fs:65-73`), while Compile builds a `.fs` library from
   `fsc`-style args (`:89-100`). The "same scenario" that is held identical is the generative
   *instantiation string*, not the project object. The stricter precondition reading — PC over a real
   non-script `FSharpProjectOptions` for the same `.fs`, in place — is genuinely untested. The
   results file is honest that this is "a different, narrower experiment ... genuinely untested," and
   its inference that a NULL in the permissive form makes a positive there "unlikely" is reasonable
   but unproven. This matters specifically because Q008's harness shape is unknown: if Q008 used
   `ParseAndCheckFileInProject` over a real compiled-args project rather than a script, then *neither*
   Q012 nor Q013 has tested the form Q008 might have used, and the script-vs-project axis becomes a
   live candidate the whole lineage has never varied. This does not change the Q013 verdict (which is
   scoped to the reconciliation it set out to test), but it should be the first variable a Q008
   reconstruction varies, or the reconstruction risks reproducing the same blind spot.

3. **Correction 2 explains why Compile does not help PC; it does not explain why PC fails on its
   own — and its causal phrasing is slightly loose.** The observation is well-supported: warm PC temp
   names (`tmp0blrck`, `tmpfhkgpt`, `tmp1zlzjg`) are all distinct from each other and from the cold PC
   names (`tmprzxqyh`, `tmpsf5yrl`, `tmptk4epy`), so each PC mints a fresh `ProvidedAssembly` and
   reuses nothing the prior Compile produced. That is a legitimate mechanistic reason there is *no
   warming*: the two API paths do not share the materialized-assembly artifact. But it is not an
   explanation of the underlying Q012 failure (why PC's own regenerated generative type is
   unresolvable) — that remains open and is the FCS-code-path follow-up Q012 already flagged. Minor
   imprecision: the write-up says the "memoized `createType` fires 2× per PC, minting a new
   temp-assembly identity." The 2× firing does not itself mint a new identity — the memoization
   (`ProbeProvider.fs:95-103`) returns the *same* `ProvidedAssembly` on the second firing within a
   call. The fresh identity comes from a fresh design-time provider instance (fresh cache) per PC
   call, not from the double-firing. The load-bearing claim (fresh temp assembly per PC, nothing
   reused from Compile) is correct and log-backed; only the stated cause of the freshness is slightly
   off.

4. **Determinism rests on 6 boolean negatives, which is adequate here but should be stated as what
   it is.** Three in-process repeats plus one full second-process run give six warm trials and six
   cold trials, all with an identical boolean outcome. That is thinner than "many repeats," and the
   *timings* are in fact highly non-deterministic (cold r1a 1516ms vs 1272ms; 140ms to 1516ms across
   colds) — so "deterministic" is true only of the resolve/fail boolean, which is the only thing the
   KILL threshold cares about. For an outcome that is a discrete name-resolution success/failure, and
   with Correction 2 supplying a mechanism for *why* it can never warm, six identical negatives plus a
   mechanism is sufficient to rule out flakiness. This is a comparable basis to Q012's determinism
   claim (which rested on 3 toy shapes plus 2 real providers, similarly boolean). KILL is correctly
   not triggered.

5. **The incidental corroboration is weighted correctly — neither oversold nor buried.** The r1b/r1c
   cold controls run *after* earlier repeats' Compile calls have executed in the same process (on
   different checkers, different tags) and still fail. The write-up (116-120) labels this "weak
   incidental evidence" that a prior Compile of a *different* scenario does not warm a later PC via
   process-global state, and explicitly says it is "not the Round 2 test." That is the right weight:
   it only rules out the crudest "any compile warms any PC" mechanism, and because a different tag is
   a different generative instantiation (hence a different temp assembly regardless), it says nothing
   about *same-scenario* cross-instance warming, which is the actual Round 2. It is reported as an
   observation, not as a substitute for the gated-out round.

## Verdict

**NULL**, matching the pre-registered threshold in `00-hypothesis.md` (lines 110-114) cleanly:
"the falsifier fails — compile-then-PC ordering makes no difference; PC still fails identically (same
'couldn't find type' diagnostic) after a prior `Compile` of the identical scenario, same checker
instance, repeated 3+ times." That is exactly what the logs show, verified above. SHIP is out (Round 1
did not pass, so its Round-3 conjunct is moot); REVISE is out (REVISE required a positive falsifier
narrowed by Round 3, and the falsifier was negative); KILL is out (perfectly deterministic boolean
across six trials, no crash, no hang).

Two qualifications travel with the NULL, in the same spirit Q012's review qualified its own
SHIP/NULL framing:

- **Cite it scoped.** This falsifies the *specific* reconciliation Q012's review named as cheapest and
  most plausible — same-checker, same-scenario compile-then-PC warming, on the toy generative shape,
  in the `.fsx`-PC / `.fs`-Compile helper form. It does not falsify "any warming mechanism could
  reconcile Q008." The correct one-line consequence is: the cheapest charitable explanation for
  Q008's disputed result is now specifically dead, so reconstructing Q008/Q09's actual harness is the
  only remaining path to reconcile them and is no longer optional. That consequence *is* earned by
  what was tested; the stronger reading ("warming is impossible") is not, and the results file's body
  (152-153) already avoids it even though its section header (148) drifts toward it.
- **The verdict is trustworthy despite Correction 1**, because the test was correctly scoped to the
  reconciliation it set out to kill and kept apples-to-apples with Q012. But Correction 1 exposes a
  blind spot the whole lineage shares (PC only ever exercised via a script, never a real `.fs`
  project), and that blind spot — not further warming-ordering tests — is where a Q008 reconstruction
  should look first.

Net effect on the standing Q008-vs-Q012 contradiction: **hardened, as pre-registered.** Q012 remains
the artifact-backed side; Q008/Q09's SHIP verdicts remain actively disputed, and the cheapest route
to clearing them has now been tried and failed.

## Follow-ups, prioritized

1. **Reconstruct Q008/Q09's actual harness** — now the clear top priority per the NULL threshold, no
   longer optional. When doing so, vary the two axes this lineage has never varied: (a) PC over a real
   non-script `FSharpProjectOptions` / compiled-args project rather than a `.fsx` script (Correction
   1's untested form, objection 2); (b) whether Q008 reused a single checker across a compile-then-
   check flow at all. If Q008 cannot be reconstructed to resolve a generative type via PC under *any*
   of these, its SHIP claim should be formally downgraded, not just flagged.
2. **The stricter same-project-object PC form** (Correction 1) — a small, self-contained follow-up:
   build one non-script `FSharpProjectOptions` for the generative `.fs`, `Compile` it, then PC the
   same file in place on the same checker. NULL here would close objection 2's residual doubt cheaply;
   it is the one warming variant most likely to differ from what Q013 tested.
3. **Cross-instance / process-global warming (the un-run Round 2)** — lower priority because a
   same-instance null makes it unlikely and the incidental corroboration weakly points the same way,
   but it is the one warming mechanism this NULL does not logically exclude. Worth one run if 1 and 2
   both come back negative.
4. **Locate the FCS code path** that makes the diagnostics-only checking API decline to emit a
   generative provider's backend assembly (carried over from Q012's follow-up 3) — the only route from
   "characterize" to "fix." Correction 2 narrows where to look: PC regenerates into a fresh temp
   assembly and never consults Compile's output, so the gap is in PC's own generative-emit path, not
   in cross-API cache sharing.
5. Real-IDE-host testing — the standing Thread-2 caveat since Q006, still untested.
