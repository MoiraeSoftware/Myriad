# Q021-reentrant-generation-live-edit-loop / Movement 3 — Results

**Status:** EXECUTED, 2026-07-16.
**Environment:** `dotnet 9.0.310`, `FSharp.Compiler.Service 43.9.101` (matches `Myriad/paket.lock`),
Windows, `dotnet run -c Release`. Full source: `artifacts/round1-liveness/Program.fs`.

## What actually happened, round by round

### Round 1 (cheapest falsifier) — PASSED as designed

Two cycles on one persistent `FSharpChecker`: cycle 1 sets the prefix's `Id` field to `int`, cycle 2
edits it to `string` (both versions bumped in lockstep, as designed). Cycle 2's generated text
reflected `System.String`, not a repeat of cycle 1's `System.Int32`.

```
[R1C1] idType=int      aVer=1 stratVer=1 bumped=true entered=true errs=0 841ms
       stripped=System.Int32    syntactic=Domain.Id
[R1C2] idType=string   aVer=2 stratVer=2 bumped=true entered=true errs=0 16ms
       stripped=System.String   syntactic=Domain.Id
Round 1 pass: true
```

The first cycle's 841ms includes cold-start (checker construction, first project options resolution);
every subsequent cycle on the same instance ran in single-digit-to-low-double-digit milliseconds. The
reentrant callback fired on every cycle (`entered=true`), and each cycle's alias-stripped type is the
one this repo's own established technique (Q001/Q010) uses to prove typed, not syntactic, resolution.

### Round 2 (full liveness, four cycles) — PASSED exactly as designed

`int -> string -> int64 -> int`, the fourth cycle deliberately repeating the first's value to rule out
a fixed one-cycle-lag artifact. All four cycles matched their own cycle's edit exactly, zero
diagnostics, no hang, no exception:

```
expected: ["System.Int32"; "System.String"; "System.Int64"; "System.Int32"]
actual:   ["System.Int32"; "System.String"; "System.Int64"; "System.Int32"]
Round 2 pass: true
```

The repeat-value cycle (R2C4) correctly returned to `System.Int32` rather than staying stuck on
`System.Int64` from the prior cycle, which is the specific case the design flagged as the one a
naive "did it change from last time" check would miss.

### Round 3 (omitted-invalidation control) — ran as designed, produced a genuine surprise

Editing the prefix to `decimal` **without** bumping either version integer still produced a fresh,
correct `System.Decimal` result — not the stale `System.Int32` from the prior cycle the hypothesis's
REVISE threshold anticipated as the likely failure mode:

```
[R3] idType=decimal  aVer=6 stratVer=6 bumped=false entered=true errs=0 13ms
     stripped=System.Decimal  syntactic=Domain.Id
Round 3 result: FRESH
```

**This is a real result, but 00-hypothesis.md's own Round 3 rationale explicitly warned against
crediting the wrong mechanism for a pass like this** — and that warning turned out to be exactly
right. Two explanations were live at this point: (a) `TransparentCompiler` genuinely doesn't need
version-bump discipline for this callback-driven path, or (b) the outer API used throughout
(`checker.ParseAndCheckProject`) always fully re-checks every file in the project on every call,
independent of any caching, making the version-bump question moot rather than answered. Distinguishing
these two was not in the original design — it became necessary only once Round 3's result came in, and
is exactly the kind of design correction this repo's own discipline asks to be reported honestly rather
than silently absorbed into a claimed clean pass.

### Round 4 (added after Round 3, not in the pre-registered design) — reproduced a known footgun instead of answering the question it was built for

Attempted to distinguish (a) from (b) by driving the outer check through
`checker.ParseAndCheckFileInProject(fileStratified, ...)` directly (Q010's own "literal form"),
reasoning that this single-file API might behave differently from whole-project `ParseAndCheckProject`.
It did not test what it was meant to: **the reentrant callback never fired at all** (`entered=false`
on both attempts), and both calls silently returned the *previous* cycle's stale `System.Decimal`
result:

```
[R4C1] idType=single   aVer=7 stratVer=7 bumped=true entered=false errs=0 21ms
       stripped=System.Decimal  syntactic=Domain.Id
[R4C2-same-version] idType=byte  aVer=7 stratVer=7 bumped=false entered=false errs=0 8ms
       stripped=System.Decimal  syntactic=Domain.Id
```

This is `Q010-prefix-stratified-generation`'s own already-documented footgun (`ParseAndCheckFileInProject`
with explicit source text silently bypasses `DocumentSource.Custom` for that file, no error signal),
reproduced independently in a new, unrelated harness — the placeholder text passed as the explicit
source argument short-circuited the callback entirely, so both calls just checked the (trivially valid)
placeholder text and returned whatever `strippedForm` last held from Round 3. Round 4 is reported here
in full because a bare deletion would hide a real, informative near-miss: it confirms the footgun
generalizes beyond Q010's original scenario, but it settles nothing about (a) vs. (b).

### Round 5 (added after Round 4, not in the pre-registered design) — the callback re-fires; what that implies about recompute is NOT settled

Two back-to-back `ParseAndCheckProject` calls on the same checker/options, with **zero edit** between
them:

```
call A (no edit): entered=true 8ms
call B (no edit, repeat): entered=true 3ms
Round 5 finding: Callback re-fires every ParseAndCheckProject call regardless of content change.
```

The callback fired on both calls, including the second one where nothing had changed since the first.

**Correction, added after independent adversarial review (see `03-review.md`, Objection 1) — the
paragraph originally here claimed this settled explanation (b) ("`ParseAndCheckProject` always fully
re-checks every file on every call, unconditionally") over explanation (a). That claim does not
survive review and is struck, not silently revised.** Round 5 shows only that the callback is
*re-consulted* on every call — it does not show that a downstream full re-typecheck happens every
time. Under `useTransparentCompiler = true`'s content-hash snapshot model, a virtual file backed by
`DocumentSource.Custom` has no disk mtime to stat, so the callback is the *only* way FCS can obtain
current text to hash and compare against its cached snapshot. Being called is consistent with a cheap
fetch-then-cache-hit that skips the expensive recompute (call it explanation (c)) just as much as it
is consistent with (b) — the two are indistinguishable from what this round measured, and if anything
the timings lean toward (c): the repeat call (3-4ms) was *faster* than the first (8-9ms) on identical
input, which is what a warm cache looks like, not a fixed unconditional cost. **The correct statement
is: the callback is re-consulted on every `ParseAndCheckProject` call; whether that triggers a real
recompute or a cheap cache hit is unresolved by what this quartet measured**, not "(b), settled."

## What this means, stated plainly

The **liveness claim itself is true and cleanly demonstrated**: Rounds 1-2 show the reentrant mechanism
Q010 proved for one check continues to produce correct, freshly-recomputed results across a persistent
checker driven through many sequential edit cycles, with no staleness, no hang, and no degradation
across four cycles including a return to a previously-seen value. **This is exactly the correctness
premise `BACKLOG.md` item 18 named as this quartet's cheapest falsifier, and it is confirmed, not
downgraded** — the review corrected an earlier draft of this section that overstated the cost finding
into "materially less favorable than item 18 assumed"; item 18's own text asked only about correctness
under an edit loop, never claimed anything about keystroke-time cost, so there is nothing here for a
cost question to downgrade.

There is a real, smaller, in-scope contribution beyond the correctness confirmation: Round 4 shows the
natural single-file incremental API — `ParseAndCheckFileInProject` called with explicit current source
text, which is exactly what an LSP host has in hand on `didChange` — silently bypasses the callback
entirely (Q010's own footgun, independently reproduced in a fresh harness). That's a genuine, concrete
plumbing constraint on item 18's "purely FSAC integration plumbing" framing: an FSAC-hosted version of
this mechanism cannot simply call the per-file API the normal way and expect the callback to fire; it
would need to route through a whole-project check or find an as-yet-untested calling pattern that
avoids the bypass.

**Honest caveat on Round 5 itself:** this was tested on a two-file toy project where a full-project
recheck costs single-digit milliseconds — nowhere near enough to distinguish (b) from (c), which is
exactly the discriminator a real scale test would provide (BACKLOG item 4 already named this open
scaling question for `BackgroundCompiler`; it now recurs for `TransparentCompiler` under
`ParseAndCheckProject`, untested by any prior quartet). Whether any calling pattern of
`ParseAndCheckFileInProject` honors `DocumentSource.Custom` is also untested — Round 4 only shows the
explicit-current-text path bypasses it.

Every timing number above is a single sample, per this repo's own standing convention across every
prior quartet; no repeated trials were run.
