# myriad-fcs-typed-codegen / Movement 4 — Adversarial review

## Three strongest objections

1. **This validated the input side only, and generalized from one data point.** All four rounds
   still generate F# as raw strings, splice them back as text, and let the compiler reparse them —
   exactly Myriad's current output strategy. Of the roughly five to six pillars a genuine
   "Myriad2" pitch needs (in-process typed hosting; typed/hygienic output instead of
   string-splicing; comptime/FSI-hosted schema evaluation, the piece that would actually subsume
   type providers; a content-addressed incremental cache at the declaration level; composable
   generator pipelines; a real plugin-discovery/packaging story), this work touched exactly one —
   in-process typed hosting with correct incremental invalidation — and tested it against a toy
   record once and one real generator once. That is a real, load-bearing result. It is not five
   sixths of a rewrite.

2. **The one real generator tested came back as a null result on the headline claim.** Fields was
   deliberately chosen as the cheapest falsifier, per the pre-registration, and it did its job: for
   a pure structural-echo generator, typed access changes nothing, because Myriad's syntax-echo
   strategy is *already* correct for this class — it never needed to resolve anything, it just
   needed to copy syntax that was already valid in its original scope. The capability claim survives
   only as an untested hypothesis for a *different* class of generator (one needing real semantic
   reasoning: DU case-shape dispatch for a serializer, distinguishing a `[<Measure>]`-annotated
   type, checking an interface implementation, resolving a type across module/project boundaries).
   None of those were tried. Citing this spike as evidence typed access is a win would be citing a
   null result as a positive one.

3. **The mechanism itself has a real, previously-unknown trap that the pre-registration didn't
   anticipate**, and it's the kind of trap that produces *silent wrong output*, not a build error:
   the default `BackgroundCompiler` path requires the caller to manually track a dependency graph
   and call `InvalidateConfiguration` on every upstream change, or it serves stale "success" on code
   that no longer compiles. The fix found here — `useTransparentCompiler = true` — resolves it, but
   that path is labeled experimental by both FCS and Ionide/FSAC as of the version tested, and
   Ionide itself doesn't default to it. Building the foundation on an experimental compiler mode is
   a real, named risk carried forward, not a solved problem.

## Does this answer the actual question — has Myriad *evolved*, or was one aspect handled?

**One aspect was handled, honestly and specifically.** The single riskiest unknown in the whole
"sister project" pitch — can you host FCS in-process, read typed information, splice generated
code back with zero disk writes, and get correct incremental behavior without inventing new
compiler theory — is now empirically de-risked with real numbers, not just plausible-sounding
architecture. That was worth doing first: if `InvalidateConfiguration`/TransparentCompiler had
turned out to be silently broken, the entire pitch would have died here, cheaply, before any
further investment. It didn't die. That's a genuine result.

But it is not evolution of Myriad, and it has not yet touched the other vantage points raised
earlier in the same conversation (type-provider-style comptime evaluation, typed/hygienic output,
composable generator pipelines, a real plugin-authoring story). Worse, the one generator actually
ported shows the mechanism's headline selling point — "typed beats syntax" — is not automatically
true; it depends entirely on what class of generator you're building, and the cheapest, most
common class (structural echo) doesn't benefit at all. Anyone reading only Round 4's "PROTOTYPE
PASSED" line without the alias-format comparison next to it would walk away with a falsely
positive read of what was proven.

## Verdict

**REVISE, not SHIP.** Foundation is real and now measured, not assumed. The claim that should
travel forward from this spike is narrow: *"in-process typed FCS hosting is a viable base to build
a Myriad successor on, and TransparentCompiler removes the worst correctness trap in that base."*
The claim that should NOT travel forward, because this spike did not earn it: *"typed access makes
Myriad's generators better."* That remains an open hypothesis, testable cheaply — the next spike
that could actually move it is porting a generator that needs real semantic reasoning (a minimal
`System.Text.Json` `JsonSerializerContext` emitter for a two-case DU is the natural next cheapest
falsifier, since DU case-shape dispatch is exactly the kind of thing syntax alone makes awkward and
the typed tree makes easy via `FSharpEntity.UnionCases`). Until that's run, "expanding
metaprogramming to different vantage points" is still a plan, not a result.
