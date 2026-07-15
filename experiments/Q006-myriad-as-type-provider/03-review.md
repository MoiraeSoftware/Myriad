# Q006-myriad-as-type-provider / Movement 4 — Adversarial review

## Three strongest objections

1. **The hypothesis pre-registered exactly the gap this result has, and the results file's own
   framing undersells it.** `00-hypothesis.md`'s validity preconditions say plainly: "A result
   confined to the private `FSharpChecker` harness Q001–Q005 used would not test the actual claim,
   only the mechanism's existence." That is precisely what Round 1 delivered — real numbers (1137ms
   cold, 47ms live re-check, a negative control that correctly errors), but through
   `FSharpChecker` as a library, never through Ionide, FSAC, or even `dotnet fsi`. `02-results.md`
   states this honestly as an open boundary and defers the call to this review, which is the right
   instinct, but a pre-registered validity precondition that already answered the question in
   advance shouldn't be treated as a live judgment call at review time. The design committed, before
   seeing results, to *not* counting this level of evidence as sufficient for the strongest form of
   the claim. Holding to that: this alone caps the verdict below full SHIP, not because the
   mechanism is doubted, but because the specific claim under test — "appears live in the IDE" —
   was never actually pointed at an IDE.

2. **The "must already be compiled in a referenced assembly" boundary is deeper than the results
   file frames it, and probably unfixable by any variant of this approach.** `02-results.md`
   attributes the boundary to a static-argument-type technicality (`System.Type` isn't a valid
   static argument; fixed by switching to a string type-name + reflection). That framing invites
   the reader to wonder if a cleverer static-parameter encoding could route around it. It cannot.
   Type providers resolve types by reflecting over `TypeProviderConfig.ReferencedAssemblies` —
   already-built `.dll`s from a separate, prior compilation. A record declared earlier in the *same*
   file, or elsewhere in the *same* project currently being compiled, is not in that set at any
   static-argument encoding, string or otherwise, because it has no compiled IL yet at the point the
   provider is asked to instantiate. This is a structural property of how every F# type provider
   works, not an artifact of this quartet's specific implementation choices — well-known in the
   ecosystem as the reason type providers always ship as a separate assembly from their consumers.
   That matters because Myriad's actual, dominant usage pattern is exactly the case this rules out:
   `[<Lenses>]` on a record in the file you're currently editing, in the project you're currently
   building. The capability proven here is real, but it's "generate companion members for a record
   type someone already published in a library," a narrower and different-shaped use case than "fix
   the IDE-invisibility gap for Myriad's actual generators" implies — most of Myriad's real
   attributed types are same-project, freshly-written, not pre-published dependencies.

3. **Incidental but relevant to a sibling quartet: Round 3 is a live data point for Q004's own gate,
   still unrun.** Q004 (`Q004-cross-assembly-typed-access/00-hypothesis.md`) pre-registers a NULL
   verdict for exactly the case where "reflection plus `FSharpType` gets equivalent information for
   every case tested" as FCS would — meaning the capability shouldn't be built on typed FCS hosting
   at all. Round 3 here used `Microsoft.FSharp.Reflection.FSharpType.GetRecordFields` against a
   compiled, externally-referenced record and got everything needed (field names, types, correct
   quotation-based access) with zero FCS involvement. That's not dispositive for Q004 — Q004's own
   design specifically wants a case reflection is plausibly weak at (private fields, SRTP-adjacent
   inference), which this round never touched — but it's a real, unplanned confirmation that
   reflection handles the *easy* case cleanly in a genuine generator port, not just a toy. Worth
   citing when Q004 actually runs, not re-deriving from scratch.

## Verdict

**REVISE.** The mechanism is real and now measured, not just argued: a generative type provider
hosting `LensesGenerator`'s actual getter/setter logic typechecks, is runtime-correct on all four
lenses, and responds to a source edit with a 47ms re-check and no rebuild of anything — the sharpest
evidence yet in this quartet lineage that F#'s own compiler-hosting protocol can deliver on the
IDE-invisibility gap named in `BACKLOG.md`. Two things keep it from full SHIP, both pre-registered
as exactly the kind of thing that would:

- **Untested against a real host.** Every prior quartet in this lineage (Q001–Q005) got a pass on
  testing inside a private `FSharpChecker` harness because the claims under test were about
  compiler-hosting mechanism and typed access, where the harness *is* an adequate proxy for the
  claim. This quartet's claim is different in kind — "appears live in the IDE" is a claim about a
  host integration, and the hypothesis correctly anticipated that the harness would not be a valid
  proxy for it. It wasn't.
- **The capability reaches a narrower slice of Myriad's real generator surface than "port a real
  Myriad generator" suggests.** It reaches record types already compiled elsewhere. It does not, and
  structurally cannot, reach the same-file-same-project attributed record that is Myriad's actual
  primary use case. This is not a scoping nitpick — it changes what "SHIP" would even mean here.
  Shipping this pattern would give Myriad a *new*, additive capability (companion members for
  already-published types, closer to what `FSharp.Data`-style consumption-side providers do) rather
  than a fix for the invisibility of Myriad's *existing* generators, which was the gap this quartet
  set out to close.

Two corrections surfaced during the build are worth carrying forward as settled, not open,
questions: `System.Type` cannot be a static argument (use a string + reflection, always), and
`UncheckedQuotations` is not needed for record-copy-update-shaped generative members — plain
`Expr.PropertyGet`/`NewRecord`/`NewTuple`/`Lambda` are sufficient. Both save real time for anyone
building on this.

**If the frontier keeps moving, two different next spikes, not one — they test different things:**

1. **Point the same harness at a real host** (Ionide via a real workspace + FSAC's own APIs, or at
   minimum `dotnet fsi --use:` loading the provider interactively) to close objection 1 directly.
   Cheap relative to this quartet's build cost, since the provider assemblies already exist.
2. **Stop trying to route around the same-project boundary and name it as a hard wall.** Given
   objection 2's structural argument, a next spike shouldn't retry static-parameter encodings — it
   should instead directly test the alternative already named in `BACKLOG.md`'s known-gaps section:
   whether hooking `MyriadSdkGenerateCode`'s existing MSBuild target into the design-time-build path
   gets FSAC to show generated members without a real build, since that path (unlike a type
   provider) runs inside the *same* compilation as the attributed type and doesn't hit this wall at
   all. Q006 answered "can type providers do it" with "yes, for a use case Myriad barely has";
   the backlog's original framing may still be the more direct route to the gap Myriad actually has.
