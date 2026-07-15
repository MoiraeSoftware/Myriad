# Experiment backlog

Everything named as a follow-up across Q001–Q003 but not yet built, plus the engineering gaps in
*current* Myriad that got verified from source along the way. Two different kinds of item — keep
them separate, because they need different next steps.

Three items formerly listed here have since been promoted to their own pre-registered quartets
rather than left as prose — see `Q004-cross-assembly-typed-access/00-hypothesis.md` (can typed
access reach a type with no source file at all, in a referenced compiled assembly, and does that
need FCS or does plain reflection already solve it) and
`Q005-self-verifying-generators/00-hypothesis.md` (can a generator typecheck its own output before
emitting it, refusing to write code that doesn't compile) — both PLANNED, not yet run — and
`Q006-myriad-as-type-provider/00-hypothesis.md` (can a real Myriad generator be reimplemented as a
generative type provider to fix the IDE-invisibility gap), now **CLOSED, REVISE**: the mechanism
works, but only reaches record types already compiled in a separate referenced assembly, not
Myriad's actual same-project attributed-type workflow, and was only tested through `FSharpChecker`
as a library, never a real IDE host. All three are tracked in `README.md`'s Index, not duplicated
here. Q006's own two named follow-ups are folded into this list below (items 7 and 8) rather than
re-promoted immediately — neither is cheap enough to justify skipping the queue.

## Spike-shaped hypotheses (need a quartet, gated the normal way)

Ordered by how directly each one closes a gap the last quartet named, not by guessed effort.

1. **Full FSI comptime loop (extends Q003).** Q003 only proved data can cross the FSI-to-host
   boundary. Not yet built: FSI evaluates a schema-like value → that value drives real text
   generation → spliced into the same `FSharpChecker` project as Q001/Q002 → typechecked. This is
   the spike that actually earns the type-provider comparison; everything before it was necessary
   but not sufficient. Cheapest next falsifier of the *loop itself*: does the splice+typecheck
   step behave the same as Q001/Q002 when the spliced file's content depends on an FSI evaluation
   result rather than typed-tree introspection? No reason to expect a difference, but Q001 already
   taught this session not to assume compiler-hosting behavior without running it.
   Stretch goal once the base loop lands, not part of the base claim: true staged compilation —
   evaluate at build time against a general implementation and bake a *specialized* result into
   generated code (a parser or state machine compiled from running the general version, not just a
   schema value read off). This needs reifying arbitrary evaluated FSI values (potentially
   closures, captured environments) back into a hygienic `SynExpr`, which is a materially harder
   problem than the primitive/record-shaped data Q003 already proved crosses the boundary cleanly —
   don't fold it into the base loop's pass/fail criteria, scope it as a separate follow-on
   hypothesis once the base loop's own verdict is in.

2. **FSI under `AssemblyLoadContext` isolation (extends Q003).** Q003 deliberately tested the
   simplest configuration (no isolation) and it passed cleanly — cleaner than expected. Myriad's
   real plugin loader (`McMaster.NETCore.Plugins`) isolates plugins into separate ALCs, and
   ALC-isolated code commonly gets its own separate loaded `FSharp.Core`, which is exactly the
   condition that would break the direct-cast result Q003 got. Untested. If it breaks, the
   mitigation (marshal through a shared contract type, or reflection-based structural reads) is
   already sketched in `Q003-fsi-comptime-eval/artifacts/`, but sketched isn't tested.

3. **Typed/AST-safe output construction (extends Q001 + Q002).** Every generator built across all
   three quartets used string-templated output, the same simplification Q001 started with. Q002's
   Round B hit a real bug from it (unparenthesized nested call, two curried args instead of one
   applied call) — exactly the class of error Myriad's real `SynExpr`-construction approach can't
   produce. Not yet tested: build the same nested-dispatch generator (or the FSI-loop one) using
   quotations or an AST-builder DSL instead of `sprintf` templates, and confirm the paren-class of
   bug genuinely can't happen that way. This is the other half of Q001's original "typed access"
   pitch — everything so far has been typed *input*, never typed *output*.

4. **Heavier scaling test for BackgroundCompiler vs TransparentCompiler (redo of Q001 Round 3).**
   Q001's own results flagged this honestly: the padding files used to test scaling from N=2 to
   N=60 were near-free to typecheck (`let x = 5`-level), so the round never actually stress-tested
   whether `BackgroundCompiler`'s "recompute everything preceding the changed file" cost (predicted
   from reading `IncrementalBuild.fs`, not measured) is real at scale. Redo with padding files that
   have genuine typecheck weight (opens, generics, real inference work) before trusting either
   compiler's scaling story past N=60 trivial files.

5. **`open`-aware refinement of the Round C syntax resolver (small, low priority).** Q002's Round C
   found a naive resolver fails on same-named types in different modules. Does tracking the
   reference file's `open` declarations fix that specific case, or does it just relocate the
   failure to a harder one (multiple opens, shadowing/precedence order, module aliases)? Lower
   priority than the above — the typed approach already won this comparison decisively; this would
   only sharpen the margin, not change the verdict.

6. **Whole-project structural invariant enforcement (extends Q002, low priority).** Q002 already
   proved whole-project typed resolution sees cross-file relationships per-file syntax cannot
   (reliably). The same mechanism, pointed at enforcement instead of emission, could let a
   generator assert a project-wide rule — "every type in this namespace must round-trip
   serialize," "every attributed union must have a matching handler" — and fail the build with a
   real diagnostic when violated, rather than only ever emitting code. Lower priority than the
   above: it's a reframing of an already-shipped capability (Q002), not a new one, and has no
   named user pain behind it yet the way Q004/Q005 do. Worth a hypothesis once Q002's lineage has
   a second real generator to test it against, not before.

7. **Real-host visibility test for the Q006 provider (extends Q006, cheap).** Q006's live-edit
   result (47ms re-check, no rebuild) ran through `FSharpChecker` as a library, which its own
   hypothesis pre-registered as insufficient to prove "appears live in the IDE." Point the same
   already-built provider assemblies at a real Ionide workspace (or at minimum `dotnet fsi --use:`
   loading the provider interactively) and confirm the same result holds. Cheap relative to Q006's
   own build cost, since nothing new needs to be built — just re-hosted.

8. **Redirect the IDE-invisibility fix at the DTB/MSBuild path directly (supersedes trying to
   route around Q006's structural wall).** Q006's review concluded the same-project boundary
   (type providers only ever see already-built referenced assemblies, never a type from the
   compilation currently in progress) is a hard wall for *any* type-provider-shaped fix to Myriad's
   actual same-file `[<Attribute>]` workflow — not worth re-attempting with a cleverer static
   parameter. The more direct route back to the gap below ("generated code is invisible to the IDE
   until a real build") is scoping whether hooking `MyriadSdkGenerateCode` into the design-time-build
   path gets FSAC to show generated members without a real build, since that path runs inside the
   *same* compilation as the attributed type and never hits Q006's wall at all.

## General type-provider capability ideas (independent of Myriad, unverified brainstorming)

Not Myriad-specific — these are about `FSharp.TypeProviders.SDK` (`FSharp.TypeProviders.SDK/src/ProvidedTypes.fs`)
itself, same status as Q007. None of these are spiked; all are source-grounded reading, not tested
code. Recorded here rather than dropped because a future session shouldn't have to re-derive the
line numbers. Promote one to its own `Qddd-` quartet before building anything.

- **Units of measure as a general phantom-type tagging system, not just physics.** Verified
  directly (`ProvidedTypes.fs:1339-1383`): `ProvidedMeasureBuilder.SI` (`1368`) is *only* a
  name-to-existing-abbreviation lookup against a hardcoded SI table (`1343-1357`) resolving into
  FSharp.Core's own `Microsoft.FSharp.Data.UnitSystems.SI.*` types — it cannot mint a new base unit.
  `Product`/`Inverse`/`Ratio`/`Square` (`1359-1363`) only *compose* already-existing measure types
  via FSharp.Core's `MeasureProduct<_,_>`/`MeasureInverse<_>` markers. The actual novel move is one
  level down: a **generative** provider (`isErased = false`) can define a brand-new
  `ProvidedTypeDefinition` stamped with `MeasureAttribute` via `AddCustomAttribute` — a real, empty,
  IL-level type, the same way any `[<Measure>] type USD` compiles — then combine it with
  `AnnotateType` (`1383`) to produce e.g. `float<USD>`. That gives compile-time, zero-runtime-cost
  non-mixing over anything schema-driven: currency, tenant IDs, coordinate frames. Real caveat, not
  yet checked against the F# language spec: unit-of-measure annotation syntax (`typ<unit>`) is only
  valid over a specific whitelist of primitive numeric types (float/int/decimal-family) at the
  language level, not arbitrary types — so a "tainted vs. sanitized `string`" variant of this idea
  may not have valid consumer syntax at all even if the `Type` object constructs successfully via
  the API. Confirm that before assuming the idea generalizes past numeric-tagged values.
- **External-signal `Invalidate()` for keeping compile-time-checked references in sync with the
  file system.** `TypeProviderForNamespaces.Invalidate()` (`16271`) is a plain method any live
  provider instance can call, wired to `ITypeProvider.Invalidate` (`16287`) which the host
  subscribes to — nothing ties it to static-parameter re-instantiation, so a `FileSystemWatcher`
  captured in the provider's closure can call it. Partial precedent already exists in the wild:
  sample-file-driven providers (the `JsonProvider<"sample.json">` style) commonly already use
  exactly this pattern to notice when their referenced sample file changes on disk, since FCS has no
  way to know a static-argument *string* secretly names an external file dependency otherwise — so
  this is real and useful, but narrower than "novel," and it's a design-time/compile-time mechanism
  only. It cannot hot-reload content into an already-running process; it can only keep the *type
  shape* an editor/compiler sees in sync with an external source of truth while you're editing.
  Don't conflate the two.
- **A provider reflecting on its own just-emitted IL mid-session.** `GetGeneratedAssemblyContents`/
  `AssemblyCompiler` (`16366`) and `RegisterGeneratedTargetAssembly` (`16372-16383`) mean a provider
  could in principle compile its own generative output to bytes, load it, and reflect over the
  result within one design-time session — no separately published dependency required. This is the
  concrete mechanism behind the open "provider chaining within one process" question first raised
  alongside Q006. Real risk, not hypothetical: `AssemblyLoadContext` identity mismatch and
  re-entering a compilation context built for the host to drive, not for a provider to re-enter.
  Low-medium confidence.
- **Arbitrary `AddCustomAttribute` injection as a bridge into runtime-reflection frameworks.**
  Accepts any `CustomAttributeData` (`950`, `1013`), not just the XmlDoc/Obsolete helpers the SDK
  wraps. A generative provider emits real IL, so stamped attributes (`JsonConverter`, EF mappings,
  DI markers, validation attributes) are visible to any *runtime-reflection-based* .NET framework
  consuming the generated assembly. Does **not** reach Roslyn analyzers or other source-level
  tooling — those read source text, not referenced IL — so "bridge into Roslyn" is the wrong framing
  even though "bridge into reflection-based frameworks" is correct.
- **Pull-based, effectively unbounded namespace/type trees via `AddMembersDelayed`.** Delayed member
  thunks (`1423-1461`, `1826`) are only forced when the host actually asks for a type's members, and
  nested types can themselves be delayed (`1854`) — so a provider can expose a huge or unbounded
  *tree* of types materialized branch-by-branch as IntelliSense navigates it. Confirmed limit: a
  single type's `evalMembers` forces its whole member queue at once, so "lazily stream individual
  members of one type" is not achievable this way, only "lazily materialize which types exist."
- **`AddDefinitionLocation` pointing go-to-definition at the source schema, not the generated
  code.** `AddDefinitionLocation(line, col, filePath)` is exposed on every provided member
  (`941`) and barely used by shipping providers. Low-ambition, low-risk, genuinely useful DX: F12 on
  a provided DB column jumps straight to the `.sql`/`.json` line that produced it. Looked for
  embedded-source/SourceLink-style PDB tricks beyond this and found no clearly wired API for it in
  the PDB path — that stronger variant is unconfirmed, treat as unlikely rather than merely
  untested.

### More ambitious ideas (a second, Fable-model brainstorming pass, explicitly briefed to ignore
the list above and swing bigger)

The six items above are real but share a failure mode: each is "an underused API in this SDK," not
"something that changes what compile time is for." A second pass, briefed explicitly on that gap
and asked for higher-risk ideas, produced five; one has since been spiked and shipped.

- **Provenance-closed-loop typing — SHIPPED, see Q008.** One generative provider embeds derivation
  provenance as real IL attributes; a second, independent provider reads and enforces it, refusing
  to generate when two providers' declared provenance disagrees. `Q008-provenance-closed-loop`
  (`CLOSED`, `SHIP`) proved this end to end: the attribute round-trips through independently-
  reflectable IL, the two-provider gate fails correctly with a diagnostic naming both versions, and
  the conflict is caught on a live source edit with no rebuild, at 19-32ms (at or below Q006's 47ms
  baseline). Scoped precisely by the review: the real win is *when and where* the check fires
  (keystroke-time, source-cited), not that such a check can exist at all — a build-time
  version-check script already does the latter, more crudely. Three follow-ups named in
  `Q008-provenance-closed-loop/03-review.md`: (1) **fine-grained per-field provenance — SHIPPED, see
  Q009** (below); (2) stress-test against a large schema assembly to confirm reflection cost stays
  flat past the toy case tested — Q009 partially advanced this (flat 3-to-12 fields) but real
  production width (50–200+ fields) is still untested; (3) fix the mismatch diagnostic being
  reported twice (identical text, same location) — recurred unchanged in Q009, still not fixed,
  now a two-quartet-old defect worth fixing once across the whole pattern rather than re-discovering
  it a third time.
- **Field-level provenance — SHIPPED, see Q009.** Extends Q008 from one whole-type version tag to
  per-field provenance: each provided member individually stamped, a client declaring a dependency
  on only a subset of fields. `Q009-field-level-provenance` (`CLOSED`, `SHIP`) proved member-level
  custom attributes survive into independently-reflectable IL via the same emission path as
  type-level (generalizing Q008's finding, which never tested this), and that selective enforcement
  works live in both directions — including a multi-field-mismatch diagnostic naming every affected
  field in one message, not just the first. Re-check cost stayed flat (15-57ms) from a 3-field to a
  12-field schema. The design's own mandated comparison against "just use N separately-versioned
  whole types" was measured honestly: both approaches achieve selective enforcement; this quartet's
  real edge is design-time cost (one `Assembly.LoadFrom` vs N) and one dependency declaration as
  data rather than N separate instantiations, not a raw client-side verbosity win. Four follow-ups
  named in `Q009-field-level-provenance/03-review.md`, not yet spiked: (1) stress-test at real
  production schema width; (2) test field *removal*, not just version bumps on a fixed field set —
  a distinct, untested failure mode from a version disagreement; (3) the duplicate-diagnostic fix
  from Q008, now recurring; (4) the decisive test of this quartet's own ergonomic argument — combine
  field-level provenance gating with Q006's real per-field accessor generation (actual data access,
  not verification-only markers) to see whether N-separate-types becomes structurally untenable
  rather than just less convenient once real data composition is at stake.
- **Witness types from a real prover.** The static parameter is a specification; the provider runs
  an SMT solver (Z3) or theorem prover at compile time and mints an unforgeable phantom witness type
  only if the proof succeeds, with the proof transcript embedded as metadata. As originally pitched
  (proving your own F# code satisfies a spec) this collapses to a typed comment, because a provider
  structurally cannot see the compilation it's part of — same wall Q006 already found. Narrower,
  de-risked version that avoids that wall entirely: point the prover at something the provider
  *can* legitimately see — an external schema, a regex (prove no catastrophic backtracking via
  automaton emptiness), a query plan against declared indexes (prove no full table scan) — since
  none of those require reaching into the current compilation. Not spiked; would need real
  SMT-solver integration, a materially bigger lift than Q008's.
- **Reachability as the type surface.** Feed the provider two protocol definitions; at compile time
  it computes their product automaton, model-checks it, and exposes only the deadlock-free reachable
  states as types and valid transitions as methods — session types by exhaustive search instead of
  hand-encoding. The most intellectually ambitious of the five, and the least tractable soon: the
  blocker is state-space explosion, a genuine hard problem in model checking, not an SDK gap, so no
  amount of clever provider engineering makes the underlying complexity go away. Would need a real
  model-checking library hosted at design time before this is even spike-shaped.
- **Adversarial settlement before a type exists.** The static parameter is a claim ("this parser
  round-trips"); the provider runs a generator and a falsifier (property-based testing, thousands of
  cases) and only emits the type if the adversary fails to break it. As originally pitched this
  breaks build reproducibility — randomized falsification means the type surface can differ between
  compiles of the identical source, which is disqualifying for CI/reproducible builds. Fix not
  identified in the original brainstorm: derive the falsifier's seed deterministically from a hash
  of the spec itself, not from wall-clock randomness. Same spec always falsifies the same way (full
  reproducibility preserved); different specs still get meaningfully different exploration. Not
  spiked; would need a property-testing harness hosted at design time.
- **Optimization as the static parameter.** `Optimal<"constraints.lp">`: the provider runs an ILP
  solver and the *solution* becomes the type — assignments as fields, objective value as a literal,
  infeasibility as a compile error citing the irreducible conflicting constraints. Weakest of the
  five and not recommended: baking a solver's answer directly into *type structure* means the type
  itself changes shape whenever the optimum moves, which fights the basic point of a stable
  interface. No identified fix short of gutting the idea (e.g. exposing the answer as a runtime
  value instead of a type, which is just ordinary codegen, not novel).

## Known engineering gaps in current Myriad (no spike needed — verified from source, not hypotheses)

Surfaced as background findings while building the quartets above, not something a future spike
needs to re-derive. Candidate real fixes, independent of whether the architecture-exploration
track above goes anywhere:

- **Generated code is invisible to the IDE until a real build.** `MyriadSdkGenerateCode` is gated
  `Condition="'$(DesignTimeBuild)' != 'true'"` and `MyriadSdkIncludeCodegenOutputDuringDesignTimeBuild`
  is an empty target (`src/Myriad.Sdk/build/Myriad.Sdk.targets`). This is the single biggest gap
  relative to type providers, and the one Myriad's own README claims to have moved past
  ("tooling to operate effectively") without yet fully delivering on. Not spiked — would need its
  own hypothesis pass to scope (e.g. does hooking the same target during DTB actually get FSAC to
  show generated members, or does FSAC need its own extension). Q006 tested the type-provider route
  to this same gap and got REVISE: the mechanism works but structurally can't reach a same-project
  attributed type, only one already compiled elsewhere — see backlog item 8 above. The DTB-hook
  route scoped here remains untested and, per Q006's review, is now the more direct candidate.
- **Codegen runs one cold process per input file.** `MyriadSdkGenerateCode`'s
  `Outputs="%(MyriadCodegen.OutputPath)"` triggers MSBuild's per-item batching, so the target (and
  its `<Exec>`) runs once per file, each paying full JIT + Fantomas-parse startup cost. Fixable
  without touching the plugin API: batch the CLI invocation, or publish it ReadyToRun/AOT.
- **No `#line` pragmas in generated output.** Errors in generated code point at the generated
  file, not the source declaration that produced it. Cheap, mechanical fix.
- **Config lives behind an indirection.** An attribute carries a string key, which is looked up
  against a `myriad.toml` section, rather than the attribute carrying typed config directly. Real
  authoring friction, smaller than the above three.
