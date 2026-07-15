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

## A closed door worth knowing about (verified from external sources, not a hypothesis)

FCS used to expose a direct AST-to-assembly compile entry point,
`FSharpChecker.CompileToDynamicAssembly` (and a related `Compile` overload taking a `ParsedInput
list`), from the `FSharp.Compiler.SourceCodeServices` era. It let a caller hand the compiler an
already-built untyped AST and get a compiled assembly back with no print-to-text/reparse round trip.
The clearest real usage was Eirik Tsarpalis's
[QuotationCompiler](https://github.com/eiriktsarpalis/QuotationCompiler), which lowered F#
quotations to a `ParsedInput` and fed it straight in. Checked directly against `service.fsi` on
`dotnet/fsharp`'s `main` branch (2026-07-15): the only `Compile` member left on `FSharpChecker` is
`Compile(argv: string[], ?userOpName)`, which resolves source file *paths* via the FileSystem API,
not an AST; `CompileToDynamicAssembly` does not appear. A 2022 issue asking whether it still exists
([dotnet/fsharp#14346](https://github.com/dotnet/fsharp/issues/14346)) was closed with no visible
replacement. Not checked against the exact pinned local package (`43.9.101`) by reflection, only the
current source tree — treat "closed" as strongly evidenced, not mechanically proven for this repo's
exact pin.

Why this matters here: it lines up with F#'s own stated design intent, not an oversight. Microsoft's
type-provider tutorial states the mechanism "isn't designed for intra-language meta-programming, even
though that domain contains some valid uses"
([source](https://learn.microsoft.com/en-us/dotnet/fsharp/tutorials/type-providers/creating-a-type-provider)).
Read together, the pattern is that F#'s sanctioned metaprogramming surface was deliberately narrowed
to type providers (`Type`/`MemberInfo`-shaped output only, via `ProvidedTypeDefinition`) and the
AST-injection back door was closed. That reframes why Myriad's disk-round-trip architecture exists at
all, and it's a second, independent line of evidence for Q006's wall: type providers were never meant
to be a route back to arbitrary AST splicing, and no amount of `ProvidedTypes.fs` cleverness recovers
it, because the boundary is deliberate on both sides, not an SDK gap to work around. If arbitrary
AST-injection is ever wanted back, the honest route is patching or vendoring FCS itself (its internals
may still carry the capability even though the public surface no longer exposes it) — a materially
bigger and riskier undertaking than anything else in this file, and not a type-provider hack at all.
Item 11 (prefix-stratified generation) needs none of this — it works entirely through ordinary
text-splice-and-reparse, the same round trip Myriad already uses today, just staged.

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

9. **Sub-file / stage-level generation caching (new, Myriad-CLI side, not yet spiked).** Myriad's
   current rebuild cache (`DEVNOTES.md`) is file-level: a hash of Myriad's version, reference paths,
   `--inputfile`, `--outputfile`, `--configkey`. Roslyn's `IIncrementalGenerator` model instead
   builds a pull-based pipeline (parse → filter attributed decls → transform → emit) memoized *per
   stage* by structural equality of just the relevant syntax node, so an edit elsewhere in the file
   doesn't force full regeneration. Doesn't need FCS or typed hosting — a restructuring of Myriad's
   existing syntax-AST pipeline, orthogonal to every Q001-Q009 thread. Cheapest falsifier: does
   splitting Myriad's real generators (`Fields`, `Lenses`) into discrete cacheable stages actually
   shrink measured regeneration work on a body-only edit, or does attribute-decl-level granularity
   already capture most of the win MSBuild's own file-level cache leaves on the table?

10. **Multi-pass generation for cross-generator visibility (new, extends Q005).** No mechanism today
    lets `[<Generator2>]` see what `[<Generator1>]` already generated in the same build — Q006 proved
    a type provider can't fix this (same-compilation wall), but Myriad's own MSBuild step is a
    *pre-build* generator, not a provider, so nothing stops restructuring the target into
    generate → typecheck-partial → generate-again against now-visible generated types → final build.
    A distinct, real capability gap named nowhere before this pass. **Superseded as the preferred
    route by item 11 below** (prefix-stratified generation), which gets the same typed cross-generator
    visibility without a fixpoint loop, using F#'s own file order as the stratification — try 11
    first; only fall back to this iterative form if 11's snapshot-thunk approach doesn't pan out.
    One named payoff that would justify either form: **usage-directed ("tree-shaking") generation**
    — query every use-site of an attributed type project-wide via `FSharpChecker.GetAllUsesOfAllSymbols`
    and generate only consumed members (e.g. 3 lenses instead of 40 for a wide record). Chicken-and-
    egg-broken in single-pass form (use-sites reference members that don't exist until generated, so
    pre-generation typecheck sees errors and the symbol search finds nothing) — only viable once
    multi-pass or stratified generation exists, and even then unverified whether F#'s reflection/
    quotation escape hatches defeat the tree-shake safety.

11. **Prefix-stratified generation — PROMOTED TO Q010, CLOSED, SHIP (scoped).** See
    `Q010-prefix-stratified-generation/03-review.md` for the full verdict; original framing kept below
    for context on how the design deviated from it (route (a)/(b) as originally scoped here were both
    speculative, Q010's actual design used only the already-proven `DocumentSource.Custom` mechanism
    from Q001/Q002/Q005, made reentrant, rather than either candidate named below). Both the mechanism
    claim (reentrant callback returns correct typed results, zero diagnostics, no hang) and the
    capability claim (a third generator correctly reused a second generator's already-generated output,
    verified by symbol resolution two independent ways) shipped. Held short of the strongest framing:
    unresolved whether the reentrancy tested was genuinely mid-flight or landing on an already-idle
    checker; proven only for the acyclic later-depends-on-earlier case, not mutual cross-generator
    dependency; a real silent API footgun found (`ParseAndCheckFileInProject` with explicit source text
    bypasses `DocumentSource.Custom` for that file, no error signal). Five follow-ups named in the
    review, not yet spiked, the most direct being instrumenting the reentrant call itself to settle the
    in-flight-vs-idle question. Original framing: a generated file whose content is computed
    mid-compilation as a function of the already-typechecked prefix of the same project (new, Thread 1
    only, high priority). A third answer to "how does new type information enter a compilation," coherent
    specifically because F# has a strict linear file order — "the compilation so far" is a
    well-defined, fully-typed prefix when the compiler reaches file N, so a generator invoked at that
    point can see real `FSharpEntity`s for everything in files 1..N-1 of the *same project* with no
    fixpoint and no paradox. This is what makes it categorically different from item 10: strictly
    stronger typed visibility (everything before you, by construction) for less new machinery.
    Two candidate mechanisms, one confirmed-available and one unverified: (a) `FSharp.Compiler.IO`'s
    `IFileSystem` shim (the mutable static hook Fable itself uses) — Myriad's host intercepts the
    read of `Generated_N.fs`, runs `ParseAndCheckFileInProject` on files 1..N-1 itself, and returns
    synthesized text; (b) `TransparentCompiler`'s snapshot API (`FSharpProjectSnapshot` /
    `FSharpFileSnapshot`) with a lazy per-file source thunk — plausible but the exact factory shape at
    the pinned `43.9.101` is **unverified, source-grounded reading only**, and it may not tolerate
    re-entrant checking from inside its own thunk, in which case fall back to route (a)'s
    semantically-identical staged form (check prefix, materialize, extend, repeat — still one pass
    per file). Cheapest falsifier: a three-file virtual project where file 3's text is computed from
    files 1-2's check results — confirm the typed prefix is complete/correct at generation time, which
    of (a)/(b) actually works, and cost against the Q001/Q002 baselines. Buildable on existing Q001
    infrastructure in roughly an afternoon. Inherits the `TransparentCompiler`-is-experimental caveat
    from `FINDINGS.md` if route (b) is used.

12. **Generated `.fsi` signature as a codegen contract gate (new, extends Q005, cheap).** Have a
    generator emit both a `.fsi` and a `.fs`, spliced into the same virtual `FSharpChecker` project
    Q001/Q002/Q005 already assemble, and let the compiler's own sig/impl matching enforce that the
    implementation doesn't silently drift from a deliberately-pinned public surface — ordinary,
    load-bearing F# machinery repurposed as a correctness boundary on generated output, sharper than
    Q005's "does it compile" into "does it compile *to the declared contract*." Cheapest falsifier
    (Q001 already taught this session not to assume compiler-hosting behavior without running it):
    does FCS actually enforce a virtual, in-memory `.fsi` against a virtual `.fs`, or does sig/impl
    matching only fire for on-disk files? One spike: a deliberately-mismatched generated pair, confirm
    the expected `FS0034`-class error fires in the virtual project.

13. **Semantic-dependency regeneration fingerprint (new, extends Myriad's rebuild cache, low
    priority — likely NULL).** Replace the *input* side of `_MyriadSdkCodeGenInputCache` with a
    fingerprint of the actual typed `FSharpSymbol`s a generator read during a typed generation pass
    (captured via `FSharpSymbolUse`), rather than a whole-file content hash — a real correctness gap
    the current file-timestamp cache misses is a cross-file generator (Q002-shaped) whose *dependency*
    changed in another file with no edit to the generator's own file at all. Different axis from item
    9 (which memoizes pipeline *stages*; this changes what counts as an input change). Honest risk,
    flagged upfront rather than oversold: for Myriad's actual generators (`Fields`, `Lenses`), the
    touched-symbol set is likely the *whole* attributed declaration, so the fingerprint degenerates to
    today's file hash and buys nothing — the same NULL trap Q001 hit with "typed beats syntax" for a
    pure structural-echo generator. Only real for the cross-file class Q002 opened. Falsifier:
    instrument one structural-echo and one cross-file generator, compare fingerprint churn to file-hash
    churn under a body-only edit.

14. **Erased, self-parsing design-time-only provider (new, revives the type-provider route to Q006's
    IDE-invisibility target, extends items 7-8).** Q006's port was *generative* (`isErased=false`) and
    hit the same-compilation wall. An **erased** provider (`isErased=true`,
    `FSharp.TypeProviders.SDK/src/ProvidedTypes.fsi:272,343`) sidesteps the wall differently: give it
    the source file path as a static parameter and have it **parse the file itself with Fantomas.FCS**
    — the same parser `Myriad.Core` already uses — inside `ApplyStaticArguments`
    (`ProvidedTypes.fs:1116`), rather than asking the compiler to resolve anything. It never queries
    the in-progress compilation, so Q006's wall doesn't bind it; this is precisely the mechanism
    sample-file providers (`JsonProvider<"sample.json">`) use to show live members with zero build,
    the property Q006's generative port could never demonstrate. Build-time codegen is untouched —
    this is a pure design-time visibility skin sitting alongside it. Real open question, not yet
    tested: does a duplicate-definition conflict arise between the erased provider's DTB-time surface
    and the real generated `.fs` file's build-time surface, and do an erased provider's members refuse
    to appear when the erasure target (the real generated type) doesn't exist yet? Cheapest falsifier:
    one erased provider over one record's path, opened in a **real Ionide workspace** (the actual
    live-host test Q006/Q007 never did, only `FSharpChecker`-as-library) — confirm members appear
    and there's no collision with the build-generated file.

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
- **A structural ceiling on embedded-DSL diagnostics, discovered rather than proposed.** Every
  "typed embedded DSL with keystroke-precise inner errors" pitch (typed SQL, a regex checked for
  catastrophic backtracking, a printf-format string) implicitly assumes a provider can attach a
  diagnostic at a *column inside* a string literal static argument. It can't: providers signal errors
  only by throwing (`failwithf` throughout `ApplyStaticArguments`, e.g. `ProvidedTypes.fs:1118`), and
  FCS attaches that message to the range of the *whole static-argument expression*, not a sub-offset
  — there's no range-carrying parameter on the `ITypeProvider` error path. This caps every DSL-in-a-
  static-arg idea at whole-argument granularity: reject the whole thing with a good message, but never
  underline character 47 of the embedded SQL. Recorded as a boundary rather than a hypothesis so a
  future idea in this space doesn't get designed against a false premise; cheapest falsifier if
  someone wants to double-check it (I doubt it changes the answer): throw with an embedded
  `(line,col)` and see whether FSAC renders the squiggle there or at the argument site.

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
- **Reversed-arrow, consumer-driven compile-time contracts — PROMOTED TO Q011, CLOSED, SHIP (but see
  the caveat below before citing it).** See `Q011-consumer-driven-contracts/03-review.md` for the full
  verdict. The coordination-semantics claim shipped cleanly: a client-side provider's own emitted
  attribute survives into reflectable IL; a schema-side provider correctly blocks a schema edit that
  breaks a stale client's recorded dependency (citing the client, field, both versions) and correctly
  clears once that client is recompiled, with no deadlock; field removal produces a diagnostic distinct
  from version-mismatch wording, closing Q009's own follow-up. **The catch, not anticipated by the
  pre-registration:** Q008/Q09's fast, incremental `FSharpChecker` checking API failed to resolve this
  provider's generative type even on the success path, for a reason investigated but not identified,
  forcing a fallback to full `checker.Compile` (106-201ms, an order of magnitude above Q008/Q09's
  15-57ms live-recheck band). This means the idea's actual selling point over a CI script — moved to
  keystroke-time, not CI-time — is **not demonstrated and is now specifically in doubt**, not merely
  untested; don't cite this quartet for that claim without first resolving the root cause (named as
  follow-up 1 in the review). Also surfaced: `DefineStaticParameters` fires more than once per logical
  check and must be memoized or type identity breaks — a real, probably lineage-wide FCS behavior,
  unconfirmed against Q008/Q09's own provider code since neither kept it as a durable artifact. Original
  framing kept below for context. Q008/Q009 point enforcement the way compilation always points:
  consumer checks producer. Reverse it. The *client*-side provider, when it generates, also records
  its own dependency set as IL attributes on its emitted types
  (`[<ConsumesField("Email","v2")>]`) — exactly the member-attribute path Q009 already proved
  survives into independently-reflectable IL. The *schema*-side provider takes a static parameter
  naming known client assemblies and refuses to compile a schema change that removes or re-versions a
  field any live client is recorded as depending on, with a diagnostic naming the client assembly and
  the consuming source location, Q009-style. That's Pact-style consumer-driven contracts with the CI
  broker deleted, moved to keystroke time in the *producer's* editor. Stays entirely inside Q006's
  wall by construction — both directions of the pair are cross-compilation, the shape Q008/Q009
  already proved out — and it's the first idea in the lineage to *exploit* the wall's shape (already-
  compiled clients are exactly the evidence base consumer-driven contracts need) rather than merely
  tolerate it. Directly answers Q009's own still-open follow-up 2 (field *removal*, not just version
  bumps) for free. The reflection half needs no new proof; the real cheapest falsifier is the loop's
  well-foundedness — when a schema author edits and the schema provider reads a now-*stale* client
  assembly, is that a feature (it's literally what a deployed, not-yet-upgraded consumer is) or does
  it wedge a legitimate coordinated upgrade? Spike must include the unwedging move (client bumps its
  declared version, recompiles, schema then passes) and confirm it doesn't deadlock when both need to
  move at once. Secondary, more mundane falsifier: whether client assemblies are even discoverable
  from the schema project's design-time context in a realistic multi-project layout — plumbing, and
  the more likely source of a REVISE than the coordination semantics.
- **Two-version schema-diff provider (new, extends Q008/Q009, stays inside the proven envelope).**
  Feed a provider two versions of a schema, both already-compiled external artifacts (`v1.dll`,
  `v2.dll`), and have it compute the structural diff at design time, exposing as typed methods only
  the *lossless* migrations (added-optional-field, widened-numeric) while lossy ones (dropped field,
  narrowed type) are absent or require an explicitly-passed witness value. Concrete, tractable
  instance of the "prove things about external artifacts" carve-out `FINDINGS.md` already names as
  the only Q006-legal form of the witness-type idea above, using the same `Assembly.LoadFrom` +
  reflection Q008/Q009 proved and `AddMembersDelayed` (`ProvidedTypes.fs:1423-1461`) to materialize
  the migration surface — no SMT solver needed, which is what makes it tractable where "reachability
  as the type surface" isn't. Cheapest falsifier, and the one thing that could kill it outright: can
  one design-time session `Assembly.LoadFrom` **two same-named schema assemblies** (`Schema.dll` v1
  and v2) without `AssemblyLoadContext` identity collision? This is the exact unresolved risk already
  flagged against the "provider reflecting on its own emitted IL" idea above — spike that one check
  before writing any diff logic.

### Round 3 — categorical reframings (a third brainstorming pass, briefed on rounds 1 and 2 and
told explicitly to ignore that ground and swing bigger)

The two lists above are real but still mostly "an underused API" or "a proven pattern applied to a
new pairing." A third pass asked the harder question — what would make generated code have a
*lifecycle*, or what's a third answer (besides Myriad's "text on disk before the build" and type
providers' "protocol during typechecking, but never same-compilation") to "how does new type
information enter a compilation" — and produced ideas more likely to be partially incoherent as
stated, recorded anyway because the kernel underneath each one is worth knowing about even where the
whole doesn't survive contact.

- **Codegen as a bidirectional lens, with provable obsolescence.** Reframe a generator as a lens:
  `get` is generation (source decl → generated module), `put` is legal back-propagation (a hand-edit
  to generated output, absorbed into the source), governed by the actual lens laws — GetPut
  (regenerating unedited output is a no-op) and PutGet (after a legal back-edit is absorbed,
  regeneration reproduces it exactly). The laws are what give this teeth over "smart merge tooling,"
  which has none: they're pre-registerable, mechanically checkable pass/fail gates, quartet-shaped by
  construction. Folds in a lifecycle half for free: each generated declaration carries the typed-symbol
  set it derived from (a Thread-1 sibling of Q008's IL-attribute provenance, but used for edit
  legality and garbage collection here, not cache invalidation the way item 13 above uses symbol
  identity); a declaration whose symbols no longer exist on regeneration is *provably obsolete* and
  removed with a diagnostic naming the vanished symbol. Mechanism: classify a hand-edit by diffing
  three trees (on-disk generated file, fresh regeneration, last manifest) — additive edits in
  generator-unclaimed positions are legal, edits inside generator-claimed spans are law violations
  that fail the build citing the span. `Lenses` is the pleasingly on-the-nose pilot generator.
  Cheapest falsifier, and the honestly-likely-to-survive tier even if the rest collapses: a
  **detect-only** version with no `put` at all — can the manifest+diff reliably classify {no-op,
  legal addition, clobber-conflict, obsolete} on a handful of adversarial hand-edits to real `Fields`
  output? If that classification is mushy under ordinary formatting noise, full law-governed `put` is
  dead and the honest verdict is "obsolescence GC only" — still independently useful, not a wasted
  spike.
- **Cross-generator linking via demand/offer resolution — typeclass coherence as a codegen phase.**
  Make Myriad's generators a linked ecosystem instead of mutually blind expanders: each generator
  emits typed **demands** ("I require `show : T -> string` for each of these `FSharpEntity`s")
  alongside its AST output, and typed **offers** ("I can derive `show` for any record whose fields all
  have `show`"). A resolution phase in the Thread-1 host solves demand/offer to a fixpoint over
  whole-project typed knowledge — exactly the configuration Q002 proved per-file Myriad cannot
  reproduce — then fails the build on an unsatisfiable demand (diagnostic naming the demanding
  generator and type) or an ambiguous one (two applicable offers, no priority), the coherence
  discipline Haskell enforces for typeclasses and that F# structurally lacks. Distinct from item 10
  above: multi-pass makes generators able to *see* each other's output types; this adds a contract
  layer with real resolution semantics, so composition happens by negotiation, not lucky ordering.
  Cheapest falsifier: two toy generators over Q002's own recursive-nested-field shape, one demanding
  `show` on a nested field type and one offering it — confirm cross-file satisfaction, a clean failure
  on an unsatisfiable demand, and a clean failure (not silent pick-one) on a duplicate offer. Named
  kill risk, stated plainly: a *useful* version likely needs real instance-overlap semantics
  (most-specific-wins, backtracking), at which point this is a typeclass solver and the honest verdict
  becomes "research project, not a spike" — the falsifier is designed to surface that early rather
  than after the harness is built.
- **Diagnostic-directed generation: the compiler's own error stream as the specification.** The most
  radical version of "annotate, then expand" available here: delete the attribute. The programmer
  writes code against declarations that don't exist yet; the generator's input is the typechecker's
  own complaint list. The Thread-1 host checks the project, harvests diagnostics with typed context at
  each range (FS0039 undefined value, FS0366 missing interface member, FS0025 incomplete match) via
  `GetSymbolUseAtLocation` and friends on the check results, generates precisely the declarations that
  discharge them, and re-checks to a bounded fixpoint (2-3 rounds) — the typechecker stops being a
  gate you pass and becomes the constraint engine driving synthesis. Scoped to stay sane, not
  unbounded program synthesis: only discharge diagnostics whose fix is uniquely determined by typed
  context *plus* an explicit project-level policy ("missing DU cases route to `DefaultHandler`",
  "missing `IStore` members derive from the record shape") — anything ambiguous stays a real error.
  Lowest-confidence idea in this file, recorded anyway because even a clean NULL would answer a
  question no quartet has asked yet: are FCS diagnostics machine-actionable enough to be an *input*,
  not just an output — which the lens idea's conflict reporting and Q005's veto path would both
  benefit from knowing regardless of this idea's own fate. Cheapest falsifier, and the one that should
  kill it fast if it's going to: can typed context recovered at an FS0366 diagnostic's range reliably
  determine the *full* required interface signature, and does one generate-recheck round actually
  discharge it without spawning new diagnostics? If the answer requires parsing error message
  strings instead of structured typed context, that's the tell the mechanism is wrong — kill it
  immediately rather than working around it.

One cross-cutting note from this pass: item 11 above (prefix-stratified generation) is the load-
bearing substrate the bidirectional-lens and diagnostic-directed ideas would both want if either is
pursued past a spike — worth sequencing after it rather than before. And a correction to how earlier
entries in this file get framed: **Thread 1 (Myriad-CLI-hosted `FSharpChecker`) is not bound by the
Q006 same-compilation wall at all** — the wall binds only the type-provider route. Several items
above blur this distinction; any idea needing same-compilation typed visibility is dead as a
*provider* (Q006) but potentially alive in the *CLI* model, subject only to softer constraints like
item 10's chicken-and-egg problem, not Q006's structural wall.

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
