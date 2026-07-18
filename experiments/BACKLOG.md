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
   **Stretch goal — PROMOTED TO Q014, CLOSED, REVISE.** See
   `Q014-fsi-staged-compilation/03-review.md` for the full verdict; original framing kept below for
   context. Q014 scoped the reification target as a real F# quotation (`Expr<'T>`), not an arbitrary
   closure — reifying a closure's captured environment back into a hygienic `SynExpr` is very likely
   structurally impossible in general, whereas a quotation is already a code-shaped data structure and
   stays inside F#'s own sanctioned metaprogramming surface — and that half held up: a quotation
   obtained via `[<ReflectedDefinition>]`/`TryGetReflectedDefinition` (no hand-built `Expr.Call`) was
   specialized and rendered by Unquote's `decompile` into F# source that reparsed, typechecked, and,
   verified by actually compiling and executing the text, ran correctly with a recursive general
   implementation (`power`, unrolled at n=4) fully unrolled away — *code*, not just Q003's *data*,
   crossing the generation-time-to-source boundary. **What didn't survive review, on independent
   re-execution:** no `FsiEvaluationSession` ever ran — the spike quietly substituted host-compile-time
   `[<ReflectedDefinition>]` capture for the generation-time FSI evaluation the hypothesis's own title
   and SHIP threshold named, and both the general implementation and its static config were source-level
   literals in the same program, so the dynamic-origin staging boundary Myriad would actually need was
   never crossed. The partial evaluator is also a hand transcription of `power`'s own three node shapes,
   not the generic `ExprShape` expander the design's own recon named but didn't build, so the capability
   claim doesn't generalize past that one shape. Four follow-ups named in the review, the most direct
   being to actually host FSI and reify a result whose origin isn't a compile-time literal — that is the
   claim this quartet's title made and didn't test. **That follow-up was run in the same session as
   `Q015-fsi-dynamic-origin-staging`, CLOSED, SHIP (narrowly scoped — like Q010, not like Q014).** A real
   `FsiEvaluationSession` evaluated a general implementation read from a separate plugin file (never in
   the host's own compiled source) plus a config value read from an environment variable; the resulting
   `MethodInfo` was confirmed to genuinely originate from FSI's dynamic assembly (not the host's), and
   `Expr.TryGetReflectedDefinition` returned the identical quotation shape Q014 got at host-compile-time —
   closing Q014's "no FSI ran" gap for real, reproduced independently at a third `n` value. But Q015's own
   review found FSI performs none of the actual staging: it compiles the plugin and hands back a
   `MethodInfo` via a one-line quotation-destructure that never executes anything, so every line of the
   actual partial-evaluation logic remains host-compiled code identical to Q014's, and the origin proven
   dynamic is the implementation body and a scalar config only — the host still hardcodes the plugin's
   function name, arity, and argument types, and the specializer is still hand-matched to `power`'s one
   shape. See `Q015-fsi-dynamic-origin-staging/03-review.md` for the full verdict and four follow-ups, the
   first being to make FSI perform real computation rather than hand back a pointer to code whose shape
   the host already statically knows. Original framing: true staged compilation — evaluate
   at build time against a general implementation and bake a *specialized* result into generated code (a
   parser or state machine compiled from running the general version, not just a schema value read off).
   The base loop's own core claim (an FSI value can drive template text) was judged not materially in
   doubt given Q003 and Q006's string-static-parameter result, so Q014 proceeded straight to the
   higher-uncertainty, higher-payoff reify-code question rather than re-proving the lower-value base case
   first — in hindsight, skipping the base loop is also what let the missing-FSI substitution go
   unnoticed until review, since the base loop's own claim would have forced an actual `FsiEvaluationSession`
   into the harness from the start.

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
   **Partially addressed as a side effect of `Q023`/`Q024` (2026-07-17), not closed.** Both quartets
   used genuinely-weighted padding files (generic records, `Map`/`List` pipelines, recursion) up to
   N=300, closing this item's specific "near-free padding files" methodology complaint for
   `TransparentCompiler`. But neither quartet ever configured `useTransparentCompiler = false` — this
   item's own actual comparison target, `BackgroundCompiler` vs `TransparentCompiler` scaling under
   genuine typecheck weight, remains untested. Cheapest remaining step: rerun `Q023`'s or `Q024`'s own
   spike (`experiments/Q023-scale-cost-reentrant-callback/artifacts/q023-spike/`) with
   `useTransparentCompiler = false`, no new harness needed.

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

8. **Redirect the IDE-invisibility fix at the DTB/MSBuild path directly — PROMOTED TO Q022, CLOSED,
   REVISE (as of 2026-07-17).** See `Q022-dtb-generation-hook/03-review.md` for the full verdict.
   Original framing kept below for context. The mechanism half of this item's own question is fully
   confirmed: removing `MyriadSdkGenerateCode`'s `Condition="'$(DesignTimeBuild)' != 'true'"` gate (on a
   scoped local copy of `Myriad.Sdk.targets` — the real, shared file was never edited) makes Myriad's
   real CLI run during a genuine design-time build (`SkipCompilerExecution=true`, the real F# compiler
   confirmed never invoked), correctly regenerating a changed attributed type's output, with the
   existing rebuild cache (`DEVNOTES.md`) still correctly no-op'ing an unchanged repeat DTB call. This
   is the first quartet in this repo's history to also test the "gets FSAC to show generated members"
   half against a literal `fsautocomplete` process over real LSP, not `FSharpChecker`-as-a-library — a
   standing gap `FINDINGS.md` had flagged since Q006 and never closed. A fresh FSAC 0.83.0 session
   picked up the newly-generated member with zero `dotnet build`, matching a gated negative control that
   correctly failed the identical check. But within one already-*running* FSAC session, an ordinary
   source-file save never re-triggers Myriad's codegen — only an actual change to the `.fsproj` itself
   (even a bare mtime touch, no content change) followed by a project reload does, a real, demonstrated
   in-session trigger that the quartet's own first-pass results write-up wrongly concluded didn't exist,
   corrected by independent review. Net: **this item's own question is answered precisely — yes, hooking
   the DTB path gets FSAC to show generated members without a real build, but only at project
   load/reload time, not during live source editing**, since an ordinary edit to the attributed `.fs`
   file never touches the `.fsproj` a reload is keyed on. Named follow-up, not yet spiked: confirm
   whether a literal VS Code + Ionide session's own real project-file watcher reliably fires a reload on
   an ordinary `.fsproj` save (this quartet only simulated the watcher via a hand-rolled LSP notification
   plus a manual `fsharp/workspaceLoad` re-issue), which would turn this into a genuinely automatic (if
   still reload-gated) experience for a real user rather than one requiring a manual trigger.
   Original framing: Q006's review concluded the same-project boundary
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
   **External confirmation, 2026-07-16, worth recording since it bears on whether this whole file's
   architecture-improvement premise is even the right target:** `fsharp/fslang-suggestions#864` ("Support
   Source Generators," open since ~2020, 131 comments, still OPEN, estimated cost **XXXXL**) is the F#
   team's own tracker for "give F# a Roslyn-style source-generator hook." FCS/compiler team member
   `vzarytovskii`, in that thread: "There is no runtime contract for source generators, the CLR itself
   doesn't know they exist. They're purely compiler-level hooks implemented in csc" — i.e. there is no
   shared substrate to plug F# into even in principle, only a from-scratch `fsc`/FCS equivalent, which is
   exactly the XXXXL estimate. Same team member, asked why F# lacks this: "F# has Myriad, which is a
   community-driven sourcegen solution" — offered as the team's actual answer, not a stopgap. This item's
   *caching design* (structural per-stage memoization) is fully portable regardless — it needs nothing
   from Roslyn's compiler internals. What is **not** portable, confirmed by an authoritative external
   source rather than only this repo's own quartet evidence: the live-in-IDE mechanism itself, since it
   depends on a compiler-hosted hook FCS has no equivalent of and the F# team has no near-term plan to
   build. See memory note `project_fsharp_no_source_generators` for the full citation.
   **Freshness re-check, 2026-07-16, verified directly via `gh issue view` rather than taken on
   faith:** `fsharp/fslang-suggestions#1396` ("F# Own Source-generation/Meta-programming," opened
   2024-12-14) is a materially more recent ask for the same thing #864 tracks — and it was closed
   `NOT_PLANNED` the same day it was opened. This is stronger, more recent evidence for the same
   conclusion, not just a repeat of the 2020 issue. `dotnet/fsharp#14300` (interop with C# libraries
   that depend on source generators) is a different, narrower question — still `OPEN`, unassigned,
   `Backlog` — and doesn't bear on whether F# gets its own generator hook. Separately, real, shipped
   prior art for a *wrapper-compiler* approach exists outside this repo's own architecture space:
   WebSharper 10's `ISourceGenerator`/`FSharpSourceGenerator` mechanism (confirmed from
   `docs.websharper.com` and the closed implementation issue,
   `dotnet-websharper/core#1476`) has `wsfsc.exe` scan the input file list for non-`.fs` files, locate
   a generator by assembly attribute, replace each matched file with its generated `.fs` output, and
   **repeat until no non-F# files remain**, before resuming normal compilation. That's a real,
   production example of iterating generation-before-compile at the whole-project level — closer in
   shape to item 10's multi-pass idea (and, more distantly, `Q010`'s prefix-stratified approach) than
   anything in the Roslyn/type-provider comparison space, and worth a look if item 10 is ever picked
   up: it settles that the "wrapper front-end iterates until fixpoint, then compiles" shape is viable
   in practice, not just in theory.

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

14. **Erased, self-parsing design-time-only provider — PROMOTED TO Q019, CLOSED, SHIP (scoped, as of
    2026-07-16).** See `Q019-erased-self-parsing-provider/03-review.md` for the full verdict; original
    framing kept below for context on what changed. The design deviated from the original framing in
    one respect worth flagging up front: this used its own `DefineStaticParameters`-based
    `Fields<SourceFilePath, RecordName>` provider rather than `ApplyStaticArguments`-on-a-single-type,
    and called `Myriad.Core.Ast.fromFilename`/`Ast.extractRecords` (Myriad's own real parsing code, a
    direct library reference to the built `Myriad.Core.dll`) rather than raw `Fantomas.FCS` calls —
    both faithful to this item's actual intent, not a substitution. All three pre-registered rounds
    shipped, independently reproduced by review: design-time member resolution with the target record
    type provably absent from every reference path (0 diagnostics); a live on-disk field edit picked up
    via `FileSystemWatcher`+`Invalidate()` with no rebuild (0 diagnostics, ~40ms); and runtime
    correctness against a genuinely independent, separately-compiled consumer. This item's own open
    question — real Ionide workspace test — remains untested (still `FSharpChecker`-as-library only,
    the standing Q006-era caveat), and the review scoped the result hard: every member is `obj`-typed
    (name completion, not typed IntelliSense), this is a parallel preview mechanism the user points at
    a file path, not Myriad's own `[<Lenses>]`-on-the-record shape made live, and the review struck one
    of the write-up's own "found by running" corrections (`assemblyReplacementMap` being load-bearing)
    as a non-reproducing, post-hoc misattribution — a real miss worth remembering: this repo's own
    "found by running, not assumed" discipline is not itself immune to being wrong, which is exactly
    what the independent-review gate exists to catch. Top named follow-up: type the provided members
    with the field's real declared type instead of `obj` — trivial for primitives, but a field typed by
    another type from the same uncompiled file walks straight back toward Q006's wall, and is the
    genuinely open question left after this quartet.
    Original framing: Q006's port was *generative* (`isErased=false`) and
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

15. **Type provider subsuming a Myriad-compiled satellite DLL — cross-project case — PROMOTED TO Q016,
    CLOSED, REVISE (as of 2026-07-16).** Round 1 (real reflection-forwarding into a Myriad-CLI-produced,
    `checker.Compile`-built satellite DLL) passed. Round 2 (regeneration + live pickup via
    `Invalidate()`/`FileSystemWatcher`) failed: the `Assembly.LoadFrom` that Round 1's identity match
    requires holds a Windows file-lock that blocks the satellite DLL from being overwritten. The
    adversarial review sharpened this past a simple pass/fail: the lock lands precisely on the *one
    thing* that makes a type provider worth using here at all — this item's own caveat below already
    conceded a TP barely earns its keep over a `<ProjectReference>` except for live re-exposure, and
    live re-exposure is exactly what breaks. The review also flagged that this quartet's own attempts
    already rule out the REVISE threshold's presumed workaround (`Assembly.Load(bytes)`, per Q011 —
    shown here to break the same identity match Round 1 needs), and that generalization beyond one
    record shape (`create`/`name`/`age`/`email`) is untested — the one available differently-shaped
    member (`map`, function-typed parameters) was never tried. See
    `Q016-satellite-dll-type-provider/03-review.md` for the full account and prioritized follow-ups.
    **Follow-up spiked and closed: `Q017-satellite-function-typed-forwarding`, SHIP (as of 2026-07-16).**
    The function-typed-shape gap named above was tested directly (Myriad's own real `map` function, two
    `FSharpFunc<_,_>` parameters plus a record) and passed cleanly with no new scaffolding — the review
    independently rebuilt and reran it, then went further and confirmed function values also flow *out*
    of the provider (a genuine `FSharpFunc` return) and through record-typed function elements, not just
    primitives. But the review reframes the win and flags a new, more important risk: the mechanism is
    just opaque-reference forwarding (nothing "function-typed" is special-cased), and it is **brittle to
    F#'s curried-arity flattening** — a source-level function-returning-function usually compiles to a
    flat multi-arg method, not an `FSharpFunc` return, invisible from the source signature, and a forward
    built against a mismatched real `MethodInfo` throws and poisons the *entire* provided type, not just
    the offending member. **This is now a standing, concrete risk for item 15 itself and for any future
    attempt to reflection-forward `Lenses`' own getter/setter-shaped output this way** (see the general
    ideas section below) — compiled arity, not source arity, must be checked first. Round 2's file-lock
    problem is untouched by Q017 and stands exactly as Q016 left it. See
    `Q017-satellite-function-typed-forwarding/03-review.md`.
    **Follow-up spiked and closed: `Q018-collectible-alc-round2-mitigation`, REVISE — materially weaker
    than Q016's own, and effectively a dead end for this item's live-re-exposure goal (as of 2026-07-16).**
    The exact mitigation named above was tested directly, in three staged rounds. The underlying OS
    mechanism is real (an isolated collectible ALC does release the Windows file-lock), but only with
    `TieredCompilation=false` — a JIT setting no real IDE host (Ionide/FSAC/MSBuild's own `dotnet.exe`)
    runs with by default, and one this item cannot assume. Worse: the actual mitigation, integrated into
    the real provider and triggered in its most favorable possible way (an explicit, pre-write eviction
    call, sidestepping a chicken-and-egg flaw in the original `FileSystemWatcher`-triggered design), still
    failed to release the lock. The review went further than the executor and closed off the one hopeful
    escape left open — a fresh `FSharpChecker` per regeneration cycle *also* fails, because the retained
    reference lives process-globally inside FCS's own type-provider hosting, not scoped to any one checker
    instance. **There is now no demonstrated in-process fix for Round 2's lock.** See
    `Q018-collectible-alc-round2-mitigation/03-review.md` for the full account, including two further
    confirmations: FCS's own referenced-assembly reader isn't the culprit (ruled out directly), and a
    narrower JIT-tuning attribute (`AggressiveOptimization`) doesn't substitute for the process-wide
    setting either.
    **Honest bottom line for this item, now settled rather than provisional: this is not a viable path to
    live re-exposure for the cross-project case.** The generalization concern (Q017) is resolved, but the
    file-lock problem (Q016 Round 2) has been tested against its most promising named mitigation and that
    mitigation is exhausted in-process, not merely unproven. Use an ordinary `<ProjectReference>` for this
    case. The only remaining in-principle escape — an out-of-process design-time host, torn down between
    edits — is untested, a large lift, and may not earn its keep over a plain `<ProjectReference>` anyway;
    don't queue another in-process ALC variant.
    Original framing kept below for context. Raised in conversation: instead
    of a provider hand-building `ProvidedTypes` members from an untyped Myriad AST (item 14's own
    open problem — `ProvidedProperty`/`ProvidedMethod` need concrete CLR `Type`s, Myriad's
    `Fantomas.FCS.Syntax` AST has none without a full typecheck), let Myriad run its *real* generator
    unmodified, compile the resulting source to a small satellite DLL via `checker.Compile` — a
    mechanism already proven working twice in this repo, not speculative: `Q014-fsi-staged-compilation`
    and `Q015-fsi-dynamic-origin-staging` both compiled spliced text to a real `.dll` via
    `checker.Compile`, `Assembly.LoadFrom`'d it, and invoked it by reflection, in both cases matching
    a second, independent correctness check — then have a type provider `Assembly.LoadFrom` that
    satellite DLL and re-expose its already-fully-resolved members. **For Myriad's cross-project usage
    (attributed type in project A, consumer wants generated members in project B which references A):
    this fully sidesteps Q006's wall with no new mechanism risk**, because by the time project B
    compiles, the satellite DLL is — by construction — an already-compiled, externally-referenced
    artifact, exactly the shape Q006/Q008/Q009/Q011 already proved type providers handle comfortably.
    Honest caveat, stated in the conversation that raised this and worth keeping attached to any
    write-up: for this cross-project case a type provider is barely pulling its weight over an ordinary
    `<ProjectReference>` — its actual differentiator is dynamic re-exposure of whatever the satellite
    DLL currently contains without static reference wiring, refreshed live via `Invalidate()` +
    `FileSystemWatcher` (the mechanism already named, and already flagged as real-but-narrower-than-
    novel, in the "External-signal `Invalidate()`" bullet under the general-ideas section below).
    Cheapest falsifier: inside a generative provider's `ApplyStaticArguments`, `Assembly.LoadFrom` a
    Myriad-produced scratch DLL and re-expose one real compiled member via reflection-forwarding — does
    it work end to end, and does `Invalidate()` correctly pick up a re-generated DLL after a file
    change without a stale-handle/file-lock problem (the satellite DLL will be rewritten by Myriad
    while a live `FsiEvaluationSession`-adjacent host may still hold it loaded — untested, and a
    real risk `Assembly.LoadFrom`'s file-locking behavior on Windows makes worth checking first).
    **Guidance for any future use of Q017's forwarding kernel, added 2026-07-16 (including against
    `Lenses`' own output, the standing risk this item's Q017 paragraph already names): check compiled
    arity via `MethodInfo` reflection at the point of forwarding, per member, not via a separately-
    authored manifest.** The `MethodInfo` being forwarded against is already in hand at generation
    time — that's the same reflection Q016/Q017 already perform — so the arity/shape check is a
    runtime assertion co-located with the forward itself, not a second artifact (e.g. a hand-maintained
    JSON manifest) that can silently drift from the compiled reality it's supposed to describe. On a
    mismatch, skip or fail that one provided member per Q017's own poisoning risk, never the whole type.

16. **Pre-build scratch-DLL type provider for same-project IDE preview — real but strictly weaker than
    Q010 for the same target (new, extends Q006/Q010, the direct route to the named IDE-invisibility
    gap).** For Myriad's actual *dominant* pattern — `[<Lenses>]` on a record in the file/project
    currently being built — item 15's satellite-DLL trick does not sidestep Q006's wall: a type
    provider resolving mid-compilation still cannot see `Person` until `Person` is compiled, and
    `Person` is part of the very compilation the provider is running inside, so Myriad cannot compile a
    real satellite DLL referencing `Person`'s real compiled type at that point either. The route around
    this is not to beat the wall but to change *when* compilation happens: Myriad's pre-build step
    already runs before `dotnet build` (`DEVNOTES.md`) — have it produce the scratch DLL *ahead of* the
    main build, cached the same way today's `_MyriadSdkCodeGenInputCache` rebuild cache works, and have
    the type provider consume the **last successfully generated version**, not the in-flight one. This
    gives eventually-consistent live IDE preview — one save behind, the same staleness character as
    Roslyn source-generator IDE preview or Fable's watch mode — not true reentrant same-compilation
    visibility. **State this plainly rather than overselling it: this is explicitly weaker than
    `Q010-prefix-stratified-generation`'s already-shipped mechanism** (reentrant
    `DocumentSource.Custom`, staying inside Myriad's own CLI-hosted `FSharpChecker`, genuine
    same-in-progress-compilation visibility) for the identical target — worth building only if a
    type-provider-shaped delivery mechanism (IntelliSense-visible members with no FCS/Ionide-specific
    host plumbing required on Myriad's side) is independently valued over Q010's approach, or as a
    fallback for a host that can't adopt Q010's reentrant-checker model. Named risks, not yet checked:
    build-ordering (does MSBuild's design-time build actually trigger, or wait on, the scratch-compile
    step with sensible timing, or does DTB run against a stale-or-missing scratch DLL on a cold
    checkout); `FileSystemWatcher` + `Invalidate()` correctness and cost under rapid successive edits,
    inherited from item 15's same open question.

17. **Real-Ionide-session test of Q020's "analyzer dominates" claim (new, 2026-07-16, extends Q020,
    cheap, closes a standing cross-cutting caveat).** `FINDINGS.md`'s own cross-cutting limitations
    section already flags this generally: every "works live in the IDE" claim in this repo has only
    ever been tested through `FSharpChecker` as a library, never a literal Ionide/FSAC session —
    judged real-but-secondary for Q008/Q09's claims, never closed for any claim in either thread.
    Q020's own review asserts an `FSharp.Analyzers.SDK` analyzer calling the identical shared analysis
    function "strictly dominates" the type-provider diagnostic channel in Ionide, but that comparison
    was argued from API shape (arbitrary-range anchoring vs. the TP channel's member-access-node
    anchor), not run inside a real FSAC session. Cheapest falsifier: build the actual analyzer adapter
    over Q020's already-written shared analysis function, open it alongside a `Q020`-style TP consumer
    in a real Ionide workspace, and confirm three things the prose claim assumes rather than tests: the
    analyzer's diagnostic actually renders (analyzer support requires SDK-version alignment and
    sometimes explicit opt-in that can silently no-op), its live range is at least as tight as the TP
    channel's anchor, and setup friction is genuinely lower for a real user than referencing a preview
    provider. A NULL here — the analyzer fails to load, or Ionide's analyzer support lags the pinned
    SDK version — would be a new finding in its own right, since nothing in this lineage has been run
    against a real IDE host yet to find out.

18. **FSAC-owned virtual generated file, modeled on rust-analyzer's out-of-process proc-macro
    architecture — a distinct route from item 8, narrower than it first looks (new, 2026-07-16,
    extends item 8 and Q010).** rust-analyzer solves Myriad's exact problem in a different language:
    proc-macro expansion runs out-of-process (`proc-macro-srv`), and the *editor's own* language
    server splices the result into the in-memory project model it already serves completions from —
    live, with no real `cargo build`. The tempting analogy is "give FSAC the same sidecar," but the
    analogy is imperfect in a way worth stating precisely before scoping any spike: rust-analyzer owns
    its own HIR/name-resolution pipeline and macro expansion is a first-class stage inside it; FSAC
    owns no equivalent semantic layer of its own — it is a thin LSP wrapper that hands source text and
    `FSharpProjectOptions` to `FSharpChecker` and reports back what FCS says. So the right target for
    an FSAC-side fix isn't "inject expanded ASTs into FSAC's model" (FSAC has no such model to inject
    into) — it's "get FSAC to construct `FSharpProjectOptions` that include a virtual generated source
    file, computed by running Myriad, and hand that to the *same* `FSharpChecker` instance FSAC already
    drives for the live session." **This is materially cheaper than it first looks, because this repo
    has already answered the hard part of that question.** `Q010-prefix-stratified-generation` proved,
    independently reproduced, that a `DocumentSource.Custom` callback made reentrant can synthesize a
    virtual file's content from the already-typechecked prefix of the same in-progress project, with
    zero diagnostics and correct symbol resolution — inside Myriad's own CLI-hosted `FSharpChecker`,
    not a standalone toy. Q010 also already found the one sharp footgun in this exact mechanism
    (`ParseAndCheckFileInProject` with explicit source text silently bypasses `DocumentSource.Custom`
    for that file, no error signal) — a trap any FSAC-side attempt would walk straight into without
    that warning. So the open question is narrower than "does virtual-file injection work at all"
    (settled: yes) — it's whether the *same* mechanism holds when the checker instance being driven is
    FSAC's own, serving a live editing session, and whether FSAC can be made to trigger Myriad's
    expansion on `didOpen`/`didChange`/project-reload and feed the result back into the project options
    it already owns. **Named risk this repo's other quartets didn't have to face: every prior spike
    was self-contained inside this repo's own checkouts; this one requires patching or forking FSAC
    itself**, an external project with its own release cadence and no guarantee a patch would be
    upstreamable — a different kind of cost than anything else in this backlog, and worth weighing
    against item 8's DTB/MSBuild-hook route (which needs no fork, only a Myriad-side target change)
    before committing to either. Cheapest falsifier, scoped down from the fork-FSAC version: before
    touching FSAC at all, confirm Q010's reentrant-`DocumentSource.Custom` mechanism still produces
    correct completion/typecheck results when the *outer* driving loop is an LSP `didChange`-shaped
    edit sequence rather than Q010's own one-shot harness — if that holds, the remaining risk is purely
    FSAC integration plumbing, not a new compiler-hosting mechanism to discover.
    **That cheapest falsifier was run: `Q021-reentrant-generation-live-edit-loop`, CLOSED, SHIP
    (scoped), as of 2026-07-16.** See `Q021-reentrant-generation-live-edit-loop/03-review.md` for the
    full verdict, independently reproduced. **Confirmed, exactly as this item asked:** on one
    persistent `FSharpChecker` (`useTransparentCompiler = true`) reused across a sequential
    edit-then-recheck loop, the reentrant callback keeps producing correct, freshly-recomputed,
    zero-diagnostic results every cycle — no staleness, no hang, across four cycles including a
    deliberate return to a previously-seen value — and, unanticipated, needs no explicit version-bump
    or invalidation discipline to do so under `ParseAndCheckProject`. **The correctness premise this
    item's falsifier named is retired.** One genuine new plumbing constraint surfaced, sharpening
    rather than dissolving the "purely FSAC integration plumbing" framing: the natural per-file
    incremental API an LSP host actually has in hand on `didChange` —
    `ParseAndCheckFileInProject` called with explicit current source text — silently bypasses
    `DocumentSource.Custom` entirely (Q010's own footgun, independently reproduced in a fresh harness),
    so an FSAC-hosted version cannot just call the per-file API the normal way; it would need to route
    through a whole-project check or find an untested calling pattern that avoids the bypass. **One
    claim from this quartet's own results write-up was struck by its adversarial review and must not
    be cited:** an early conclusion that `ParseAndCheckProject` "always fully re-checks every file on
    every call, unconditionally" is unsupported — the quartet's own Round 5 only shows the callback is
    *re-consulted* every call, which is equally consistent with a cheap fetch feeding a cache hit under
    `TransparentCompiler`'s content-hash model, and the two-file toy project tested cannot discriminate
    the two at the scale that would matter. **Whether a real FSAC-hosted version of this mechanism
    would be keystroke-cheap or pay a full-project-recheck cost on every edit is therefore still
    genuinely open**, not settled in either direction — the single highest-priority named follow-up is
    a scale test (a few hundred virtual files behind the same callback, one edited, cost on the
    unchanged remainder measured) before committing to building the actual FSAC fork/patch this item
    describes.
    **That scale test was run: `Q023-scale-cost-reentrant-callback`, CLOSED, REVISE (as of
    2026-07-17) — with the executor's own headline claim corrected by review, landing more favorably
    for this item than the quartet's own first-pass conclusion.** N ∈ {10, 50, 150, 300}, genuinely
    typecheck-weighted files (not `let x = 5` padding — this also incidentally closes item 4's
    long-standing "padding files were near-free" gap). The executor's corrected numbers (two honestly
    disclosed deviations along the way, see the quartet's own `02-results.md`) showed both no-op-repeat
    and edit-one-file cost growing with N relative to cold, read at first as "editOne tracks cold,
    caching is largely absent" — a pessimistic answer for this item's keystroke-cost question.
    **Independent review found that reading doesn't survive scrutiny of which file the spike always
    edited.** The design hardcoded the edit target as file index 0 — the compilation-order *first*
    file, with the maximum possible number of order-successors — at every N, not a representative
    "unrelated file." A position sweep at N=300, run by the reviewer and then independently
    re-confirmed a third time with a durable, checked-in artifact
    (`Q023-.../artifacts/q023-spike/run-n300-editidx{0,150,299}.txt`), found cost is linear in the
    number of files *after* the edited one: editing the **last** file (zero successors) is
    statistically indistinguishable from a no-op repeat (255ms vs. 258ms); editing the **first** file
    (299 successors) costs close to cold (1339ms of a 2640ms cold check). **The corrected finding:
    FCS's `TransparentCompiler` genuinely does skip the compilation-order prefix before an edit and
    re-checks only the tail below it — real, working incremental caching under the reentrant callback,
    not its absence.** This directly answers this item's own keystroke-cost question in the *favorable*
    direction for most edits, with one caveat that keeps it from being unambiguous good news for Myriad
    specifically: attributed domain types often sit early in build order precisely because other code
    depends on them, which is close to the worst-case position actually measured — the expensive case
    isn't a rare pathology for Myriad's own real usage, even though it isn't the universal case first
    concluded. Two follow-ups named by the review, not yet spiked: rerun the position sweep across
    multiple N (this session only spot-checked N=300) to get the actual per-edit-cost-vs-position curve
    item 18 needs for a real estimate; and test the dependency-chained variant this quartet's own
    design deliberately deferred (real cross-file `open`s, closer to Myriad's actual cross-generator
    visibility shape) to separate conservative compilation-order invalidation from genuine
    dependency-forced invalidation. See `Q023-scale-cost-reentrant-callback/03-review.md`.
    **The first of those two follow-ups was run immediately: `Q024-position-sweep-across-scale`,
    CLOSED, SHIP scoped (as of 2026-07-17).** A full sweep — 5 edit positions at each of N ∈
    {10, 50, 150, 300}, 20 runs, reusing Q023's own spike binary verbatim — found the per-successor
    marginal cost fits a clean line at every N, and the fitted slope shows no detectable systematic
    drift from N=10 to N=300, independently reproduced (the peak slope lands at a *middle* N in both
    the executor's and the reviewer's own separate rerun, directly refuting the pre-registered "slope
    grows with N" REVISE trigger rather than merely failing to find it). **This confirms the linear-
    in-successors model this item's cost estimate now rests on generalizes across the tested scale
    range, not just the single N=300 point Q023 itself measured.** Review scoped three framings down
    without reversing the verdict, worth citing correctly rather than the headline: "slope is
    essentially N-invariant" overstates what 5 points per fit can support (95% CI is ±76% at N=10;
    only the N=150/300 fits are tightly constrained — the defensible claim is "no *detectable*
    systematic drift," not a proven invariant); "intercept cross-validates the repeat median" is
    mildly circular, since the zero-successor point is itself one of the points feeding that same
    regression; one N=50 data point the write-up itself flagged as anomalous was confirmed, on
    independent rerun, to be a sampling outlier rather than a real plateau. **Most important for this
    item specifically: Q024 measures a cost model (whole-project `ParseAndCheckProject`, independent
    files), not a real FSAC live-editing session** — this item's own keystroke-cost question is
    better characterized than before, but still not directly measured against the actual host path an
    FSAC integration would use. The review's own top-priority follow-up, not yet spiked: test whether
    FSAC's own incremental per-file editing path (not a direct whole-project
    `ParseAndCheckProject` call) reproduces this same positional cost curve — the real remaining gap
    between "the cost model is well-characterized" and "this item's actual keystroke cost is known."
    Second follow-up, shared with Q023: test the dependency-chained variant both quartets' own designs
    deliberately deferred. See `Q024-position-sweep-across-scale/03-review.md`.

19. **A `myriad-live` watcher daemon that drives Q022's own already-proven reload signal on every save
    (new, 2026-07-17, extends Q022/item 8, composes only already-shipped pieces).** Prompted by
    inspecting `FSharp.Compiler.PortaCode`'s `fslive` tool (dead since 2020, not reusable directly, but
    its architecture is): a standalone process that watches project source files and reacts per-save,
    no compiler or IDE fork required. Q022's own review already found the missing piece for item 18's
    live-edit gap: an actual `.fsproj` mtime change (even a no-op touch) followed by a reissued
    `fsharp/workspaceLoad` re-triggers Myriad's codegen inside an *already-running* FSAC session, no
    restart. Nothing currently drives that signal on an ordinary source save. This item is exactly that
    driver: watch the attributed `.fs` files, re-run codegen (now a single batched process per project
    as of the `793bc98` fix, cheap enough to run on every save), then touch the `.fsproj`. Unlike item
    18, this needs no FSAC fork — it only automates a trigger Q022 already proved works when done by
    hand. Named risks, none yet checked: reload-storm behavior if saves arrive faster than a reload
    completes (needs debouncing); whether an automated `.fsproj` touch could race a real concurrent edit
    to the `.fsproj` itself; whether Ionide's own project-watch path (as opposed to Q022's raw FSAC/LSP
    harness) reacts to a script-driven touch the same way — untested, and item 17's real-Ionide-session
    harness would be the natural place to check it. Cheapest falsifier: extend Q022's own Round 2a/2b
    FSAC harness with a `FileSystemWatcher` on the attributed source file that touches the `.fsproj` and
    reissues `workspaceLoad` automatically, and confirm the reload signal still fires correctly when
    driven by a script instead of manually — if it does, item 18's live-edit gap is closed for the FSAC
    case without ever forking FSAC.

20. **Interpret typed FCS expressions directly, instead of hosting FSI, for the real generation-time
    computation Q014/Q015 never achieved — PROMOTED TO Q025, CLOSED, SHIP scoped (as of 2026-07-18).**
    See `Q025-typed-expr-interpreter/03-review.md` for the full verdict. Original framing kept below for
    context. A hand-written interpreter matching exactly six `FSharpExprPatterns` cases (`Const`,
    `NewRecord`, `Call`, `Let`, `Value`, `IfThenElse`) against a real, checker-produced `FSharpExpr`
    reflection-constructs records (including nested ones, a generalization review confirmed) and
    reflection-invokes already-compiled reference functions across a `Let`/`Call`/`IfThenElse` chain,
    driving a differing generated-member-name list, with zero `FsiEvaluationSession`/`Reflection.Emit`
    anywhere — reaching Q015's unmet goal by never hosting FSI at all, rather than by fixing Q015's own
    mechanism. Review found a real accuracy defect in the frozen docs: generic method calls were claimed
    to fall through to the interpreter's `NotImplementedException` safety net but don't — they match the
    ordinary `Call` arm and crash with a raw `InvalidOperationException` because `methodTypeArgs` is
    discarded, never fed to `MakeGenericMethod`. Scope, unchanged by the correction: a mechanism proof
    over six patterns and two fixtures, not general F# semantics — `Lambda`/`Application`, DU pattern
    matching, recursion, and generic calls are all unsupported, exactly the pre-registered REVISE
    ceiling. Named follow-ups, not yet spiked: interpret a function body (`Lambda`/`Application`) rather
    than just a value binding that calls functions; a DU/pattern-match fixture, since Myriad's own
    union-based generators are the obvious real target and none of the six implemented arms touch them.
    Original framing below. Prompted by `FSharp.Compiler.PortaCode`'s `CodeModel.fs`/`FromCompilerService.fs`/
    `Interpreter.fs`: it defines `DExpr`, a serializable form of FCS's resolved (post-typecheck)
    `FSharpExpr`, and a tree-walking interpreter that reflection-invokes against already-compiled
    reference assemblies with no `Reflection.Emit` needed by default. Q015's own review named the exact
    gap this could close: FSI never actually executed the general implementation under test, it only
    compiled a plugin and handed back a `MethodInfo` for host-compiled code via a one-line quotation
    destructure. Interpreting a typed FCS expression tree directly would let a generator actually run
    plugin-author logic against real data at generation time, with no `FsiEvaluationSession`, no dynamic
    assembly, and no compiled backend needed for the code being interpreted — combined with Q010's
    reentrant callback, a generator could check the compilation prefix and then *interpret* (not just
    re-typecheck) an attributed declaration's typed body to derive the config another generator
    consumes. Real costs, not glossed over: PortaCode itself is dead (2020-11-23, `FSharp.Compiler.
    Service 38.0.0`, the old `FSharp.Compiler.SourceCodeServices` namespace) — nothing here is
    vendorable, `FromCompilerService.fs`'s `FSharpExpr` walk would need porting to the current pin's
    `FSharpExprPatterns` active patterns, mechanical but real work, not a copy-paste; the interpreter's
    own semantics are explicitly approximate for exotic language features per its own `README.md`;
    reflection-invoke needs the referenced assemblies actually loaded and resolvable, which brushes
    against Q018's finding that FCS retains type-provider-hosting state process-globally — untested
    whether plain reflection-invoke with no type provider involved has the same retention problem.
    Cheapest falsifier: don't port the whole interpreter — hand-write the walk for one trivial
    `FSharpExpr` shape (e.g. a record construction or a string-literal-returning function body) against
    the current FCS pin, and confirm reflection-invoke against an already-compiled reference assembly
    produces the correct runtime value with no `FsiEvaluationSession` anywhere in the harness.

21. **Analyzer-driven preview of a generator's output at the attribute site, short of Q022's full
    live-generation goal (new, 2026-07-17, extends Q019/Q020, distinct from item 17; ceiling confirmed,
    not assumed, 2026-07-17 against PortaCode's `feature/analyzers` branch).** Item 17 tests whether
    Q020's diagnostic-anchoring claim survives a real Ionide session; this is a different, smaller
    capability. Run the plugin generator itself, in-memory, against the attributed type's parsed shape
    inside an `FSharp.Analyzers.SDK` analyzer, and surface the generated member signatures as an
    info-severity **diagnostic** at the attribute site (e.g. "will generate: `Lens<Person,string>
    name`") — without ever splicing generated code anywhere. **Diagnostic text only, deliberately not
    hover/tooltip content — checked directly, not assumed.** `FSharp.Compiler.PortaCode`'s
    `origin/feature/analyzers` branch (dead since 2021-02-17) prototyped exactly the richer version —
    an `OnCheckFile`/`TryAdditionalToolTip` analyzer hooked directly into FCS's own checking pipeline,
    able to inject arbitrary hover content, not just diagnostics — for `FST-1033-analyzers.md`
    (`fsharp/fslang-design`), Don Syme's own RFC for a compiler-service-level analyzer extension point.
    It required a patched `fsc.exe` from a personal fork branch (`--compilertool:`, "Requires branch
    feature/analyzers from dotnet/fsharp") and never shipped: confirmed directly against current
    `dotnet/fsharp` source, `FSharpAnalyzer`/`AnalyzerAttribute` have zero occurrences anywhere in it,
    and the RFC doc itself ends on an open TODO list, no implemented status. That closes the question
    of whether this item could aim higher than plain diagnostics using only the stock, shippable
    `FSharp.Analyzers.SDK` — it can't; the hover/tooltip-injection capability specifically needs the
    FST-1033 hook, which doesn't exist outside a personal, five-years-dead compiler fork, and standing
    one up is the same "materially bigger and riskier undertaking than anything else in this file" this
    repo's own closed-door note on AST-injection already warned against. This deliberately gives up the
    goal every other item in this list chases (making generated code itself live and usable) in exchange
    for something cheap and buildable today: no DTB hook, no FSAC change, no reentrant checker, no
    compiler fork, just an ordinary analyzer calling Myriad's own existing generator function and
    formatting its result as diagnostic text. The ceiling is real, low, and now known to be structural
    rather than a matter of not trying hard enough — a developer sees what would exist but can never
    call it, no IntelliSense on the actual member — so this doesn't compete with item 8/Q022's actual
    goal, it's a stopgap for the gap between "no signal at all" and "a real build." Cheapest falsifier:
    wire one existing generator (`Fields` or `Lenses`) into a minimal analyzer that runs it on save and
    formats the result as an info diagnostic at the attribute, confirmed rendering in a real Ionide
    session (piggybacking on item 17's harness if that's built first).

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
- **Reconstruct and re-verify Q008/Q09 — DONE, 2026-07-16.** Q008 and Q009 shipped with no saved
  `artifacts/`, and Q012 found that the fast, incremental `FSharpChecker` checking API
  (`ParseAndCheckFileInProject`) they both claim to have used **never** resolves a generative provided
  type — a direct, reproducible contradiction. `Q013-compile-then-pc-warming` (`CLOSED`, `NULL`) then
  tested the cheapest reconciling explanation (compile-then-PC warming) and it failed, but named the
  untested axis a reconstruction should vary first: real, non-script `FSharpProjectOptions` vs the
  `.fsx` script both Q012/Q013 used. **That reconstruction has now happened, and it was won by luck as
  much as design: Q008/Q09's actual original scratch source was found intact, unintentionally
  preserved, in a session job's own temp directory** (never copied into either quartet's `artifacts/`
  before closing). Rebuilt unmodified, both quartets' claimed results reproduced exactly, timings
  included. A direct same-checker isolation then confirmed Q013's named axis as the exact explanation:
  Q008/Q09's real harness drives PC with a hand-built, non-script `FSharpProjectOptions`; every
  Q012/Q013 shape used `GetProjectOptionsFromScript` instead. Given identical checker, identical
  provider, identical consumer text, the script route fails with Q012's exact diagnostic and the
  real-project-options route resolves cleanly, deterministic across repeats. Full account:
  `Q008-provenance-closed-loop/RECONSTRUCTION.md`, `Q009-field-level-provenance/RECONSTRUCTION.md`, and
  `FINDINGS.md`'s credibility section. Q008/Q09's SHIP verdicts are no longer disputed. **Named
  follow-ups, not yet done:** retest Q011's own two real providers and Q012's toy probes via the
  real-project-options route (both were only ever checked via `checker.Compile` or the script route
  respectively) — cheap, would round out the picture but don't block anything.
- **Stricter same-project-object PC form (new, extends Q013, cheap).** `Q013-compile-then-pc-warming`'s
  own review named this as the one warming variant most likely to differ from what Q013 actually
  tested: Compile a non-script `.fs` project, then PC the *same* project object, in place, on the same
  checker — as opposed to Q013's tested form (Compile via `fsc`-style args, PC via a separate `.fsx`
  script), which never shared a project object at all. A NULL here would close the residual doubt
  Q013's Correction 1 left open cheaply, before or alongside the Q008/Q09 reconstruction above, since
  both use the same underlying provider infrastructure already saved in `Q013-compile-then-pc-warming/
  artifacts/`.
- **Cross-instance / process-global compile-then-PC warming (new, extends Q013, low priority).** The
  Round 2 named in `Q013-compile-then-pc-warming/01-design.md` but correctly not run, since it was
  gated behind a positive Round 1 and Round 1 came back NULL. A same-instance null makes a
  process-global positive unlikely (and Q013's own incidental observation — cold controls run after
  earlier repeats' Compile calls in the same process still fail — weakly points the same way), but it
  is the one warming mechanism Q013's NULL does not logically exclude. Worth one run only if both items
  above come back negative too.
- **Locate the FCS code path behind PC's generative-emit failure (new, extends Q012/Q013).** Named as
  out of scope for Q012's own probe spike and still unlocated after Q013. Q013 narrowed where to look:
  each `ParseAndCheckFileInProject` call mints a fresh temp assembly for a generative provided type and
  never consults a prior `checker.Compile`'s output, so the gap is inside PC's own generative-emit
  path, not cross-API cache sharing. The only route from "characterize" to "fix," not required for the
  Q008/Q09 reconstruction itself.
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

### Round 4 — reversed information flow (a fourth brainstorming pass, run on a different model —
Fable, not the model that wrote rounds 1-3 — briefed on `Q016`/`Q017`/`Q018` and `Q019`-in-progress,
told explicitly to find a division of labor where Myriad and a type provider do genuinely different,
complementary jobs rather than compete to produce the same type surface)

Every combination tried so far, including `Q019` itself, has one thing in common: the type provider
consumes something Myriad made, or stands entirely alone. This pass was asked to find the opposite
shape — Myriad consuming something *about* the type provider — and produced one idea judged strong
enough to record verbatim rather than paraphrased into the house style, not yet spiked:

- **Tiered erasure: an erased-TP façade whose runtime fallback Myriad harvests and retires,
  bound through a runtime registry.** The observation: an erased provider can give a use site a
  correct typed surface at the first keystroke, computed purely from its own static arguments, with
  zero dependence on build state — but per the Q006 wall it can only ever erase to already-compiled
  *generic* machinery (an interpreter, reflection, a runtime-compiled delegate), never anything
  specialized to that specific use site, because it cannot bind to code in the compilation currently
  in progress. Myriad is the mirror image: it can put real, specialized, compiled, AOT-safe code
  *into* the current compilation, but has no design-time surface and generates from declarations, not
  from call sites. The proposed mechanism: an erased provider (`Codec<"schema-literal">`) has every
  provided member erase to `Registry.Invoke(key, args)`, where `key` hashes the static-argument tuple
  plus member name and `Registry` is an ordinary runtime dispatch table that falls back to a generic
  interpreter on a cache miss. Separately, a new Myriad generator parses the project's own source
  (the same `Fantomas.FCS` parse Myriad already does, no typechecking needed) looking for
  `SynType.App`/`SynType.StaticConstant` nodes matching the provider's instantiations — TP static
  arguments are always literals, so they're syntactically recoverable with no attribute needed on
  anything — and for each distinct static-argument tuple found, emits a real, specialized,
  reflection-free implementation into a generated `.fs` file tagged `[<SpecializationFor("key")>]`.
  At runtime, `Registry`'s first miss triggers a one-time scan of loaded assemblies for that
  attribute and self-populates. Binding happens through an attribute at run time, not through the
  compiler at design time — the only direction the wall permits — so a specialization compiled in the
  very same build as its use site still ends up executing in place of the interpreter, with no
  staleness (the façade is a pure function of source text, always fresh) and no file-lock problem
  (nothing is ever `Assembly.LoadFrom`'d at design time, unlike `Q016`/`Q018`). A harvest/use
  mismatch (e.g. an `#if`-guarded instantiation Myriad's harvest pass never sees) degrades to the
  interpreter, never a type error. **Why this isn't a rename of `Q016`-18 or `Q019`:** those all have
  the TP consuming an artifact Myriad produced (or, for `Q019`, consuming nothing but also producing
  nothing Myriad-specialized) — information flows from Myriad to the type surface, and the two fight
  over freshness. Here information flows from source *instantiations* to Myriad, the TP never loads
  anything Myriad made, and the only degradable thing is a performance tier, not correctness.
  **The honest weak point, argued against itself:** erasure fixes the runtime *signature* before any
  specialization exists, so `Registry.Invoke` traffics only in already-boxed/erased representations —
  a specialization can improve the work done inside the call, never the types at the call boundary.
  Worse, a competent interpreter would just compile and cache an expression-tree delegate on first
  use, at which point the Myriad-specialized tier may be within noise of the fallback on an ordinary
  desktop JIT — the whole mechanism may buy nothing there. It only earns its keep where the fallback
  is *structurally* disqualified: Native AOT, trimmed, or no-JIT targets (iOS, WASM) where
  reflection-emit is unavailable and reflection metadata gets trimmed away. It is explicitly not a
  fix for `[<Lenses>]`-shaped generation, which has no internal per-call work to specialize in the
  first place — if "AOT-safe erased providers" turns out to be a capability nobody actually needs,
  the idea dies on relevance, not mechanism. **Cheapest falsifiers, in order, each cheap enough to
  kill it before the next is built:** (1) a hand-written skeleton with no TP and no Myriad — one
  `Registry.Invoke`, one hand-written `[<SpecializationFor>]` implementation, the attribute-scan
  registration — published with Native AOT and trimming on; if the reflective/interpreted fallback
  survives AOT/trimming fine on its own, the entire capability delta is gone and the idea is dead
  before any provider code is written; (2) the same skeleton benchmarked, cached-compiled-delegate
  interpreter vs. specialized code, both called through the same boxed `Registry.Invoke` signature —
  if the delta is under roughly 2x on ordinary JIT and (1) already failed, close it; (3) a harvest
  probe — parse a file containing a real `Codec<"...">` instantiation with `Fantomas.FCS` and confirm
  the static constant is recoverable from the untyped AST in every position that matters (type
  abbreviations, annotations, inherit clauses), the one genuinely new Myriad-side claim, checkable in
  isolation from the rest. Only promote to a quartet if (1) and (3) both pass; (2) then only
  calibrates how the results section is allowed to word the performance claim, it doesn't gate
  whether the quartet gets built at all.
  **A sharper failure mode than anything above, added 2026-07-16, that should gate the design before
  any falsifier is run: the provider and the Myriad generator are two independent compilers for the
  same schema, and if they don't share one canonical lowering, a mismatch doesn't degrade to the
  interpreter — it silently binds the typed façade to the wrong specialized implementation.** The
  writeup above states a harvest miss degrades cleanly to the interpreter; it does not consider a
  harvest *hit* where the provider's own idea of a member's shape (its erasure decision, made from the
  static-argument literal) and the generator's own idea of that same member's shape (parsed
  independently from the same literal) have quietly diverged — different null handling, field order, or
  enum encoding, say, because the two lowerings were never actually forced to agree. The registry sees
  a matching key and binds it; the call typechecks, because the façade's *type* came from the provider,
  which never asked the generator anything; and the runtime result is wrong with no diagnostic anywhere.
  That is strictly worse than the clean-degradation case the design already accounts for. Mitigation
  needed before this is spike-shaped, not after: a single shared `Codec.Core` package (parse the schema
  once, canonicalize it once, derive a `ShapeId` and a per-member ABI descriptor once) that *both* the
  provider and the generator call — neither should contain its own schema-lowering logic — plus an
  ABI hash embedded in the generated specialization's own attribute that the registry validates before
  binding, refusing (falling back to the interpreter, or failing loudly under a strict mode) on
  mismatch rather than trusting a bare key match. **This also reorders the falsifier sequence given
  above:** run an AOT/trimming rooting probe *before* falsifier (1)'s reflective-fallback comparison,
  since AOT/trimming survival is the entire reason this idea exists (per its own scoping, it is
  explicitly not a fix for anything the fallback already handles adequately on ordinary JIT) — the test
  must show the generated specialization is discoverable by the registry *without* preserving the whole
  assembly's reflection metadata for the trimmer, since blanket preservation defeats the trimming
  benefit the idea is supposed to buy. If that fails, nothing else about the design matters. The harvest
  probe (3) above should also be stress-cased against aliasing, not just direct literals — a
  `[<Literal>]`-bound schema string consumed as `Codec<Schema>` rather than `Codec<"...">` is exactly
  the kind of thing real users alias, and if FCS accepts it as a valid provider static argument while
  the syntax-only harvester only sees an identifier (not the literal it resolves to), the "syntax-only,
  no typechecking needed" claim breaks for that case and needs either partial constant evaluation or a
  documented restriction — worth finding out before, not after, building the rest of the mechanism.

### Round 5 — informed by an external finding (Fable again, briefed this time on `fsharp/fslang-suggestions#864`:
F# has no Roslyn-source-generator equivalent and never will without a compiler-team-scale effort; the
team's own stated answer is "use Myriad" — see `project_fsharp_no_source_generators` memory note. Asked
what a type provider could borrow from source generators' *design* since it structurally can't borrow the
*mechanism*, specifically the two things C# generators have that this lineage has already found type
providers structurally lack: precise diagnostic anchoring, and a staged/incrementally-memoized pipeline)

- **One shared analysis, two diagnostic channels — PROMOTED TO Q020, CLOSED, SHIP (scoped, as of
  2026-07-16).** See `Q020-shared-analysis-diagnostic-channels/03-review.md` for the full verdict;
  original framing kept below for context. All three pre-registered rounds shipped and were
  independently reproduced by review, which then drove eight additional consumer shapes beyond the one
  the executor tested: the obsolete-attribute use-site channel generalizes cleanly (anchors the
  member-access node — receiver plus `.member` — never swallowing a containing pipe/lambda/conditional),
  and is genuinely new to this lineage — the first non-fatal, use-site-anchored, severity-controlled
  diagnostic a type provider has produced here (Q008/Q09/Q011 only ever threw at the static-argument
  site). But the review struck the idea's own headline claim: "one shared analysis, two channels that
  cannot disagree" is **tautological** — two callers of the identical pure function trivially agree, the
  same mechanism/capability conflation Q019's own Round 3 hit — and found the two halves are
  **un-entangled**, not one capability with two faces: the build-time diagnostics half needs *zero*
  type-provider machinery, and it is the half the idea itself calls more broadly useful, yet it was only
  ever demonstrated as a 25-line standalone format script, never wired into `IMyriadGenerator` for real.
  The review also did the honest weighing this idea's own novelty gate asked for and left open: an
  `FSharp.Analyzers.SDK` analyzer calling the identical shared function **strictly dominates** this
  result in Ionide (true arbitrary-range anchoring at the actual problem, no obsolete-attribute
  workaround, no synthetic fake members), leaving the type-provider channel's real niche narrow twice
  over — Visual Studio specifically, and only for a developer who has adopted a `Q019`-style preview
  provider and references its members; a VS user editing their own `[<Lenses>]` record directly gets
  nothing from this channel. Top follow-up, needing none of this quartet's TP machinery: build the real
  `IMyriadGenerator` diagnostics API the hypothesis itself named as the more valuable outcome.
  Original framing: The repo's standing finding is purely negative: a type provider can only
  signal an error by throwing, and FCS pins that message to the whole static-argument expression, never
  an arbitrary range. This doesn't dispute that wall — it routes around it by giving Myriad and a
  provider *the same analysis pass* (a function that parses a record's declaring file, exactly the
  extraction `Q019`'s provider already does, but returns a typed diagnostic list — code, severity,
  message, source range, and an "associated member" when one applies — not just a shape) and letting two
  independent emitters consume it. Myriad's CLI renders each diagnostic as a canonical MSBuild line
  (`path(line,col,line,col): warning MYR012: message`) anchored at the true declaration-site range —
  a real, small, and currently entirely absent capability on its own: Myriad plugins today have no
  diagnostics API at all, only "generate or throw." Separately, an *erased* provider (the `Q019` shape)
  never throws on a member-level problem; instead it still provides the member, but stamps it with
  `ObsoleteAttribute(message, isError)` (an API this SDK already exposes and shipping providers already
  use informally) or, for a diagnostic with no natural member to hang off, a synthetic member whose
  backtick-quoted name carries the message so it surfaces directly in the completion list. FCS then
  places the *live* diagnostic at every place the user actually types that member — the one location a
  provider genuinely controls, and arguably the more useful one, since that's where the user is looking
  while typing, not at the record's own declaration. **Why this is new relative to everything above and
  in `Q019`:** every prior combination in this lineage moved *code or types* between Myriad and a
  provider (satellite-DLL forwarding, a harvested runtime registry, a parallel preview type); none moved
  *diagnostics*, and no quartet has given Myriad plugins a diagnostics API of any kind. **Argued against
  itself:** obsolete-attribute errors carry a fixed "This construct is deprecated" prefix, a generic
  code, no quick-fix, and stay invisible until the user actually touches the poisoned member; the
  informal precedent for message-bearing provided members means the real novelty is the *shared-analysis
  contract* (one analysis, both channels can never disagree), not the poisoned-member trick itself — if a
  future reviewer judges the contract to be packaging rather than capability, this scopes down to "Myriad
  gets a diagnostics API," independently useful but much smaller than framed; and, the sharpest risk,
  `FSharp.Analyzers.SDK` analyzers already run live inside FSAC today and *can* report a diagnostic at an
  arbitrary range — an analyzer calling the identical shared analysis function gets true Roslyn-style
  anchoring in Ionide specifically, with no type provider involved at all, and may simply dominate the
  live channel for any Ionide user, leaving the provider path relevant mainly for hosts without analyzer
  support (Visual Studio) or for use-site-specific anchoring. **Cheapest falsifier:** on top of the
  `Q019` harness, add one member deliberately stamped `AddObsoleteAttribute("test message MYR012", true)`
  and one backtick-named synthetic member; open in an editor, use the poisoned member, and check whether
  the *custom* message renders (not just the generic obsolete text) and whether the squiggle lands at the
  use site rather than the provider's own instantiation line. The Myriad-side MSBuild-line emitter is
  separately and independently verifiable in minutes by hand-printing one canonical diagnostic line and
  confirming an editor anchors it at the declaration range.
- **Last-known-good staging for the erased provider (weaker, flagged high-null-risk by its own author).**
  `Q019`'s provider does one flat parse-and-rebuild per instantiation with no memoized intermediate
  stages, so its behavior while the watched file is mid-edit and briefly unparseable is unspecified —
  either it throws or serves garbage. Roslyn generators survive exactly this because earlier pipeline
  stages are memoized and carry the last good value forward when a later stage fails; that's arguably
  the capability that makes them feel "alive," not their raw speed. Proposed borrow: split `Q019`'s
  provider into content-hash-memoized stages (bytes → parse; parse → extracted shape, memoized by
  structural equality of the *shape* so pure formatting edits don't invalidate; shape → provided
  members), and on a parse failure, stage one returns the previous good value tagged stale (optionally
  surfaced as a marker member saying so) instead of failing the whole type. **Why new:** `Q019`'s own
  named follow-up was about member *typing* (real types vs. `obj`), not liveness robustness under a
  broken intermediate file state — no prior round treated the provider's *evaluation shape* itself as
  the thing worth borrowing from source generators. **Argued against itself, and likely fatal:** FCS's
  own provider-invalidation granularity may already make this moot — if FCS simply doesn't re-instantiate
  the provider at all while the watched file is syntactically broken, the previous provided type just
  persists on its own with no staging machinery needed, and the whole idea collapses to a NULL that
  mainly documents an FCS invalidation behavior nobody had pinned down (`Q019`'s own review left this
  exact question open). **Cheapest falsifier, under an hour:** in the `Q019` harness, introduce a syntax
  error into the watched file, trigger whatever re-check path was used for the live-edit round, and
  observe directly whether completion collapses or silently persists unchanged. Persists → NULL, done,
  the residue is only a documented FCS fact. Collapses → there's a real surface, and the next question
  (does FCS re-enter the provider per keystroke or only per invalidation event) determines whether
  staging has anything real to be incremental over.

Between the two, the first is the one worth spiking if either is: its falsifier is equally cheap, and its
downside residue (Myriad plugins gaining an actual diagnostics API) is independently worth having even if
the type-provider half of the idea doesn't survive review. The second idea's most probable outcome is a
NULL whose only real product is characterizing FCS's own provider-invalidation behavior — worth knowing,
but not obviously worth a full quartet on its own first.

## Known engineering gaps in current Myriad (no spike needed — verified from source, not hypotheses)

Surfaced as background findings while building the quartets above, not something a future spike
needs to re-derive. Candidate real fixes, independent of whether the architecture-exploration
track above goes anywhere:

- **Generated code is invisible to the IDE until a real build — DTB gate removed for real,
  2026-07-17 (see item 8 / Q022 for the full mechanism account).** `MyriadSdkGenerateCode`'s
  `Condition="'$(DesignTimeBuild)' != 'true'"` gate and the empty
  `MyriadSdkIncludeCodegenOutputDuringDesignTimeBuild` target are both gone from the real, shared
  `src/Myriad.Sdk/build/Myriad.Sdk.targets` — not just the Q022 scratch copy. Re-verified directly
  against this repo's own test project via a real `-p:DesignTimeBuild=true
  -p:SkipCompilerExecution=true` invocation: codegen runs, the compiled `.dll`'s mtime never moves
  (the real F# compiler is never invoked), and a repeat DTB call still correctly no-ops. Still only
  closes the gap at project load/reload time, exactly as Q022 found — an ordinary source-file save
  does not itself re-run codegen in an already-running IDE session; only a project reload does.
- **Codegen runs one cold process per input file — SHIPPED, 2026-07-17.** `MyriadSdkGenerateCode` now
  runs Myriad exactly once per project (not once per file) via a new `--manifest <file>.toml` CLI
  mode: MSBuild writes one small TOML file (one `[[unit]]` table per attributed file, built in a new
  `_MyriadSdkFlattenParams` target) and a single `<Exec>` processes all of them in one process,
  loading plugins once. This was not a pure size-preserving refactor — the old per-item
  `Outputs="%(MyriadCodegen.OutputPath)"` batching turned out to already be a false promise of
  per-file incrementality: `_MyriadSdkCodeGenInputCache` is one combined hash over every codegen
  input, so editing any single attributed file already forced every other generated file to
  regenerate too (confirmed directly: editing one file caused four unrelated generated files to be
  rewritten). Batching into one process therefore loses no real granularity, only removes N-1
  redundant cold-process launches. Two non-obvious MSBuild traps found only by testing end-to-end,
  not by reasoning about the XML: a `<Target>`'s own `Condition` is evaluated *before* its
  `DependsOnTargets`/`BeforeTargets` chain runs, so a naive `Condition="'@(MyriadCodegen)' != ''"`
  guard always saw an empty list and skipped the whole target, silently — the guard has to live on
  the `<Exec>` task itself instead; and forcing MSBuild to batch a helper target per-item has to key
  off `%(MyriadCodegen.OutputPath)` (guaranteed unique per `<Compile>` item), not
  `%(MyriadCodegen.Identity)` (the *input* file, which several different `<Compile>` items can
  legitimately share via `MyriadFile`) — keying on the wrong one silently merged multiple files'
  `MyriadParams`/`Generators` together. Full account: `DEVNOTES.md`'s "Why one process, not one per
  file" and "A target's `Condition` runs before its own dependencies" sections. All 58 existing tests
  plus `samples/Example` verified passing; committed as `793bc98`.
- **The project-context TOML writer no longer depends on MSBuild's implicit `;`-splitting of
  `Include` attributes — SHIPPED, 2026-07-17.** `_MyriadContext`'s multi-line TOML arrays
  (`referencePaths`, `compile`, etc.) used to be built as `[;$(_ReferencePaths);]`, relying on
  MSBuild silently splitting that one string into three separate items at the unescaped `;`
  characters so `WriteLinesToFile` would render them across multiple physical lines — the same class
  of fragility this repo's own git history already shows repeated fixes for ("escape with a single
  apostrophe," "try msbuild escaping targets," "fix for windows (again)"). Replaced with explicit
  `%0a`-embedded construction. Verified: item counts round-trip exactly (11 compile items matching 9
  explicit `<Compile>` entries plus MSBuild's 2 auto-generated assembly-info files), and a real
  embedded comma inside a path (`...AssemblyAttributes.fs`) doesn't break parsing, since the
  single-quoted TOML literal-string quoting (unchanged) protects it regardless of the outer
  delimiter mechanism. Committed as `793bc98`.
- **`Myriad.Sdk.targets`' `OutputPath` for `MyriadInlineGeneration` files had a stray trailing `)` —
  FIXED, 2026-07-17.** Left over from a 2022 refactor (`c818a70`) that removed a
  `[System.IO.Path]::GetFullPath(...)` wrapper but left its closing paren behind, producing an
  `OutputPath` metadata value that could never exist on disk. Since that value fed directly into
  MSBuild's own up-to-date check for the target, every build regenerated every
  `MyriadInlineGeneration` file regardless of whether anything had changed — confirmed by a direct
  before/after comparison (4 of 5 vs. 5 of 5 batched items correctly skipped on a no-op rebuild).
  One-character fix; committed as `793bc98`.
- **No `#line` pragmas in generated output.** Errors in generated code point at the generated
  file, not the source declaration that produced it. Cheap, mechanical fix.
- **Config lives behind an indirection.** An attribute carries a string key, which is looked up
  against a `myriad.toml` section, rather than the attribute carrying typed config directly. Real
  authoring friction, smaller than the above three.
- **Myriad diagnostics API — SHIPPED, 2026-07-17.** Q020's own top follow-up ("give `IMyriadGenerator` a
  real diagnostics API") has now been built, not just designed. `DiagnosticSeverity`, `MyriadDiagnostic`,
  and `IMyriadGeneratorWithDiagnostics` live in `src/Myriad.Core/Types.fs`; rendering as canonical
  MSBuild lines (`path(line,col,line,col): severity CODE: message`, with a line-1 fallback when no range
  is given) is a pure function, `Diagnostics.format`, in the new `src/Myriad.Core/Diagnostics.fs`;
  `runGenerator` (`src/Myriad/Program.fs`) adds one `:? IMyriadGeneratorWithDiagnostics` branch that
  prints each diagnostic and only fails the build (via the same `!CompilationError` path the old
  exception-based failure already used) on an `Error`-severity diagnostic — `Warning`/`Info` diagnostics
  print but let generation succeed. Every existing plugin (`Fields`, `Lenses`, `DUCases`) is untouched
  through the old `IMyriadGenerator` interface path. Five new tests cover it in
  `test/Myriad.IntegrationPluginTests`: three pure unit tests of `Diagnostics.format` (range-anchored,
  no-range fallback, `Info` severity), one real end-to-end MSBuild round trip
  (`DiagnosticsWarningGen`/`ArbitaryFile3.fs`, proving a Warning diagnostic doesn't fail a real build and
  its output is used), and one CLI-subprocess test of the error path (`DiagnosticsErrorGen`, proving a
  non-zero exit, the diagnostic's code/message printed, and no output file written) — all 58 tests in
  the suite pass. This was engineering, not a hypothesis, exactly per this file's own carve-out; no
  quartet was needed or run.
- **Structure that diagnostics API, and any future `FSharp.Analyzers.SDK` adapter (item 17 above),
  around one shared analysis function from the start, not two (added 2026-07-16).** Q020 found the
  build-time and provider-side diagnostic halves are already un-entangled; the risk in building both
  separately is re-deriving the same record-shape analysis logic twice and having the two drift once
  either changes independently. Concrete shape worth committing to before either is built: a pure
  `SharedAnalysis.analyze : ParsedInput -> MyriadDiagnostic list` function (reusing `Q019`'s own
  self-parse-with-`Myriad.Core.Ast` approach, not a new parser), consumed by (a) the CLI-side emitter
  above and (b) an analyzer adapter if item 17 ships. Neither consumer should contain analysis logic of
  its own — this is a structural constraint on the two items above, not a new capability in itself.
