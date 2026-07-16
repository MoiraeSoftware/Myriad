# Q020-shared-analysis-diagnostic-channels / Movement 2 — Design

**Status:** IN PROGRESS.
**Location:** `experiments/Q020-shared-analysis-diagnostic-channels/artifacts/`.
**Pins:** identical to `Q019` — FCS `43.9.101`, `FSharp.TypeProviders.SDK`'s `ProvidedTypes.fs`/`.fsi`
copied from `Q019-erased-self-parsing-provider/artifacts/` (already known-good, not re-fetched),
`Fantomas.Core` `7.0.5`, `Myriad.Core.dll` built Release from `src/Myriad.Core`. Project shape (
`.DesignTime`/`.Runtime` split, `assemblyReplacementMap`, `CopyLocalLockFileAssemblies` on both) copied
from `Q019`'s working configuration rather than re-derived, since `Q019`'s review already confirmed
what's load-bearing there (`addDefaultProbingLocation`) vs. defensive-only (`assemblyReplacementMap`).

## Round 1 — falsifier: does `AddObsoleteAttribute` anchor at the use site with correct severity?

1. `DiagTP.DesignTime`/`DiagTP.Runtime`: a minimal erased provider, `DiagTP.Provided.Sample`, with
   **no** static parameters (deliberately simplified relative to `Q019` — this round isolates the
   obsolete-attribute mechanism itself, not self-parsing, which `Q019` already validated). Two
   properties: `Good` (plain, no attribute) and `Bad` (stamped `AddObsoleteAttribute("Q020 test
   message", false)`).
2. Harness consumer file: reference both `Good` and `Bad` from distinct, deliberately-placed lines/
   columns (e.g. indented, on a line with other tokens around it) so the range assertion is meaningful,
   not trivially "line 1 column 1."
3. `checker.ParseAndCheckFileInProject`, inspect `FSharpDiagnostic` list: assert `Good`'s use produces
   **zero** diagnostics (negative control); `Bad`'s use produces exactly one diagnostic containing the
   literal string `"Q020 test message"`, with `Severity = Warning`, and `(StartLine, StartColumn,
   EndLine, EndColumn)` matching the exact token range of the `.Bad` reference in the consumer text
   (computed independently from the written source, not read back from the diagnostic itself — e.g. by
   locating the substring's index and converting to line/col before the check runs, so the assertion
   can't trivially pass by construction).
4. Repeat with `AddObsoleteAttribute("Q020 test message", true)` on a third property, `BadError`;
   assert `Severity = Error`, same message and range behavior.

Kill condition, pre-registered: if the diagnostic's range is the provider's own static-parameter/
type-instantiation site (or the whole containing expression) rather than the specific member-access
token, or if the message is replaced by FCS's generic obsolete text with no trace of the custom string,
stop — Rounds 2/3 would be building on a mechanism that doesn't clear this quartet's own bar.

## Round 2 — falsifier: is a non-identifier member name legal and genuinely discoverable?

1. Add a fourth property to `DiagTP.Provided.Sample` whose name is a string containing a colon and
   spaces (` `` warning MYR012: field 'Meta' unresolvable, exposed as obj` `` `, escaped as needed for
   `ProvidedProperty`'s constructor — resolved concretely while building, since `ProvidedProperty`'s
   name parameter's accepted character set isn't verified from documentation alone).
2. Confirm `checker.ParseAndCheckFileInProject` accepts a hand-written double-backtick reference to the
   exact name with zero diagnostics (baseline: it's at least legal and accessible).
3. **Discoverability, the actual claim, checked separately from accessibility:** call
   `FSharpCheckFileResults.GetDeclarationListInfo` (or `GetDeclarationListSymbols`, whichever the pinned
   FCS version's API surface actually exposes — checked directly, not assumed) at a member-access
   position on the provided type (e.g. right after `Sample().`), and assert the synthetic name appears
   in the returned completion items. This is the discriminating test Fable's own write-up named:
   accessible-if-you-already-know-the-name is a much weaker claim than appears-in-completion.

## Round 3 — capability falsifier: one analysis, two consistent emitters

1. `SharedAnalysis.fs` (a plain library module, no TP/Myriad-CLI dependency of its own): a function
   `analyze : filePath: string -> DiagnosticInfo list` where `DiagnosticInfo = { Code: string; Severity:
   Severity; Message: string; Range: (int*int*int*int); Member: string option }`. Implementation reuses
   `Q019`'s exact self-parsing approach (`Myriad.Core.Ast.fromFilename`/`extractRecords`), extended to
   detect the concrete condition: a record field whose declared `SynType` is a named type that is
   **not** a recognized primitive/BCL type and **is** itself a record defined in the *same* file (a real
   instance of `Q019`'s own named open question, not a contrived one) — e.g. `Company = { title: string;
   meta: Meta }` where `Meta` is declared in the same file. Produces one `DiagnosticInfo` naming the
   field, the unresolvable type, and a fixed code (`MYR012`).
2. **Emitter A (Myriad-CLI-style):** a small console program that calls `SharedAnalysis.analyze`,
   converts each result to the MSBuild canonical line, `sprintf "%s(%d,%d,%d,%d): %s %s: %s" filePath sl
   sc el ec (severityWord) code message`, and prints it to stdout. Verify the printed line matches
   Microsoft's documented canonical format (`origin(line,col,line,col): category code: text`,
   `learn.microsoft.com/en-us/visualstudio/msbuild/msbuild-diagnostic-format-for-tasks`) character for
   character, not just "looks similar."
3. **Emitter B (TP live channel):** `DiagTP`'s provider (now given `Q019`'s static parameters back,
   `SourceFilePath`/`RecordName`) calls the *same* `SharedAnalysis.analyze` function during
   instantiation, and for any `DiagnosticInfo` whose `Member` matches a field it's about to provide,
   calls `AddObsoleteAttribute` on that member with the identical `Message`/`Severity`-derived `isError`
   instead of providing it plainly.
4. Consumer test: reference the poisoned field from Emitter B's provided type; assert the live
   diagnostic's message matches Emitter A's printed message for the *same* underlying finding (same
   field name, same code, same core text — allowing only for each channel's own presentation wrapping,
   not a difference in substance). This is the "cannot disagree" claim, checked by literal comparison,
   not by argument.

## Reproduction

```
dotnet build src/Myriad.Core -c Release          # from repo root, if not already built
cd experiments/Q020-shared-analysis-diagnostic-channels/artifacts
dotnet build DiagTP.DesignTime && dotnet build DiagTP.Runtime
dotnet run --project Harness -- round1
dotnet run --project Harness -- round2
dotnet run --project Harness -- round3
```

No MSBuild integration with Myriad's own `Myriad.Sdk` targets or `IMyriadGenerator` interface — deliberate,
matching every prior quartet. Emitter A is a standalone console program demonstrating the diagnostic
*format*, not a modification to Myriad's real CLI (`src/Myriad/Program.fs`) — wiring it into the real CLI
is an explicit, named follow-up if this quartet ships, not part of the spike itself.
