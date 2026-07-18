// Round 2: does the in-process incremental cache pay off, and does it correctly
// invalidate a downstream file when its dependency breaks? See ../../02-results.md.
// Headline finding: neither bumping the changed file's own version, nor the
// downstream file's version, propagates a breaking change. Only an explicit
// checker.InvalidateConfiguration(options) call does.

open System.Diagnostics
open System.Collections.Concurrent
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text

let files = ConcurrentDictionary<string, string>()
let fileA = @"C:\virt\A.fs"
let fileB = @"C:\virt\Generated.fs"

let personSrcV1 =
    """module A
type MyAttribute() = inherit System.Attribute()
type Id = int
[<My>]
type Person = { Name: string; Age: Id; Nick: string option }
"""

// Breaking change: Age is gone. Anything downstream that reads p.Age should now fail to typecheck.
let personSrcV2 =
    """module A
type MyAttribute() = inherit System.Attribute()
type Id = int
[<My>]
type Person = { Name: string; Nick: string option }
"""

let generatedSrc =
    "module Generated\nopen A\n\nlet describe (p: Person) = sprintf \"%s/%d\" p.Name p.Age\n"

files.[fileA] <- personSrcV1
files.[fileB] <- generatedSrc

let docSource =
    DocumentSource.Custom(fun path ->
        async {
            match files.TryGetValue path with
            | true, txt -> return Some(SourceText.ofString txt :> ISourceText)
            | _ -> return None
        })

let checker = FSharpChecker.Create(keepAssemblyContents = true, documentSource = docSource)

let time (label: string) (f: unit -> Async<FSharpCheckFileAnswer>) =
    async {
        let sw = Stopwatch.StartNew()
        let! ans = f ()
        sw.Stop()
        let outcome =
            match ans with
            | FSharpCheckFileAnswer.Succeeded r ->
                if r.Diagnostics.Length = 0 then "OK"
                else sprintf "ERRORS(%d): %s" r.Diagnostics.Length (r.Diagnostics.[0].Message)
            | FSharpCheckFileAnswer.Aborted -> "ABORTED"
        printfn "%-58s %6dms  %s" label sw.ElapsedMilliseconds outcome
        return ans
    }

let run =
    async {
        let! baseOpts, _ = checker.GetProjectOptionsFromScript(fileA, SourceText.ofString personSrcV1)

        let stableOpts =
            { baseOpts with
                ProjectFileName = @"C:\virt\spike.fsproj"
                SourceFiles = [| fileA; fileB |] }

        let checkA ver = fun () -> async { let! _, ans = checker.ParseAndCheckFileInProject(fileA, ver, SourceText.ofString files.[fileA], stableOpts) in return ans }
        let checkB ver = fun () -> async { let! _, ans = checker.ParseAndCheckFileInProject(fileB, ver, SourceText.ofString files.[fileB], stableOpts) in return ans }

        printfn "=== Trial 1: cold start ==="
        let! _ = time "check A v0 (cold - resolves framework, typechecks A)" (checkA 0)
        let! _ = time "check B v0 (cold - first look at Generated.fs)" (checkB 0)

        printfn ""
        printfn "=== Trial 2: re-check UNCHANGED files, same version ==="
        let! _ = time "check A v0 again (same content, same version)" (checkA 0)
        let! _ = time "check B v0 again (same content, same version)" (checkB 0)

        printfn ""
        printfn "=== Trial 3: re-check UNCHANGED files, version bumped anyway ==="
        let! _ = time "check A v1 (same content, version bumped)" (checkA 1)
        let! _ = time "check B v1 (same content, version bumped)" (checkB 1)

        printfn ""
        printfn "=== Trial 4: mutate B only (append a comment), bump B's version ==="
        files.[fileB] <- generatedSrc + "// touch 1\n"
        let! _ = time "check B v2 (B content changed, A untouched)" (checkB 2)
        let! _ = time "check A v1 again (A untouched - should be cheap if cache is fine-grained)" (checkA 1)

        printfn ""
        printfn "=== Trial 5: the real test - break A (remove Age field), does B's stale result get invalidated? ==="
        files.[fileA] <- personSrcV2
        let! _ = time "check A v2 (BREAKING change: Person no longer has Age)" (checkA 2)
        printfn "  (A now typechecks fine on its own - Age just doesn't exist anymore)"
        let! ansB = time "check B v2 AGAIN, same content/version as trial 4, but A changed underneath it" (checkB 2)
        match ansB with
        | FSharpCheckFileAnswer.Succeeded r when r.Diagnostics.Length = 0 ->
            printfn ""
            printfn "  !! STALE: B still reports success even though it references p.Age, which no longer exists."
        | FSharpCheckFileAnswer.Succeeded r ->
            printfn ""
            printfn "  Correctly invalidated: B now reports %d error(s), e.g. %s" r.Diagnostics.Length r.Diagnostics.[0].Message
        | FSharpCheckFileAnswer.Aborted -> ()

        printfn ""
        printfn "=== Trial 6: same scenario, but bump B's version too (the realistic 'regenerate B' case) ==="
        let! ansB2 = time "check B v3, version bumped, content identical to v2" (checkB 3)
        match ansB2 with
        | FSharpCheckFileAnswer.Succeeded r when r.Diagnostics.Length > 0 ->
            printfn "  Correctly caught: %d error(s), e.g. %s" r.Diagnostics.Length r.Diagnostics.[0].Message
        | FSharpCheckFileAnswer.Succeeded _ ->
            printfn "  Still reports success - even a version bump on B alone did not see A's change."
        | FSharpCheckFileAnswer.Aborted -> ()

        printfn ""
        printfn "=== Trial 6b: explicitly tell FCS the project config/dependency changed ==="
        checker.InvalidateConfiguration(stableOpts)
        let! ansB3 = time "check B v4, AFTER InvalidateConfiguration(stableOpts)" (checkB 4)
        match ansB3 with
        | FSharpCheckFileAnswer.Succeeded r when r.Diagnostics.Length > 0 ->
            printfn "  Correctly caught after InvalidateConfiguration: %d error(s), e.g. %s" r.Diagnostics.Length r.Diagnostics.[0].Message
        | FSharpCheckFileAnswer.Succeeded _ ->
            printfn "  STILL stale even after InvalidateConfiguration."
        | FSharpCheckFileAnswer.Aborted -> ()

        return ()
    }

[<EntryPoint>]
let main _ =
    Async.RunSynchronously run
    0
