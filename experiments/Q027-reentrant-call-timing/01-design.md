# Q027 — Movement 2: Design

## Overview

One console app, `artifacts/q027-spike/`, run as multiple independent fresh `dotnet run` processes
(one per repeat — deliberately not reusing one process across repeats, the specific trap Q023's own
design fell into and had to disclose).

## Files

Three virtual files, each built from Q023's own `mkPrefixSrc` template
(`Q023-.../artifacts/q023-spike/Program.fs:24-42`), renamed per file, `extraTags = 0` for all three
(no scale sweep here — file *content weight* matters, file *count* does not, since this quartet
holds project size fixed at 3):

```fsharp
let files = ConcurrentDictionary<string, string>()
let fileA = @"C:\virt-q027\A.fs"
let fileB = @"C:\virt-q027\B.fs"
let fileC = @"C:\virt-q027\C.fs"

let mkSrc (moduleName: string) =
    // Q023's own mkPrefixSrc(i, extraTags=0) template, renamed per module - genuine typecheck
    // weight (generic record, Map/List pipeline, explicit recursion), not `let x = 5` padding.
    sprintf
        "module %s\n\
         type Record = { Id: int; Name: string; Tags: string list; Meta: Map<string,int> }\n\
         type Wrapper<'a> = { Value: 'a; Items: 'a list; Count: int }\n\
         let make (id: int) (name: string) (tags: string list) : Record =\n\
         \x20   { Id = id; Name = name; Tags = tags; Meta = tags |> List.mapi (fun i t -> t, i) |> Map.ofList }\n\
         let rec fold (f: 'acc -> 'a -> 'acc) (acc: 'acc) (xs: 'a list) : 'acc =\n\
         \x20   match xs with\n\
         \x20   | [] -> acc\n\
         \x20   | x :: rest -> fold f (f acc x) rest\n\
         let wrap (v: 'a) : Wrapper<'a> = { Value = v; Items = [v]; Count = 1 }\n\
         let combine (a: Wrapper<'a>) (b: Wrapper<'a>) : Wrapper<'a> =\n\
         \x20   { Value = a.Value; Items = a.Items @ b.Items; Count = a.Count + b.Count }\n\
         let records = [ for k in 1..20 -> make k (sprintf \"item%%d\" k) [ \"a\"; \"b\"; string k ] ]\n\
         let total = records |> fold (fun acc r -> acc + r.Meta.Count) 0\n"
        moduleName
```

## Instrumentation

```fsharp
let sw = Stopwatch()

let timeCheck (checker: FSharpChecker) (path: string) (opts: FSharpProjectOptions) : int64 =
    sw.Restart()
    let _, ans =
        checker.ParseAndCheckFileInProject(path, 0, SourceText.ofString files.[path], opts)
        |> Async.RunSynchronously
    sw.Stop()
    match ans with
    | FSharpCheckFileAnswer.Aborted -> failwithf "check of %s aborted" path
    | FSharpCheckFileAnswer.Succeeded _ -> ()
    sw.ElapsedMilliseconds
```

Reentrant `DocumentSource.Custom` callback on the outer, persistent checker (`checker1`):

```fsharp
let mutable reentrantMsA = -1L
let mutable reentrantMsB = -1L

let docSource =
    DocumentSource.Custom(fun path ->
        async {
            if path = fileA then return Some(SourceText.ofString files.[fileA] :> ISourceText)
            elif path = fileB then return Some(SourceText.ofString files.[fileB] :> ISourceText)
            elif path = fileC then
                // Reentrant: checker1 is the SAME instance the outer ParseAndCheckProject call
                // (still unreturned on the stack) is using. Time these two calls precisely.
                reentrantMsA <- timeCheck checker1 fileA optsRef
                reentrantMsB <- timeCheck checker1 fileB optsRef
                return Some(SourceText.ofString files.[fileC] :> ISourceText)
            else return None
        })
```

## Sequence

1. `checker1 = FSharpChecker.Create(keepAssemblyContents = true, documentSource = docSource,
   useTransparentCompiler = true)`.
2. Build `opts` via `GetProjectOptionsFromScript(fileA, ...)` overridden with `SourceFiles = [|
   fileA; fileB; fileC |]`, matching Q010/Q026's own pattern.
3. `checker1.ParseAndCheckProject(opts)` — the outer drive. This is where `reentrantMsA`/
   `reentrantMsB` get set, via the callback above, **while this call has not yet returned**.
4. **Warm repeat** (same checker, now unambiguously idle — the outer call above has fully
   returned): `warmMsA = timeCheck checker1 fileA opts`, `warmMsB = timeCheck checker1 fileB opts`.
5. **Cold check** (brand-new checker instances, never touched `fileA`/`fileB` before, default
   `DocumentSource.Filesystem` — no callback needed for a single direct call with explicit source
   text): for each of A and B, `let coldChecker = FSharpChecker.Create(useTransparentCompiler =
   true)` then `coldMs = timeCheck coldChecker file opts` (same `opts`, i.e. the same 3-file project
   shape, so the *only* difference between cold and reentrant/warm is which checker instance and
   whether other files were already processed — not project shape).
6. Print `reentrantMsA/B`, `warmMsA/B`, `coldMsA/B`, and the ratios `reentrant/cold`, `reentrant/warm`,
   `warm/cold`.
7. Exit.

## Reproduction

Run the whole program as **5 independent fresh processes** (`dotnet run -c Release --no-build`,
repeated 5 times, not reusing one process's in-memory checkers across repeats):

```
dotnet build experiments/Q027-reentrant-call-timing/artifacts/q027-spike -c Release
for i in 1 2 3 4 5; do
  dotnet run --project experiments/Q027-reentrant-call-timing/artifacts/q027-spike -c Release --no-build
done
```

Report the median of each of the six numbers (`reentrantMsA/B`, `warmMsA/B`, `coldMsA/B`) across
the 5 runs, plus the full per-run table (not just the median) so the spread is visible — this
lineage's own cross-cutting caveat ("every timing number is a single sample") is exactly what
taking 5 independent process-level repeats is meant to soften, honestly reported rather than
collapsed into one number.

FCS pinned `43.9.101`, matching every prior quartet in this sub-line.

## What would make this NULL / KILL / REVISE, stated before running

- **NULL** if the reentrant, warm, and cold numbers are too close together, or too noisy relative to
  each other, to distinguish any of the three conditions (e.g. all three within the same few
  milliseconds due to how cheap these particular files are, or run-to-run variance exceeding the
  gaps between conditions) — report this plainly rather than picking a side the data doesn't
  support.
- **REVISE** if the result is directionally clear but doesn't cleanly match either bound (e.g.
  reentrant sits meaningfully between cold and warm, not close to either) — report the actual
  number, not force it into "confirms mid-flight" or "confirms cache hit."
- **The two clean, falsifiable outcomes**, stated before running:
  - **Reentrant ≈ warm, both ≪ cold** → supports Q010 review's suspected benign explanation:
    FCS's own `SourceFiles`-order walk had already resolved A/B before ever requesting C's source,
    and the "reentrant" call is a cache hit, not forced fresh work. This would mean every "mid-
    compilation," "while a check is already in flight" framing used across Q010/Q021/Q023/Q024/
    Q026's write-ups should be retired in favor of "correctly and safely queries the *already-
    resolved* typed prefix," a real but weaker mechanism claim.
  - **Reentrant ≈ cold, both ≫ warm** → supports the stronger framing: the reentrant call is doing
    material fresh work at the moment it's made, consistent with genuine mid-flight interleaving
    (though still not direct proof of concurrent execution, only that the *cost* looks like first-
    touch work rather than a cache hit).
