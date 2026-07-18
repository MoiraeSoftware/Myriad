# Q002 artifacts

One console project, `nested-dispatch/`, pinned to `FSharp.Compiler.Service` 43.9.101. Runs both
Round A (cross-file resolution falsifier) and Round B (the generator build) in one program —
Round B only executes if Round A passes.

```
cd nested-dispatch && dotnet run
```

`round-c-syntax-resolver/`, pinned to `Fantomas.Core` 7.0.5 (matches Myriad's own pin), built as a
same-day addendum after the first close-out flagged Round C as reasoned-not-measured. Uses
Myriad's own `Ast.fs` parsing/attribute-matching logic (copied verbatim, credited inline in
`Program.fs`) to run a real syntax-only cross-file resolver head to head against Round A/B's typed
approach, including the adversarial two-same-named-types case.

```
cd round-c-syntax-resolver && dotnet run
```
