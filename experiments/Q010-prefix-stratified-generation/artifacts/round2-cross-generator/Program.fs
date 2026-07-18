// Q010 Round 2 - cross-generator composition (the capability claim).
//
// Three virtual files:
//   A.fs          - the domain record (Person).
//   B.fs          - a hand-written stand-in for LensesGenerator's real output on Person
//                   (namespace-recursive wrapper + nested PersonLenses module + open parent +
//                    one `let <FieldName> = (getter, setter)` tuple-of-lambdas per field, default
//                    non-piped setter order `(x) (value)` - faithful to src/Myriad.Plugins/LensesGenerator.fs).
//   Stratified.fs - a THIRD generator (a JSON serializer) whose content is computed on demand,
//                   mid-check, by reentrantly typed-inspecting B.fs's already-generated PersonLenses
//                   module and reusing its accessors instead of re-deriving field access.
//
// The capability claim: the generated PersonJson.serialize reference to Domain.PersonLenses.Name
// resolves to a REAL typed symbol from B.fs (proven by symbol lookup), not a textual coincidence.
//
// See ../../02-results.md for the run this produced.

open System
open System.Collections.Concurrent
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text

let files = ConcurrentDictionary<string, string>()
let fileA = @"C:\virt\A.fs"
let fileB = @"C:\virt\B.fs"
let fileStratified = @"C:\virt\Stratified.fs"

let aSrc =
    "namespace Domain\n\ntype Person = { Name: string; Age: int }\n"

// Faithful to LensesGenerator: field-name-cased bindings, default (non-piped) setter order (x)(value),
// tuple of two parenthesised lambdas, nested module named <Record>Lenses, `open` of the parent namespace,
// wrapped in a recursive namespace (SynModuleOrNamespace.CreateNamespace isRecursive=true).
let bSrc =
    "namespace rec Domain\n\nmodule PersonLenses =\n    open Domain\n    let Name = ((fun (x: Person) -> x.Name), (fun (x: Person) (value: string) -> { x with Name = value }))\n    let Age = ((fun (x: Person) -> x.Age), (fun (x: Person) (value: int) -> { x with Age = value }))\n"

files.[fileA] <- aSrc
files.[fileB] <- bSrc

let mutable checkerRef : FSharpChecker = Unchecked.defaultof<_>
let mutable optsRef : FSharpProjectOptions = Unchecked.defaultof<_>
let mutable reentrantEntered = false
let mutable generatedText = "<unset>"
let mutable discoveredLensBindings : (string * string) list = []   // (bindingName, formattedType)

let rec collectEntities decls = seq {
    for d in decls do
        match d with
        | FSharpImplementationFileDeclaration.Entity(e, sub) ->
            yield e
            yield! collectEntities sub
        | _ -> () }

let rec collectMfvs decls = seq {
    for d in decls do
        match d with
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(v, _, _) -> yield v
        | FSharpImplementationFileDeclaration.Entity(_, sub) -> yield! collectMfvs sub
        | _ -> () }

// Extract getter return-type name from a lens binding's type: ((P -> ret) * (P -> v -> P)).
let getterReturnTypeName (v: FSharpMemberOrFunctionOrValue) =
    try
        let t = v.FullType
        if t.IsTupleType && t.GenericArguments.Count >= 1 then
            let getterFn = t.GenericArguments.[0]
            if getterFn.IsFunctionType && getterFn.GenericArguments.Count >= 2 then
                Some(getterFn.GenericArguments.[getterFn.GenericArguments.Count - 1].Format FSharpDisplayContext.Empty)
            else None
        else None
    with _ -> None

// Compute Stratified.fs (PersonJson) FROM the typed check results of the B.fs prefix.
let synthesizePersonJson (checkedB: FSharpCheckFileResults) =
    let implB = checkedB.ImplementationFile |> Option.get
    let lensModule =
        collectEntities implB.Declarations
        |> Seq.find (fun e -> e.DisplayName = "PersonLenses")
    // Discover the lens bindings by typed inspection, not by hardcoding "Name"/"Age".
    let bindings =
        collectMfvs implB.Declarations
        |> Seq.filter (fun v -> not v.IsMember && v.DeclaringEntity.IsSome && v.DeclaringEntity.Value.DisplayName = "PersonLenses")
        |> Seq.map (fun v ->
            let retTy = getterReturnTypeName v |> Option.defaultValue "?"
            v.DisplayName, v.FullType.Format FSharpDisplayContext.Empty, retTy)
        |> List.ofSeq
    discoveredLensBindings <- bindings |> List.map (fun (n, t, _) -> n, t)
    printfn "  [callback] typed-inspected Domain.%s, found %d lens binding(s):" lensModule.DisplayName bindings.Length
    for (n, t, ret) in bindings do
        printfn "    %s : %s   (getter returns %s)" n t ret

    // Emit accessors from the discovered binding names; pick the sprintf specifier from the
    // typed getter return type (string -> %s, otherwise %d).
    let letLines =
        bindings
        |> List.map (fun (n, _, _) -> sprintf "    let get%s, _ = Domain.PersonLenses.%s" n n)
        |> String.concat "\n"
    let jsonParts =
        bindings
        |> List.map (fun (n, _, ret) ->
            // NB: ret formats fully-qualified (e.g. "Microsoft.FSharp.Core.string"), so match on suffix.
            let spec = if ret.EndsWith "string" then "\\\"%s\\\"" else "%d"
            sprintf "\\\"%s\\\":%s" (n.ToLowerInvariant()) spec)
        |> String.concat ","
    let jsonArgs =
        bindings
        |> List.map (fun (n, _, _) -> sprintf "(get%s p)" n)
        |> String.concat " "
    sprintf
        "module Domain.PersonJson\n\nlet serialize (p: Domain.Person) =\n%s\n    sprintf \"{%s}\" %s\n"
        letLines jsonParts jsonArgs

let makeReentrantDocSource () =
    DocumentSource.Custom(fun path ->
        async {
            if path = fileA then return Some(SourceText.ofString files.[fileA] :> ISourceText)
            elif path = fileB then return Some(SourceText.ofString files.[fileB] :> ISourceText)
            elif path = fileStratified then
                reentrantEntered <- true
                // Reentrantly check A then B on the same in-flight checker/opts.
                let! _, ansA = checkerRef.ParseAndCheckFileInProject(fileA, 0, SourceText.ofString files.[fileA], optsRef)
                match ansA with
                | FSharpCheckFileAnswer.Aborted -> failwith "reentrant check of A aborted"
                | FSharpCheckFileAnswer.Succeeded _ -> ()
                let! _, ansB = checkerRef.ParseAndCheckFileInProject(fileB, 0, SourceText.ofString files.[fileB], optsRef)
                let checkedB =
                    match ansB with
                    | FSharpCheckFileAnswer.Succeeded r -> r
                    | FSharpCheckFileAnswer.Aborted -> failwith "reentrant check of B aborted"
                let text = synthesizePersonJson checkedB
                generatedText <- text
                files.[fileStratified] <- text
                return Some(SourceText.ofString text :> ISourceText)
            else return None
        })

[<EntryPoint>]
let main _ =
    printfn "Q010 Round 2 - cross-generator composition"
    printfn ""

    let docSource = makeReentrantDocSource ()
    let checker = FSharpChecker.Create(keepAssemblyContents = true, documentSource = docSource, useTransparentCompiler = true)
    checkerRef <- checker

    let run =
        async {
            let! baseOpts, _ = checker.GetProjectOptionsFromScript(fileA, SourceText.ofString aSrc)
            let opts =
                { baseOpts with
                    ProjectFileName = @"C:\virt\spike.fsproj"
                    SourceFiles = [| fileA; fileB; fileStratified |] }
            optsRef <- opts

            // Outer whole-project check - forces Stratified.fs's source through the reentrant callback.
            let! projResults = checker.ParseAndCheckProject(opts)
            printfn "  ParseAndCheckProject returned; reentrant callback fired: %b" reentrantEntered

            printfn ""
            printfn "=== Generated Stratified.fs (PersonJson) text (never written to disk) ==="
            printfn "%s" generatedText

            let projErrors =
                projResults.Diagnostics
                |> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)
            let stratErrors =
                projErrors |> Array.filter (fun d -> d.FileName = fileStratified)
            printfn "=== Diagnostics ==="
            printfn "  project-wide errors: %d" projErrors.Length
            for d in projErrors do printfn "    %s (%s @ %A)" d.Message d.FileName d.Range
            printfn "  errors on Stratified.fs: %d" stratErrors.Length

            // Re-check Stratified.fs on its own to obtain FSharpCheckFileResults for symbol queries.
            let stratText = files.[fileStratified]
            let! _, ansStrat = checker.ParseAndCheckFileInProject(fileStratified, 0, SourceText.ofString stratText, opts)
            let checkedStrat =
                match ansStrat with
                | FSharpCheckFileAnswer.Succeeded r -> r
                | FSharpCheckFileAnswer.Aborted -> failwith "Stratified check aborted"

            printfn ""
            printfn "=== Typed-resolution proof: does PersonJson's reference to PersonLenses.Name"
            printfn "    resolve to a REAL symbol declared in B.fs, or is it a textual coincidence? ==="

            // (1) GetAllUsesOfAllSymbolsInFile: find the use of the lens binding 'Name' inside Stratified.fs
            //     whose resolved symbol was DECLARED in B.fs.
            let allUses = checkedStrat.GetAllUsesOfAllSymbolsInFile() |> Array.ofSeq
            let lensRefUses =
                allUses
                |> Array.choose (fun u ->
                    match u.Symbol with
                    | :? FSharpMemberOrFunctionOrValue as m
                        when m.DeclaringEntity.IsSome && m.DeclaringEntity.Value.DisplayName = "PersonLenses" ->
                        Some(m, u)
                    | _ -> None)
            printfn ""
            printfn "  Uses in Stratified.fs that resolved to a PersonLenses member: %d" lensRefUses.Length
            let mutable allFromB = lensRefUses.Length > 0
            for (m, u) in lensRefUses do
                let declFile = m.DeclarationLocation.FileName
                let fromB = declFile = fileB
                if not fromB then allFromB <- false
                printfn "    ref '%s' at %A -> symbol declared at %s (in B.fs: %b)"
                    m.DisplayName u.Range declFile fromB

            // (2) GetSymbolUseAtLocation on the exact 'PersonLenses.Name' reference text.
            printfn ""
            printfn "  GetSymbolUseAtLocation on the generated 'Domain.PersonLenses.Name' reference:"
            let lines = stratText.Replace("\r\n", "\n").Split('\n')
            let mutable located = false
            for i in 0 .. lines.Length - 1 do
                let line = lines.[i]
                let marker = "Domain.PersonLenses.Name"
                let idx = line.IndexOf(marker)
                if idx >= 0 && not located then
                    located <- true
                    let endCol = idx + marker.Length            // 0-based end column
                    let lineNo = i + 1                           // 1-based line
                    let symUse =
                        checkedStrat.GetSymbolUseAtLocation(lineNo, endCol, line, [ "Domain"; "PersonLenses"; "Name" ])
                    match symUse with
                    | Some su ->
                        let decl = su.Symbol.DeclarationLocation
                        printfn "    resolved to symbol '%s', declared at file=%s range=%A"
                            su.Symbol.DisplayName (decl |> Option.map (fun r -> r.FileName) |> Option.defaultValue "<none>") decl
                        match decl with
                        | Some r when r.FileName = fileB ->
                            printfn "    CONFIRMED: 'PersonLenses.Name' round-trips to B.fs's actual binding."
                        | _ -> printfn "    NOT confirmed as declared in B.fs."
                    | None -> printfn "    GetSymbolUseAtLocation returned None at (%d,%d)." lineNo endCol

            let capabilityPass = stratErrors.Length = 0 && allFromB && reentrantEntered
            printfn ""
            printfn "=== Result ==="
            printfn "  reentrant callback fired            : %b" reentrantEntered
            printfn "  zero errors on generated Stratified : %b" (stratErrors.Length = 0)
            printfn "  every PersonLenses ref resolved to a typed symbol declared in B.fs : %b" allFromB
            if capabilityPass then
                printfn "  CAPABILITY PASS: a third generator reused a second generator's already-generated,"
                printfn "  still-virtual output by TYPED resolution, within one in-flight compilation."
            return (if capabilityPass then 0 else 1)
        }
    Async.RunSynchronously(run, timeout = 60_000)
