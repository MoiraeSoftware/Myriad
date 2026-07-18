module Verify.Program

open System
open System.Reflection

// Independent reflection-only verifier -- no ProvidedTypes.fs, no FSharp.Compiler.Service,
// no FsiEvaluationSession anywhere in this program. Mirrors Q008's AttrCheck: loads an
// already-built Consumer.dll/Consumer2.dll by plain System.Reflection and confirms the provider
// really did generate the members the FSI evaluation should have produced -- not just that
// "something" got generated.

let getStaticProp (t: Type) (name: string) =
    t.GetProperty(name, BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static)

let round1 (consumerDll: string) =
    printfn "===== ROUND 1 VERIFY: %s =====" consumerDll
    let asm = Assembly.LoadFrom consumerDll
    // The generative provider's type alias ("T" in Consumer.fs) is nested under the enclosing
    // F# module, which itself compiles to a static class named after the module -- so the real
    // IL type is "Consumer+T", not "Consumer". Confirmed by dumping Consumer.dll's types directly
    // (see 02-results.md's corrections section) before writing this lookup.
    let t = asm.GetType("Consumer+T")
    if isNull (box t) then
        printfn "  FAIL: type 'Consumer+T' not found"
        false
    else
        let names = [ "P0"; "P1"; "P2" ]
        let mutable allOk = true
        for i, name in List.indexed names do
            match getStaticProp t name with
            | null ->
                printfn "  FAIL: property %s not found" name
                allOk <- false
            | p ->
                let v = p.GetValue(null) :?> int
                let ok = v = i
                printfn "  %s = %d (expect %d) : %s" name v i (if ok then "OK" else "MISMATCH")
                if not ok then allOk <- false
        // negative control: P3 must NOT exist (N was 3, not 4)
        match getStaticProp t "P3" with
        | null -> printfn "  P3 absent, as expected (negative control OK)"
        | _ ->
            printfn "  FAIL: P3 unexpectedly exists"
            allOk <- false
        let allProps = t.GetProperties(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static)
                       |> Array.map (fun p -> p.Name)
                       |> Array.filter (fun n -> n.StartsWith "P" && n.Length = 2)
        printfn "  exact P* property count: %d (expect 3)" allProps.Length
        if allProps.Length <> 3 then allOk <- false
        match getStaticProp t "EvaluatedTypeName" with
        | null -> printfn "  (EvaluatedTypeName property not found)"
        | p -> printfn "  EvaluatedTypeName = %s" (p.GetValue(null) :?> string)
        printfn "ROUND 1 VERIFY verdict: %s" (if allOk then "PASS" else "FAIL")
        allOk

let round2 (consumerDll: string) =
    printfn "===== ROUND 2 VERIFY: %s =====" consumerDll
    let asm = Assembly.LoadFrom consumerDll
    let t = asm.GetType("Consumer2+T")
    if isNull (box t) then
        printfn "  FAIL: type 'Consumer2+T' not found"
        false
    else
        let expected = [ "Name"; "Age"; "Email" ]
        let mutable allOk = true
        for name in expected do
            match getStaticProp t name with
            | null ->
                printfn "  FAIL: property %s not found" name
                allOk <- false
            | p ->
                let v = p.GetValue(null) :?> string
                let ok = v = name
                printfn "  %s = %s (expect %s) : %s" name v name (if ok then "OK" else "MISMATCH")
                if not ok then allOk <- false
        // negative control: a name not in the list must not exist
        match getStaticProp t "Nickname" with
        | null -> printfn "  Nickname absent, as expected (negative control OK)"
        | _ ->
            printfn "  FAIL: Nickname unexpectedly exists"
            allOk <- false
        match getStaticProp t "EvaluatedTypeName" with
        | null -> printfn "  (EvaluatedTypeName property not found)"
        | p -> printfn "  EvaluatedTypeName = %s" (p.GetValue(null) :?> string)
        printfn "ROUND 2 VERIFY verdict: %s" (if allOk then "PASS" else "FAIL")
        allOk

[<EntryPoint>]
let main argv =
    match argv with
    | [| "round1"; dll |] -> if round1 dll then 0 else 1
    | [| "round2"; dll |] -> if round2 dll then 0 else 1
    | [| "both"; dll1; dll2 |] ->
        let r1 = round1 dll1
        let r2 = round2 dll2
        printfn "\nSummary: Round1=%b Round2=%b" r1 r2
        if r1 && r2 then 0 else 1
    | _ ->
        eprintfn "usage: Verify (round1 <Consumer.dll> | round2 <Consumer2.dll> | both <Consumer.dll> <Consumer2.dll>)"
        2
