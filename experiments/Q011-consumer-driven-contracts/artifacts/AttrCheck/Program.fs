// Round 1 falsifier + independent client-attribute dumper.
// Zero SDK involvement: references only ClientTP.Runtime.dll, reads IL via plain reflection.
module AttrCheck.Program

open System
open System.Reflection

let flags = BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static ||| BindingFlags.Instance

// Every (declaringType, memberName, fieldName, version) recorded as ConsumesFieldAttribute in an assembly.
let readConsumes (path: string) =
    let a = Assembly.LoadFrom(path)
    [ for t in a.GetTypes() do
        for p in t.GetProperties(flags) do
            for cad in p.GetCustomAttributesData() do
                if cad.AttributeType.FullName = "ClientTP.Runtime.ConsumesFieldAttribute" then
                    let fn  = cad.ConstructorArguments.[0].Value :?> string
                    let ver = cad.ConstructorArguments.[1].Value :?> string
                    yield (t.FullName, p.Name, cad.AttributeType.Assembly.GetName().Name, fn, ver) ]

[<EntryPoint>]
let main argv =
    match argv with
    | [| "dump"; path |] ->
        printfn "Dumping ConsumesFieldAttribute stamps in: %s" path
        let rows = readConsumes path
        for (ty, mem, attrAsm, fn, ver) in rows do
            printfn "  %s.%s -> ConsumesField(fieldName=%A, version=%A) [attr assembly: %s]" ty mem fn ver attrAsm
        printfn "TOTAL ConsumesFieldAttribute stamps: %d" rows.Length
        0
    | [| "round1"; path |] ->
        printfn "Loading (plain reflection, no SDK): %s" path
        let rows = readConsumes path
        for (ty, mem, attrAsm, fn, ver) in rows do
            printfn "type '%s'" ty
            printfn "  property '%s'" mem
            printfn "    attribute: ClientTP.Runtime.ConsumesFieldAttribute"
            printfn "    attribute assembly: %s" attrAsm
            printfn "    ctor arg [0] (FieldName) = %A" fn
            printfn "    ctor arg [1] (Version)   = %A" ver
        printfn ""
        printfn "ConsumesFieldAttribute instances found (client-emitted, member-level): %d" rows.Length
        let ok =
            rows |> List.exists (fun (_, mem, attrAsm, fn, ver) ->
                mem = "Name" && fn = "Name" && ver = "v1" && attrAsm = "ClientTP.Runtime")
        printfn "ROUND 1 (independent client-emitted member reflection) verdict: %s" (if ok then "PASS" else "FAIL")
        if ok then 0 else 1
    | _ ->
        eprintfn "usage: AttrCheck (round1 <dll> | dump <dll>)"
        2
