module AttrCheck.Program

open System
open System.IO
open System.Reflection

// Round 1 falsifier: from a program that never touched ProvidedTypes.fs / the design-time
// assembly, Assembly.LoadFrom the independently-built SchemaAsm.dll and read a MEMBER-LEVEL
// FieldProvenanceAttribute back via plain System.Reflection.PropertyInfo.GetCustomAttributesData().
// Mirrors Q008's Round 1 exactly, but at PROPERTY granularity instead of TYPE granularity.

[<EntryPoint>]
let main argv =
    let schemaAsmPath =
        if argv.Length > 0 then argv.[0]
        else
            let baseDir = AppContext.BaseDirectory
            let mutable d = DirectoryInfo(baseDir)
            while d <> null && d.Name <> "tp-field-provenance-spike" do d <- d.Parent
            Path.Combine(d.FullName, "SchemaAsm", "bin", "Release", "net8.0", "SchemaAsm.dll")

    printfn "Loading (plain reflection, no SDK): %s" schemaAsmPath
    let asm = Assembly.LoadFrom schemaAsmPath

    // Target the single-property Round 1 type.
    let taggedName = "SchemaAsm.Schemas+Tagged"
    let ty =
        match asm.GetType(taggedName) with
        | null -> failwithf "type '%s' not found" taggedName
        | t -> t

    printfn "type '%s'" ty.FullName
    let mutable found = 0
    let mutable ok = false
    for p in ty.GetProperties() do
        for cad in p.GetCustomAttributesData() do
            if cad.AttributeType.Name = "FieldProvenanceAttribute" then
                found <- found + 1
                let fieldArg =
                    if cad.ConstructorArguments.Count > 0 then string cad.ConstructorArguments.[0].Value else "<none>"
                let verArg =
                    if cad.ConstructorArguments.Count > 1 then string cad.ConstructorArguments.[1].Value else "<none>"
                printfn "  property '%s'" p.Name
                printfn "    attribute: %s" cad.AttributeType.FullName
                printfn "    attribute assembly: %s" (cad.AttributeType.Assembly.GetName().Name)
                printfn "    ctor arg [0] (FieldName) = %A" fieldArg
                printfn "    ctor arg [1] (Version)   = %A" verArg
                if fieldArg = "Name" && verArg = "v1" then ok <- true

    printfn ""
    printfn "FieldProvenanceAttribute instances found (member-level): %d" found
    printfn "ROUND 1 (independent member-level reflection) verdict: %s"
        (if found > 0 && ok then "PASS" else "FAIL")
    if found > 0 && ok then 0 else 1
