module AttrCheck.Program

open System
open System.IO
open System.Reflection

// Round 1 falsifier: from a program that never touched ProvidedTypes.fs / the design-time
// assembly, Assembly.LoadFrom the independently-built SchemaAsm.dll and read the emitted
// SchemaVersionAttribute back via PLAIN System.Reflection.CustomAttributeData.

[<EntryPoint>]
let main argv =
    let schemaAsmPath =
        if argv.Length > 0 then argv.[0]
        else
            // default: locate SchemaAsm.dll relative to this exe's build output
            let baseDir = AppContext.BaseDirectory
            let mutable d = DirectoryInfo(baseDir)
            while d <> null && d.Name <> "tp-provenance-spike" do d <- d.Parent
            Path.Combine(d.FullName, "SchemaAsm", "bin", "Release", "net8.0", "SchemaAsm.dll")

    printfn "Loading (plain reflection, no SDK): %s" schemaAsmPath
    let asm = Assembly.LoadFrom schemaAsmPath

    let mutable found = 0
    let mutable ok = false
    for t in asm.GetTypes() do
        let cads = t.GetCustomAttributesData()
        for cad in cads do
            if cad.AttributeType.Name = "SchemaVersionAttribute" then
                found <- found + 1
                let argVal =
                    if cad.ConstructorArguments.Count > 0 then string cad.ConstructorArguments.[0].Value
                    else "<none>"
                printfn "  type '%s'" t.FullName
                printfn "    attribute: %s" cad.AttributeType.FullName
                printfn "    attribute assembly: %s" (cad.AttributeType.Assembly.GetName().Name)
                printfn "    ctor arg [0] (Version) = %A" argVal
                if argVal = "v2" then ok <- true

    printfn ""
    printfn "SchemaVersionAttribute instances found: %d" found
    printfn "ROUND 1 (independent reflection) verdict: %s"
        (if found > 0 && ok then "PASS" else "FAIL")
    if found > 0 && ok then 0 else 1
