module RuntimeConsumer.Program

// Round 3: independent of the provider's own design-time machinery. This record has no relationship
// to MyriadPreview beyond having fields of matching names -- tests that the provider's erased members,
// once a real compiled instance exists, read the correct values via runtime dynamic reflection,
// decoupled from Round 1's "type never compiled anywhere" precondition (which must NOT leak in here).
type Person = { name: string; age: int }

// Type-provider static arguments must be simple literals in this parser position -- a
// __SOURCE_DIRECTORY__ + "..." concatenation (works at ordinary call sites) is rejected here
// (FS0010/FS1241), found by running. An absolute literal path is used instead.
type P = MyriadPreview.Provided.Fields<"C:/Users/Dave/Documents/GitHub/Myriad/experiments/Q019-erased-self-parsing-provider/artifacts/SampleLib/Person.fs", "SampleNs.Person">

[<EntryPoint>]
let main _argv =
    let person = { name = "Ada"; age = 42 }
    let p = P(box person)
    let nameViaProvider = p.name
    let ageViaProvider = p.age
    let nameDirect = person.name
    let ageDirect = person.age
    printfn "via provider (dynamic reflection): name=%A age=%A" nameViaProvider ageViaProvider
    printfn "direct field access (outside provider): name=%A age=%A" nameDirect ageDirect
    let nameOk = (nameViaProvider :?> string) = nameDirect
    let ageOk = (ageViaProvider :?> int) = ageDirect
    printfn "agreement: name=%b age=%b" nameOk ageOk
    if nameOk && ageOk then 0 else 1
