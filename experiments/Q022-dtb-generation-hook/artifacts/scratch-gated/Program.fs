module Program

[<EntryPoint>]
let main _ =
    let person: Input.Person = { Name = "test" }
    printfn "%s" (Consumer.nameGetter person)
    0
