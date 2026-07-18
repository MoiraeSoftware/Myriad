module Consumer1
type P = MyriadPreview.Provided.Fields<"C:/Users/Dave/Documents/GitHub/Myriad/experiments/Q019-erased-self-parsing-provider/artifacts/SampleLib/Person.fs", "SampleNs.Person">
let describe (instance: obj) : string =
    let p = P(instance)
    sprintf "%A / %A" (p.name) (p.age)
