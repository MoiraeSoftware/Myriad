module Consumer

// Round 1 -- cheapest falsifier. Expr = "1+2" evaluated by FSI (inside the provider's own
// DefineStaticParameters call, itself inside THIS dotnet build's fsc.exe process) to 3, which
// should drive generation of exactly 3 int properties P0, P1, P2 with values 0, 1, 2.
type T = FsiParamTP.Provided.Container<"1+2">

let p0 = T.P0
let p1 = T.P1
let p2 = T.P2
let evaluatedTypeName = T.EvaluatedTypeName
