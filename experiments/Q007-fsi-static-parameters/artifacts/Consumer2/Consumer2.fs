module Consumer2

// Round 2 -- richer value drives generated-member shape. Expr evaluates (via FSI, same
// nested-hosting mechanism as Round 1) to a `string list`; the provider generates one property
// per list element, named after the element's own content.
type T = FsiParamTP.Provided.Container<"[\"Name\"; \"Age\"; \"Email\"]">

let a = T.Name
let b = T.Age
let c = T.Email
let evaluatedTypeName = T.EvaluatedTypeName
