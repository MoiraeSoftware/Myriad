namespace Domain

open Myriad.Plugins

// Review variant B: a field whose type is itself another record, not a primitive. Tests whether the
// reentrant typed inspection surfaces the REAL nested type (Address) - and, separately, whether
// ReentrantJsonGenerator's toy string-vs-%d type sniffing copes with a non-primitive field.
type Address = { City: string; Zip: int }

[<Generator.LensesAttribute("person")>]
type Person = { Name: string; Age: int; Home: Address }
