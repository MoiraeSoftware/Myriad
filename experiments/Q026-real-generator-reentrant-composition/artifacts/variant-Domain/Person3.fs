namespace Domain

open Myriad.Plugins

// Review variant A: three primitive fields, mixed string/int, different order than the original
// two-field Person. Tests whether ReentrantJsonGenerator's binding-discovery and the composition
// mechanism are robust to a field count and ordering the executor never exercised.
[<Generator.LensesAttribute("person")>]
type Person = { Name: string; Age: int; Email: string }
