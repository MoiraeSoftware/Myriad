namespace Domain

open Myriad.Plugins

[<Generator.LensesAttribute("person")>]
type Person = { Name: string; Age: int }
