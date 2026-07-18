namespace SampleNs

open Myriad.Plugins

[<Generator.Fields "fields">]
type Person = { name: string; age: int }
