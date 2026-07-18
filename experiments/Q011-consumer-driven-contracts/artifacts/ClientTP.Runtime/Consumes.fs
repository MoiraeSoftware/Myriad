namespace ClientTP.Runtime

open System

/// The REVERSED arrow: a client stamps this on its OWN emitted members to record
/// exactly which schema field (and the version it saw) it consumed at generation time.
[<AttributeUsage(AttributeTargets.All, AllowMultiple = true)>]
type ConsumesFieldAttribute(fieldName: string, version: string) =
    inherit Attribute()
    member _.FieldName = fieldName
    member _.Version = version
