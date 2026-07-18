namespace SchemaTP.Runtime

open System

/// Stamped per-field by SchemaTP on each provided property. Q009's mechanism, unchanged.
[<AttributeUsage(AttributeTargets.All, AllowMultiple = false)>]
type FieldProvenanceAttribute(fieldName: string, version: string) =
    inherit Attribute()
    member _.FieldName = fieldName
    member _.Version = version
