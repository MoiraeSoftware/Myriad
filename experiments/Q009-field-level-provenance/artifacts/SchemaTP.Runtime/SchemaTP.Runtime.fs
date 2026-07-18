namespace SchemaTP.Runtime

open System

/// A REAL custom attribute (not one of the SDK's built-in helpers) carrying
/// FIELD-LEVEL provenance: which field, and what version. Lives in the runtime
/// assembly so it is independently reflectable from any consumer that references
/// only this DLL. Stamped on individual provided PROPERTIES (member-level), not
/// on the provided type as a whole.
[<AttributeUsage(AttributeTargets.All, AllowMultiple = false)>]
type FieldProvenanceAttribute(fieldName: string, version: string) =
    inherit Attribute()
    member _.FieldName = fieldName
    member _.Version = version

type SchemaRuntimeHelper() =
    static member Help() = "schema-help"

#if !IS_DESIGNTIME
[<assembly:CompilerServices.TypeProviderAssembly("SchemaTP.DesignTime.dll")>]
do ()
#endif
