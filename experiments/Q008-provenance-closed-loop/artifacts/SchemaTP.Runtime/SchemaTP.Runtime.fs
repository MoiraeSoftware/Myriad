namespace SchemaTP.Runtime

open System

/// A REAL custom attribute (not one of the SDK's built-in helpers) carrying a
/// schema version string. Lives in the runtime assembly so it is independently
/// reflectable from any consumer that references only this DLL.
[<AttributeUsage(AttributeTargets.All, AllowMultiple = false)>]
type SchemaVersionAttribute(version: string) =
    inherit Attribute()
    member _.Version = version

type SchemaRuntimeHelper() =
    static member Help() = "schema-help"

#if !IS_DESIGNTIME
[<assembly:CompilerServices.TypeProviderAssembly("SchemaTP.DesignTime.dll")>]
do ()
#endif
