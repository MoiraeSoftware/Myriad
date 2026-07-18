namespace FsiParamTP.Runtime

/// Marker/helper type so the runtime assembly has at least one real member. The generative
/// provider's members are all defined at design time and remapped onto this assembly via
/// assemblyReplacementMap; the runtime assembly itself carries no Fsi-specific logic.
type FsiParamRuntimeHelper() =
    static member Help() = "fsi-param-help"

#if !IS_DESIGNTIME
[<assembly: Microsoft.FSharp.Core.CompilerServices.TypeProviderAssembly("FsiParamTP.DesignTime.dll")>]
do ()
#endif
