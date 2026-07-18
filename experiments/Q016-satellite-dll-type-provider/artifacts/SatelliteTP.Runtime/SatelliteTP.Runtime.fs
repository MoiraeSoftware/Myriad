namespace SatelliteTP.Runtime

type SatelliteRuntimeHelper() =
    static member Help() = "satellite-help"

#if !IS_DESIGNTIME
[<assembly:CompilerServices.TypeProviderAssembly("SatelliteTP.DesignTime.dll")>]
do ()
#endif
