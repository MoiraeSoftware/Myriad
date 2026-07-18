namespace ClientTP.Runtime

type ClientRuntimeHelper() =
    static member Help() = "client-help"

#if !IS_DESIGNTIME
[<assembly:CompilerServices.TypeProviderAssembly("ClientTP.DesignTime.dll")>]
do ()
#endif
