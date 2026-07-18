namespace ProbeSimple.Runtime

// Runtime assembly carries the redirect to the design-time provider DLL that FCS loads.
[<assembly: CompilerServices.TypeProviderAssembly("ProbeSimple.DesignTime.dll")>]
do ()
