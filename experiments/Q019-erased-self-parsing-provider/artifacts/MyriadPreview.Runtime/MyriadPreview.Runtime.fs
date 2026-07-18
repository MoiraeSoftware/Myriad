namespace MyriadPreview.Runtime

// Erased provider: this module exists only to carry the TypeProviderAssembly attribute. No runtime
// helper types are needed here (unlike Q016's satellite forwarding case) because every provided
// member's invokeCode calls straight into System.Reflection / Microsoft.FSharp.Reflection, both
// already part of FSharp.Core / the BCL.

#if !IS_DESIGNTIME
[<assembly:CompilerServices.TypeProviderAssembly("MyriadPreview.DesignTime.dll")>]
do ()
#endif
