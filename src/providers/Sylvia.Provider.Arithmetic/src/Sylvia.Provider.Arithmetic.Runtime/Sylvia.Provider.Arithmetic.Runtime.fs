namespace Sylvia

// Put the TypeProviderAssemblyAttribute in the runtime DLL, pointing to the design-time DLL
[<assembly:CompilerServices.TypeProviderAssembly("Sylvia.Provider.Arithmetic.DesignTime.dll")>]
do ()
