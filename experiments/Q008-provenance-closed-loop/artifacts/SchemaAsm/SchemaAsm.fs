module SchemaAsm.Schemas

// Using the generative SchemaTP provider bakes a real, stamped type into THIS assembly's IL.
// SchemaAsm references only SchemaTP.Runtime.dll (the runtime/TPRTC), never the design-time DLL
// or ProvidedTypes.fs directly.
type SchemaV2 = SchemaTP.Provided.Schema<"v2">

// Surface a couple of members so the type is genuinely used and retained.
let declaredVersion : string = SchemaV2.DeclaredVersion
let fieldCount : int = SchemaV2.FieldCount
