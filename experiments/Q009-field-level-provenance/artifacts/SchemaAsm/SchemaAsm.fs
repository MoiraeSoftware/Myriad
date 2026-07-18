module SchemaAsm.Schemas

// Using the generative SchemaTP provider bakes real, per-field-stamped types into THIS assembly's
// IL. SchemaAsm references only SchemaTP.Runtime.dll, never the design-time DLL or ProvidedTypes.fs.
//
// Several variants are baked so the harness can express a "schema change" as repointing the client
// at a different, already-compiled variant WITHOUT rebuilding this assembly (a compiled schema's
// static argument cannot be edited live; only the consumer's ClientTP arguments can). This is the
// feasible realisation of the design's Round 3 "bump the schema's static argument" step.

// --- Round 1 falsifier: ONE provided type with ONE stamped property ---
type Tagged = SchemaTP.Provided.Schema<"Name:v1">

// --- Round 2 small-schema variants (base: Name:v1;Age:v2;Email:v3) ---
type SchemaBaseline    = SchemaTP.Provided.Schema<"Name:v1;Age:v2;Email:v3">
type SchemaEmailBumped = SchemaTP.Provided.Schema<"Name:v1;Age:v2;Email:v4">
type SchemaAgeBumped   = SchemaTP.Provided.Schema<"Name:v1;Age:v3;Email:v3">
type SchemaNameAgeBumped = SchemaTP.Provided.Schema<"Name:v2;Age:v3;Email:v3">

// --- Round 3 wide-schema variants (12 fields F0..F11; client depends on F0,F3,F7,F9) ---
type WideBaseline =
    SchemaTP.Provided.Schema<"F0:v1;F1:v1;F2:v1;F3:v1;F4:v1;F5:v1;F6:v1;F7:v1;F8:v1;F9:v1;F10:v1;F11:v1">
// F5 is NOT among the client's dependencies -> irrelevant change
type WideIrrelevantBump =
    SchemaTP.Provided.Schema<"F0:v1;F1:v1;F2:v1;F3:v1;F4:v1;F5:v2;F6:v1;F7:v1;F8:v1;F9:v1;F10:v1;F11:v1">
// F3 IS among the client's dependencies -> relevant change
type WideRelevantBump =
    SchemaTP.Provided.Schema<"F0:v1;F1:v1;F2:v1;F3:v2;F4:v1;F5:v1;F6:v1;F7:v1;F8:v1;F9:v1;F10:v1;F11:v1">

// Reference a member of each so the generative types are genuinely used and retained in IL.
let _t   : string = Tagged.Name
let _b   : string = SchemaBaseline.Name
let _eb  : string = SchemaEmailBumped.Name
let _ab  : string = SchemaAgeBumped.Name
let _nab : string = SchemaNameAgeBumped.Name
let _wb  : string = WideBaseline.F0
let _wib : string = WideIrrelevantBump.F0
let _wrb : string = WideRelevantBump.F0
