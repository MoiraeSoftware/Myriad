namespace DummyAsm

/// A tiny assembly whose only purpose is to be read via File.ReadAllBytes + Assembly.Load
/// during ProbeTwoParamsIO's instantiation, structurally mirroring SchemaProvider's
/// readClientConsumes I/O + reflection shape (without any enforcement logic).
type Marker() =
    member _.Hello = "hello"
