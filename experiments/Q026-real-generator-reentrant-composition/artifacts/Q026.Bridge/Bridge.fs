/// Q026's disclosed side-channel: IMyriadGenerator.Generate(context: GeneratorContext) has no
/// parameter carrying a live FSharpChecker/FSharpProjectOptions handle (confirmed from
/// src/Myriad.Core/Types.fs). A real generator that wants to reentrant-query another file's
/// already-typed content needs one anyway, so the harness publishes it here before invoking the
/// generator. This is a real, named gap in IMyriadGenerator's current shape, not hidden plumbing.
module Q026Bridge

open System.Collections.Concurrent
open FSharp.Compiler.CodeAnalysis

let mutable Checker : FSharpChecker option = None
let mutable Opts : FSharpProjectOptions option = None
let mutable PersonFilePath = ""
let mutable PersonLensesFilePath = ""

/// Real content of each virtual file, populated by the harness's DocumentSource.Custom callback
/// as it computes each file's text - mirrors Q010 Round 2's own `files` cache exactly.
let Files = ConcurrentDictionary<string, string>()
