# Q003-fsi-comptime-eval / Movement 3 — Execute + write up (falsifier only)

**Status:** DONE, 2026-07-14. All six stages passed on the first run — no bug this time, unlike
Q002's Round B.

```
Stage 1: FSharpChecker alone, before FSI exists              -> OK, A.fs diagnostics: [||]
Stage 2: create FsiEvaluationSession alongside FSharpChecker  -> OK, session created
Stage 3: evaluate a trivial expression via FSI                -> OK, Result: 3 : System.Int32
Stage 4: re-check FSharpChecker AFTER FSI has run              -> OK, A.fs diagnostics: [||]
Stage 5: FSI evaluates a string, host casts to System.String  -> OK, "Name:string;Age:int"
Stage 6: FSI evaluates (string*string) list, host casts AS THAT TYPE -> OK, direct cast succeeded
```

Stage 6 output in full, since it's the load-bearing one:

```
ReflectionType: Microsoft.FSharp.Collections.FSharpList`1[[System.Tuple`2[[System.String,...],[System.String,...]], ...]]
ReflectionType.Assembly: FSharp.Core, Version=9.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a
Host's own FSharp.Core identity: FSharp.Core, Version=9.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a
Direct cast to host's (string*string) list SUCCEEDED: [("Name", "string"); ("Age", "int")]
```

The concern going in was real: FSI dynamically compiles and evaluates code, and if that code ran
against a *different* loaded copy of `FSharp.Core` than the host process, the direct cast would
throw `InvalidCastException` even though the runtime shapes matched — a known class of failure in
script-hosting scenarios generally. It didn't happen here. `FsiEvaluationSession`, created with no
special isolation configuration, resolved to the exact same `FSharp.Core, Version=9.0.0.0,
...b03f5f7f11d50a3a` the host process itself was already running — same assembly identity, so the
cast is a normal, safe downcast, not a cross-boundary marshal.

**This is a cleaner result than expected going in.** The falsifier was written with a fallback
path (reflection-based structural read) in case the direct cast failed; it wasn't needed.

## What this does and doesn't establish

Established: the mechanism coexists cleanly, and F#-typed data (not just BCL primitives) crosses
the FSI-to-host boundary as a directly usable value, in the simplest possible hosting
configuration (no explicit `AssemblyLoadContext` isolation, default settings on both sides).

Not established: whether this holds under a more production-realistic setup. Myriad's real
plugin loader (`McMaster.NETCore.Plugins`, per Q001's earlier research context) loads plugins into
separate `AssemblyLoadContext`s specifically for isolation — and ALC-isolated code commonly *does*
end up with its own separate loaded copy of `FSharp.Core`, which is exactly the condition under
which this cast would have failed. This falsifier deliberately tested the simplest configuration
first, per the "cheapest falsifier" discipline; it does not clear the ALC-isolated case, which is
the one that would actually matter for a real plugin host.
