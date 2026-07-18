This quartet reuses `Q023-scale-cost-reentrant-callback/artifacts/q023-spike/` verbatim — same
built `.exe`, same `Program.fs` (already supports `<N> <editIdx>` argv as of Q023's own addendum),
no source changes, no rebuild. Only new invocations with different `(N, editIdx)` pairs.

`logs/` in this folder holds all 20 raw run outputs (`N{n}-idx{i}.txt`), one per (N, editIdx)
combination in the sweep grid documented in `../01-design.md`.
