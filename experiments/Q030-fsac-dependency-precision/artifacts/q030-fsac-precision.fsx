// Q030 - tests whether FSAC/FCS invalidation after an edit is DEPENDENCY-PRECISE (re-analyzes only a
// file's true transitive dependents) or COMPILATION-ORDER-CONSERVATIVE (re-analyzes the whole
// compilation-order suffix regardless of whether later files actually reference the edited file).
//
// Q023/Q024/Q029 all measured a LINEAR dependency chain (file i references file i-1), which makes
// "compilation-order successors" and "true dependents" identical by construction, so none of them
// could distinguish the two regimes. This harness builds a WIDE/SHALLOW shape instead: one early Hub
// file, a few scattered later files that reference Hub, and many unrelated later files that do NOT.
// Editing Hub (value-only) then makes the two hypotheses predict very different sets of re-analyzed
// files: dependency-precise => {Hub + true dependents}; conservative => {Hub + whole suffix}.
//
// Measurement mechanism (LSP plumbing, timestamped receive queue, documentAnalyzed cascade capture) is
// reused VERBATIM from Q029's q029-fsac-cost.fsx, which its own adversarial review confirmed is a
// sound, position-gated control. Only the project TOPOLOGY and the per-edit SIGNAL (collect the full
// set of re-analyzed files, not wait for one specific last file) are changed.
//
// Usage:  dotnet fsi q030-fsac-precision.fsx <projectDir> <N> <hubIndex> <dependentsCsv> <editPositionsCsv> <reps>
//   e.g.  dotnet fsi q030-fsac-precision.fsx C:\tmp\q030a 30 2 7,16,25 2 3
//         (N=30 files, Hub=File0002, dependents File0007/0016/0025, edit Hub 3x)

open System
open System.Diagnostics
open System.IO
open System.Text
open System.Text.Json

let projectDir = Path.GetFullPath(fsi.CommandLineArgs.[1])
let N = int fsi.CommandLineArgs.[2]
let hubIndex = int fsi.CommandLineArgs.[3]
let dependents = fsi.CommandLineArgs.[4].Split(',') |> Array.map int |> Set.ofArray
let editPositions = fsi.CommandLineArgs.[5].Split(',') |> Array.map int
let reps = int fsi.CommandLineArgs.[6]
let fsacToolDir = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "fsac-tool"))

let logPath = Path.Combine(__SOURCE_DIRECTORY__, sprintf "q030-N%d.log" N)
let csvPath = Path.Combine(__SOURCE_DIRECTORY__, sprintf "q030-events-N%d.csv" N)
let summaryPath = Path.Combine(__SOURCE_DIRECTORY__, sprintf "q030-summary-N%d.csv" N)
let log = new StreamWriter(logPath, false, AutoFlush = true)
// Raw LSP transcript ALWAYS written for this quartet - the exact set of documentAnalyzed events IS the
// evidence, so it must be independently inspectable, not just summarized.
let rawLog = new StreamWriter(Path.Combine(__SOURCE_DIRECTORY__, sprintf "q030-N%d.raw.log" N), false, AutoFlush = true)
let logLine (s: string) =
    let line = sprintf "[%s] %s" (DateTime.Now.ToString("HH:mm:ss.fff")) s
    log.WriteLine(line)
    printfn "%s" line

// ---------------------------------------------------------------------------
// 1. Generate a real, genuinely-weighted, WIDE/SHALLOW project.
// ---------------------------------------------------------------------------

// Genuine typecheck weight per file (generic record, generic wrapper, Map/List pipeline, explicit
// recursion, a 20-record comprehension) - the exact shape from Q023's/Q029's mkFileSrc, so this is not
// invalidated by BACKLOG item 4's "let x = 5 padding" mistake. `marker` is the value-only edit knob (a
// string literal that changes file CONTENT without changing any exported type/signature).
//
// The `total` line's tail term is what makes topology:
//   - Hub file (i = hubIndex): `+ 0`  -- Hub references nobody; it is the edited root.
//   - dependent file:          `+ File{hubIndex}.total`  -- genuinely references Hub's exported value.
//   - unrelated file:          `+ 0`  -- references nobody. NOT a dependent of Hub.
// So editing Hub's marker (value-only) leaves Hub's exported `total` type/signature byte-identical, and
// only the three dependent files have any source reference to Hub at all.
let mkFileSrc (i: int) (marker: string) =
    let tail =
        if i <> hubIndex && dependents.Contains i then sprintf "File%04d.total" hubIndex
        else "0"
    sprintf
        "module File%04d\n\
         type Record%04d = { Id: int; Name: string; Tags: string list; Meta: Map<string,int> }\n\
         type Wrapper%04d<'a> = { Value: 'a; Items: 'a list; Count: int }\n\
         let make (id: int) (name: string) (tags: string list) : Record%04d =\n\
         \x20   { Id = id; Name = name; Tags = tags; Meta = tags |> List.mapi (fun i t -> t, i) |> Map.ofList }\n\
         let rec fold (f: 'acc -> 'a -> 'acc) (acc: 'acc) (xs: 'a list) : 'acc =\n\
         \x20   match xs with\n\
         \x20   | [] -> acc\n\
         \x20   | x :: rest -> fold f (f acc x) rest\n\
         let wrap (v: 'a) : Wrapper%04d<'a> = { Value = v; Items = [v]; Count = 1 }\n\
         let records = [ for k in 1..20 -> make k (sprintf \"item%%d\" k) [ \"a\"; \"b\"; string k; \"%s\" ] ]\n\
         let total = (records |> fold (fun acc r -> acc + r.Meta.Count) 0) + %s\n"
        i i i i i marker tail

let fileName i = sprintf "File%04d.fs" i
let filePath i = Path.Combine(projectDir, fileName i)

// classification of a file for reporting
let classify i =
    if i = hubIndex then "hub"
    elif dependents.Contains i then "dependent"
    else "unrelated"

let writeProject () =
    if Directory.Exists projectDir then Directory.Delete(projectDir, true)
    Directory.CreateDirectory projectDir |> ignore
    // Sanity: every dependent must sit AFTER the hub in compilation order (else it can't reference it).
    for d in dependents do
        if d <= hubIndex then failwithf "dependent %d is not after hubIndex %d" d hubIndex
    for i in 0 .. N - 1 do
        File.WriteAllText(filePath i, mkFileSrc i "v0")
    let compiles =
        [ for i in 0 .. N - 1 -> sprintf "    <Compile Include=\"%s\" />" (fileName i) ]
        |> String.concat "\n"
    let fsproj =
        sprintf
            "<Project Sdk=\"Microsoft.NET.Sdk\">\n\
             \x20 <PropertyGroup>\n\
             \x20   <OutputType>Library</OutputType>\n\
             \x20   <TargetFramework>net9.0</TargetFramework>\n\
             \x20 </PropertyGroup>\n\
             \x20 <ItemGroup>\n%s\n  </ItemGroup>\n\
             </Project>\n"
            compiles
    let fsprojPath = Path.Combine(projectDir, "q030proj.fsproj")
    File.WriteAllText(fsprojPath, fsproj)
    fsprojPath

let fsprojPath = writeProject ()
logLine (sprintf "generated project: %s  (N=%d files, hub=File%04d, dependents=%s)"
            projectDir N hubIndex (String.Join(",", dependents |> Set.toList |> List.map (sprintf "File%04d"))))
logLine (sprintf "topology: hub=%d  dependents=[%s]  unrelated-count=%d"
            hubIndex (String.Join(",", dependents |> Set.toList)) (N - 1 - dependents.Count))

// dotnet restore so FSAC's project cracker has obj/project.assets.json. No build needed.
logLine "running dotnet restore ..."
let restore = ProcessStartInfo("dotnet", sprintf "restore \"%s\"" fsprojPath)
restore.UseShellExecute <- false
restore.RedirectStandardOutput <- true
restore.RedirectStandardError <- true
let rp = Process.Start(restore)
let restoreOut = rp.StandardOutput.ReadToEnd()
rp.WaitForExit()
logLine (sprintf "dotnet restore exit=%d" rp.ExitCode)
if rp.ExitCode <> 0 then
    logLine restoreOut
    failwith "dotnet restore failed"

// ---------------------------------------------------------------------------
// 2. LSP plumbing (from Q022/Q029), timestamped receive queue. UNCHANGED mechanism.
// ---------------------------------------------------------------------------

let psi = ProcessStartInfo("dotnet")
psi.Arguments <- "tool run fsautocomplete"
psi.WorkingDirectory <- fsacToolDir
psi.RedirectStandardInput <- true
psi.RedirectStandardOutput <- true
psi.RedirectStandardError <- true
psi.UseShellExecute <- false
let proc = Process.Start(psi)
let stdinStream = proc.StandardInput.BaseStream
let stdoutStream = proc.StandardOutput.BaseStream

let stderrThread =
    System.Threading.Thread(fun () ->
        try
            let mutable line = proc.StandardError.ReadLine()
            while not (isNull line) do
                logLine (sprintf "[stderr] %s" line)
                line <- proc.StandardError.ReadLine()
        with _ -> ())
stderrThread.IsBackground <- true
stderrThread.Start()

let readExactly (stream: Stream) (n: int) =
    let buf = Array.zeroCreate<byte> n
    let mutable read = 0
    while read < n do
        let r = stream.Read(buf, read, n - read)
        if r = 0 then failwith "stream closed unexpectedly"
        read <- read + r
    buf

let readMessage () : JsonDocument option =
    let headerBytes = ResizeArray<byte>()
    let mutable finished = false
    let last4 = Array.zeroCreate<byte> 4
    let mutable eof = false
    while not finished && not eof do
        let b = stdoutStream.ReadByte()
        if b = -1 then eof <- true
        else
            headerBytes.Add(byte b)
            last4.[0] <- last4.[1]; last4.[1] <- last4.[2]; last4.[2] <- last4.[3]; last4.[3] <- byte b
            if last4 = [| byte '\r'; byte '\n'; byte '\r'; byte '\n' |] then finished <- true
    if eof then None
    else
        let headerText = Encoding.ASCII.GetString(headerBytes.ToArray())
        let lengthLine =
            headerText.Split([| "\r\n" |], StringSplitOptions.None)
            |> Array.tryFind (fun l -> l.StartsWith("Content-Length"))
        match lengthLine with
        | Some line ->
            let len = line.Split(':').[1].Trim() |> int
            let bodyBytes = readExactly stdoutStream len
            Some(JsonDocument.Parse(Encoding.UTF8.GetString(bodyBytes)))
        | None -> None

type Msg = { At: DateTime; Doc: JsonDocument }
let messageQueue = System.Collections.Concurrent.ConcurrentQueue<Msg>()
let readerThread =
    System.Threading.Thread(fun () ->
        try
            let mutable go = true
            while go do
                match readMessage () with
                | Some doc ->
                    let at = DateTime.Now
                    lock rawLog (fun () ->
                        rawLog.WriteLine(sprintf "[%s] %s" (at.ToString("HH:mm:ss.fff")) (doc.RootElement.ToString())))
                    messageQueue.Enqueue({ At = at; Doc = doc })
                | None -> go <- false
        with ex -> logLine (sprintf "[reader-error] %s" (ex.ToString())))
readerThread.IsBackground <- true
readerThread.Start()

let mutable nextId = 1
let sendRaw (json: string) =
    let bytes = Encoding.UTF8.GetBytes(json)
    let header = Encoding.ASCII.GetBytes(sprintf "Content-Length: %d\r\n\r\n" bytes.Length)
    stdinStream.Write(header, 0, header.Length)
    stdinStream.Write(bytes, 0, bytes.Length)
    stdinStream.Flush()

let sendRequest (methodName: string) (paramsObj: obj) =
    let id = nextId
    nextId <- nextId + 1
    sendRaw (JsonSerializer.Serialize({| jsonrpc = "2.0"; id = id; ``method`` = methodName; ``params`` = paramsObj |}))
    id
let sendNotification (methodName: string) (paramsObj: obj) =
    sendRaw (JsonSerializer.Serialize({| jsonrpc = "2.0"; ``method`` = methodName; ``params`` = paramsObj |}))

let tryGetInt (el: JsonElement) (name: string) =
    match el.TryGetProperty(name) with
    | true, v when v.ValueKind = JsonValueKind.Number -> Some(v.GetInt32())
    | _ -> None

let waitForResponse (id: int) (timeoutMs: int) : JsonElement option =
    let sw = Stopwatch.StartNew()
    let mutable result = None
    while result.IsNone && sw.ElapsedMilliseconds < int64 timeoutMs do
        match messageQueue.TryDequeue() with
        | true, m ->
            match tryGetInt m.Doc.RootElement "id" with
            | Some rid when rid = id -> result <- Some m.Doc.RootElement
            | _ -> ()
        | false, _ -> System.Threading.Thread.Sleep(5)
    result

let drainUntilQuiet (quietMs: int) (maxTotalMs: int) =
    let sw = Stopwatch.StartNew()
    let lastMessageAt = Stopwatch.StartNew()
    let mutable go = true
    while go do
        match messageQueue.TryDequeue() with
        | true, _ -> lastMessageAt.Restart()
        | false, _ -> System.Threading.Thread.Sleep(20)
        if lastMessageAt.ElapsedMilliseconds > int64 quietMs then go <- false
        if sw.ElapsedMilliseconds > int64 maxTotalMs then go <- false

let fileUri (path: string) =
    let full = Path.GetFullPath(path).Replace("\\", "/")
    if full.StartsWith("/") then "file://" + full else "file:///" + full

let docAnalyzedLeaf (m: Msg) : string option =
    let root = m.Doc.RootElement
    match root.TryGetProperty("method") with
    | true, meth when meth.GetString() = "fsharp/documentAnalyzed" ->
        let uri = root.GetProperty("params").GetProperty("textDocument").GetProperty("uri").GetString()
        Some(Path.GetFileName(Uri.UnescapeDataString(uri)))
    | _ -> None

// ---------------------------------------------------------------------------
// 3. LSP session: initialize, load workspace, open all files. UNCHANGED.
// ---------------------------------------------------------------------------

let rootUri = fileUri projectDir
let initId =
    sendRequest "initialize"
        {| processId = Process.GetCurrentProcess().Id
           rootUri = rootUri
           rootPath = projectDir
           capabilities = {||}
           trace = "off" |}
match waitForResponse initId 60000 with
| Some _ -> logLine "initialize OK"
| None -> logLine "TIMEOUT initialize"
sendNotification "initialized" {||}
drainUntilQuiet 2000 30000

let fsprojUri = fileUri fsprojPath
logLine (sprintf "fsharp/workspaceLoad -> %s" fsprojUri)
let wsId = sendRequest "fsharp/workspaceLoad" {| textDocuments = [| {| uri = fsprojUri |} |] |}
match waitForResponse wsId 180000 with
| Some _ -> logLine "workspaceLoad OK"
| None -> logLine "TIMEOUT workspaceLoad"
drainUntilQuiet 4000 90000

for i in 0 .. N - 1 do
    let p = filePath i
    sendNotification "textDocument/didOpen"
        {| textDocument = {| uri = fileUri p; languageId = "fsharp"; version = 1; text = File.ReadAllText p |} |}
drainUntilQuiet 6000 120000
logLine "all files opened, initial analysis settled"

// ---------------------------------------------------------------------------
// 4. Edit cycles: edit File{p} (usually Hub), then COLLECT THE FULL SET of files that
//    re-analyze. The set (not a single settle time) is the decisive signal.
// ---------------------------------------------------------------------------

let mutable fileVersion = System.Collections.Generic.Dictionary<int,int>()
for i in 0 .. N - 1 do fileVersion.[i] <- 1

// Returns the ordered list of (leaf, offsetMs) documentAnalyzed events observed after the edit, until
// the cascade goes quiet (quietMs with no further messages) or a hard cap is hit.
let editCollect (p: int) (marker: string) : (string * int64) list =
    // clear any stale queued messages first
    drainUntilQuiet 1000 8000
    let newText = mkFileSrc p marker
    File.WriteAllText(filePath p, newText)
    let ver = fileVersion.[p] + 1
    fileVersion.[p] <- ver
    let uri = fileUri (filePath p)
    let tSend = DateTime.Now
    sendNotification "textDocument/didChange"
        {| textDocument = {| uri = uri; version = ver |}
           contentChanges = [| {| text = newText |} |] |}
    sendNotification "textDocument/didSave" {| textDocument = {| uri = uri |}; text = newText |}

    // Collect every documentAnalyzed (received after tSend) until the stream goes quiet. A generous
    // quiet window (4s) guarantees a 27-file suffix cascade would be fully captured if it happened.
    let events = System.Collections.Generic.List<string * int64>()
    let sw = Stopwatch.StartNew()
    let lastEventSw = Stopwatch.StartNew()
    let mutable go = true
    while go do
        match messageQueue.TryDequeue() with
        | true, m ->
            match docAnalyzedLeaf m with
            | Some leaf when m.At >= tSend ->
                events.Add(leaf, int64 (m.At - tSend).TotalMilliseconds)
                lastEventSw.Restart()
            | _ -> lastEventSw.Restart()  // any traffic resets quiet timer, but only docAnalyzed recorded
        | false, _ -> System.Threading.Thread.Sleep(5)
        if lastEventSw.ElapsedMilliseconds > 4000L then go <- false
        if sw.ElapsedMilliseconds > 60000L then go <- false
    events |> List.ofSeq

// Warm-up edit at Hub, discarded (first-typecheck tax; FINDINGS.md caveat).
logLine "=== warm-up edit (DISCARDED) ==="
let warmEvents = editCollect hubIndex "warm"
logLine (sprintf "warm-up: hub edit produced %d documentAnalyzed events (discarded)" warmEvents.Length)

// results: one row per (editPosition, rep, analyzedLeaf)
let eventRows = System.Collections.Generic.List<int*int*string*int64*string>()
// summary: one row per (editPosition, rep)
let summaryRows = System.Collections.Generic.List<int*int*int*int*int*bool>()

for p in editPositions do
    for r in 1 .. reps do
        let marker = sprintf "v%d_%d" p r
        let events = editCollect p marker
        // Deduplicate to the SET of distinct files analyzed (the edited file often fires twice).
        let distinctLeaves = events |> List.map fst |> List.distinct
        let analyzedIdx =
            distinctLeaves
            |> List.choose (fun leaf ->
                let m = System.Text.RegularExpressions.Regex.Match(leaf, @"File(\d+)\.fs")
                if m.Success then Some(int m.Groups.[1].Value) else None)
            |> List.sort
        let hubHit = analyzedIdx |> List.contains hubIndex
        let depsHit = analyzedIdx |> List.filter (fun i -> dependents.Contains i)
        let unrelatedHit = analyzedIdx |> List.filter (fun i -> i <> hubIndex && not (dependents.Contains i))
        logLine (sprintf "edit p=%d (%s) rep=%d: %d distinct files re-analyzed | hubHit=%b depsHit=%d/%d unrelatedHit=%d"
                    p (classify p) r analyzedIdx.Length hubHit depsHit.Length dependents.Count unrelatedHit.Length)
        logLine (sprintf "    analyzed indices: [%s]" (String.Join(",", analyzedIdx)))
        logLine (sprintf "    dependents hit:   [%s]" (String.Join(",", depsHit)))
        logLine (sprintf "    unrelated hit:    [%s]" (String.Join(",", unrelatedHit)))
        // record event rows (with first-arrival offset per distinct leaf)
        for leaf in distinctLeaves do
            let firstOffset = events |> List.filter (fun (l,_) -> l = leaf) |> List.map snd |> List.min
            let idxOpt =
                let m = System.Text.RegularExpressions.Regex.Match(leaf, @"File(\d+)\.fs")
                if m.Success then Some(int m.Groups.[1].Value) else None
            let cls = match idxOpt with Some i -> classify i | None -> "other"
            eventRows.Add(p, r, leaf, firstOffset, cls)
        summaryRows.Add(p, r, analyzedIdx.Length, depsHit.Length, unrelatedHit.Length, hubHit)

// ---------------------------------------------------------------------------
// 5. Emit CSVs + verdict-oriented summary, shut down.
// ---------------------------------------------------------------------------

let csv = new StreamWriter(csvPath, false)
csv.WriteLine("editPosition,rep,analyzedLeaf,firstOffsetMs,classification")
for (p, r, leaf, off, cls) in eventRows do
    csv.WriteLine(sprintf "%d,%d,%s,%d,%s" p r leaf off cls)
csv.Flush(); csv.Close()
logLine (sprintf "wrote %s" csvPath)

let scsv = new StreamWriter(summaryPath, false)
scsv.WriteLine("editPosition,editClass,rep,numAnalyzed,numDependentsHit,totalDependents,numUnrelatedHit,hubHit")
for (p, r, nAll, nDep, nUnrel, hub) in summaryRows do
    scsv.WriteLine(sprintf "%d,%s,%d,%d,%d,%d,%d,%b" p (classify p) r nAll nDep dependents.Count nUnrel hub)
scsv.Flush(); scsv.Close()
logLine (sprintf "wrote %s" summaryPath)

logLine "=== INTERPRETATION (Hub edits only) ==="
logLine (sprintf "N=%d  hubIndex=%d  dependents={%s}  compilation-order-successors-of-hub=%d  true-dependents=%d"
            N hubIndex (String.Join(",", dependents |> Set.toList)) (N - 1 - hubIndex) dependents.Count)
logLine "Prediction A (dependency-precise):        Hub edit re-analyzes ~ {hub + true dependents}"
logLine "Prediction B (compilation-order-conserv.): Hub edit re-analyzes ~ {hub + whole suffix}"
let hubRows = summaryRows |> Seq.filter (fun (p,_,_,_,_,_) -> p = hubIndex) |> Seq.toList
if not hubRows.IsEmpty then
    let meds = hubRows |> List.map (fun (_,_,n,_,_,_) -> n)
    logLine (sprintf "OBSERVED (hub edits): numAnalyzed per rep = [%s]  (successors=%d, dependents=%d)"
                (String.Join(",", meds)) (N - 1 - hubIndex) dependents.Count)

sendRequest "shutdown" {||} |> ignore
System.Threading.Thread.Sleep(300)
sendNotification "exit" {||}
System.Threading.Thread.Sleep(300)
try proc.Kill() with _ -> ()
log.Flush(); log.Close()
rawLog.Flush(); rawLog.Close()
