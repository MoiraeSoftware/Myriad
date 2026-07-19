// Q029 - measures a real fsautocomplete (FSAC) session's per-edit incremental re-analysis cost as a
// function of edit position in compilation order, to test whether Q023/Q024's linear-in-successors
// cost model (measured via checker.ParseAndCheckProject as a library) reproduces through FSAC's real
// LSP-driven per-file incremental path.
//
// Usage:  dotnet fsi q029-fsac-cost.fsx <projectDir> <N> <positionsCsv> <reps>
//   e.g.  dotnet fsi q029-fsac-cost.fsx C:\tmp\q029a 20 0,19 2        (cheapest falsifier)
//         dotnet fsi q029-fsac-cost.fsx C:\tmp\q029a 20 0,5,10,14,19 3 (full sweep)
//
// LSP plumbing is copied from Q022's lsp-client-mtimeonly.fsx (proven to drive FSAC 0.83.0), with the
// receive queue extended to timestamp every message at receipt for latency measurement.

open System
open System.Diagnostics
open System.IO
open System.Text
open System.Text.Json

let projectDir = Path.GetFullPath(fsi.CommandLineArgs.[1])
let N = int fsi.CommandLineArgs.[2]
let positions = fsi.CommandLineArgs.[3].Split(',') |> Array.map int
let reps = int fsi.CommandLineArgs.[4]
let fsacToolDir = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "fsac-tool"))

let logPath = Path.Combine(__SOURCE_DIRECTORY__, sprintf "q029-N%d.log" N)
let csvPath = Path.Combine(__SOURCE_DIRECTORY__, sprintf "q029-results-N%d.csv" N)
let log = new StreamWriter(logPath, false, AutoFlush = true)
// Optional raw LSP transcript (every received message, timestamped) so a reviewer can independently
// verify the documentAnalyzed cascade. Enabled by setting env Q029_RAW=1.
let rawEnabled = Environment.GetEnvironmentVariable("Q029_RAW") = "1"
let rawLog =
    if rawEnabled then new StreamWriter(Path.Combine(__SOURCE_DIRECTORY__, sprintf "q029-N%d.raw.log" N), false, AutoFlush = true)
    else null
let logLine (s: string) =
    let line = sprintf "[%s] %s" (DateTime.Now.ToString("HH:mm:ss.fff")) s
    log.WriteLine(line)
    printfn "%s" line

// ---------------------------------------------------------------------------
// 1. Generate a real, genuinely-weighted, compilation-order-chained project.
// ---------------------------------------------------------------------------

// Genuine typecheck weight per file (generic record, generic wrapper, Map/List pipeline, explicit
// recursion, a 20-record comprehension) - the exact shape from Q023's mkPrefixSrc. `marker` is the
// value-only edit knob: a string literal that changes file CONTENT without changing any exported
// type/signature. `prevRef` chains this file onto the previous one's `total` so editing an early file
// genuinely forces re-checking of its successors.
let mkFileSrc (i: int) (marker: string) =
    let prevRef = if i = 0 then "0" else sprintf "File%04d.total" (i - 1)
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
        i i i i i marker prevRef

let fileName i = sprintf "File%04d.fs" i
let filePath i = Path.Combine(projectDir, fileName i)

let writeProject () =
    if Directory.Exists projectDir then Directory.Delete(projectDir, true)
    Directory.CreateDirectory projectDir |> ignore
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
    let fsprojPath = Path.Combine(projectDir, "q029proj.fsproj")
    File.WriteAllText(fsprojPath, fsproj)
    fsprojPath

let fsprojPath = writeProject ()
logLine (sprintf "generated project: %s  (N=%d files)" projectDir N)

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
// 2. LSP plumbing (from Q022 lsp-client-mtimeonly.fsx), timestamped receive queue.
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

// Each queue entry is (receiptTime, doc) so inter-message latency is measurable.
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
                    if rawEnabled then
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

// documentAnalyzed uri comparison: FSAC lowercases the drive letter and URL-encodes; compare on the
// file's leaf name, which is unique per file here (FileNNNN.fs).
let docAnalyzedLeaf (m: Msg) : string option =
    let root = m.Doc.RootElement
    match root.TryGetProperty("method") with
    | true, meth when meth.GetString() = "fsharp/documentAnalyzed" ->
        let uri = root.GetProperty("params").GetProperty("textDocument").GetProperty("uri").GetString()
        Some(Path.GetFileName(Uri.UnescapeDataString(uri)))
    | _ -> None

// ---------------------------------------------------------------------------
// 3. LSP session: initialize, load workspace, open all files.
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

// didOpen all files (real content from disk).
for i in 0 .. N - 1 do
    let p = filePath i
    sendNotification "textDocument/didOpen"
        {| textDocument = {| uri = fileUri p; languageId = "fsharp"; version = 1; text = File.ReadAllText p |} |}
drainUntilQuiet 5000 120000
logLine "all files opened, initial analysis settled"

// ---------------------------------------------------------------------------
// 4. Edit cycles.
// ---------------------------------------------------------------------------

let lastLeaf = fileName (N - 1)
let lastUri = fileUri (filePath (N - 1))

// Position of `total` in the last file, for the hover freshness cross-check. `total` is on the last
// line ("let total = ..."). Compute 0-based line/char of the identifier `total` after `let `.
let hoverPosLastFile () =
    let text = File.ReadAllText(filePath (N - 1)).Replace("\r\n", "\n").Split('\n')
    let mutable res = None
    for li in 0 .. text.Length - 1 do
        if res.IsNone && text.[li].StartsWith("let total ") then res <- Some(li, 4) // char 4 = 't' of total
    match res with Some p -> p | None -> failwith "could not locate 'let total' in last file"

let mutable fileVersion = System.Collections.Generic.Dictionary<int,int>()
for i in 0 .. N - 1 do fileVersion.[i] <- 1

// One edit cycle: bump marker in File{p}, didChange+didSave, measure ms from didChange until the LAST
// file's documentAnalyzed arrives; then hover the last file's `total` for a freshness/latency check.
let editCycle (p: int) (marker: string) : int64 * int64 * bool =
    // clear any stale queued messages first
    drainUntilQuiet 800 8000
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

    // Wait for documentAnalyzed of the LAST file (URI-leaf match) received after tSend.
    // If p IS the last file, we still wait for its own re-analysis.
    let sw = Stopwatch.StartNew()
    let mutable settleAt : DateTime option = None
    while settleAt.IsNone && sw.ElapsedMilliseconds < 120000L do
        match messageQueue.TryDequeue() with
        | true, m ->
            match docAnalyzedLeaf m with
            | Some leaf when leaf = lastLeaf && m.At >= tSend -> settleAt <- Some m.At
            | _ -> ()
        | false, _ -> System.Threading.Thread.Sleep(3)
    let settleMs =
        match settleAt with
        | Some at -> int64 (at - tSend).TotalMilliseconds
        | None -> -1L

    // Freshness cross-check: hover `total` in the last file, time request->response, confirm resolved.
    drainUntilQuiet 500 5000
    let (hl, hc) = hoverPosLastFile ()
    let hswStart = DateTime.Now
    let hid = sendRequest "textDocument/hover" {| textDocument = {| uri = lastUri |}; position = {| line = hl; character = hc |} |}
    let hResp = waitForResponse hid 60000
    let hoverMs = int64 (DateTime.Now - hswStart).TotalMilliseconds
    let hoverResolved =
        match hResp with
        | Some r ->
            match r.TryGetProperty("result") with
            | true, res -> res.ValueKind <> JsonValueKind.Null
            | _ -> false
        | None -> false
    settleMs, hoverMs, hoverResolved

// Warm-up edit at a mid position, discarded (first-typecheck tax; FINDINGS.md caveat).
logLine "=== warm-up edit (DISCARDED) ==="
let warmP = positions.[positions.Length / 2]
let (wS, wH, wR) = editCycle warmP "warm"
logLine (sprintf "warm-up: p=%d settleMs=%d hoverMs=%d resolved=%b (discarded)" warmP wS wH wR)

let results = System.Collections.Generic.List<int*int*int*int64*int64*bool>()
for p in positions do
    for r in 1 .. reps do
        let marker = sprintf "v%d_%d" p r
        let (s, h, resolved) = editCycle p marker
        let successors = N - 1 - p
        logLine (sprintf "p=%2d successors=%2d rep=%d settleMs=%5d hoverMs=%5d resolved=%b" p successors r s h resolved)
        results.Add((N, p, successors, s, h, resolved))

// ---------------------------------------------------------------------------
// 5. Emit CSV + summary, shut down.
// ---------------------------------------------------------------------------

let csv = new StreamWriter(csvPath, false)
csv.WriteLine("N,position,successors,rep,settleMs,hoverMs,hoverResolved")
let mutable ri = 0
let repOf = System.Collections.Generic.Dictionary<int,int>()
for (n, p, succ, s, h, resolved) in results do
    let rep = (if repOf.ContainsKey p then repOf.[p] else 0) + 1
    repOf.[p] <- rep
    csv.WriteLine(sprintf "%d,%d,%d,%d,%d,%d,%b" n p succ rep s h resolved)
csv.Flush(); csv.Close()
logLine (sprintf "wrote %s" csvPath)

// per-position median settle
logLine "=== SUMMARY (median settleMs per position) ==="
let byPos = results |> Seq.groupBy (fun (_,p,_,_,_,_) -> p) |> Seq.sortBy fst
for (p, rows) in byPos do
    let arr = rows |> Seq.map (fun (_,_,succ,s,_,_) -> succ, s) |> Seq.toArray
    let succ = fst arr.[0]
    let settles = arr |> Array.map snd |> Array.sort
    let med = settles.[settles.Length / 2]
    logLine (sprintf "position %2d  successors=%2d  medianSettleMs=%5d  (raw: %s)" p succ med (String.Join(",", settles)))

sendRequest "shutdown" {||} |> ignore
System.Threading.Thread.Sleep(300)
sendNotification "exit" {||}
System.Threading.Thread.Sleep(300)
try proc.Kill() with _ -> ()
log.Flush(); log.Close()
