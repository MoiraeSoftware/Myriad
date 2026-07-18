// Minimal hand-rolled LSP client used to drive a real `fsautocomplete` (FSAC) session for Q022
// Round 2. Usage: dotnet fsi lsp-client.fsx <projectDir> <mode>
//   mode = "cold"  -> initialize, open Consumer.fs, hover on Name and Age, then exit (Round 2a)
//   mode = "live"  -> same as cold, then edit Person.fs to add Email, send didChange+didSave,
//                     re-hover on Email without restarting FSAC (Round 2b)
open System
open System.Diagnostics
open System.IO
open System.Text
open System.Text.Json

let projectDir = Path.GetFullPath(fsi.CommandLineArgs.[1])
let mode = fsi.CommandLineArgs.[2]
let fsacToolDir = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "fsac-tool"))
let logPath = Path.Combine(__SOURCE_DIRECTORY__, sprintf "lsp-client-fsprojtouch.%s.%s.log" (Path.GetFileName(projectDir)) mode)
let log = new StreamWriter(logPath, false, AutoFlush = true)
let logLine (s: string) =
    let line = sprintf "[%s] %s" (DateTime.Now.ToString("HH:mm:ss.fff")) s
    log.WriteLine(line)
    printfn "%s" line

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

// Drain stderr on a background thread so it never blocks the process, log it.
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
        if b = -1 then
            eof <- true
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
            let bodyText = Encoding.UTF8.GetString(bodyBytes)
            Some(JsonDocument.Parse(bodyText))
        | None -> None

let messageQueue = System.Collections.Concurrent.ConcurrentQueue<JsonDocument>()
let readerThread =
    System.Threading.Thread(fun () ->
        try
            let mutable go = true
            while go do
                match readMessage () with
                | Some doc ->
                    logLine (sprintf "[recv] %s" (doc.RootElement.ToString()))
                    messageQueue.Enqueue(doc)
                | None -> go <- false
        with ex -> logLine (sprintf "[reader-error] %s" (ex.ToString())))
readerThread.IsBackground <- true
readerThread.Start()

let mutable nextId = 1
let sendRaw (json: string) =
    let bytes = Encoding.UTF8.GetBytes(json)
    let header = sprintf "Content-Length: %d\r\n\r\n" bytes.Length
    let headerBytes = Encoding.ASCII.GetBytes(header)
    stdinStream.Write(headerBytes, 0, headerBytes.Length)
    stdinStream.Write(bytes, 0, bytes.Length)
    stdinStream.Flush()
    logLine (sprintf "[send] %s" json)

let sendRequest (methodName: string) (paramsObj: obj) =
    let id = nextId
    nextId <- nextId + 1
    let envelope = {| jsonrpc = "2.0"; id = id; ``method`` = methodName; ``params`` = paramsObj |}
    sendRaw (JsonSerializer.Serialize(envelope))
    id

let sendNotification (methodName: string) (paramsObj: obj) =
    let envelope = {| jsonrpc = "2.0"; ``method`` = methodName; ``params`` = paramsObj |}
    sendRaw (JsonSerializer.Serialize(envelope))

let tryGetInt (el: JsonElement) (name: string) =
    match el.TryGetProperty(name) with
    | true, v when v.ValueKind = JsonValueKind.Number -> Some(v.GetInt32())
    | _ -> None

/// Waits (polling the queue) up to timeoutMs for a response whose "id" matches, draining and
/// logging every other message (notifications, diagnostics, etc.) along the way.
let waitForResponse (id: int) (timeoutMs: int) : JsonElement option =
    let sw = Diagnostics.Stopwatch.StartNew()
    let mutable result = None
    while result.IsNone && sw.ElapsedMilliseconds < int64 timeoutMs do
        match messageQueue.TryDequeue() with
        | true, doc ->
            let root = doc.RootElement
            match tryGetInt root "id" with
            | Some rid when rid = id -> result <- Some root
            | _ -> ()
        | false, _ -> System.Threading.Thread.Sleep(50)
    result

let canaryPath = Path.Combine(projectDir, "obj", "Debug", "net9.0", "generation.canary")
let logCanary (label: string) =
    if File.Exists canaryPath then
        logLine (sprintf "CANARY[%s] = %O" label (File.GetLastWriteTimeUtc canaryPath))
    else
        logLine (sprintf "CANARY[%s] = <missing>" label)

let fileUri (path: string) =
    let full = Path.GetFullPath(path).Replace("\\", "/")
    if full.StartsWith("/") then "file://" + full else "file:///" + full

let readFileText (path: string) = File.ReadAllText(path)

// ---- LSP session ----

let rootUri = fileUri projectDir
logLine (sprintf "rootUri = %s" rootUri)

let initId =
    sendRequest
        "initialize"
        {| processId = Diagnostics.Process.GetCurrentProcess().Id
           rootUri = rootUri
           rootPath = projectDir
           capabilities = {||}
           trace = "off" |}

match waitForResponse initId 60000 with
| Some resp -> logLine (sprintf "initialize response received: %b" (resp.TryGetProperty("result") |> fst))
| None -> logLine "TIMEOUT waiting for initialize response"

sendNotification "initialized" {||}

// Give FSAC's background workspace load (its own internal DTB via Ionide.ProjInfo) time to settle.
// No reliable "loaded" signal is assumed up front (per 00-hypothesis.md's own validity precondition
// not to assume protocol behavior) -- instead we drain and log everything for a quiet period.
let drainUntilQuiet (quietMs: int) (maxTotalMs: int) =
    let sw = Diagnostics.Stopwatch.StartNew()
    let mutable lastMessageAt = Diagnostics.Stopwatch.StartNew()
    let mutable go = true
    while go do
        match messageQueue.TryDequeue() with
        | true, _ -> lastMessageAt.Restart()
        | false, _ -> System.Threading.Thread.Sleep(100)
        if lastMessageAt.ElapsedMilliseconds > int64 quietMs then go <- false
        if sw.ElapsedMilliseconds > int64 maxTotalMs then go <- false
    logLine (sprintf "drainUntilQuiet finished after %dms" sw.ElapsedMilliseconds)

drainUntilQuiet 2000 30000

// FSAC does not auto-discover projects from rootUri/initialized alone (confirmed empirically: a
// hover request before this point fails with "Couldn't find ... in LoadedProjects"). Its own custom
// LSP extension `fsharp/workspaceLoad` must be called explicitly with the target .fsproj path(s) --
// this is what a real Ionide client does after its own `fsharp/workspacePeek` discovery step; here
// the single known project path is passed directly.
let fsprojPath = Directory.GetFiles(projectDir, "*.fsproj") |> Array.exactlyOne
let fsprojUri = fileUri fsprojPath
logLine (sprintf "fsharp/workspaceLoad -> %s" fsprojUri)
let workspaceLoadId = sendRequest "fsharp/workspaceLoad" {| textDocuments = [| {| uri = fsprojUri |} |] |}
match waitForResponse workspaceLoadId 120000 with
| Some resp -> logLine (sprintf "workspaceLoad response: %s" (resp.ToString()))
| None -> logLine "TIMEOUT waiting for workspaceLoad response"

drainUntilQuiet 3000 60000
logCanary "after initial workspaceLoad"

let consumerPath = Path.Combine(projectDir, "Consumer.fs")
let consumerUri = fileUri consumerPath
let consumerText = readFileText consumerPath

sendNotification
    "textDocument/didOpen"
    {| textDocument =
        {| uri = consumerUri
           languageId = "fsharp"
           version = 1
           text = consumerText |} |}

drainUntilQuiet 2000 30000

/// 0-based line/character of the first occurrence of `needle` on the line containing `onLine`.
let findPosition (text: string) (needle: string) =
    let lines = text.Replace("\r\n", "\n").Split('\n')
    let mutable result = None
    for i in 0 .. lines.Length - 1 do
        if result.IsNone then
            let idx = lines.[i].IndexOf(needle: string)
            if idx >= 0 then result <- Some(i, idx)
    match result with
    | Some p -> p
    | None -> failwithf "could not find '%s' in text" needle

let hoverAt (uri: string) (text: string) (needle: string) (label: string) =
    // needle is expected to be "Module.Member" -- position on a character INSIDE the member
    // name after the dot, not the module name, or a hover would trivially "resolve" against the
    // module regardless of whether the member itself exists.
    let dotIdx = needle.LastIndexOf('.')
    if dotIdx < 0 then failwithf "needle '%s' must contain a '.'" needle
    let memberName = needle.Substring(dotIdx + 1)
    let (line, moduleCol) = findPosition text needle
    let col = moduleCol + dotIdx + 1 + (memberName.Length / 2)
    let id = sendRequest "textDocument/hover" {| textDocument = {| uri = uri |}; position = {| line = line; character = col |} |}
    match waitForResponse id 30000 with
    | Some resp ->
        let hasResult, resultEl = resp.TryGetProperty("result")
        if hasResult && resultEl.ValueKind <> JsonValueKind.Null then
            logLine (sprintf "HOVER %s -> RESOLVED: %s" label (resultEl.ToString()))
            true
        else
            logLine (sprintf "HOVER %s -> NULL (unresolved)" label)
            false
    | None ->
        logLine (sprintf "HOVER %s -> TIMEOUT" label)
        false

logLine "=== querying Name (sanity, should always resolve) ==="
let nameOk = hoverAt consumerUri consumerText "PersonLenses.Name" "Name"

logLine "=== querying Age (the real Round 1 question) ==="
let ageOk = hoverAt consumerUri consumerText "PersonLenses.Age" "Age"

logLine (sprintf "RESULT mode=%s dir=%s nameOk=%b ageOk=%b" mode (Path.GetFileName projectDir) nameOk ageOk)

if mode = "live" then
    logLine "=== Round 2b: live edit, no restart ==="
    let personPath = Path.Combine(projectDir, "Person.fs")
    let personUri = fileUri personPath
    let personText = readFileText personPath
    let newPersonText = personText.Replace("{ Name: string; Age: int }", "{ Name: string; Age: int; Email: string }")
    if newPersonText = personText then failwith "Person.fs edit did not match expected text, aborting"
    File.WriteAllText(personPath, newPersonText)

    // Real editors send didOpen once, then didChange (full or incremental) as the user types,
    // then didSave on save. We didOpen Person.fs now (it wasn't open before) then didChange+didSave,
    // matching a plausible real sequence for "open the file, edit it, save it".
    sendNotification "textDocument/didOpen" {| textDocument = {| uri = personUri; languageId = "fsharp"; version = 1; text = personText |} |}
    drainUntilQuiet 1000 10000
    sendNotification
        "textDocument/didChange"
        {| textDocument = {| uri = personUri; version = 2 |}
           contentChanges = [| {| text = newPersonText |} |] |}
    drainUntilQuiet 1000 10000
    sendNotification "textDocument/didSave" {| textDocument = {| uri = personUri |}; text = newPersonText |}

    // Give FSAC a generous window to notice, possibly re-trigger DTB, and settle.
    drainUntilQuiet 5000 60000
    logCanary "after didSave (Round 2b)"

    let consumerText2 = readFileText consumerPath
    // Consumer.fs doesn't reference Email yet -- add and didChange it too, matching a real edit.
    let newConsumerText = consumerText2 + "\nlet emailGetter = fst PersonLenses.Email\n"
    File.WriteAllText(consumerPath, newConsumerText)
    sendNotification
        "textDocument/didChange"
        {| textDocument = {| uri = consumerUri; version = 2 |}
           contentChanges = [| {| text = newConsumerText |} |] |}
    drainUntilQuiet 2000 30000

    logLine "=== querying Email (Round 2b real question: live pickup without restart) ==="
    let emailOk = hoverAt consumerUri newConsumerText "PersonLenses.Email" "Email"
    logLine (sprintf "RESULT-2b mode=%s dir=%s emailOk=%b" mode (Path.GetFileName projectDir) emailOk)

    if not emailOk then
        // Per 01-design.md's own "what a review should press on": rule out a false negative before
        // concluding FSAC doesn't re-trigger. A real editor's LSP client also sends
        // workspace/didChangeWatchedFiles for on-disk file changes (registered via
        // client/registerCapability), which this script never sent -- try it explicitly.
        logLine "=== Round 2c: explicit workspace/didChangeWatchedFiles, does that unblock it? ==="
        sendNotification
            "workspace/didChangeWatchedFiles"
            {| changes = [| {| uri = personUri; ``type`` = 2 |} |] |} // FileChangeType.Changed = 2
        drainUntilQuiet 5000 60000
        logCanary "after workspace/didChangeWatchedFiles (Round 2c)"
        let emailOk2 = hoverAt consumerUri newConsumerText "PersonLenses.Email" "Email"
        logLine (sprintf "RESULT-2c mode=%s dir=%s emailOk(after didChangeWatchedFiles)=%b" mode (Path.GetFileName projectDir) emailOk2)

        if not emailOk2 then
            // Round 2d: does an explicit, manual re-issue of fsharp/workspaceLoad (the same call
            // Ionide's own "reload projects" command would make) unblock it? Rounds out the honest
            // picture: automatic live pickup vs. explicit-reload fallback.
            logLine "=== Round 2d: explicit manual fsharp/workspaceLoad re-issue ==="
            let reloadId = sendRequest "fsharp/workspaceLoad" {| textDocuments = [| {| uri = fsprojUri |} |] |}
            match waitForResponse reloadId 120000 with
            | Some resp -> logLine (sprintf "manual reload response: %s" (resp.ToString()))
            | None -> logLine "TIMEOUT waiting for manual reload response"
            drainUntilQuiet 3000 60000
            logCanary "after manual fsharp/workspaceLoad reload (Round 2d)"
            let emailOk3 = hoverAt consumerUri newConsumerText "PersonLenses.Email" "Email"
            logLine (sprintf "RESULT-2d mode=%s dir=%s emailOk(after manual workspaceLoad)=%b" mode (Path.GetFileName projectDir) emailOk3)

            if not emailOk3 then
                // Round 2e: does telling FSAC the .fsproj ITSELF changed (didChangeWatchedFiles on
                // the project file, not the source file) invalidate whatever cache is short-
                // circuiting re-cracking, even with no actual .fsproj content change?
                logLine "=== Round 2e: workspace/didChangeWatchedFiles on the .fsproj itself, then reload ==="
                // REVIEW ADD: actually mutate the .fsproj bytes on disk (2e original only sent a notification)
                File.AppendAllText(fsprojPath, "\n  <!-- review touch -->\n")
                sendNotification "workspace/didChangeWatchedFiles" {| changes = [| {| uri = fsprojUri; ``type`` = 2 |} |] |}
                drainUntilQuiet 2000 30000
                let reloadId2 = sendRequest "fsharp/workspaceLoad" {| textDocuments = [| {| uri = fsprojUri |} |] |}
                match waitForResponse reloadId2 120000 with
                | Some resp -> logLine (sprintf "second manual reload response: %s" (resp.ToString()))
                | None -> logLine "TIMEOUT waiting for second manual reload response"
                drainUntilQuiet 3000 60000
                logCanary "after fsproj didChangeWatchedFiles + reload (Round 2e)"
                let emailOk4 = hoverAt consumerUri newConsumerText "PersonLenses.Email" "Email"
                logLine (sprintf "RESULT-2e mode=%s dir=%s emailOk(after fsproj-touch reload)=%b" mode (Path.GetFileName projectDir) emailOk4)

sendRequest "shutdown" {||} |> ignore
System.Threading.Thread.Sleep(500)
sendNotification "exit" {||}
System.Threading.Thread.Sleep(500)
try proc.Kill() with _ -> ()
log.Flush()
log.Close()
