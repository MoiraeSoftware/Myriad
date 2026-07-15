// Validates experiments/Qddd-*/ dirs against the QUARTET schema (see README.md).
// Rules enforced (status-aware - the strict all-four rule applies at CLOSE):
//   planned -> 00-hypothesis.md
//   running -> 00-hypothesis.md, 01-design.md
//   parked  -> 00-hypothesis.md
//   closed  -> 00-hypothesis.md, 01-design.md, 02-results.md, 03-review.md
// Every quartet dir needs a parseable quartet.json whose id matches the dir name (case-insensitive
// prefix match, e.g. dir "Q001-fcs-typed-codegen" <-> id "Q001"). All required files must exist
// AND be non-empty. Stray movement-numbered files outside the canon are flagged.
// Folders not matching Qddd-* are exempt (one-off notes, infra scratch) - see README.md.
//
// Usage:
//   dotnet fsi experiments/check-quartets.fsx            # validate, exit 1 on failure
//   dotnet fsi experiments/check-quartets.fsx --stamp    # also write last_validated into passing quartet.json

open System
open System.IO
open System.Text.Json
open System.Text.Json.Nodes
open System.Text.RegularExpressions

let scriptDir = __SOURCE_DIRECTORY__
let stamp = fsi.CommandLineArgs |> Array.contains "--stamp"

let canon = [ "00-hypothesis.md"; "01-design.md"; "02-results.md"; "03-review.md" ]

let requiredByStatus =
    dict [
        "planned", canon |> List.take 1
        "running", canon |> List.take 2
        "parked", canon |> List.take 1
        "closed", canon
    ]

let checkQuartet (qdir: DirectoryInfo) : string list =
    let jpath = Path.Combine(qdir.FullName, "quartet.json")
    if not (File.Exists jpath) then
        [ sprintf "%s: missing quartet.json" qdir.Name ]
    else
        let meta =
            try Some(JsonNode.Parse(File.ReadAllText jpath))
            with ex -> None

        match meta with
        | None -> [ sprintf "%s: quartet.json unparseable" qdir.Name ]
        | Some meta ->
            let errors = ResizeArray<string>()

            let qid = meta.["id"] |> Option.ofObj |> Option.map (fun n -> n.GetValue<string>()) |> Option.defaultValue ""
            if not (qdir.Name.StartsWith(qid + "-", StringComparison.OrdinalIgnoreCase)) then
                errors.Add(sprintf "%s: quartet.json id '%s' does not match dir name" qdir.Name qid)

            let status = meta.["status"] |> Option.ofObj |> Option.map (fun n -> n.GetValue<string>()) |> Option.defaultValue ""
            if not (requiredByStatus.ContainsKey status) then
                errors.Add(sprintf "%s: status '%s' not in %s" qdir.Name status (String.Join(", ", requiredByStatus.Keys |> Seq.sort)))
            else
                for fname in requiredByStatus.[status] do
                    let fpath = Path.Combine(qdir.FullName, fname)
                    if not (File.Exists fpath) then
                        errors.Add(sprintf "%s: [%s] missing %s" qdir.Name status fname)
                    elif FileInfo(fpath).Length = 0L then
                        errors.Add(sprintf "%s: [%s] %s is empty" qdir.Name status fname)

                for stray in qdir.GetFiles("0*-*.md") do
                    if not (List.contains stray.Name canon) then
                        errors.Add(sprintf "%s: non-canonical movement file %s (fold into %s)" qdir.Name stray.Name (String.Join("/", canon)))

                if stamp && errors.Count = 0 then
                    meta.["last_validated"] <- JsonValue.Create(DateTime.UtcNow.ToString("yyyy-MM-dd"))
                    let opts = JsonSerializerOptions(WriteIndented = true)
                    File.WriteAllText(jpath, meta.ToJsonString(opts) + "\n")

            List.ofSeq errors

let qdirs =
    Directory.GetDirectories(scriptDir)
    |> Array.map DirectoryInfo
    |> Array.filter (fun d -> Regex.IsMatch(d.Name, @"^Q\d{3}-"))
    |> Array.sortBy (fun d -> d.Name)

if qdirs.Length = 0 then
    printfn "No Qddd-*/ dirs found under experiments/."
    exit 1

let mutable problemCount = 0
for qdir in qdirs do
    match checkQuartet qdir with
    | [] -> printfn "ok    %s" qdir.Name
    | errors ->
        for e in errors do
            printfn "FAIL  %s" e
        problemCount <- problemCount + errors.Length

printfn ""
printfn "%d quartet(s), %d problem(s)." qdirs.Length problemCount
exit (if problemCount > 0 then 1 else 0)
