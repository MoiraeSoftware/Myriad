module File0000
type Record0000 = { Id: int; Name: string; Tags: string list; Meta: Map<string,int> }
type Wrapper0000<'a> = { Value: 'a; Items: 'a list; Count: int }
let make (id: int) (name: string) (tags: string list) : Record0000 =
    { Id = id; Name = name; Tags = tags; Meta = tags |> List.mapi (fun i t -> t, i) |> Map.ofList }
let rec fold (f: 'acc -> 'a -> 'acc) (acc: 'acc) (xs: 'a list) : 'acc =
    match xs with
    | [] -> acc
    | x :: rest -> fold f (f acc x) rest
let wrap (v: 'a) : Wrapper0000<'a> = { Value = v; Items = [v]; Count = 1 }
let records = [ for k in 1..20 -> make k (sprintf "item%d" k) [ "a"; "b"; string k; "v0_2" ] ]
let total = (records |> fold (fun acc r -> acc + r.Meta.Count) 0) + 0
