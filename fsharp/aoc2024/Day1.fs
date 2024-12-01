module AoC2024.Day1

let computeDistance (l1: int list, l2: int list) =
    let (sls, srs) = (List.sort l1, List.sort l2)
    List.zip sls srs |> List.map (fun (a, b) -> a - b |> abs) |> List.sum

let computeSimilarity (l1: int list, l2: int list) =
    let lookupL = List.countBy id l1
    let lookupR = List.countBy id l2 |> Map.ofList

    List.sumBy
        (fun (key, occ) ->
            match lookupR.TryGetValue key with
            | true, v -> occ * key * v
            | _ -> 0)
        lookupL

let private splitLine (line: string) =
    let parts = line.Split("   ")
    (int parts.[0], int parts.[1])

let day1 (input: string list) =
    List.map splitLine input |> List.unzip |> computeDistance

let day1B (input: string list) =
    List.map splitLine input |> List.unzip |> computeSimilarity
