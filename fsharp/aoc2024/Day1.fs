module AoC2024.Day1

open Shared

let computeSimilarity (l1: int list, l2: int list) =
    let lookupL = List.countBy id l1
    let lookupR = List.countBy id l2 |> Map.ofList
    List.sumBy
        (fun (key, occ) ->
            match lookupR.TryGetValue key with
            | true, v -> occ * key * v
            | _ -> 0)
        lookupL

let day1 (input: string list) =
    input
    |> List.map (String.split2 "   " >> Pair.map int)    
    |> List.unzip
    |> Pair.map List.sort
    |> Pair.curry List.zip
    |> List.map (Pair.curry (-) >> abs)
    |> List.sum

let day1B (input: string list) =
    List.map (String.split2 "   " >> Pair.map int) input    
    |> List.unzip
    |> computeSimilarity
