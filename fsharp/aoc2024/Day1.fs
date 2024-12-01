module AoC2024.Day1

let computeDistance (l1: int list, l2: int list) =
    let (sls, srs) = (List.sort l1, List.sort l2)
    List.zip sls srs |>
        List.map (fun (a,b) -> a - b |> abs) |> List.sum

let computeSimilarity (l1: int list, l2: int list) =
    let keys = l1
    let lookup = List.countBy id l2 |> Map.ofList
    
    List.sumBy (fun k -> 
        match lookup.TryGetValue k with
        | true, v -> v * k
        | _ -> 0) keys
    
let day1 (input: string list) =
    let splitLine (line: string) =
        let parts = line.Split("   ")
        (int parts.[0], int parts.[1])
    
    List.map splitLine input |>
        List.unzip |>        
        computeDistance
    
    
let day1B (input: string list) =
    let splitLine (line: string) =
        let parts = line.Split("   ")
        (int parts.[0], int parts.[1])
    
    List.map splitLine input |>
        List.unzip |>        
        computeSimilarity