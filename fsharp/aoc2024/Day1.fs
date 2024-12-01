module AoC2024.Day1

let computeDistance (l1: int list, l2: int list) =
    let (sls, srs) = (List.sort l1, List.sort l2)
    List.zip sls srs |>
        List.map (fun (a,b) -> a - b |> abs) |> List.sum
    
let day1 (input: string list) =
    let splitLine (line: string) =
        let parts = line.Split("   ")
        (int parts.[0], int parts.[1])
    let pairs = List.map splitLine input
    
    let toLists (ls, rs) (l,r) =
        l:: ls, r :: rs
    
    List.fold toLists ([], []) pairs |>
        computeDistance
    
    
