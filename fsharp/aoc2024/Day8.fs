module AoC2024.Day8

open AoC2024.Shared

type Cell =
    | Empty
    | Content of char

let parseLine (s: string) : Cell array =
    let rec parseRec charList =
        match charList with
        | [] -> []
        | h::t ->
            match h with
            | '.' -> Empty :: (parseRec t)
            | '#' -> Empty :: (parseRec t) //To more easily parse the examples
            | c -> Content c :: (parseRec t)
    
    s.ToCharArray() |> Array.toList |> parseRec |> List.toArray           

let antinodeA g (x1, y1) (x2,y2) =
    let xa = x1 - (x2 - x1)
    let ya = y1 - (y2 - y1)
    match Grid.cell g xa ya with
    | None -> []
    | Some _ -> [(xa,ya)]

let antinodeB g (x1, y1) (x2,y2) =
    let stepX = (x2 - x1)
    let stepY = (y2 - y1)
    
    let hasItem (x,y) =
        Grid.cell g x y <> None
    
    seq {
        let mutable step = 0
        while true do            
            yield x1 - (step * stepX), y1 - (step * stepY)
            step <- step + 1
    } |> Seq.takeWhile hasItem |> Seq.toList
    
let antinodes antinodeF g antennas =
    let antennasCoords = List.map fst antennas
    let pairs = List.allPairs antennasCoords antennasCoords |> List.filter (fun (a,b) -> a <> b)
    List.map (fun (a,b) -> antinodeF g a b) pairs |> List.concat

let private day8Priv antinodeF (input: string list) =
    let grid : Grid<Cell> = List.map parseLine input |> List.toArray
    let isFilled (coord: int*int) (item: Cell) = 
        match item with
        | Empty -> false
        | Content _ -> true
        
    let antennas = Grid.filter grid isFilled 
    let antennasBySymbol = List.groupBy snd antennas
    let antinodes = List.map (fun (s, antennas) -> antinodes antinodeF grid antennas) antennasBySymbol
    
    antinodes |> List.collect id |> List.distinct |> List.length

let day8 (input: string list) =
    day8Priv antinodeA input

let day8B input =
   day8Priv antinodeB input