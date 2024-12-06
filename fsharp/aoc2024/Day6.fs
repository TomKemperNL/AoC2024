module AoC2024.Day6

type Guard =
    | Up
    | Down
    | Left
    | Right

type Item =
    | Guard of Guard
    | Obstacle
    | Empty

type Map = Item[][]

module Map =
    let cell (m: Map) x y =
        let max = m.Length
        if (0 <= y && y < max) && (0 <= x && x < max) then
            Some (m[y][x])
        else None

    let find (m: Map) pred=
        seq {
            for x in 0 .. (m.Length - 1) do
                for y in 0 .. (m.Length - 1) do
                    if pred (x,y) then
                        yield (x, y)
        } |> Seq.head
            
    let findGuard (m: Map) : int*int =
        let hasGuard (x,y) : bool =
            match m.[y].[x] with
            | Guard _ -> true
            | _ -> false        
        find m hasGuard

let parse (lines: string list) : Map =    
    let charToItem c = 
        match c with
            | '.' -> Empty
            | '^' -> Guard Up
            | 'v' -> Guard Down
            | '<' -> Guard Left
            | '>' -> Guard Right
            | '#' -> Obstacle
            | _ -> failwith $"Unknown character: %c{c}"
    let lineToItemArray (s: string) =
         s.ToCharArray() |> Array.map charToItem         
    List.map lineToItemArray lines |> List.toArray

module Guard =    
    let rotate (g: Guard) =
        match g with
        | Up -> Right
        | Right -> Down
        | Down -> Left
        | Left -> Up
        
    let next (x,y) (g: Guard) = 
        match g with
        | Up -> (x, y - 1)
        | Down -> (x, y + 1)
        | Left -> (x - 1, y)
        | Right -> (x + 1, y)
    
    let step (map: Map) (x,y) (g: Guard) =
        let nx, ny = next (x,y) g
        match Map.cell map nx ny  with
        | None -> None
        | Some Empty ->
            Some (map, (nx, ny), g)
        | Some Obstacle ->
            Some (map, (x,y), rotate g)
        | Some (Guard _) ->
            Some (map, (nx, ny), g) //Treat startposition as empty
        
    let rec walk m (x,y) g =
        match step m (x,y) g with
        | None -> []
        | Some (m', (x', y'), g') -> (x', y') :: walk m' (x', y') g'
    

let day6 (input: string list) =
    let map = parse input
    let guard = Map.findGuard map
    
    let visited = Guard.walk map guard Guard.Up
    let visited = (guard) :: visited
    List.distinct visited |> List.length
    
let day6B (input: string list) =
    let map = parse input
    let guard = Map.findGuard map
    42 //Geeen idee nog