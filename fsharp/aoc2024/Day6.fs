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
        
    let variant m (obsX, obsY) =
        let copy = Array.map (Array.copy) m
        copy.[obsY].[obsX] <- Obstacle
        copy

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
            Some ((nx, ny), g)
        | Some Obstacle ->
            Some ((x,y), rotate g)
        | Some (Guard _) ->
            Some ((nx, ny), g) //Treat startposition as empty
        
    let rec walk m (x,y) g =
        match step m (x,y) g with
        | None -> []
        | Some ((nx, ny), ng) -> (nx, ny) :: walk m (nx, ny) ng
    
    
    let willWalkIntoVisited map visited (x, y) g =
        let visitedSet = Set.ofList visited
        
        let rec willWalkIntoVisited (x, y) g steps =
            if steps > 500 then
                false
            else
                match step map (x,y) g with
                | None -> false
                | Some ((nx, ny), ng) ->
                    if Set.contains ((nx, ny), ng) visitedSet then
                        true
                    else
                        willWalkIntoVisited (nx, ny) ng (steps + 1)
        willWalkIntoVisited (x, y) g 0
    
    let walkB m (x, y) g =
        let rec walkBRec visited obstacles (x, y) g =           
            match step m (x,y) g with
            | None -> visited, obstacles
            | Some ((nx, ny), ng) ->
                let dirIfObstactle = rotate g                
                match step m (x,y) (dirIfObstactle) with
                | None ->
                    let newVisited = ((nx, ny), ng) :: visited
                    walkBRec newVisited obstacles (nx, ny) ng
                | Some ((nextIfObstacleX, nextIfObstacleY), nextIfObstacleDir) ->
                    let variant = Map.variant m (nx, ny)                    
                    if willWalkIntoVisited variant visited (x,y) g then
                        let newVisited = ((nx, ny), ng) :: visited
                        let newObstacles = (nx, ny) :: obstacles
                        walkBRec newVisited newObstacles (nx, ny) ng
                    else
                        let newVisited = ((nx, ny), ng) :: visited
                        walkBRec newVisited obstacles (nx, ny) ng
    
        walkBRec [((x,y),g)] [] (x,y) g
    
let day6 (input: string list) =
    let map = parse input
    let guard = Map.findGuard map
    
    let visited = Guard.walk map guard Guard.Up
    let visited = (guard) :: visited
    List.distinct visited |> List.length
    
let day6B (input: string list) =
    let map = parse input
    let guardX, guardY = Map.findGuard map
    let (visited, obstacles) = Guard.walkB map (guardX, guardY) Guard.Up    
    let visitedLengthToCheck = List.map fst visited |> List.distinct |> List.length
    List.length obstacles
    
    