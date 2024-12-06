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
            Some ((nx, ny), g)
        | Some Obstacle ->
            Some ((x,y), rotate g)
        | Some (Guard _) ->
            Some ((nx, ny), g) //Treat startposition as empty
        
    let rec walk m (x,y) g =
        match step m (x,y) g with
        | None -> []
        | Some ((nx, ny), ng) -> (nx, ny) :: walk m (nx, ny) ng
    
    let rec walkB m (x,y) g =
        match step m (x,y) g with
        | None -> []
        | Some ((nx, ny), ng) -> (nx, ny, ng) :: walkB m (nx, ny) ng

    let obstacleCandidate map visitedWithDir ((fromX, fromY, fromDir), (toX, toY))=
        let nextDir = rotate fromDir
        let nextX, nextY = next (fromX, fromY) nextDir        
        List.contains (nextX, nextY, nextDir) visitedWithDir && Map.cell map toX toY = Some Empty

let day6 (input: string list) =
    let map = parse input
    let guard = Map.findGuard map
    
    let visited = Guard.walk map guard Guard.Up
    let visited = (guard) :: visited
    List.distinct visited |> List.length
    
let day6B (input: string list) =
    let map = parse input
    let guardX, guardY = Map.findGuard map
    let visitedWithDirection = Guard.walkB map (guardX, guardY) Guard.Up
    let visitedWithDirection = (guardX, guardY, Guard.Up) :: visitedWithDirection
    
    let obstacleCandidates = List.map (fun (x, y, g) -> ((x, y, g), Guard.next (x,y) g)) visitedWithDirection
    let viableCandidates = List.filter (Guard.obstacleCandidate map visitedWithDirection) obstacleCandidates
    let viableCoordinates = List.map snd viableCandidates
    List.distinct viableCoordinates |> List.length
    