module AoC2024.Day4

open System

type Grid<'T> = 'T[][]

module Grid =
    let row (g: Grid<'T>) (ix: int) =
        g[ix]
        
    let rows (g: Grid<'T>) =
        g

    let column (g: Grid<'T>) (ix: int) =
        Array.map (fun (a: 'T[]) -> a[ix]) g
    
    let columns (g: Grid<'T>) =
        seq {
            for i in 0 .. (g.Length - 1) -> column g i            
        } |> Seq.toArray
    
    let cell (g: Grid<'T>) x y =
        let max = g.Length
        if (0 <= y && y < max) && (0 <= x && x < max) then
            Some (g[y][x])
        else None
            
    let diagonal (g: Grid<'T>) xoffset =
        seq {
            for i in 0 .. (g.Length - 1) do                
                yield cell g (i + xoffset) i
        } |> Seq.choose id |> Seq.toArray
    
    let diagonals (g: Grid<'T>) =
        let offSet = g.Length - 1
        seq { for xoffset in (-1 * offSet)..offSet -> diagonal g xoffset} |> Seq.toArray
        

let parse (lines: string list) : Grid<Char> =
    List.map (fun (s: string) -> s.ToCharArray()) lines |> List.toArray

module Array =
    let findPatternIndexes array pattern =
        let sourceAsList = List.ofArray array
        let patternAsList = List.ofArray pattern
        let rec findInList lst pattern patrec ixc ixf found =
            match lst with
            | [] ->
                match patrec with
                | [] -> ixf :: found
                | _ -> found
            | (h :: t) ->
                match patrec with
                | [] ->
                    findInList (h::t) pattern pattern ixc ixc (ixf :: found)
                | (ph :: pt) when h = ph ->
                    findInList t pattern pt (ixc+1) ixf found
                | _ ->
                    findInList t pattern pattern (ixc+1) (ixc+1) found
        findInList sourceAsList patternAsList patternAsList 0 0 [] |> List.rev

let day4 input =
    let grid = parse input
    let patterns = ["XMAS"; "SAMX"]
    let rows = Grid.rows grid
    let columns = Grid.columns grid
    let diagonals = Grid.diagonals grid
    
    let sources = Array.collect id [|rows;columns;diagonals|] |> Array.toList
    let xmasses = List.map (fun s -> Array.findPatternIndexes s ("XMAS".ToCharArray())) sources |> List.concat |> List.length
    let samxes = List.map (fun s -> Array.findPatternIndexes s ("SAMX".ToCharArray())) sources |> List.concat |> List.length
    xmasses + samxes
    
       
    