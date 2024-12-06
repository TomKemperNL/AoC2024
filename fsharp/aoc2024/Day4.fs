module AoC2024.Day4

open System
open Shared

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
   
    let findCoord (g: Grid<'T>) (f: (int*int) -> 'T -> bool) =
        seq {
            for x in 0 .. (g.Length - 1) do
                for y in 0 .. (g.Length - 1) do
                    if f (x,y) (g.[y].[x]) then
                        yield (x, y)
        }
            
    let diagonalLR (g: Grid<'T>) xoffset =
        seq {
            for i in 0 .. (g.Length - 1) do                
                yield cell g (i + xoffset) i
        } |> Seq.choose id |> Seq.toArray
    
    let diagonalsLR (g: Grid<'T>) =
        let offSet = g.Length - 1
        seq { for xoffset in (-1 * offSet)..offSet -> diagonalLR g xoffset} |> Seq.toArray
    
    let diagonalRL (g: Grid<'T>) xoffset =
        seq {
            for i in 0 .. (g.Length - 1)  do                
                yield cell g ((-1 * i) + xoffset) i
        } |> Seq.choose id |> Seq.toArray
    
    let diagonalsRL (g: Grid<'T>) =
        let maxOffSet = (2 * g.Length) - 1
        seq { for xoffset in 0..maxOffSet -> diagonalRL g xoffset} |> Seq.toArray
    
        

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
                    if h = List.head pattern then
                        findInList t pattern (List.tail pattern) (ixc+1) ixc found
                    else 
                        findInList t pattern pattern (ixc+1) (ixc+1) found
                    
        findInList sourceAsList patternAsList patternAsList 0 0 [] |> List.rev

let day4 input =
    let grid = parse input
    let rows = Grid.rows grid
    let columns = Grid.columns grid
    let diagonalsLR = Grid.diagonalsLR grid
    let diagonalsRL = Grid.diagonalsRL grid
    
    let xmasFoundRow = Array.map (fun s -> Array.findPatternIndexes s ("XMAS".ToCharArray())) rows |> Array.sumBy (List.length)
    let xmasFoundCol = Array.map (fun s -> Array.findPatternIndexes s ("XMAS".ToCharArray())) columns |> Array.sumBy (List.length)
    let xmasFoundDiag = Array.map (fun s -> Array.findPatternIndexes s ("XMAS".ToCharArray())) diagonalsLR |> Array.sumBy (List.length)
    let xmasFoundDiag2 = Array.map (fun s -> Array.findPatternIndexes s ("XMAS".ToCharArray())) diagonalsRL |> Array.sumBy (List.length)
    let samxFoundRow = Array.map (fun s -> Array.findPatternIndexes s ("SAMX".ToCharArray())) rows |> Array.sumBy (List.length)        
    let samxFoundCol = Array.map (fun s -> Array.findPatternIndexes s ("SAMX".ToCharArray())) columns |> Array.sumBy (List.length)
    let samxFoundDiag = Array.map (fun s -> Array.findPatternIndexes s ("SAMX".ToCharArray())) diagonalsLR |> Array.sumBy (List.length)
    let samxFoundDiag2 = Array.map (fun s -> Array.findPatternIndexes s ("SAMX".ToCharArray())) diagonalsRL |> Array.sumBy (List.length)
    
    let xmasses = xmasFoundRow + xmasFoundCol + xmasFoundDiag + xmasFoundDiag2
    let samxes =  samxFoundRow + samxFoundCol + samxFoundDiag + samxFoundDiag2
    xmasses + samxes
   
let isXmas (g: Grid<Char>) (x, y) c=
    match c with
    | 'A' -> 
        let neighbours = [(x - 1, y - 1); (x - 1, y + 1); (x + 1, y + 1); (x + 1, y - 1)]
        let neighbourChars = List.map (fun (x,y) -> Grid.cell g x y) neighbours |> List.choose id
        match neighbourChars with
        | ['M'; 'M'; 'S'; 'S'] -> true
        | ['S'; 'M'; 'M'; 'S'] -> true
        | ['M'; 'S'; 'S'; 'M'] -> true
        | ['S'; 'S'; 'M'; 'M'] -> true
        | _ -> false
    | _ -> false
       
let day4B input =
    let grid = parse input
    let checkXMas = isXmas grid
    Grid.findCoord grid checkXMas |> Seq.length 