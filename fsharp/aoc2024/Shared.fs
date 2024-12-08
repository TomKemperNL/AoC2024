module AoC2024.Shared

open System.Text.RegularExpressions

// ParseRegex parses a regular expression and returns a list of the strings that match each group in
// the regular expression.
// List.tail is called to eliminate the first element in the list, which is the full matched expression,
// since only the matches for each group are wanted.
let (|ParseRegex|_|) regex str =
    let m = Regex(regex).Match(str)

    if m.Success then
        Some(List.tail [ for x in m.Groups -> x.Value ])
    else
        None

module Map =
    let findOrElse ifNone key map =
        match Map.tryFind key map with
        | None -> ifNone
        | Some item -> item
        
module Pair =
    let map f (a, b) = (f a, f b)
    
    let curry f (a,b) = f a b
    
module String =
    let split2 (pattern:string) (s: string) =
        let parts = s.Split pattern
        (parts.[0], parts.[1])
        
    let split (pattern:string) (s: string) =
        let parts = s.Split pattern
        List.ofArray parts
        
module List =
     //Gejat van https://dev.to/ducaale/computing-permutations-of-a-list-in-f-1n6k
    //(aangezien dit toch niet de echte oplossing gaat worden)
    let variants list =
        let rec inserts e = function
        | [] -> [[e]]
        | x::xs as list -> (e::list)::(inserts e xs |> List.map (fun xs' -> x::xs'))
        List.fold (fun accum x -> List.collect (inserts x) accum) [[]] list
        


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
        
    let filter (g: Grid<'T>) (f: (int*int) -> 'T -> bool) : ((int*int)*'T) list=
        seq {
            for x in 0 .. (g.Length - 1) do
                for y in 0 .. (g.Length - 1) do
                    let content = g.[y].[x]
                    if f (x,y) content then
                        yield (x,y), content
        } |> Seq.toList
        
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
    
           