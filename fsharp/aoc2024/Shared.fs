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