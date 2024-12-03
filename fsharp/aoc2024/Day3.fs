module AoC2024.Day3

open AoC2024.Shared


type Operation =
    | Mul of int * int

type Token =
    | Operation of Operation
    | Do
    | Dont

let rec parseOperation (s: string) =
    match s with
    | "" -> []
    | ParseRegex "^mul\((\d+),(\d+)\)" [ a; b ] ->
        Mul(int a, int b) :: (parseOperation (s.Substring(1)))
    | _ ->
        (parseOperation (s.Substring(1)))
        
let rec parseOperationB (s: string) =
    match s with
    | "" -> []
    | ParseRegex "^mul\((\d+),(\d+)\)" [ a; b ] ->
        (Operation (Mul(int a, int b))) :: (parseOperationB (s.Substring(1)))
    | ParseRegex "^do\(\)" [] ->
        Do :: (parseOperationB (s.Substring(1)))
    | ParseRegex "^don't\(\)" [] ->
        Dont :: (parseOperationB (s.Substring(1)))
    | _ ->
        (parseOperationB (s.Substring(1)))        

let executeOperation (o: Operation) =
    match o with
    | Mul(a, b) -> a * b
    
let executeOperationB (curr, sum) next =
    match (curr, next) with
    | _, Do -> (Do, sum)
    | _, Dont -> (Dont, sum)
    | Do, Operation (Mul (a,b)) -> (Do, sum + a * b)
    | Dont, Operation (Mul _) -> (Dont, sum)

let day3 (input: string) =
    parseOperation input |> List.map executeOperation |> List.sum

let day3b (input: string) =
    let ops = parseOperationB input 
    ops |> List.fold executeOperationB (Do, 0) |> snd