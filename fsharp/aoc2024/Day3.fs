module AoC2024.Day3

open AoC2024.Shared


type Operation = Mul of int * int

let rec parseOperation (s: string) =
    match s with
    | "" -> []
    | ParseRegex "^mul\((\d+),(\d+)\)" [ a; b ] ->
        Mul(int a, int b) :: (parseOperation (s.Substring(1)))
    | _ ->
        (parseOperation (s.Substring(1)))

let executeOperation (o: Operation) =
    match o with
    | Mul(a, b) -> a * b

let day3 (input: string) =
    parseOperation input |> List.map executeOperation |> List.sum
