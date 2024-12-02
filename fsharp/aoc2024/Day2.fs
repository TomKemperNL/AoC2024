module AoC2024.Day2

open Shared

type Report = int list

let isSafe (r: Report) =
    let pairs = List.pairwise r
    let allUp = List.forall (fun (a, b) -> a < b) pairs
    let allDOwn = List.forall (fun (a, b) -> a > b) pairs
    let difOk = List.forall (fun (a, b) -> abs(a - b) <= 3) pairs
    (allUp || allDOwn) && difOk

let day2 (input: string list) =
    input 
    |> List.map (String.split " " >> List.map int)
    |> List.filter isSafe
    |> List.length