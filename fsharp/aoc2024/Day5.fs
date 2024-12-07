module AoC2024.Day5

open Shared

type OrderRule = int * int
type Update = int list

module OrderRule =
    let satisfies rule update =
        let minor, major = rule
        match List.tryFindIndex ((=) minor) update with
        | Some ix ->
            match List.tryFindIndex ((=) major) update with
            | Some ix2 -> ix < ix2
            | None -> true
        | None -> true
    
    let reorder (rules: OrderRule list) (update: Update) : Update =
       
        
        let allVariants = List.variants update
        List.find (fun perm -> List.forall (fun r -> satisfies r perm) rules) allVariants

let parse (input: string list) =
    let rules, updates = input 
                         |> List.filter (fun (s: string) -> s.Length > 0)
                         |> List.partition (fun s -> s.Contains "|")
    let rules = rules |> List.map (fun s -> s.Split "|" |> Array.map int |> (fun [|a;b|] -> a, b))
    let updates = updates |> List.map (fun s -> s.Split "," |> Array.map int |> Array.toList)
    
    (rules, updates)

let day5 (input: string list) =
    let rules, updates = parse input
    let validUpdates = updates
                       |> List.filter (fun u -> rules |> List.forall (fun r -> OrderRule.satisfies r u))
    let middles = List.map (fun u -> List.item (List.length u / 2) u) validUpdates
    List.sum middles
    
let day5B (input: string list) =
    let rules, updates = parse input
    let invalidUpdates = updates
                       |> List.filter (fun u -> not <| List.forall (fun r -> OrderRule.satisfies r u) rules )
    let reorderdUpdates = List.map (OrderRule.reorder rules) invalidUpdates
    let middles = List.map (fun u -> List.item (List.length u / 2) u) reorderdUpdates
    List.sum middles