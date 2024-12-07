module AoC2024.Day7

open AoC2024.Shared


type Equation = {
    Outcome: int64
    Parts: int64 list
}

type Operator = 
    | Add
    | Times

let parse s =
    match s with
    | ParseRegex "(\\d+): (.*)" [outcome; parts] ->
        { Outcome = int64 outcome; Parts = List.map int64 (String.split " " parts) }
    | _ -> failwith ("Invalid input " + s)

let canSolve eq =    
    let rec trySolve outcome (parts: int64 list) =
        if outcome < 0L then false
        else
        match outcome, parts with
        | 0L, [] -> true
        | _, [] -> false
        | oc, [h1;h2] ->
            oc = h1 + h2 || oc = h1 * h2
        | oc, (h1::(h2::t)) ->
            let add = h1 + h2
            let times = h1 * h2            
            trySolve oc (add::t) || trySolve oc (times::t)
            
    trySolve eq.Outcome eq.Parts

let day7 (input: string list) : int64 =
    let eqs = List.map parse input
    let solvableEqs = List.filter canSolve eqs
    List.map (_.Outcome) solvableEqs |> List.sum