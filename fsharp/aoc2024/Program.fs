// For more information see https://aka.ms/fsharp-console-apps

open System.Text.RegularExpressions

let res = Regex("d'").Match("Wat is d'it")
printfn "Test"
printfn $"{res.Success}"
