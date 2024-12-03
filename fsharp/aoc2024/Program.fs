// For more information see https://aka.ms/fsharp-console-apps

open System.Text.RegularExpressions

let res = Regex("regex").Match("regex doet dingen")
printfn "Test"
printfn $"{res.Success}"
