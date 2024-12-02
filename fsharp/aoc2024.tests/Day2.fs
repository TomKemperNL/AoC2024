module AoC2024.Day2.Tests


open System.IO
open NUnit.Framework

[<SetUp>]
let Setup () = ()


let example1 = """7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9"""

[<Test>]
let day1Example () =
    let exampleInput = example1.Split("\n")
    Assert.AreEqual(2, day2 (List.ofArray exampleInput))
    ()

let input = File.ReadLines "./Day2.txt" |> List.ofSeq

[<Test>]
let day2A () =
    Assert.AreEqual(0, day2 input)
    ()    