module AoC2024.Day1.Tests

open System.IO
open NUnit.Framework

[<SetUp>]
let Setup () = ()


let example1 = """3   4
4   3
2   5
1   3
3   9
3   3"""

[<Test>]
let day1Example () =
    let exampleInput = example1.Split("\n")
    Assert.AreEqual(11, day1 (List.ofArray exampleInput))
    ()

[<Test>]
let day1BExample () =
    let exampleInput = example1.Split("\n")
    Assert.AreEqual(31, day1B (List.ofArray exampleInput))
    ()

let input = File.ReadLines "./Day1.txt" |> List.ofSeq

[<Test>]
let day1A () =
    Assert.AreEqual(2192892, day1 input)
    ()

[<Test>]
let day1B () =
    Assert.AreEqual(22962826, day1B input)
    ()    
   