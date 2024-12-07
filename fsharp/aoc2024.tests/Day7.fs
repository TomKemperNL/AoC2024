module AoC2024.Day7.Tests

open System.IO
open NUnit.Framework


let exampleInput = """190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"""

[<Test>]
let day7Example () =    
    Assert.AreEqual(3749, day7 (exampleInput.Split("\r\n") |> Array.toList))
    ()


let input = File.ReadAllLines "./Day7.txt" |> Array.toList

// [<Test>]
let day7A () =
    Assert.AreEqual(0, day7 input)
    ()