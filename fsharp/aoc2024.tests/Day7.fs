﻿module AoC2024.Day7.Tests

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
let canParseInput () =
    let expected = {
        Outcome = 3267
        Parts = [81; 40; 27]
    }
  
    Assert.AreEqual(expected, parse "3267: 81 40 27")
    ()


[<Test>]
let day7Example () =    
    Assert.AreEqual(3749L, day7 (exampleInput.Split("\r\n") |> Array.toList))
    ()

[<Test>]
let day7BExample () =    
    Assert.AreEqual(11387L, day7B (exampleInput.Split("\r\n") |> Array.toList))
    ()


let input = File.ReadAllLines "./Day7.txt" |> Array.toList

[<Test>]
let day7A () =
    Assert.AreEqual(0303766880536L, day7 input)
    ()
    
[<Test>]
let day7B () =
    Assert.AreEqual(337041851384440L, day7B input)
    ()    