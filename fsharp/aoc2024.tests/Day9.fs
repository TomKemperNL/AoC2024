module AoC2024.Day9.Tests

open System.IO
open NUnit.Framework


let exampleInput = """2333133121414131402"""

[<Test>]
let day4Example () =    
    Assert.AreEqual(1928, day9 exampleInput)
    ()
    

let input = File.ReadAllText "./Day4.txt"

[<Test>]
let day4A () =
    Assert.AreEqual(0, day9 input)
    ()    