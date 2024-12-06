module AoC2024.Day5.Tests

open System.IO
open NUnit.Framework


let exampleInput = """47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"""

[<Test>]
let day5Example () =
    let exampleInput = exampleInput.Split("\r\n")
    Assert.AreEqual(143, day5 (List.ofArray exampleInput))
    ()

[<Test>]
let day5BExample () =
    let exampleInput = exampleInput.Split("\r\n")
    Assert.AreEqual(123, day5B (List.ofArray exampleInput))
    ()    

let input = File.ReadAllLines "./Day5.txt" |> Array.toList

[<Test>]
let day5A () =
    Assert.AreEqual(4959, day5 input)
    ()
    
// [<Test>]
let day5B () =
    Assert.AreEqual(0, day5B input)
    ()    