﻿module AoC2024.Day6.Tests

open System.IO
open NUnit.Framework

let exampleInput = """....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."""

[<Test>]
let day6Example () =    
    Assert.AreEqual(41, day6 (exampleInput.Split("\r\n") |> Array.toList))
    ()
    

let input = File.ReadAllLines "./Day6.txt" |> Array.toList

[<Test>]
let day6A () =
    Assert.AreEqual(5145, day6 input)
    ()
    
[<Test>]
let day6B () =
    Assert.AreEqual(6, day6B input)
    ()    