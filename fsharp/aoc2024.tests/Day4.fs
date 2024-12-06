module AoC2024.Day4.Tests

open System.IO
open NUnit.Framework


[<SetUp>]
let Setup () = ()


let exampleInput = """MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"""

let miniGrid = [|
    [| 0; 1 |]
    [| 2; 3 |]
|]

[<Test>]
let computeDiagonals () =
    let ds = Grid.diagonals miniGrid
    Assert.AreEqual(ds[0],[| 2 |])
    Assert.AreEqual(ds[1],[| 0;3 |])
    Assert.AreEqual(ds[2],[| 1 |])
    
[<Test>]
let computeColumns () =
    let cs = Grid.columns miniGrid
    Assert.AreEqual(cs[0],[| 0;2 |])
    Assert.AreEqual(cs[1],[| 1;3 |])
    
[<Test>]
let findInArrays () =
    let source = [|0;1;2;3;0;1;2;3|]
    let found = Array.findPatternIndexes source [|1;2;3|] 
    Assert.AreEqual([|1;5|], found |> List.toArray)    


[<Test>]
let day4Example () =    
    Assert.AreEqual(18, day4 (exampleInput.Split("\r\n") |> Array.toList))
    ()


let input = File.ReadAllLines "./Day4.txt" |> Array.toList

[<Test>]
let day4A () =
    Assert.AreEqual(0, day4 input)
    ()