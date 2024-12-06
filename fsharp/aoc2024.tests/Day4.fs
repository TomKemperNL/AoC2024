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
let weirdStuff () =
    let target = "XMAS".ToCharArray()
    let source = "XMASAMX.MM".ToCharArray()
    let found = Array.findPatternIndexes source target
    Assert.AreEqual(0, found[0])
    
[<Test>]
let weirdStuff2 () =
    let target = "XMAS".ToCharArray()
    let source = "....XXMAS.".ToCharArray()
    let found = Array.findPatternIndexes source target
    Assert.AreEqual(5, found[0])
    
[<Test>]
let weirdStuff3 () =
    let target = "XMAS".ToCharArray()
    let source = ".X.X.XMASX".ToCharArray()
    let found = Array.findPatternIndexes source target
    Assert.AreEqual(5, found[0])    
    
[<Test>]
let day4Example () =    
    Assert.AreEqual(18, day4 (exampleInput.Split("\r\n") |> Array.toList))
    ()


let input = File.ReadAllLines "./Day4.txt" |> Array.toList

[<Test>]
let day4A () =
    Assert.AreEqual(0, day4 input)
    ()