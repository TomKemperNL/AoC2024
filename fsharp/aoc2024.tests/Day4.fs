module AoC2024.Day4.Tests

open System.IO
open NUnit.Framework


[<SetUp>]
let Setup () = ()




let miniGrid = [|
    [| 0; 1 |]
    [| 2; 3 |]
|]

[<Test>]
let computeDiagonalsLR () =
    let ds = Grid.diagonalsLR miniGrid
    Assert.AreEqual(ds[0],[| 2 |])
    Assert.AreEqual(ds[1],[| 0;3 |])
    Assert.AreEqual(ds[2],[| 1 |])
    
[<Test>]
let computeDiagonalsRL () =
    let ds = Grid.diagonalsRL miniGrid
    Assert.AreEqual(ds[0],[| 0 |])
    Assert.AreEqual(ds[1],[| 1;2 |])
    Assert.AreEqual(ds[2],[| 3 |])
    
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
    let target = "XMAS".ToCharArray()
    let source = "XMASAMX.MM".ToCharArray()
    let found = Array.findPatternIndexes source target
    Assert.AreEqual(0, found[0])

[<TestCase("XMASAMX.MM", "XMAS", [|0|])>]
[<TestCase("....XXMAS.", "XMAS", [|5|])>]
[<TestCase(".X.X.XMASX", "XMAS", [|5|])>]
[<TestCase("XXXMASX", "XMAS", [|2|])>]
[<TestCase("XXXMASXMAS", "XMAS", [|2;6|])>]
let weirdStuff (s: string, t:string, e: int array) =
    let target = t.ToCharArray()
    let source = s.ToCharArray()
    let found = Array.findPatternIndexes source target
    Assert.AreEqual(e, List.toArray found)

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

[<Test>]
let day4Example () =    
    Assert.AreEqual(18, day4 (exampleInput.Split("\r\n") |> Array.toList))
    ()

[<Test>]
let day4BExample () =    
    Assert.AreEqual(9, day4B (exampleInput.Split("\r\n") |> Array.toList))
    ()

let input = File.ReadAllLines "./Day4.txt" |> Array.toList

[<Test>]
let day4A () =
    Assert.AreEqual(2618, day4 input)
    ()
    
[<Test>]
let day4B () =
    Assert.AreEqual(2011, day4B input)
    ()    