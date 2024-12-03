module AoC2024.Day3.Tests

open System.IO
open NUnit.Framework

[<SetUp>]
let Setup () = ()


let exampleInput = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

[<Test>]
let day3Example () =    
    Assert.AreEqual(161, day3 exampleInput)
    ()
    
let exampleInputB = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"    
[<Test>]
let day3BExample () =    
    Assert.AreEqual(48, day3b exampleInputB)
    ()    
    
let input = File.ReadAllText "./Day3.txt"

[<Test>]
let day3A () =
    Assert.AreEqual(159833790, day3 input)
    ()
    
[<Test>]
let day3B () =
    Assert.AreEqual(89349241, day3b input)
    ()        