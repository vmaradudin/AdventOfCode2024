namespace ``2024``

open AdventOfCode2024.Day01
open Xunit

module ``Day 01``=
    let testInput1 = 
        [|
          "3   4"
          "4   3"
          "2   5"
          "1   3"
          "3   9"
          "3   3"
        |]  
    
    [<Fact>]
    let ``Puzzle 1`` () =
        Assert.Equal(11, puzzle1 testInput1)
    
    [<Fact>]
    let ``Puzzle 2`` () =
        Assert.Equal(31, puzzle2 testInput1)      