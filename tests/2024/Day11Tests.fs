namespace ``2024``

open AdventOfCode2024.Day11
open Xunit

module ``Day 11`` =
    let testInput = 
        [|
           "125 17"
        |]  
    
    [<Fact>]
    let ``Puzzle 1`` () =
        Assert.Equal(55312L, puzzle1 testInput)

    [<Fact>]
    let ``Puzzle 2`` () =
        Assert.Equal(65601038650482L, puzzle2 testInput)