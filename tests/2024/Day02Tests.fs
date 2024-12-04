namespace ``2024``

open AdventOfCode2024.Day02
open Xunit

module ``Day 02``=
    let testInput = 
        [|
            "7 6 4 2 1"
            "1 2 7 8 9"
            "9 7 6 2 1"
            "1 3 2 4 5"
            "8 6 4 4 1"
            "1 3 6 7 9"
        |]  
    
    [<Fact>]
    let ``Puzzle 1`` () =
        Assert.Equal(2, puzzle1 testInput)
    
    [<Fact>]
    let ``Puzzle 2`` () =
        Assert.Equal(4, puzzle2 testInput)