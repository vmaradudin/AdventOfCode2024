namespace ``2024``

open AdventOfCode2024.Day09
open Xunit

module ``Day 09`` =
    let testInput = 
        [|
           "2333133121414131402"
        |]  
    
    [<Fact>]
    let ``Puzzle 1`` () =
        Assert.Equal(1928L, puzzle1 testInput)
    
    [<Fact>]
    let ``Puzzle 2`` () =
        Assert.Equal(2858L, puzzle2 testInput)