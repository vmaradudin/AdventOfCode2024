namespace ``2024``

open AdventOfCode2024.Day12
open Xunit

module ``Day 12`` =
    let testInput = 
        [|
           "RRRRIICCFF"
           "RRRRIICCCF"
           "VVRRRCCFFF"
           "VVRCCCJFFF"
           "VVVVCJJCFE"
           "VVIVCCJJEE"
           "VVIIICJJEE"
           "MIIIIIJJEE"
           "MIIISIJEEE"
           "MMMISSJEEE"
        |]  
    
    [<Fact>]
    let ``Puzzle 1`` () =
        Assert.Equal(1930, puzzle1 testInput)
    
    [<Fact>]
    let ``Puzzle 2`` () =
        Assert.Equal(1206, puzzle2 testInput)