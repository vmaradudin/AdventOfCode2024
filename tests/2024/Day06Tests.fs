namespace ``2024``

open AdventOfCode2024.Day06
open Xunit

module ``Day 06`` =
    let testInput = 
        [|
           "....#....."
           ".........#"
           ".........."
           "..#......."
           ".......#.."
           ".........."
           ".#..^....."
           "........#."
           "#........."
           "......#..."
        |]  
    
    [<Fact>]
    let ``Puzzle 1`` () =
        Assert.Equal(41, puzzle1 testInput)
    
    [<Fact>]
    let ``Puzzle 2`` () =
        Assert.Equal(6, puzzle2 testInput)