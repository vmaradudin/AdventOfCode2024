namespace ``2024``

open AdventOfCode2024.Day10
open Xunit

module ``Day 10`` =
    let testInput = 
        [|
           "89010123"
           "78121874"
           "87430965"
           "96549874"
           "45678903"
           "32019012"
           "01329801"
           "10456732"
        |]  
    
    [<Fact>]
    let ``Puzzle 1`` () =
        Assert.Equal(36, puzzle1 testInput)
    
    [<Fact>]
    let ``Puzzle 2`` () =
        Assert.Equal(81, puzzle2 testInput)