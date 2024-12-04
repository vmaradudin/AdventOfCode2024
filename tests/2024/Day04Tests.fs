namespace ``2024``

open AdventOfCode2024.Day04
open Xunit

module ``Day 04`` =
    let testInput = 
        [|
            "MMMSXXMASM"
            "MSAMXMSMSA"
            "AMXSXMAAMM"
            "MSAMASMSMX"
            "XMASAMXAMM"
            "XXAMMXXAMA"
            "SMSMSASXSS"
            "SAXAMASAAA"
            "MAMMMXMMMM"
            "MXMXAXMASX"
        |]  
    
    [<Fact>]
    let ``Puzzle 1`` () =
        Assert.Equal(18, puzzle1 testInput)
    
    [<Fact>]
    let ``Puzzle 2`` () =
        Assert.Equal(9, puzzle2 testInput)