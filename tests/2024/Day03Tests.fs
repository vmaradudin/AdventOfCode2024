namespace ``2024``

open AdventOfCode2024.Day03
open Xunit

module ``Day 03`` =
    let testInput = 
        [|
            "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
        |]  
    let testInput2 = 
        [|
            "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
        |]  
    [<Fact>]
    let ``Puzzle 1`` () =
        Assert.Equal(161L, puzzle1 testInput)
    
    [<Fact>]
    let ``Puzzle 2`` () =
        Assert.Equal(48L, puzzle2 testInput2)      