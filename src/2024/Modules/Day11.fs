namespace AdventOfCode2024

open Common.Types

module Day11 =
    let getStones (input:string array) =
        input |> Array.collect (fun a -> a.Split " " |> Array.map int64 |> Array.map (fun a -> (a, 1L)))

    let changeStone (number, count) =
        match number, (number|>string) with
        | 0L, _ -> [|1L, count|]
        | _, stringNumber when stringNumber.Length % 2 = 0 -> 
            [|
                (stringNumber.Substring(0,stringNumber.Length/2) |> int64),count
                (stringNumber.Substring(stringNumber.Length/2) |>int64),count
            |]
        | _ -> [|number*2024L, count|]

    let rec blink times (stones:(int64*int64) array)  =
        match times with
        | 0 -> stones |> Array.sumBy snd
        |_ ->  
            stones |> Array.collect changeStone |> Array.groupBy fst |> Array.map (fun (number, stones) -> number, stones |> Array.sumBy snd) 
            |> blink (times - 1)

    let puzzle1 input = input |> getStones |> blink 25

    let puzzle2 input = input |> getStones |> blink 75

    let Solution = (new Solution(11, puzzle1, puzzle2) :> ISolution).Execute