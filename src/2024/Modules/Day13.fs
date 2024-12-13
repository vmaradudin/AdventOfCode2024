namespace AdventOfCode2024

open System.Text.RegularExpressions
open Common.Types

module Day13 =
    let regex = new Regex(@"X[\+=](?<x>\d+), Y[\+=](?<y>\d+)")
    let setUp additionalDistance (input:string array) = 
        input |> Array.chunkBySize 4 |> Array.map (fun s -> 
            let a = s|> Array.take 3 |> Array.map (fun b -> b |> regex.Match |> fun c -> (int64 c.Groups["x"].Value, int64 c.Groups["y"].Value))
            (a[0], a[1], (fst a[2] + additionalDistance, snd a[2] + additionalDistance)))
    
    let solve ((ax,ay), (bx, by), (px, py)) =
        match ((bx*py - by*px) / (ay*bx - ax*by), (bx*py - by*px) % (ay*bx - ax*by)) with
        | (a, 0L) -> Some(3L * a + (px - ax*a) / bx)
        |_ -> None
        |> Option.defaultValue 0L

    let puzzle1 input = 
        input |> setUp 0L|> Array.sumBy solve
        
    let puzzle2 input = 
        input |> setUp 10000000000000L|> Array.sumBy solve

    let Solution = (new Solution(13, puzzle1, puzzle2) :> ISolution).Execute