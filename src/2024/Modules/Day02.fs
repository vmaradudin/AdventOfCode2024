namespace AdventOfCode2024

open Common.Types
open System.Text.RegularExpressions

module Day02 =

    let check (levels: int array) =
        let diffs = levels |> Array.windowed 2 |> Array.map (fun a -> a[1] - a[0]) 
        let goodDecreasing = diffs |> Array.sumBy(fun a -> if a >= -3 && a<0 then 1 else 0)
        let goodIncreasing = diffs |> Array.sumBy(fun a -> if a > 0 && a<4 then 1 else 0)
        if goodDecreasing= diffs.Length ||  goodIncreasing = diffs.Length then 1 else 0

    let tolerantCheck (levels: int array) =
        let tolerantCandidates = [|0..levels.Length-1|] |> Array.map (fun i -> levels|> Array.removeAt i)
        if tolerantCandidates |> Array.exists (fun a -> check a = 1) then 1 else 0
    
    let puzzle1 (input:string array) = 
        input |> Array.map (fun a -> a.Split(" ")|> Array.map int) |> Array.map (fun a -> check a) |> Array.sum

    let puzzle2 (input:string array) = 
        input |> Array.map (fun a -> a.Split(" ")|> Array.map int) |> Array.map (fun a -> tolerantCheck a) |> Array.sum

    let Solution = (new Solution(2, puzzle1, puzzle2) :> ISolution).Execute