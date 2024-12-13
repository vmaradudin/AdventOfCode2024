namespace AdventOfCode2024

open Common.Types
open System.Text.RegularExpressions

module Day05 =
    let sequenced (pages:int array) = 
        [|0..pages.Length-2|] |> Array.map(fun i -> [|i+1..pages.Length-1|]|> Array.map(fun j -> pages.[i], pages.[j])) |> Array.concat
    let parse (input:string array) = 
        let invalids = input |> Array.takeWhile (fun a -> a <> "") |> Array.map (fun a -> a.Split("|")|> Array.map int) |> Array.map (fun a -> a.[1],a.[0])
        let pages = 
            input |> Array.skipWhile (fun a -> a <> "") |> Array.skip 1 
            |> Array.map (fun a -> a.Split(",")|> Array.map int |> fun p -> p, sequenced p)

        pages, invalids

    let validate (invalids:(int*int) array) (pages:int array, sequenced:(int*int) array) = 
        sequenced |> Array.exists (fun a -> invalids |> Array.contains a) |> fun a -> if a then 0 else pages[(pages.Length - 1)/2]

    let puzzle1 (input:string array) = 
        input |> parse 
        |> fun (parsed,invalids) -> parsed |> Array.map (validate invalids) |> Array.sum
            
    let comparer (rules:(int*int) array) a b=
        match (a,b) with
        | (a,b) when rules |> Array.contains (a, b) -> 1
        | (a,b) when rules |> Array.contains (b, a) -> -1
        | _ -> 0

    let puzzle2 input = 

        let reversedRules =
            input |> parse 
            |> fun (_,invalids) -> invalids

        let failed = 
            input |> parse 
            |> fun (parsed,invalids) -> parsed |> Array.filter (fun a -> validate invalids a = 0) |> Array.map (fun (a,_) -> a)

        failed |> Array.map (fun a -> a |> Array.sortWith (comparer reversedRules)) |> Array.sumBy (fun a -> a.[(a.Length - 1)/2])


    let Solution = (new Solution(5, puzzle1, puzzle2) :> ISolution).Execute