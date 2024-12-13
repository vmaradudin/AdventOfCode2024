namespace AdventOfCode2024

open Common.Types
open System.Text.RegularExpressions

module Day10 =
    let parse (input:string array) =
        input|> Array.mapi (fun i x -> (x |> Seq.indexed |> Seq.filter( fun (_,c) -> c='0') |> Seq.map (fun (j,_) -> Set.empty.Add(i,j)) |> Array.ofSeq)) |> Array.collect id

    let move (input:string array) (s:(int*int) Set) =
        let currentLevel = s.MinimumElement |> fun (i,j) -> input.[i].[j]
        s |> Set.map (fun (i,j) -> Set.empty.Add(i-1,j).Add(i+1,j).Add(i,j-1).Add(i,j+1)) |> Set.toSeq |> Set.unionMany |> Set.filter (fun (i,j) -> i >= 0 && j >= 0 && i < input.Length && j < input.[0].Length)
        |> Set.filter (fun (i,j) -> (input.[i].[j] |> int) - (currentLevel|>int) = 1)
    
    let rec climb (input:string array) (s:((int*int) Set) array) =
       if (s[0].MinimumElement|> fun (i,j) -> input.[i].[j]) = '9' then s
       else
            s |> Array.map (move input) |> climb input

    let rec climb2 (input:string array) (s:((int*int)*int) array) =
       if (s[0]|>fst|> fun (i,j) -> input.[i].[j]) = '9' then s |> Array.sumBy snd
       else
            s |> Array.map (fun ((i,j),p) -> (move input (Set.empty.Add(i,j))) |> Set.toArray |> Array.map (fun c -> (c,p))) |> Array.collect id |> Array.groupBy (fun (c,p) -> c) 
            |> Array.map (fun (c,p) -> c, p |> Array.sumBy snd)
            |> climb2 input           

    let puzzle1 (input:string array) = 
        input |> parse|> climb input |> Array.sumBy (fun a -> a.Count)

    let puzzle2 (input:string array) = 
        input |> parse|> Array.toSeq |> Set.unionMany |> Set.toArray |> Array.map (fun a -> (a,1)) |> climb2 input

    let Solution = (new Solution(10, puzzle1, puzzle2) :> ISolution).Execute