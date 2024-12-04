namespace AdventOfCode2024

open Common.Types
open System.Text.RegularExpressions

module Day03 =
    let regex = new Regex(@"mul\((?<x>\d{1,3}),(?<y>\d{1,3})\)")
  
    let puzzle1 (input:string array) = 
        let inp = input|> String.concat ""
        inp |> regex.Matches |> Seq.map (fun m -> (m.Groups.["x"].Value |>int64, int64 m.Groups.["y"].Value)) |> Seq.sumBy (fun (x, y) -> x * y)
        

    let puzzle2 input = 
        let inp = input|> String.concat "" |> fun a -> a.Split("do()") |> Array.map (fun x -> x.Split("don't()") |> fun q -> q[0]) |> String.concat ""
        inp |> regex.Matches |> Seq.map (fun m -> (m.Groups.["x"].Value |>int64, int64 m.Groups.["y"].Value)) |> Seq.sumBy (fun (x, y) -> x * y)

        
    let Solution = (new Solution(3, puzzle1, puzzle2) :> ISolution).Execute