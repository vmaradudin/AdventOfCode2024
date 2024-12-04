namespace AdventOfCode2024

open Common.Types

module Day01 =
    let puzzle1 (input:string array) = 
        input 
        |> Array.map (fun s -> s.Split(" ", System.StringSplitOptions.RemoveEmptyEntries) |> Array.map int |> fun i -> i[0],i[1])
        |> Array.unzip
        |> fun (a,b) -> a |> Array.sort, b |> Array.sort
        ||> Array.zip
        |> Array.sumBy (fun (a,b) -> a-b |> abs)


    let puzzle2 (input:string array) = 
        input 
        |> Array.map (fun s -> s.Split(" ", System.StringSplitOptions.RemoveEmptyEntries) |> Array.map int |> fun i -> i[0],i[1])
        |> Array.unzip
        |> fun (a,b) -> (b|> Array.countBy id , a|> Array.countBy id) 
        |> fun (a,b) -> a|> Array.sumBy (fun (k, v) -> b |> Array.tryFind(fun (k1, _) -> k1 = k)|> fun q -> match q with |Some(_,v1) -> k*v*v1 |_ -> 0) 

    let Solution = (new Solution(1, puzzle1, puzzle2) :> ISolution).Execute