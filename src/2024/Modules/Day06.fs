namespace AdventOfCode2024

open Common.Types
open System.Text.RegularExpressions

module Day06 =
    let rec step (input:string array) (visited:(int*int) Set) direction ((i:int), (j:int)) =
        let next = 
            match direction with
            | 'v' -> (i+1,j)
            | '^' -> (i-1,j)
            | '<' -> (i,j-1)
            | '>' -> (i,j+1)
            | _ -> (i,j)

        match (next) with
        | (i,j) when i < 0 || i>= input.Length || j < 0 || j >= input[0].Length -> (visited, (i,j))
        | (i,j) when not (input[i][j] = '#') -> step input (visited |> Set.add (i,j)) direction next
        | _ -> step input visited (match direction with | 'v' -> '<' | '<' -> '^' | '^' -> '>' | '>' -> 'v' |_ -> direction) (i,j)
           
    let rec step2 (input:string array) (visited:(int*int*char) Set) direction ((i:int), (j:int)) =
        let next = 
            match direction with
            | 'v' -> (i+1,j,'v')
            | '^' -> (i-1,j,'^')
            | '<' -> (i,j-1,'<')
            | '>' -> (i,j+1,'>')
            | _ -> failwith "invalid direction"

        match (next) with
        | (i,j,d) when visited |> Set.contains next -> true
        | (i,j,_) when i < 0 || i>= input.Length || j < 0 || j >= input[0].Length -> false
        | (i,j,d) when not (input[i][j] = '#') -> step2 input (visited |> Set.add (i,j,d)) direction (next |> fun (a,b,_) -> (a,b)) 
        | _ -> step2 input visited (match direction with | 'v' -> '<' | '<' -> '^' | '^' -> '>' | '>' -> 'v' |_ -> direction) (i,j)

    let setBarrier (input:string array) (i:int) (j:int) = 
        input |> Array.mapi (fun idx l -> if idx = i then l |> String.mapi (fun j1 c -> if j1 = j then '#' else c ) else l)

    let puzzle1 (input:string array) = 
        let startPosition = input |> Array.map (fun a -> a.IndexOfAny([|'v';'^';'<';'>'|])) |> fun arr -> (arr, arr |> Array.findIndex (fun a -> a >= 0)) |> fun (arr, i) -> (i,arr[i])
        step input ([|startPosition|]|> Set.ofArray) (input[startPosition |> fst][startPosition|>snd]) startPosition |> fun (visited,_) -> visited.Count

    let puzzle2 (input:string array) = 
        let startPosition = input |> Array.map (fun a -> a.IndexOfAny([|'v';'^';'<';'>'|])) |> fun arr -> (arr, arr |> Array.findIndex (fun a -> a >= 0)) |> fun (arr, i) -> (i,arr[i], input[i][arr[i]])
        let way = step input ([|startPosition|> fun (a,b,_) -> (a,b)|]|> Set.ofArray) (input[startPosition |> fun (a,b,_) -> a][startPosition|> fun (a,b,_) -> b]) (startPosition |> fun (a,b,_) -> (a,b)) |> fun (visited,_) -> visited
        way |> Set.map (fun (a,b) -> (setBarrier input a b))
        |> Set.filter (fun a -> step2 a ([|startPosition |]|> Set.ofArray) (startPosition |> fun (_,_,a) -> a) (startPosition |> fun (a,b,_) -> (a,b)))
        |> Set.count

    let Solution = (new Solution(6, puzzle1, puzzle2) :> ISolution).Execute