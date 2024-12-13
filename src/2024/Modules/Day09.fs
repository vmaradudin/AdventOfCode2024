namespace AdventOfCode2024

open Common.Types
open System.Text.RegularExpressions

module Day09 =
    let parse (input:string array) =
        input[0] |> Array.ofSeq |> Array.map (fun a -> a - '0'|>  int64) |> Array.mapi (fun i x -> (if not ((i|>int64)%2L = 0L) then None else Some((i|>int64)/2L)), x)

    let rec calc (arr: (int64 option * int64) array) (pos:int64) (sum:int64) =
        if arr|>Array.isEmpty then sum
        else
           match (arr|> Array.head), (arr |> Array.last) with
           | (Some (i), x), _ -> calc (arr |> Array.tail) (pos + x) ([pos..(pos + x - 1L)] |> Seq.sumBy (fun p -> p * i) |> (+) sum)
           | _, (None,_) -> calc (arr |> Array.take (arr.Length - 1)) pos sum
           | (None, x), (Some (j), y) when y > x -> calc ((Array.append (arr |> Array.take (arr.Length - 1) |> Array.tail) [|(Some(j), y - x)|]) |> Array.append [|Some(j),x|]) pos sum
           | (None, x), (Some (j), y) when y = x -> calc (((arr |> Array.take (arr.Length - 1) |> Array.tail)) |> Array.append [|Some(j),x|]) pos sum
           | (None, x), (Some (j), y) when y < x -> calc ((Array.append  [|(None, x - y)|] (arr |> Array.take (arr.Length - 1) |> Array.tail)) |> Array.append [|Some(j),y|]) pos sum
    
    let rec move i (arr: (int64 option * int64) array) =
        if i = (arr |> Array.length) then arr
        else
        if (arr[arr.Length - i] |> fst) = None then move (i+1) arr
        else
        let item = arr[arr.Length - i]
        let newPos = arr|> Array.tryFindIndex(fun (j, y) -> j.IsNone && y >= (item |> snd))
        match newPos with
            |None -> move (i+1) arr 
            |Some(idx) when idx >= arr.Length - i -> move (i+1) arr
            |Some(idx) -> move  (i+1) (arr|> Array.removeAt(arr.Length - i) |> Array.insertAt(arr.Length - i) (None, arr[arr.Length - i] |> snd) |> Array.removeAt(idx) |> Array.insertAt idx (None, (arr[idx] |> snd) - (item |> snd)) |> Array.insertAt idx item)


    let puzzle1 (input:string array) = 
        let parsed = parse input
        calc parsed 0L 0L
        

    let puzzle2 (input:string array) = 
        parse input |> move 1
        |> Array.fold (fun (sum,p) (i, x) -> 
            match i with 
            |None -> (sum, p + x)
            |Some(v) -> ([p..p+x-1L] |> Seq.sum 
            |> fun a -> (sum + a*v,p+x))) (0L,0L)
            |> fun (a,_) -> a

    let Solution = (new Solution(9, puzzle1, puzzle2) :> ISolution).Execute