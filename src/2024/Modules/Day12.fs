namespace AdventOfCode2024

open Common.Types

module Day12 =
    let getIslandStart (input:string array) =
        input |> Array.indexed 
        |> Array.tryPick(fun (i,l) -> (l |> Seq.tryFindIndex(fun c -> c >= 'A' && c<='Z')) |> function |Some(j) -> Some(i,j) |_-> None)

    let rec getIsland (input:string array) (islandId:char) (queue: (int*int) Set) =
        if queue.IsEmpty then input
        else
        let item = queue |> Set.minElement
        let symbol = input[fst item][snd item]
        let neighbours = 
            item |> fun (i,j) -> [i,j+1;i, j-1; i+1,j; i-1,j]
            |> List.filter (fun (r,c) -> r >= 0 && r < input.Length && c >= 0 && c < input.[r].Length && input.[r].[c] = symbol) |> Set.ofList
        let newQueue = queue.Remove(item) |> Set.union neighbours
        let newLine = input.[fst item] |> fun a -> a.Remove(snd item,1).Insert(snd item, islandId|>string)
        let newInput = input |> Array.removeAt (fst item)|> Array.insertAt (fst item) newLine
        getIsland newInput islandId newQueue

    let getBorderCount (input:string array) (i,j)=
        [
            (i, j + 1)
            (i, j - 1)
            (i + 1, j)
            (i - 1, j)
        ]
        |> List.filter (fun (r,c) -> (r >= 0 && r < input.Length && c >= 0 && c < input.[r].Length) && (input.[r].[c] = input[i][j]) )
        |> List.length
        |> fun a -> 4 - a

    let rec getIslands  (islandId:char) (input:string array) =
        match input |> getIslandStart with
        | None -> input
        | Some (row, col) -> 
            let newInput = getIsland input islandId (Set.singleton (row,col))
            getIslands  (char(int islandId + 1)) newInput
   
    let puzzle1 input = 
        let islands = input |> getIslands  (char(100))
        islands |> Array.mapi (fun i a -> a|> Seq.mapi (fun j c -> (c, getBorderCount input (i,j))) |> Array.ofSeq)
        |> Array.concat
        |> Array.groupBy fst
        |> Array.map (fun (_,v) -> (v |> Array.sumBy snd) * v.Length)
        |> Array.sum

    let getSides (input:string array) (borders:(int*int) array) =
        let sides = 
            borders |> Array.map (fun (i,j) -> 
                [
                    if (i=0) || not (input.[i-1].[j] = input.[i].[j]) then Some(0, (i-1,j)) else None;
                    if (j=0) || not (input.[i].[j-1] = input.[i].[j]) then Some(3, (i,j-1)) else None;
                    if (i=input.Length-1) || not (input.[i+1].[j] = input.[i].[j]) then Some(2, (i+1,j)) else None;
                    if (j=input.[i].Length-1) || not (input.[i].[j+1] = input.[i].[j]) then Some(1, (i,j+1)) else None;
                ] |> List.choose id |> Array.ofList) |> Array.concat

        let tops = 
            sides |> Array.filter (fun (a,_) -> a = 0) |> Array.map (fun (_,v) -> v) |> Array.groupBy (fun (i,_) -> i) 
            |> Array.map (fun (i,v) -> (i, v |> Array.map (fun (_,v) -> v)|> Array.sort))
            |> Array.map (fun (i,v) -> (i, (v |> Array.pairwise |> Array.sumBy (fun (a,b) -> if b - a > 1 then 1 else 0)) + 1))
        let rights = 
            sides |> Array.filter (fun (a,_) -> a = 1) |> Array.map (fun (_,v) -> v) |> Array.groupBy (fun (_,i) -> i) 
            |> Array.map (fun (i,v) -> (i, v |> Array.map (fun (v,_) -> v)|> Array.sort))
            |> Array.map (fun (i,v) -> (i, (v |> Array.pairwise |> Array.sumBy (fun (a,b) -> if b - a > 1 then 1 else 0)) + 1))
        let lefts = 
            sides |> Array.filter (fun (a,_) -> a = 3) |> Array.map (fun (_,v) -> v) |> Array.groupBy (fun (_,i) -> i) 
            |> Array.map (fun (i,v) -> (i, v |> Array.map (fun (v,_) -> v)|> Array.sort))
            |> Array.map (fun (i,v) -> (i, (v |> Array.pairwise |> Array.sumBy (fun (a,b) -> if b - a > 1 then 1 else 0)) + 1))
        let bottoms =
            sides |> Array.filter (fun (a,_) -> a = 2) |> Array.map (fun (_,v) -> v) |> Array.groupBy (fun (i,_) -> i) 
            |> Array.map (fun (i,v) -> (i, v |> Array.map (fun (_,v) -> v)|> Array.sort))
            |> Array.map (fun (i,v) -> (i, (v |> Array.pairwise |> Array.sumBy (fun (a,b) -> if b - a > 1 then 1 else 0)) + 1))

        (borders |> Array.head |> fun (x,y) -> input[x][y]),
        (tops |> Array.sumBy snd) + (rights |> Array.sumBy snd) + (lefts|> Array.sumBy snd) + (bottoms|> Array.sumBy snd)

        
    let puzzle2 input = 
        let islands = input |> getIslands  (char(100))
        let dots = islands |> Array.mapi (fun i a -> a|> Seq.mapi (fun j c -> (c, (getBorderCount input (i,j)), (i,j))) |> Array.ofSeq)
        let areas = dots |> Array.concat |> Array.countBy (fun (c,_,_) -> c)
        let borders = dots |> Array.concat |> Array.groupBy (fun (c,_,_) -> c) |> Array.map (fun (c,a) -> (a|> Array.filter (fun (_,b,_) -> b > 0) |> Array.map (fun(_,_,k) -> k)))
        let sides = borders |> Array.map (getSides islands)
        Array.map2 (fun (_,a) (_,b) -> a*b)  (areas |> Array.sortBy fst) (sides |> Array.sortBy fst) |> Array.sum

    let Solution = (new Solution(12, puzzle1, puzzle2) :> ISolution).Execute