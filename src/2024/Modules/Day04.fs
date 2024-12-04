namespace AdventOfCode2024

open Common.Types
open System.Text.RegularExpressions

module Day04 =
    let read (input:string array) i j = 
        if not (input[i][j] = 'X') then 0 else
        if (j + 3 < input[i].Length                      && input[i][j+1]='M'   && input[i][j+2]='A'   && input[i][j+3]='S'  ) then 1 else 0
        +
        if (j - 3 >= 0                                   && input[i][j-1]='M'   && input[i][j-2]='A'   && input[i][j-3]='S'  ) then 1 else 0
        +
        if (i - 3 >= 0                                   && input[i-1][j]='M'   && input[i-2][j]='A'   && input[i-3][j]='S'  ) then 1 else 0
        +
        if (i + 3 < input.Length                         && input[i+1][j]='M'   && input[i+2][j]='A'   && input[i+3][j]='S'  ) then 1 else 0
        +
        if (j + 3 < input[i].Length && i+3 <input.Length && input[i+1][j+1]='M' && input[i+2][j+2]='A' && input[i+3][j+3]='S') then 1 else 0
        +
        if (j - 3 >= 0 && i-3 >=0                        && input[i-1][j-1]='M' && input[i-2][j-2]='A' && input[i-3][j-3]='S') then 1 else 0
        +
        if (i - 3 >= 0 && j+3<input[i].Length            && input[i-1][j+1]='M' && input[i-2][j+2]='A' && input[i-3][j+3]='S') then 1 else 0
        +
        if (i + 3 < input.Length && j-3>=0               && input[i+1][j-1]='M' && input[i+2][j-2]='A' && input[i+3][j-3]='S') then 1 else 0

    let read2 (input:string array) i j = 
        if 
            (input[i][j] = 'A') && 
            (i>0 && i<input.Length-1 && j>0 && j<input[i].Length-1) && 
            ((input[i-1][j-1] = 'M' && input[i+1][j+1] = 'S') || (input[i-1][j-1] = 'S' && input[i+1][j+1] = 'M')) &&
            ((input[i-1][j+1] = 'M' && input[i+1][j-1] = 'S') || (input[i-1][j+1] = 'S' && input[i+1][j-1] = 'M')) 
        then 1
        else 0

    let puzzle1 (input:string array) = 
        input |> Array.mapi (fun i _ -> input[i] |> Seq.mapi(fun j _ -> (read input i j)) |> Seq.sum) |> Array.sum

    let puzzle2 input = 
        input |> Array.mapi (fun i _ -> input[i] |> Seq.mapi(fun j _ -> (read2 input i j)) |> Seq.sum) |> Array.sum

    let Solution = (new Solution(4, puzzle1, puzzle2) :> ISolution).Execute