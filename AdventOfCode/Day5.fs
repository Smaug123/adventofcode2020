namespace AdventOfCode

open AdventOfCode.Internals

[<RequireQualifiedAccess>]
module Day5 =

    let interpretBase2 (i : bool seq) : int =
        i
        |> Seq.fold (fun s x -> if x then 2 * s + 1 else 2 * s) 0

    let seats () =
        Utils.readResource "Day5Input.txt"
        |> List.map (fun s ->
            let row =
                s.[0..6]
                |> Seq.map (function | 'F' -> false | 'B' -> true | x -> failwithf "oh no: %+A" x)
                |> interpretBase2
            let col =
                s.[7..9]
                |> Seq.map (function | 'L' -> false | 'R' -> true | x -> failwithf "oh no: %+A" x)
                |> interpretBase2
            (row, col)
        )
        |> Seq.map (fun (row, col) -> 8 * row + col)

    let part1 () =
        seats ()
        |> Seq.max

    let part2 () =
        seats ()
        |> Seq.sortDescending
        |> Seq.pairwise
        |> Seq.filter (fun (a, b) -> a + 2 = b)
        |> Seq.head
        |> fun (a, _) -> a + 1
