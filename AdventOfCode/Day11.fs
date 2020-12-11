namespace AdventOfCode

open AdventOfCode.Internals
open System.Collections.Generic
open System.Collections.Immutable

[<RequireQualifiedAccess>]
module Day11 =

    [<Struct>]
    type Status =
        | Empty
        | Occupied
        | Floor
        static member Parse (c : char) : Status =
            match c with
            | '#' -> Occupied
            | 'L' -> Empty
            | '.' -> Floor
            | _ -> failwithf "Unexpected char: %c" c

    let board () =
        Utils.readResource "Day11Input.txt"
        |> List.map (Seq.map Status.Parse >> Array.ofSeq)
        |> Array.ofList

    let adjacent (board : Status [] []) : ImmutableDictionary<int * int, struct(int * int) list> =
        let go (i : int) (j : int) (maxI : int) (maxJ : int) =
            let is = if i = 0 then [0 ; 1] elif i = maxI - 1 then [maxI-2 ; maxI-1] else [i-1 ; i ; i+1]
            [
                for r in is do
                    if j > 0 then
                        yield struct(r, j - 1)
                    if j < maxJ - 1 then
                        yield struct(r, j + 1)
                if i > 0 then
                    yield struct(i - 1, j)
                if i < maxI - 1 then
                    yield struct(i + 1, j)
            ]

        seq {
            let maxI = board.Length
            for i in 0..maxI-1 do
                let maxJ = board.[i].Length
                for j in 0..maxJ-1 do
                    yield ((i, j), go i j maxI maxJ)
        }
        |> Seq.map KeyValuePair
        |> ImmutableDictionary.CreateRange

    let queenMoves (board : Status [] []) : ImmutableDictionary<int * int, struct(int * int) list> =
        let rec go (di : int) (dj : int) maxI maxJ (i : int) (j : int) =
            let newI, newJ = i+di, j+dj
            if newI < 0 || newI >= maxI || newJ < 0 || newJ >= maxJ then
                struct(i, j)
            else

            match board.[newI].[newJ] with
            | Occupied | Empty ->
                struct(newI, newJ)
            | Floor ->
                go di dj maxI maxJ newI newJ

        seq {
            let maxI = board.Length
            for i in 0..maxI-1 do
                let maxJ = board.[i].Length
                for j in 0..maxJ-1 do
                    let pos =
                        [
                            for di in (if i > 0 then -1 else 0)..(if i = maxI-1 then 0 else 1) do
                                for dj in (if j > 0 then -1 else 0)..(if j = maxJ - 1 then 0 else 1) do
                                    if di <> 0 || dj <> 0 then
                                        yield go di dj maxI maxJ i j
                        ]
                    yield ((i, j), pos)
        }
        |> Seq.map KeyValuePair
        |> ImmutableDictionary.CreateRange

    type StepOutput =
        | Mutated
        | Unchanged

    type Part =
        | Part1
        | Part2

    let step (positions : ImmutableDictionary<int * int, struct(int * int) list>) (occupancyEmptyThreshold : int) (seats : Status [] []) : StepOutput =
        [
            for i in 0..seats.Length - 1 do
                for j in 0..seats.[i].Length - 1 do
                    match seats.[i].[j] with
                    | Floor -> ()
                    | _ ->

                    let occ =
                        positions.[i, j]
                        |> List.sumBy (fun struct(adj1, adj2) ->
                            match seats.[adj1].[adj2] with
                            | Occupied -> 1
                            | _ -> 0
                        )

                    match seats.[i].[j] with
                    | Empty ->
                        if occ = 0 then yield ((i, j), Occupied)
                    | Occupied ->
                        if occ >= occupancyEmptyThreshold then yield ((i, j), Empty)
                    | _ -> ()
        ]
        |> List.fold (fun _ ((i, j), stat) ->
            seats.[i].[j] <- stat
            Mutated
        ) Unchanged

    let positions (puzzle : Part) (seats : Status [] []) =
        match puzzle with
        | Part1 -> adjacent seats
        | Part2 -> queenMoves seats

    let part1 () : int =
        let seats = board ()

        let positions = positions Part1 seats
        let rec go () =
            match step positions 4 seats with
            | Unchanged ->
                ()
            | Mutated ->
                go ()

        go ()

        seats
        |> Array.map (Array.sumBy (function | Occupied -> 1 | _ -> 0))
        |> Array.sum

    let part2 (seats : Status [] []) : int =
        let positions = positions Part2 seats

        let rec go () =
            match step positions 5 seats with
            | Unchanged ->
                ()
            | Mutated ->
                go ()

        go ()

        seats
        |> Array.map (Array.sumBy (function | Occupied -> 1 | _ -> 0))
        |> Array.sum

module Program =
    [<EntryPoint>]
    let main _ =
        let board =
            [
                "L.LL.LL.LL"
                "LLLLLLL.LL"
                "L.L.L..L.."
                "LLLL.LL.LL"
                "L.LL.LL.LL"
                "L.LLLLL.LL"
                "..L.L....."
                "LLLLLLLLLL"
                "L.LLLLLL.L"
                "L.LLLLL.LL"
            ]
            |> List.map (Seq.map Day11.Status.Parse >> Array.ofSeq)
            |> Array.ofList
        printfn "%i" (Day11.part2 board)
        0