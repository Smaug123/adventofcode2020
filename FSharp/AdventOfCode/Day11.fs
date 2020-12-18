namespace AdventOfCode

open AdventOfCode.Internals

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

    let adjacent (board : Status [] []) : struct(int * int) [] [] [] =
        let arr : struct(int * int) [] [] [] =
            [|
                for _ in 1..board.Length do
                    yield [|
                        for _ in 1..board.[0].Length do
                            yield Array.zeroCreate 8
                    |]
            |]

        let go (i : int) (j : int) (maxI : int) (maxJ : int) =
            let is = if i = 0 then [0 ; 1] elif i = maxI - 1 then [maxI-2 ; maxI-1] else [i-1 ; i ; i+1]
            let mutable count = 0
            for r in is do
                if j > 0 then
                    arr.[i].[j].[count] <- struct(r, j - 1)
                    count <- count + 1
                if j < maxJ - 1 then
                    arr.[i].[j].[count] <- struct(r, j + 1)
                    count <- count + 1
            if i > 0 then
                arr.[i].[j].[count] <- struct(i - 1, j)
                count <- count + 1
            if i < maxI - 1 then
                arr.[i].[j].[count] <- struct(i + 1, j)
                count <- count + 1

            if count <= 7 then arr.[i].[j].[count] <- struct(-1,0)

        let maxI = board.Length
        for i in 0..maxI-1 do
            let maxJ = board.[i].Length
            for j in 0..maxJ-1 do
                go i j maxI maxJ

        arr

    let queenMoves (board : Status [] []) : struct(int * int) [] [] [] =
        let arr : struct(int * int) [] [] [] =
            [|
                for _ in 1..board.Length do
                    yield [|
                        for _ in 1..board.[0].Length do
                            yield Array.zeroCreate 8
                    |]
            |]

        let rec go (count : int ref) (origI : int) (origJ : int) (di : int) (dj : int) maxI maxJ (i : int) (j : int) =
            let newI, newJ = i+di, j+dj
            if newI < 0 || newI >= maxI || newJ < 0 || newJ >= maxJ then
                arr.[origI].[origJ].[count.Value] <- struct(i, j)
                incr count
            else

            match board.[newI].[newJ] with
            | Occupied | Empty ->
                arr.[origI].[origJ].[count.Value] <- struct(newI, newJ)
                incr count
            | Floor ->
                go count origI origJ di dj maxI maxJ newI newJ

        let maxI = board.Length
        for i in 0..maxI-1 do
            let maxJ = board.[i].Length
            for j in 0..maxJ-1 do
                let count = ref 0
                for di in (if i > 0 then -1 else 0)..(if i = maxI-1 then 0 else 1) do
                    for dj in (if j > 0 then -1 else 0)..(if j = maxJ - 1 then 0 else 1) do
                        if di <> 0 || dj <> 0 then
                            go count i j di dj maxI maxJ i j

                if count.Value <= 7 then arr.[i].[j].[count.Value] <- struct(-1,0)

        arr

    type StepOutput =
        | Mutated
        | Unchanged

    type Part =
        | Part1
        | Part2

    let step (positions : struct(int * int) [] [] []) (occupancyEmptyThreshold : int) (seats : Status [] []) : StepOutput =
        [
            for i in 0..seats.Length - 1 do
                for j in 0..seats.[i].Length - 1 do
                    match seats.[i].[j] with
                    | Floor -> ()
                    | _ ->

                    let occ =
                        let rec go (count : int) (acc : int) =
                            if count > 7 then acc else
                            let struct(a, b) = positions.[i].[j].[count]
                            if a = -1 then acc else
                            go (count + 1) (acc + match seats.[a].[b] with | Occupied -> 1 | _ -> 0)
                        go 0 0

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
