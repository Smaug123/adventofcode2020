namespace AdventOfCode

open System.Collections.Immutable
open AdventOfCode.Internals

[<RequireQualifiedAccess>]
module Day24 =

    type Direction =
        | West
        | SouthWest
        | SouthEast
        | East
        | NorthWest
        | NorthEast

    [<RequireQualifiedAccess>]
    module Direction =
        let parse (s : string) : Direction list =
            let rec go (acc : Direction list) (i : int) =
                if i >= s.Length then List.rev acc else
                match s.[i] with
                | 'w' -> go (West :: acc) (i + 1)
                | 'e' -> go (East :: acc) (i + 1)
                | 's' ->
                    match s.[i + 1] with
                    | 'w' -> go (SouthWest :: acc) (i + 2)
                    | 'e' -> go (SouthEast :: acc) (i + 2)
                    | c -> failwithf "Parse error (south then '%c')" c
                | 'n' ->
                    match s.[i + 1] with
                    | 'w' -> go (NorthWest :: acc) (i + 2)
                    | 'e' -> go (NorthEast :: acc) (i + 2)
                    | c -> failwithf "Parse error (north then '%c')" c
                | c -> failwithf "Parse error ('%c')" c

            go [] 0

        let position (directions : Direction list) =
            let rec go (posEast : int) (north : int) (directions : Direction list) =
                match directions with
                | [] -> posEast, north
                | dir :: directions ->
                    match dir with
                    | West -> go (posEast - 2) north directions
                    | East -> go (posEast + 2) north directions
                    | SouthEast -> go (posEast + 1) (north - 1) directions
                    | SouthWest -> go (posEast - 1) (north - 1) directions
                    | NorthEast -> go (posEast + 1) (north + 1) directions
                    | NorthWest -> go (posEast - 1) (north + 1) directions

            go 0 0 directions

    let part1 () =
        Utils.readResource "Day24Input.txt"
        |> List.map Direction.parse
        |> List.map Direction.position
        |> List.groupBy id
        |> List.map (snd >> List.length)
        |> List.fold (fun state i -> if i % 2 = 1 then state + 1 else state) 0

    let neighbours (east : int) (north : int) =
        [
            east + 2, north
            east - 2, north
            east + 1, north + 1
            east + 1, north - 1
            east - 1, north + 1
            east - 1, north - 1
        ]

    let part2 () =
        let blackPositions =
            Utils.readResource "Day24Input.txt"
            |> List.map Direction.parse
            |> List.map Direction.position
            |> List.groupBy id
            |> List.filter (fun (_, c) -> List.length c % 2 = 1)
            |> List.map fst

        let rec go (count : int) (board : ImmutableHashSet<int * int>) =
            if count = 100 then board else

            let blackSquaresStaying =
                board
                |> Seq.choose (fun (east, north) ->
                    let count =
                        let rec go (state : int) (neighbours : _ list) =
                            if state > 2 then state else
                            match neighbours with
                            | [] -> state
                            | n :: neighbours ->
                                if board.Contains n then go (state + 1) neighbours else go state neighbours
                        go 0 (neighbours east north)
                    if count = 0 || count > 2 then None else Some (east, north)
                )
                |> ImmutableHashSet.CreateRange

            let whiteSquaresFlipping =
                board
                |> Seq.collect (fun (east, north) ->
                    neighbours east north
                    |> List.filter (board.Contains >> not)
                )
                |> Seq.groupBy id
                |> Seq.choose (fun (pos, c) -> if Seq.length c = 2 then Some pos else None)
                |> ImmutableHashSet.CreateRange

            go (count + 1) (whiteSquaresFlipping.Union blackSquaresStaying)

        let blackSquares = go 0 (ImmutableHashSet.CreateRange blackPositions)
        blackSquares.Count