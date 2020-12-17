namespace AdventOfCode

open System.Text
open AdventOfCode.Internals

[<RequireQualifiedAccess>]
module Day17 =

    let sumNeighbourhoodPart1 (state : bool[][][]) (i : int) (j : int) (k : int) =
        seq {
            for x in i-1..i+1 do
                for y in j-1..j+1 do
                    for z in k-1..k+1 do
                        if x <> i || y <> j || z <> k then
                            if x >= 0 && y >= 0 && z >= 0 && x < state.Length && y < state.[x].Length && z < state.[x].[y].Length then
                                yield state.[x].[y].[z]
        }
        |> Seq.sumBy (fun i -> if i then 1 else 0)

    let stepPart1 (state : bool[][][]) : bool[][][] =
        let answer : bool[][][] =
            [|
                for _ in -1..state.Length do
                    yield [|
                        for _ in -1..state.[0].Length do
                            yield Array.zeroCreate<bool> (state.[0].[0].Length + 2)
                    |]
            |]
        for i in -1..state.Length do
            for j in -1..state.[0].Length do
                for k in -1..state.[0].[0].Length do
                    let sum = sumNeighbourhoodPart1 state i j k
                    let res =
                        if i < 0 || i = state.Length || j < 0 || j = state.[0].Length || k < 0 || k = state.[0].[0].Length then
                            sum = 3
                        elif state.[i].[j].[k] && (sum = 2 || sum = 3) then true
                        elif state.[i].[j].[k] = false then sum = 3
                        else false
                    answer.[i + 1].[j + 1].[k + 1] <- res
        answer

    let part1 () =
        Utils.readResource "Day17Input.txt"
        |> List.map (Seq.map (function | '.' -> false | '#' -> true | _ -> failwith "Unexpected char") >> Seq.toArray)
        |> Array.ofList
        |> Array.singleton
        |> stepPart1
        |> stepPart1
        |> stepPart1
        |> stepPart1
        |> stepPart1
        |> stepPart1
        |> Array.sumBy (Array.sumBy (Array.sumBy (function | false -> 0 | true -> 1)))

    let sumNeighbourhoodPart2 (state : bool[][][][]) (i : int) (j : int) (k : int) (l : int) =
        seq {
            for x in i-1..i+1 do
                for y in j-1..j+1 do
                    for z in k-1..k+1 do
                        for w in l-1..l+1 do
                            if x <> i || y <> j || z <> k || w <> l then
                                if x >= 0 && y >= 0 && z >= 0 && w >= 0 && x < state.Length && y < state.[x].Length && z < state.[x].[y].Length && w < state.[x].[y].[z].Length then
                                    yield state.[x].[y].[z].[w]
        }
        |> Seq.sumBy (fun i -> if i then 1 else 0)

    // We'll squeeze out a small benefit by avoiding reifying the array on the final step.
    let stepPart2Instructions (state : bool[][][][]) : (int * int * int * int) seq * int * int * int * int =
        seq {
            for i in -1..state.Length do
                for j in -1..state.[0].Length do
                    for k in -1..state.[0].[0].Length do
                        for l in -1..state.[0].[0].[0].Length do
                            let sum = sumNeighbourhoodPart2 state i j k l
                            if i < 0 || i = state.Length || j < 0 || j = state.[0].Length || k < 0 || k = state.[0].[0].Length || l < 0 || l = state.[0].[0].[0].Length then
                                if sum = 3 then yield (i, j, k, l)
                            elif state.[i].[j].[k].[l] && (sum = 2 || sum = 3) then yield (i, j, k, l)
                            elif state.[i].[j].[k].[l] = false && sum = 3 then yield (i, j, k, l)
                            else ()
        }, state.Length + 2, state.[0].Length + 2, state.[0].[0].Length + 2, state.[0].[0].[0].Length + 2

    let stepPart2Execute (instructions : (int * int * int * int) seq, w, x, y, z) : bool[][][][] =
        let answer : bool[][][][] =
            [|
                for _ in 1..w do
                    yield [|
                        for _ in 1..x do
                            yield [|
                                for _ in 1..y do
                                    yield Array.zeroCreate<bool> z
                            |]
                    |]
            |]
        for (i, j, k, l) in instructions do
            answer.[i + 1].[j + 1].[k + 1].[l + 1] <- true
        answer

    let part2 () =
        Utils.readResource "Day17Input.txt"
        |> List.map (Seq.map (function | '.' -> false | '#' -> true | _ -> failwith "Unexpected char") >> Seq.toArray)
        |> Array.ofList
        |> Array.singleton
        |> Array.singleton
        |> (stepPart2Instructions >> stepPart2Execute)
        |> (stepPart2Instructions >> stepPart2Execute)
        |> (stepPart2Instructions >> stepPart2Execute)
        |> (stepPart2Instructions >> stepPart2Execute)
        |> (stepPart2Instructions >> stepPart2Execute)
        |> stepPart2Instructions
        |> fun (a, _, _, _, _) -> a |> Seq.length
