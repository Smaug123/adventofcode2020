namespace AdventOfCode

open AdventOfCode.Internals

[<RequireQualifiedAccess>]
module Day6 =

    let part1 () =
        Utils.readResource "Day6Input.txt"
        |> Utils.splitAt ((=) "")
        |> Seq.map (List.map Set.ofSeq)
        |> Seq.map Set.unionMany
        |> Seq.map Set.count
        |> Seq.sum

    let part2 () =
        Utils.readResource "Day6Input.txt"
        |> Utils.splitAt ((=) "")
        |> Seq.map (List.map Set.ofSeq)
        |> Seq.map Set.intersectMany
        |> Seq.map Set.count
        |> Seq.sum
