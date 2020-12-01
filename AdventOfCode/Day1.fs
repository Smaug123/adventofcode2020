namespace AdventOfCode

open AdventOfCode.Internals

[<RequireQualifiedAccess>]
module Day1 =

    let rec private go1 (target : int) (seen : int Set) (xs : int list) =
        match xs with
        | [] -> None
        | x :: xs ->
            if Set.contains (target - x) seen then
                Some (x, target - x)
            else go1 target (Set.add x seen) xs

    let part1 () =
        Utils.readResource "Day1Input.txt"
        |> List.map int
        |> go1 2020 Set.empty
        |> Option.map (fun (x, y) -> x * y)

    let rec private go2 (target : int) (twoSums : Map<int, int * int>) (seenSoFar : int Set) (xs : int list) =
        match xs with
        | [] -> None
        | x :: xs ->
           match Map.tryFind (target - x) twoSums with
           | Some (a, b) -> Some (a, b, x)
           | None ->
               let twoSums =
                   seenSoFar
                   |> Set.fold (fun m i -> Map.add (i + x) (i, x) m) twoSums
               go2 target twoSums (Set.add x seenSoFar) xs


    let part2 () =
        Utils.readResource "Day1Input.txt"
        |> List.map int
        |> go2 2020 Map.empty Set.empty
        |> Option.map (fun (a, b, c) -> a * b * c)