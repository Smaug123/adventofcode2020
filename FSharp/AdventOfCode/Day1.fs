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

    /// Quadratic-space single-pass algorithm.
    /// Note that one might reasonably expect this to be slow, because it allocates a lot.
    /// We could do a *lot* better by backing the twoSums Map with an array [0..2020], assuming there are no negative
    /// elements in the list.
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

    /// Constant-space cubic-time algorithm which should be blazing fast thanks to
    /// good cache locality.
    let part2' () =
        let arr =
            Utils.readResource "Day1Input.txt"
            |> List.map int
            // In real life we would do this more sensibly, rather than going to and from lists.
            // Life's too short.
            |> List.toArray

        seq {
            for i in 0..arr.Length - 1 do
                for j in i..arr.Length - 1 do
                    // very optimise, such lift
                    let sum = arr.[i] + arr.[j]
                    for k in j..arr.Length - 1 do
                        if sum + arr.[k] = 2020 then
                            yield (arr.[i], arr.[j], arr.[k])
        }
        |> Seq.tryHead
        |> Option.map (fun (a, b, c) -> a * b * c)

    let part2 () =
        Utils.readResource "Day1Input.txt"
        |> List.map int
        |> go2 2020 Map.empty Set.empty
        |> Option.map (fun (a, b, c) -> a * b * c)