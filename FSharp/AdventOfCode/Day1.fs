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

    let rec private go1' (target : int) (seen : ArrayBackedMap<1, unit>) (xs : int list) =
        match xs with
        | [] -> None
        | x :: xs ->
            match ArrayBackedMap.tryFind (target - x) seen with
            | None ->
                ArrayBackedMap.Mutate.addSilently x () seen
                go1' target seen xs
            | Some () ->
                Some (x, target - x)

    /// This one is a lot faster thanks to avoiding the Set data structure.
    let part1' () =
        let input =
            Utils.readResource "Day1Input.txt"
            |> List.map int

        let map = ArrayBackedMap.make<1, unit> 2020 []
        go1' 2020 map input
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

    let part2 () =
        Utils.readResource "Day1Input.txt"
        |> List.map int
        |> go2 2020 Map.empty Set.empty
        |> Option.map (fun (a, b, c) -> a * b * c)

    /// The same algorithm as go2, but mutably using an array to store the map.
    let rec private go2' (target : int) (twoSums : ArrayBackedMap<1, int * int>) (seenSoFar : ArrayBackedMap<1, unit>) (xs : int list) =
        match xs with
        | [] -> None
        | x :: xs ->
            match ArrayBackedMap.tryFind (target - x) twoSums with
            | Some (a, b) -> Some (a, b, x)
            | None ->
                seenSoFar
                |> ArrayBackedMap.iter (fun i () -> ArrayBackedMap.Mutate.addSilently (i + x) (i, x) twoSums)
                ArrayBackedMap.Mutate.addSilently x () seenSoFar
                go2' target twoSums seenSoFar xs

    /// The same as go2', but we avoid constructing any tuples.
    let rec private go2'' (target : int) (twoSums1 : ArrayBackedMap<1, int>) (twoSums2 : ArrayBackedMap<1, int>) (seenSoFar : ArrayBackedMap<1, unit>) (xs : int list) =
        match xs with
        | [] -> None
        | x :: xs ->
            match ArrayBackedMap.tryFind (target - x) twoSums1 with
            | Some a -> Some (a, ArrayBackedMap.find (target - x) twoSums2, x)
            | None ->
                seenSoFar
                |> ArrayBackedMap.iter (fun i () ->
                    let key = i + x
                    ArrayBackedMap.Mutate.addSilently key i twoSums1
                    ArrayBackedMap.Mutate.addSilently key x twoSums2
                )
                ArrayBackedMap.Mutate.addSilently x () seenSoFar
                go2'' target twoSums1 twoSums2 seenSoFar xs

    let part2' () =
        let input =
            Utils.readResource "Day1Input.txt"
            |> List.map int

        let twoSums = ArrayBackedMap.make<1, int * int> 2020 []
        let seenSoFar = ArrayBackedMap.make<1, unit> 2020 []

        go2' 2020 twoSums seenSoFar input
        |> Option.map (fun (a, b, c) -> a * b * c)

    let part2'' () =
        let input =
            Utils.readResource "Day1Input.txt"
            |> List.map int

        let twoSums1 = ArrayBackedMap.make<1, int> 2020 []
        let twoSums2 = ArrayBackedMap.make<1, int> 2020 []
        let seenSoFar = ArrayBackedMap.make<1, unit> 2020 []

        go2'' 2020 twoSums1 twoSums2 seenSoFar input
        |> Option.map (fun (a, b, c) -> a * b * c)

    /// Constant-space cubic-time algorithm which should be blazing fast thanks to
    /// good cache locality.
    let part2Cubic () =
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

