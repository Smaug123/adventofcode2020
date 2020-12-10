namespace AdventOfCode

open AdventOfCode.Internals

[<RequireQualifiedAccess>]
module Day10 =

    let differences (s : int seq) : int seq =
        seq {
            use s = s.GetEnumerator ()
            if not (s.MoveNext ()) then () else
            let mutable prev = s.Current
            while (s.MoveNext ()) do
                yield s.Current - prev
                prev <- s.Current
        }

    let part1 () =
        let numbers =
            Utils.readResource "Day10Input.txt"
            |> List.map int
            |> List.sort

        let results =
            differences numbers
            |> Seq.groupBy id
            |> Seq.map (fun (k, v) -> k, Seq.length v)
            |> Map.ofSeq

        // There's an additional 3, always, and there's an additional min(numbers) which sounds like (but is not
        // formally guaranteed) it's 1.
        // For part 2's purposes, we will also make an assertion that there are no 2-gaps; only 1- and 3-gaps.
        assert(numbers.[0] = 1)
        assert (results |> Map.toSeq |> Seq.map fst |> Set.ofSeq = Set.ofList [1 ; 3])
        (results.[3] + 1) * (results.[1] + 1)

    /// Key insights that make this dumb solution work:
    /// * if we ever see n, n+3, then we can split the problem up at that point; both n and n+3 must be present.
    /// * since there is no difference of 2 in the list, we only need to care about the length of a run, not the numbers
    ///   in it.
    let part2 () : int64 =
        let numbers =
            Utils.readResource "Day10Input.txt"
            |> List.map int
            |> List.sort

        /// Split across boundaries with a difference of 3.
        /// For example, [1;2;4;7;8] -> [[1;2;4];[7;8]].
        let rec split (xs : int list) : int list list =
            match xs with
            | [] -> [[]]
            | [x] -> [[x]]
            | x :: y :: rest ->
                match split (y :: rest) with
                | [] -> failwith "logic error"
                | fst :: rest ->
                    if y - x = 3 then [x] :: fst :: rest else (x :: fst) :: rest

        let isValid (l : int list) : bool =
            Set.isSubset (Set.ofSeq (differences l)) (Set.ofList [1 ; 2 ; 3])

        /// All ordered subsets of elements of the input list.
        /// This is, of course, exponential in time and space.
        let rec subLists (l : 'a list) : 'a list list =
            match l with
            | [] -> [[]]
            | x :: xs ->
                subLists xs
                |> List.collect (fun l -> [l ; x :: l])

        /// Hilariously inefficient computation of the Part 2 answer for a given sequence.
        let bruteForce (l : int list) : int64 =
            match l with
            | [] -> 1L
            | [_] -> 1L
            | [_ ; _] -> 1L
            | l ->
                let fst = l.[0]
                let last = l.[l.Length - 1]
                l.[1..l.Length - 2]
                |> subLists
                |> List.filter (fun l -> isValid (fst :: l @ [last]))
                |> List.length
                |> int64

        /// Memoise the result of `bruteForce`, since only the length in a consecutive run matters.
        let computeRun (cache : Map<int, int64>) (l : int list) : Map<int, int64> * int64 =
            let len = List.length l
            match Map.tryFind len cache with
            | Some v -> cache, v
            | None ->
                let result = bruteForce l
                Map.add len result cache, result

        split (0 :: numbers @ [numbers.[numbers.Length - 1] + 3])
        |> List.fold (fun (cache, soFar) next ->
            let cache, result = computeRun cache next
            printfn "%A" result
            cache, soFar * result
        ) (Map.empty, 1L)
        |> snd