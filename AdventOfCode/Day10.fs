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

    let rec permissible (cache : Map<int, int64>) (l : int) : Map<int, int64> * int64 =
        if l < 3 then
            // Since the first and last elements must remain fixed, we can't do anything different.
            cache, 1L
        elif l = 3 then
            // We can choose freely to drop the middle element.
            cache, 2L
        else

        match cache.TryFind l with
        | Some v -> cache, v
        | None ->
            // The answer is generated either by saying the second element will stay...
            let cache, m1 = permissible cache (l - 1)
            // or by saying the second element is dropped but the third element will stay...
            let cache, m2 = permissible cache (l - 2)
            // or by saying the second and third elements are dropped but the fourth element will stay.
            let cache, m3 = permissible cache (l - 3)
            // Nothing else is possible, because we introduce a gap of more than 3 if we drop any more elements.

            cache, m1 + m2 + m3

    /// Iterate through the seq determining the successive counts between which f becomes true.
    /// For example, [1;2;4;7;8] and (fun x y -> y - x = 3) outputs [3;2].
    let runs<'a> (f : 'a -> 'a -> bool) (xs : 'a seq) : int seq =
        seq {
            use xs = xs.GetEnumerator ()
            if not (xs.MoveNext ()) then () else
            let mutable prev = xs.Current
            let mutable i = 1
            while (xs.MoveNext ()) do
                if f prev xs.Current then
                    yield i
                    i <- 1
                else
                    i <- i + 1
                prev <- xs.Current

            yield i
        }

    let part2 () : int64 =
        Utils.readResource "Day10Input.txt"
        |> List.map int
        |> List.sort
        |> runs (fun x y -> y - x = 3)
        // there is an implicit gap of 3 on the end, i.e. an implicit 1 appended to `runs`, so we need to start the
        // following fold from 2L instead of 1L
        |> Seq.fold (fun (cache, total) i ->
            let cache, toMul = permissible cache i
            cache, toMul * total
        ) (Map.empty, 2L)
        |> snd
