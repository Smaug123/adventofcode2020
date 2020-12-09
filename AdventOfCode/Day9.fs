namespace AdventOfCode

open AdventOfCode.Internals

[<RequireQualifiedAccess>]
module Day9 =

    type AvailableSums<'a> =
        {
            /// This is best explained by example. See the unit test. This is basically a collection of ring buffers,
            /// which essentially moves a triangular window of valid answers across the torus represented by the 2D
            /// array.
            Buffer : 'a [] []
            mutable Ptr : int
            Modulus : int
            Numbers : 'a []
        }

    [<RequireQualifiedAccess>]
    module AvailableSums =

        /// Couldn't get statically resolved type parameters and a (+) constraint to work straight away, so just
        /// did it old-school
        let create<'a> (plus : 'a -> 'a -> 'a) (preamble : 'a seq) (n : int) : AvailableSums<'a> =
            let preamble = Seq.toArray preamble

            let buffer = [| for _ in 0..n-1 do yield Array.zeroCreate n |]
            for i in 0..n-1 do
                let whichBuffer = buffer.[i]
                for j in 0..(n - i - 1) do
                    whichBuffer.[j] <- plus (preamble.[i]) (preamble.[(j + i) % n])

            {
                Modulus = n
                Ptr = 0
                Buffer = buffer
                Numbers = preamble
            }

        /// List the available sums. Order is well-defined by the two elements a number is reached from:
        /// * Sums reachable from a more recent element come earlier;
        /// * Within the sums reachable from the same element, sums reachable from a more recent element come earlier.
        let toSeq<'a> (s : AvailableSums<'a>) : 'a seq =
            seq {
                for i in 0..s.Modulus - 1 do
                    let whichBuffer = s.Buffer.[(i + s.Ptr) % s.Modulus]
                    for j in 0..(s.Modulus - i - 1) do
                        yield whichBuffer.[j]
            }

        /// Incorporate the next number in the stream, discarding the initial one.
        let advance (plus : 'a -> 'a -> 'a) (i : 'a) (s : AvailableSums<'a>) : unit =
            s.Numbers.[s.Ptr] <- i
            for count in 0..s.Modulus - 1 do
                s.Buffer.[(s.Ptr + count) % s.Modulus].[(s.Modulus - count) % s.Modulus] <- plus i s.Numbers.[(s.Ptr + count) % s.Modulus]
            s.Ptr <- (s.Ptr + 1) % s.Modulus

        let isAvailable (i : 'a) (s : AvailableSums<'a>) : bool =
            toSeq s
            |> Seq.contains i

    let part1 () =
        let numbers =
            Utils.readResource "Day9Input.txt"
            |> List.map int64
        let available = AvailableSums.create<int64> (+) numbers.[0..24] 25
        let rec go (rest : int64 list) =
            match rest with
            | [] -> failwith "Hit the end"
            | r :: rest ->
                if AvailableSums.isAvailable r available then
                    AvailableSums.advance (+) r available
                    go rest
                else
                    r
        go numbers.[25..]

    let tryMinAndMax<'a when 'a : comparison> (s : 'a seq) : ('a * 'a) option =
        use s = s.GetEnumerator ()
        if not <| s.MoveNext () then None else
        let mutable min = s.Current
        let mutable max = s.Current
        while s.MoveNext () do
            if s.Current < min then min <- s.Current else
            if s.Current > max then max <- s.Current

        Some (min, max)

    let part2 () =
        let badNumber = part1 ()
        let numbers =
            Utils.readResource "Day9Input.txt"
            |> List.map int64
            |> List.toArray

        assert(numbers |> Array.forall (fun i -> i >= 0L))

        let rec go (target : int64) (ptr : int) =
            if ptr >= numbers.Length then failwith "hit the end"
            let rec go2 (sum : int64) (endPoint : int) =
                if endPoint >= numbers.Length then None else
                let sum = sum + numbers.[endPoint]
                if sum = target then Some endPoint else
                go2 sum (endPoint + 1)

            match go2 0L ptr with
            | None -> go target (ptr + 1)
            | Some endPoint -> (ptr, endPoint)

        let start, endPoint = go badNumber 0
        match tryMinAndMax numbers.[start..endPoint] with
        | None -> failwith "empty seq"
        | Some (min, max) -> max + min