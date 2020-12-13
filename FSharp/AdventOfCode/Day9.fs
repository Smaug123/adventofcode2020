namespace AdventOfCode

open AdventOfCode.Internals

[<RequireQualifiedAccess>]
module Day9 =

    [<NoEquality ; NoComparison>]
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

        /// Create a new AvailableSums, taking the first n elements of the input array as the preamble.
        let inline create (input : ^a array) (n : int) : AvailableSums< ^a> =
            let buffer = [| for _ in 0..n-1 do yield Array.zeroCreate n |]
            for i in 0..n-1 do
                let whichBuffer = buffer.[i]
                for j in 0..(n - i - 1) do
                    whichBuffer.[j] <- input.[i] + input.[(j + i) % n]

            {
                Modulus = n
                Ptr = 0
                Buffer = buffer
                Numbers = input.[0..n-1]
            }

        /// List the available sums. Order is well-defined by the two elements a number is reached from:
        /// * Sums reachable from a more recent element come earlier;
        /// * Within the sums reachable from the same element, sums reachable from a more recent element come earlier.
        /// For the sake of efficiency, we actually only use this in tests.
        let toSeq<'a> (s : AvailableSums<'a>) : 'a seq =
            seq {
                for i in 0..s.Modulus - 1 do
                    let whichBuffer = s.Buffer.[(i + s.Ptr) % s.Modulus]
                    for j in 0..(s.Modulus - i - 1) do
                        yield whichBuffer.[j]
            }

        /// Incorporate the next number in the stream, discarding the initial one.
        let inline advance (i : ^a) (s : AvailableSums< ^a>) : unit =
            s.Numbers.[s.Ptr] <- i
            for count in 0..s.Modulus - 1 do
                s.Buffer.[(s.Ptr + count) % s.Modulus].[(s.Modulus - count) % s.Modulus] <-
                    i + s.Numbers.[(s.Ptr + count) % s.Modulus]
            s.Ptr <- (s.Ptr + 1) % s.Modulus

        /// Efficiency! Gotta get that loop blazing - ain't nobody got time for the obvious implementation, namely
        /// `toSeq s |> Seq.contains target`
        let inline isAvailable (target : ^a) (s : AvailableSums< ^a>) : bool =
            let mutable result = false
            let mutable i = 0
            let max = s.Modulus - 1
            while not result && i < max do
                let whichBuffer = s.Buffer.[(i + s.Ptr) % s.Modulus]
                let endPoint = s.Modulus - i - 1
                let mutable j = 0
                while not result && j <= endPoint do
                    if whichBuffer.[j] = target then result <- true else j <- j + 1

                i <- i + 1

            result

    let numbers () =
        Utils.readResource' "Day9Input.txt"
        |> Array.map int64

    let part1 (numbers : int64 array) =
        let available = AvailableSums.create numbers 25
        let rec go (i : int) =
            if i >= numbers.Length then failwith "Hit the end" else
            let value = numbers.[i]
            if AvailableSums.isAvailable value available then
                AvailableSums.advance value available
                go (i + 1)
            else
                value
        go 25

    let part2 (numbers : int64 array) =
        let badNumber = part1 numbers

        let rec go' (target : int64) (current : int64) (start : int) (endPtr : int) =
            if current < target then go target current start endPtr
            elif current = target then Some (start, endPtr)
            else go' target (current - numbers.[start]) (start + 1) endPtr

        and go (target : int64) (current : int64) (start : int) (endPtr : int) =
            if endPtr = numbers.Length - 1 then None else

            // Advance endPtr and see if we've gone too big
            let nextTry = current + numbers.[endPtr + 1]
            if nextTry < target then go target nextTry start (endPtr + 1)
            elif nextTry = target then Some (start, endPtr)
            else
                go' target (current - numbers.[start]) (start + 1) endPtr

        let start, endPoint = go badNumber 0L 0 -1 |> Option.get
        match Seq.tryMinAndMax numbers.[start..endPoint] with
        | None -> failwith "empty seq"
        | Some (min, max) -> max + min