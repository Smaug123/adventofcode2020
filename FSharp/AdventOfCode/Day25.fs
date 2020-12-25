namespace AdventOfCode

open System.Collections.Immutable
open AdventOfCode.Internals

[<RequireQualifiedAccess>]
module Day25 =

    /// Least-significant bit first.
    let rec toBinary (i : int) : bool list =
        if i = 0 then [] else
        (i % 2 = 1) :: toBinary (i / 2)

    let ofBinary (s : bool seq) =
        s
        |> Seq.fold (fun (power, acc) bit -> power * 2, if bit then power + acc else acc) (1, 0)
        |> snd

    let powerMod (a : int) (b : int) (modulus : int) : int =
        let modulus = int64 modulus
        let rec go (power : int64) (acc : int64) (b : bool list) =
            match b with
            | [] -> int<int64> acc
            | bit :: b ->
                go ((power * power) % modulus) (if bit then (acc * power) % modulus else acc) b
        toBinary b
        |> go (int64 a) 1L

    // b is the bits of B, stored in reverse order, in an array.
    let powerMod' (a : int) (b : bool array) (modulus : int) : int =
        let modulus = int64 modulus
        /// `i` is the current index into `b`.
        let rec go (power : int64) (acc : int64) (i : int) =
            if i >= b.Length then int<int64> acc else
            go ((power * power) % modulus) (if b.[i] then (acc * power) % modulus else acc) (i + 1)
        go (int64 a) 1L 0

    let incr (b : bool array) : bool array option =
        let rec go (i : int) =
            if i >= b.Length then
                let answer = Array.zeroCreate (b.Length + 1)
                answer.[answer.Length - 1] <- true
                Some answer
            else
                if b.[i] = false then
                    b.[i] <- true
                    None
                else
                    b.[i] <- false
                    go (i + 1)
        go 0

    let part1 () =
        let cardKey, doorKey =
            Utils.readResource "Day25Input.txt"
            |> List.map int
            |> function | [x ; y] -> x, y | _ -> failwith "Unexpected input"

        let rec go (b : bool array) =
            if powerMod' 7 b 20201227 = cardKey then Choice1Of2 (ofBinary b)
            elif powerMod' 7 b 20201227 = doorKey then Choice2Of2 (ofBinary b)
            else
                match incr b with
                | None -> go b
                | Some b -> go b

        let b = [| true |]
        match go b with
        | Choice1Of2 i ->
            powerMod doorKey i 20201227
        | Choice2Of2 i ->
            powerMod cardKey i 20201227

    let part2 () =
        Utils.readResource "Day25Input.txt"
        |> ignore
        -1
