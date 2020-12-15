namespace AdventOfCode

open System.Collections.Generic
open AdventOfCode.Internals
open System.Collections.Immutable

[<RequireQualifiedAccess>]
module Day15 =
    let get () =
        Utils.readResource "Day15Input.txt"
        |> List.head
        |> fun i -> i.Split ','
        |> Array.map int

    [<Measure>]
    type count

    [<Measure>]
    type turn
    let go maxTurn (seen : ImmutableDictionary<int, int<turn> * ValueOption<int<turn>>>) (turn : int<turn>) (mostRecentlySpoken : int) =
        let d = Dictionary(seen)

        let say i (turn : int<turn>) =
            match d.TryGetValue i with
            | false, _ -> d.[i] <- (turn, ValueOption.None)
            | true, (lastTurn, _) ->
                d.[i] <- (turn, ValueSome lastTurn)

        let rec go (turn : int<turn>) (mostRecentlySpoken : int) =
            if maxTurn < turn then mostRecentlySpoken else
            let toSay =
                match d.[mostRecentlySpoken] with
                | _, ValueNone ->
                    0
                | lastSeen, ValueSome (seenBefore) ->
                    (lastSeen - seenBefore) / 1<turn>

            say toSay turn

            go (turn + 1<turn>) toSay

        go turn mostRecentlySpoken

    let part1 () =
        let nums = get ()

        let state =
            nums
            |> Seq.mapi (fun turn num -> KeyValuePair(num, (1<turn> + turn * 1<turn>, ValueOption.None)))
            |> ImmutableDictionary.CreateRange

        go 2020<turn> state (nums.Length * 1<turn> + 1<turn>) nums.[nums.Length - 1]

    // This is *weirdly* slow and I don't know why. I've rewritten in the most obviously-fast way
    // I can, and it's still 1.5sec. Much confuse.
    let go2 maxTurn (d : array<int>) (turn : int) (mostRecentlySpoken : int) =
        let mutable turn = turn
        let mutable mostRecentlySpokenPos = mostRecentlySpoken * 2
        let mutable isDone = false
        while not isDone do
            if maxTurn < turn then isDone <- true else

            mostRecentlySpokenPos <-
                if d.[mostRecentlySpokenPos + 1] = -1 then 0 else
                2 * (d.[mostRecentlySpokenPos] - d.[mostRecentlySpokenPos + 1])

            d.[mostRecentlySpokenPos + 1] <-
                if d.[mostRecentlySpokenPos] = 0 && d.[mostRecentlySpokenPos + 1] = 0 then
                    -1
                else
                    d.[mostRecentlySpokenPos]
            d.[mostRecentlySpokenPos] <- turn

            turn <- turn + 1

        mostRecentlySpokenPos >>> 1

    let part2 () =
        let nums : int array = get ()

        let state = Array.zeroCreate<int> (30000000 * 2)
        nums
        |> Seq.iteri (fun turn num ->
            state.[2 * num] <- turn + 1
            state.[2 * num + 1] <- -1
        )

        go2 30000000 state (nums.Length + 1) nums.[nums.Length - 1]