namespace AdventOfCode

open System
open AdventOfCode.Internals

[<RequireQualifiedAccess>]
module Day23 =

    [<Measure>]
    type value

    let rec go (requiredSteps : int) (counter : int) (m : int<value> array) =
        if counter >= requiredSteps then () else
        let modulus = m.Length
        let currentCup = counter % modulus
        // 389125467, current cup 0
        // cupsToMove: 1, 2, 3
        let cupsToMove =
            [|
                (currentCup + 1) % modulus
                (currentCup + 2) % modulus
                (currentCup + 3) % modulus
            |]
        // value 3<value>
        let currentCupValue = m.[currentCup]
        let rec findDestination (desired : int<value>) =
            let desired = if desired = 0<value> then modulus * 1<value> else desired
            let destination =
                m
                |> Array.findIndex ((=) desired)

            if Array.contains destination cupsToMove then
               findDestination (desired - 1<value>)
            else destination

        // value 4
        let destination = findDestination (currentCupValue - 1<value>)

        // Place the cupsToMove immediately right of the destination
        // That is, move cupsToMove.[0] to destination+1 etc.

        // value 8, 9, 1
        let movingCups = cupsToMove |> Array.map (fun i -> m.[i])

        // for i in 1..1 do
        let endReshuffle = if destination - 3 < cupsToMove.[0] then destination - 3 + modulus else destination - 3
        for i in cupsToMove.[0]..endReshuffle do
            m.[i % modulus] <- m.[(i + 3) % modulus]
        for i in 0..2 do
            m.[(destination - i + modulus) % modulus] <- movingCups.[2-i]

        go requiredSteps (counter + 1) m

    let part1 () =
        let order =
            Utils.readResource "Day23Input.txt"
            |> List.exactlyOne
            |> Seq.map (fun c -> (int c - int '0') * 1<value>)
            |> Array.ofSeq

        go 100 0 order
        let startIndex = Array.findIndex ((=) 1<value>) order
        Array.append order.[startIndex+1..] order.[0..startIndex - 1]
        |> Array.map (fun i -> i + (int '0') * 1<value> |> char)
        |> String

    let part2 () =
        let order =
            Utils.readResource "Day23Input.txt"
            |> List.exactlyOne
            |> Seq.map (fun c -> (int c - int '0'))
            |> Array.ofSeq

        let size = 1_000_000
        let succ = ArrayBackedMap.make size []

        let initialise () =
            for p, s in Array.pairwise order do
                ArrayBackedMap.Mutate.addSilently p s succ
            let m = Array.max order
            if m + 1 < size then
                ArrayBackedMap.Mutate.addSilently order.[order.Length - 1] (m + 1) succ

                for i in (m + 1)..(size - 1) do
                    ArrayBackedMap.Mutate.addSilently i (i + 1) succ
                ArrayBackedMap.Mutate.addSilently size order.[0] succ
            else
                ArrayBackedMap.Mutate.addSilently order.[order.Length - 1] order.[0] succ

        initialise ()

        let doOneStep (index : int) =
            let next = ArrayBackedMap.find index succ
            let nextNext = ArrayBackedMap.find next succ
            let nextNextNext = ArrayBackedMap.find nextNext succ
            let movingIndices = [| next ; nextNext ; nextNextNext |]
            let rec destination (d : int) =
                let d = if d = 1 then size else d - 1
                if Array.contains d movingIndices then destination d
                else d
            let destination = destination index
            let nextNextNextNext = ArrayBackedMap.find nextNextNext succ

            let wasNext = ArrayBackedMap.find destination succ
            ArrayBackedMap.Mutate.addSilently index nextNextNextNext succ
            ArrayBackedMap.Mutate.addSilently destination next succ
            ArrayBackedMap.Mutate.addSilently nextNextNext wasNext succ
            nextNextNextNext

        let rec go (steps : int) (i : int) =
            if steps = 10_000_000 then
                i
            else
                let i = doOneStep i
                go (steps + 1) i

        go 0 order.[0] |> ignore
        let next = ArrayBackedMap.find 1 succ
        let nextNext = ArrayBackedMap.find next succ
        int64 next * int64 nextNext
