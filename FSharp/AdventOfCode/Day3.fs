namespace AdventOfCode

open AdventOfCode.Internals

[<RequireQualifiedAccess>]
module Day3 =

    type Square =
        | Tree
        | Empty

    type Board = Board of Square [] []

    let parseBoard (s : string seq) =
        s
        |> Seq.map (fun s ->
            s
            |> Seq.map (function | '.' -> Empty | '#' -> Tree | _ -> failwith "Unexpected character")
            |> Seq.toArray
        )
        |> Seq.toArray
        |> Board

    let private get () =
        Utils.readResource "Day3Input.txt"
        |> parseBoard

    type State =
        {
            Down : int
            Position : int
            Count : int
        }

    // This is expressed rather unnaturally as a fold, thanks to me bolting on the Part 2 functionality after completing
    // Part 1. Really it would be more naturally a loop where we updated the position until one coordinate became too
    // big, rather than a fold over the rows where we skip some rows.
    let countTrees (right : int) (down : int) (Board b) =
        let len = b.[0].Length
        let makeState (goRight : bool) (count : int) (oldState : State) : State =
            {
                Count = count
                Position = if goRight then (oldState.Position + right) % len else oldState.Position
                Down = (oldState.Down + 1) % down
            }

        b
        |> Array.fold (fun data row ->
            if data.Down > 0 then makeState false data.Count data else
            let newCount =
                match row.[data.Position] with
                | Tree -> 1
                | Empty -> 0
                |> fun i -> i + data.Count

            makeState true newCount data

        ) { Count = 0 ; Position = 0 ; Down = 0 }
        |> fun i -> i.Count

    let part1 () =
        get ()
        |> countTrees 3 1

    let part2 () =
        let b = get ()

        let one = countTrees 1 1 b
        let three = countTrees 3 1 b
        let five = countTrees 5 1 b
        let seven = countTrees 7 1 b
        let rogue = countTrees 1 2 b

        one * three * five * seven * rogue
