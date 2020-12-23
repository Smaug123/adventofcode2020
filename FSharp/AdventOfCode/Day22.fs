namespace AdventOfCode

open System.Collections.Generic
open AdventOfCode.Internals

[<RequireQualifiedAccess>]
module Day22 =

    let rec playCombatToEnd (deck1 : int list) (deck2 : int list) =
        match deck1, deck2 with
        | top1 :: rest1, top2 :: rest2 ->
            if top1 > top2 then
                playCombatToEnd (rest1 @ [top1 ; top2]) rest2
            else
                playCombatToEnd rest1 (rest2 @ [top2 ; top1])
        | [], deck
        | deck, [] -> deck

    let score (deck : int list) =
        deck
        |> List.rev
        |> List.mapi (fun i elt -> (i + 1) * elt)
        |> List.sum

    let part1 () =
        let player1, player2 =
            Utils.readResource "Day22Input.txt"
            |> Seq.splitAt ((=) "")
            |> Seq.map (List.skip 1 >> List.map int)
            |> Seq.toList
            |> function
                | [x ; y] -> x, y
                | xs -> failwithf "Expected two people, got: %+A" xs

        let endDeck = playCombatToEnd player1 player2
        score endDeck

    type Winner =
        | Player1
        | Player2

    type Deck =
        {
            Elements : int list
            Mask : int64
        }

    let toDeck (elts : int list) =
        {
            Elements = elts
            Mask =
                elts
                |> List.fold (fun state i -> state ||| (1L <<< i)) 0L
        }

    // This would all be nicer if it were ImmutableHashSet, but HashSet is so much faster.
    // Key (but unproved) insight: it is impossible to permute either deck by playing, so we can represent a deck as a
    // bitmask for the cache.
    let rec playRecursiveCombat (gameHistory : HashSet<int64 * int64>) (deck1 : Deck) (deck2 : Deck) : Winner * int list =
        if not <| gameHistory.Add (deck1.Mask, deck2.Mask) then Winner.Player1, deck1.Elements else
        match deck1.Elements, deck2.Elements with
        | top1 :: rest1, top2 :: rest2 ->
            let winner =
                if top1 <= rest1.Length && top2 <= rest2.Length then
                    playRecursiveCombat (HashSet()) (toDeck (List.take top1 rest1)) (toDeck (List.take top2 rest2))
                    |> fst
                else
                    if top2 < top1 then Winner.Player1 else Winner.Player2
            match winner with
            | Player1 ->
                let deck1 =
                    {
                        Elements = rest1 @ [top1 ; top2]
                        Mask = deck1.Mask + (1L <<< top2)
                    }
                let deck2 =
                    {
                        Elements = rest2
                        Mask = deck2.Mask - (1L <<< top2)
                    }
                playRecursiveCombat gameHistory deck1 deck2
            | Player2 ->
                let deck1 =
                    {
                        Elements = rest1
                        Mask = deck1.Mask - (1L <<< top1)
                    }
                let deck2 =
                    {
                        Elements = rest2 @ [top2 ; top1]
                        Mask = deck2.Mask + (1L <<< top1)
                    }
                playRecursiveCombat gameHistory deck1 deck2
        | [], deck -> Player2, deck
        | deck, [] -> Player1, deck

    let part2 () =
        let deck1, deck2 =
            Utils.readResource "Day22Input.txt"
            |> Seq.splitAt ((=) "")
            |> Seq.map (List.skip 1 >> List.map int)
            |> Seq.toList
            |> function
                | [x ; y] -> x, y
                | xs -> failwithf "Expected two people, got: %+A" xs

        let winner, deck = playRecursiveCombat (HashSet()) (toDeck deck1) (toDeck deck2)
        printfn "Winner: %+A" winner
        printfn "Deck: %+A" deck

        score deck
