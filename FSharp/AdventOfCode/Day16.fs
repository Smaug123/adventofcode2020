namespace AdventOfCode

open AdventOfCode.Internals
open System

[<RequireQualifiedAccess>]
module Day16 =

    type Interval =
        {
            Min : int
            Max : int
        }

    type Data =
        {
            MyTicket : int list
            OtherTickets : int list list
            Restrictions : Map<string, Interval list>
        }

    let parse (lines : string list) =
        let info =
            lines
            |> Seq.splitAt String.IsNullOrEmpty
            |> Seq.toArray

        let ranges, mine, others =
            match info with
            | [| ranges ; mine ; others |] -> ranges, mine, others
            | _ -> failwithf "Unexpected number of groupings"

        let myTicket =
            match mine with
            | ["your ticket:" ; x] ->
                x.Split ","
                |> Array.map int
                |> List.ofArray
            | _ -> failwith "Expected my ticket :("

        let ranges =
            ranges
            |> List.map (fun i ->
                match i.Split ":" with
                | [| name ; range |] ->
                    range.Split " or "
                    |> Seq.map (fun i ->
                        match i.Split '-' with
                        | [| a ; b |] ->
                            let a = int a
                            let b = int b
                            if a <= b then
                                { Min = int a ; Max = int b }
                            else
                                failwithf "Interval is unexpectedly not increasing: %i, %i" a b
                        | _ -> failwithf "Malformed range: %s" i
                    )
                    |> Seq.toList
                    |> fun s -> (name, s)
                | _ -> failwithf "Malformed range containing string: %s" i
            )

        let tickets =
            match others with
            | "nearby tickets:" :: others ->
                others
                |> List.map (fun i -> i.Split ',' |> Array.map int |> List.ofArray)
            | _ -> failwith "Malformed other-tickets"

        {
            MyTicket = myTicket
            Restrictions = ranges |> Map.ofList
            OtherTickets = tickets
        }

    // This data structure is currently fairly inefficient. We could do better by binary-searching keys.
    type Shadow =
        {
            // Map of start points to end points
            Intervals : Map<int, int>
        }
        member this.ContainingInterval (p : int) : Interval option =
            let possibleStart =
                this.Intervals
                |> Map.toSeq
                |> Seq.filter (fun (start, _) -> start <= p)
                |> Seq.tryLast
            match possibleStart with
            | Some (start, endP) -> if p <= endP then Some { Min = start ; Max = endP } else None
            | None -> None

        member this.NextIntervalStartingAfter (p : int) : Interval option =
            this.Intervals
            |> Map.toSeq
            |> Seq.filter (fun (start, _) -> start > p)
            |> Seq.tryHead
            |> Option.map (fun (a, b) -> { Min = a ; Max = b })

    [<RequireQualifiedAccess>]
    module Shadow =
        let empty = { Intervals = Map.empty }

        /// Include this interval in the shadow, given that it starts at the beginning of an interval already in the shadow.
        let rec extendBoundary (interval : Interval) (shadow : Shadow) : Shadow =
            let maxOfThisInterval = shadow.Intervals.[interval.Min]
            if maxOfThisInterval >= interval.Max then shadow else
            match shadow.NextIntervalStartingAfter maxOfThisInterval with
            | Some { Min = nextMin ; Max = nextMax } ->
                if nextMin > interval.Max then
                    // Just need to extend our current interval
                    {
                        Intervals = shadow.Intervals |> Map.add interval.Min interval.Max
                    }
                elif nextMin = interval.Max then
                    // Combine these two intervals
                    {
                        Intervals = shadow.Intervals |> Map.remove nextMin |> Map.add interval.Min nextMax
                    }
                else
                    {
                        Intervals = shadow.Intervals |> Map.remove nextMin |> Map.add interval.Min nextMax
                    }
                    |> extendBoundary interval
            | None ->
                {
                    Intervals = shadow.Intervals |> Map.add interval.Min interval.Max
                }

        let extend (interval : Interval) (shadow : Shadow) : Shadow =
            match shadow.ContainingInterval interval.Min with
            | Some { Min = start1 ; Max = _ } ->
                extendBoundary { Min = start1 ; Max = interval.Max } shadow
            | None ->
                match shadow.NextIntervalStartingAfter interval.Min with
                | None ->
                    {
                        Intervals = shadow.Intervals |> Map.add interval.Min interval.Max
                    }
                | Some nextInterval ->
                    {
                        Intervals = shadow.Intervals |> Map.remove nextInterval.Min |> Map.add interval.Min nextInterval.Max
                    }
                    |> extendBoundary { Min = interval.Min ; Max = interval.Max }

    let part1 () =
        let data =
            Utils.readResource "Day16Input.txt"
            |> parse

        let validRanges =
            data.Restrictions
            |> Map.fold (fun shadow _ restriction ->
                restriction
                |> List.fold (fun shadow r -> Shadow.extend r shadow) shadow
            ) Shadow.empty

        data.OtherTickets
        |> List.map (fun t -> t |> List.choose (fun t -> match validRanges.ContainingInterval t with | None -> Some t | Some _ -> None))
        |> List.sumBy List.sum

    let updatePossibilities (possibilities : string Set list) (restrictions : Map<string, Interval list>) (ticket : int list) : string Set list =
        List.zip ticket possibilities
        |> List.map (fun (ticket, possibilities) ->
            possibilities
            |> Set.filter (fun possibility ->
                restrictions.[possibility] |> List.exists (fun interval -> interval.Min <= ticket && ticket <= interval.Max)
            )
        )

    let part2 () =
        let data =
            Utils.readResource "Day16Input.txt"
            |> parse

        let validRanges =
            data.Restrictions
            |> Map.fold (fun shadow _ restriction ->
                restriction
                |> List.fold (fun shadow r -> Shadow.extend r shadow) shadow
            ) Shadow.empty

        let names = data.Restrictions |> Map.toSeq |> Seq.map fst |> Set.ofSeq

        let restricted =
            data.OtherTickets
            |> List.filter (fun t -> t |> List.forall (fun t -> match validRanges.ContainingInterval t with | None -> false | Some _ -> true))
            |> List.fold (fun state ticket ->
                updatePossibilities state data.Restrictions ticket
            ) (data.MyTicket |> List.map (fun _ -> names))

        let rec reduce (known : Map<string, int>) (constraints : Map<int, string Set>) =
            if constraints.IsEmpty then known else
            let newlyKnown, known =
                constraints
                |> Map.fold (fun (newlyKnown, known) pos constraints ->
                    match Seq.tryExactlyOne constraints with
                    | Some constr ->
                        newlyKnown |> Set.add constr, known |> Map.add constr pos
                    | None ->
                        newlyKnown, known
                ) (Set.empty, known)
            if newlyKnown.IsEmpty then failwith "A sweep did not let us update anything; problem is undetermined" else
            constraints
            |> Map.toSeq
            |> Seq.choose (fun (pos, constraints) ->
                let newSet = Set.difference constraints newlyKnown
                if newSet.IsEmpty then None else Some (pos, newSet)
            )
            |> Map.ofSeq
            |> reduce known

        restricted
        |> List.mapi (fun i s -> (i, s))
        |> Map.ofList
        |> reduce Map.empty
        |> Map.toSeq
        |> Seq.sortBy snd
        |> Seq.map fst
        |> Seq.toList
        |> List.zip data.MyTicket
        |> List.filter (fun (_, r) -> r.StartsWith "departure")
        |> List.map fst
        |> List.fold (fun i j -> int64 j * i) 1L
