namespace AdventOfCode

open AdventOfCode.Internals
open System

[<RequireQualifiedAccess>]
module Day20 =

    [<Struct>]
    type Status =
        | On
        | Off
        static member Parse (c : char) =
            match c with
            | '#' -> On
            | '.' -> Off
            | _ -> failwithf "Unrecognised jigsaw status '%c'" c

    type Tile = | Tile of Status [] []

    [<Measure>]
    type tile

    let parse (s : string list) : int<tile> * Tile =
        match s with
        | [] -> failwith "Can't parse an empty tile!"
        | first :: rest ->
            let number =
                match first.Split(' ', ':') with
                | [| "Tile" ; last ; "" |] ->
                    match Int32.TryParse last with
                    | false, _ -> failwithf "Malformed first line '%s'" first
                    | true, v -> v
                | xs -> failwithf "First line not of correct format: '%s' (%+A)" first xs

            let tile =
                rest
                |> List.map (Seq.map Status.Parse >> Seq.toArray)
                |> List.toArray
                |> Tile
            number * 1<tile>, tile

    [<Measure>]
    type hash

    type Edges =
        {
            Left : int<hash>
            Right : int<hash>
            Top : int<hash>
            Bottom : int<hash>
        }

    let interpretBinary (s : Status seq) : int =
        s
        |> Seq.fold (fun s x -> match x with | On -> 2 * s + 1 | Off -> 2 * s) 0

    // Return the edges and their flipped versions
    let edges (Tile tile) : Edges * Edges =
        let normal =
            {
                Left =
                    seq {
                        for i in 1..tile.Length do
                            yield tile.[tile.Length - i].[0]
                    }
                    |> interpretBinary
                    |> fun i -> i * 1<hash>
                Right =
                    seq {
                        for i in 1..tile.Length do
                            let pos = tile.Length - i
                            yield tile.[pos].[tile.[pos].Length - 1]
                    }
                    |> interpretBinary
                    |> fun i -> i * 1<hash>
                Top =
                    tile.[0]
                    |> interpretBinary
                    |> fun i -> i * 1<hash>
                Bottom =
                    tile.[tile.Length - 1]
                    |> interpretBinary
                    |> fun i -> i * 1<hash>
            }
        let flipped =
            {
                Left =
                    seq {
                        for i in 0..tile.Length-1 do
                            yield tile.[i].[0]
                    }
                    |> interpretBinary
                    |> fun i -> i * 1<hash>
                Right =
                    seq {
                        for i in 0..tile.Length-1 do
                            yield tile.[i].[tile.[i].Length - 1]
                    }
                    |> interpretBinary
                    |> fun i -> i * 1<hash>
                Top =
                    tile.[0]
                    |> Array.rev
                    |> interpretBinary
                    |> fun i -> i * 1<hash>
                Bottom =
                    tile.[tile.Length - 1]
                    |> Array.rev
                    |> interpretBinary
                    |> fun i -> i * 1<hash>
            }
        normal, flipped

    type Side =
        | Left
        | Right
        | Top
        | Bottom

    type Flippage =
        | Normal
        | Flipped

    type Matches =
        {
            /// Tiles which can fit along the top row.
            /// The set contains tuples of (tile, which side of that tile is against our top row, whether the tile has been flipped).
            TopTiles : (int<tile> * Side * Flippage) Set
            /// Tiles which can fit along the bottom row
            /// The set contains tuples of (tile, which side of that tile is against our bottom row, whether the tile has been flipped).
            BottomTiles : (int<tile> * Side * Flippage) Set
            /// Tiles which can fit along the left column
            /// The set contains tuples of (tile, which side of that tile is against our left column, whether the tile has been flipped).
            LeftTiles : (int<tile> * Side * Flippage) Set
            /// Tiles which can fit along the right column
            /// The set contains tuples of (tile, which side of that tile is against our right column, whether the tile has been flipped).
            RightTiles : (int<tile> * Side * Flippage) Set
        }

    let matches (tiles : Map<int<tile>, Tile>) : Map<int<tile>, Matches> =
        let edges = tiles |> Map.map (fun _ -> edges)
        let edgesGrouped : Map<int<hash>, Set<int<tile> * Side * Flippage>> =
            edges
            |> Map.fold (fun locations tile (edges, flipped) ->
                locations
                |> Map.change edges.Left ((function | None -> Set.singleton (tile, Left, Normal) | Some a -> Set.add (tile, Left, Normal) a) >> Some)
                |> Map.change edges.Right ((function | None -> Set.singleton (tile, Right, Normal) | Some a -> Set.add (tile, Right, Normal) a) >> Some)
                |> Map.change edges.Top ((function | None -> Set.singleton (tile, Top, Normal) | Some a -> Set.add (tile, Top, Normal) a) >> Some)
                |> Map.change edges.Bottom ((function | None -> Set.singleton (tile, Bottom, Normal) | Some a -> Set.add (tile, Bottom, Normal) a) >> Some)
                |> Map.change flipped.Left ((function | None -> Set.singleton (tile, Left, Flipped) | Some a -> Set.add (tile, Left, Flipped) a) >> Some)
                |> Map.change flipped.Right ((function | None -> Set.singleton (tile, Right, Flipped) | Some a -> Set.add (tile, Right, Flipped) a) >> Some)
                |> Map.change flipped.Top ((function | None -> Set.singleton (tile, Top, Flipped) | Some a -> Set.add (tile, Top, Flipped) a) >> Some)
                |> Map.change flipped.Bottom ((function | None -> Set.singleton (tile, Bottom, Flipped) | Some a -> Set.add (tile, Bottom, Flipped) a) >> Some)
            ) Map.empty

        edges
        |> Map.map (fun tile (normal, flipped) ->
            {
                LeftTiles =
                    edgesGrouped.[normal.Left] |> Set.filter (fun (x, _, _) -> x <> tile)
                    //|> Set.union (edgesGrouped.[flipped.Left] |> Set.filter (fun (x, _, _) -> x <> tile))
                TopTiles = edgesGrouped.[normal.Top] |> Set.filter (fun (x, _, _) -> x <> tile)
                BottomTiles = edgesGrouped.[normal.Bottom] |> Set.filter (fun (x, _, _) -> x <> tile)
                RightTiles = edgesGrouped.[normal.Right] |> Set.filter (fun (x, _, _) -> x <> tile)
            }
        )

    let part1 () =
        let tiles =
            Utils.readResource "Day20Input.txt"
            |> Seq.splitAt ((=) "")
            |> Seq.map parse
            |> Map.ofSeq

        let matches = matches tiles
        // A match will show up twice: once with both flipped, and once with both normal.
        matches
        |> Map.toSeq
        |> Seq.choose (fun (i, m) ->
            let unfilledSlots =
                [
                    m.BottomTiles
                    m.LeftTiles
                    m.TopTiles
                    m.RightTiles
                ]
                |> List.map Set.isEmpty
                |> List.sumBy (function | true -> 1 | false -> 0)
            if unfilledSlots = 2 then
                Some (int64 (i / 1<tile>))
            else None
        )
        |> Seq.toList
        |> fun i -> if i.Length <> 4 then failwithf "Unexpected corner count: %i" i.Length else i
        |> List.fold (*) 1L

    type Placement =
        {
            Tile : int<tile>
            /// Which side of the tile is on top?
            EdgeOnTop : Side
            /// Has the edge on top been flipped?
            Flippage : Flippage
        }

    let rotateClockwise side =
        match side with
        | Left -> Top
        | Top -> Right
        | Right -> Bottom
        | Bottom -> Left

    let rotateAnticlockwise side =
        match side with
        | Left -> Bottom
        | Top -> Left
        | Right -> Top
        | Bottom -> Right

    /// Assemble a row of pieces out to the right.
    let rec assembleLine (matches : Map<int<tile>, Matches>) (placementLeft : Placement) (placementNext : Placement) : Placement seq =
        seq {
            yield placementLeft

            match placementNext.EdgeOnTop, placementNext.Flippage with
            | Side.Top, Normal ->
                match matches.[placementNext.Tile].RightTiles |> Seq.tryHead with
                | None -> yield placementNext
                | Some (tile, edgeAgainstOurRight, Normal) ->
                    yield! assembleLine matches placementNext { Tile = tile ; EdgeOnTop = rotateClockwise edgeAgainstOurRight ; Flippage = Normal }
                | Some (tile, edgeAgainstOurRight, Flipped) ->
                    yield! assembleLine matches placementNext { Tile = tile ; EdgeOnTop = rotateAnticlockwise edgeAgainstOurRight ; Flippage = Flipped }
            | Side.Top, Flipped ->
                match matches.[placementNext.Tile].LeftTiles |> Seq.tryHead with
                | None -> yield placementNext
                | Some (tile, edgeAgainstOurLeft, Normal) ->
                    yield! assembleLine matches placementNext { Tile = tile ; EdgeOnTop = rotateAnticlockwise edgeAgainstOurLeft ; Flippage = Flipped }
                | Some (tile, edgeAgainstOurLeft, Flipped) ->
                    yield! assembleLine matches placementNext { Tile = tile ; EdgeOnTop = rotateClockwise edgeAgainstOurLeft ; Flippage = Normal }
            | Side.Bottom, Normal ->
                match matches.[placementNext.Tile].LeftTiles |> Seq.tryHead with
                | None -> yield placementNext
                | Some (tile, edgeAgainstOurLeft, Normal) ->
                    yield! assembleLine matches placementNext { Tile = tile ; EdgeOnTop = rotateClockwise edgeAgainstOurLeft ; Flippage = Normal }
                | Some (tile, edgeAgainstOurLeft, Flipped) ->
                    yield! assembleLine matches placementNext { Tile = tile ; EdgeOnTop = rotateAnticlockwise edgeAgainstOurLeft ; Flippage = Flipped }
            | Side.Bottom, Flipped ->
                match matches.[placementNext.Tile].RightTiles |> Seq.tryHead with
                | None -> yield placementNext
                | Some (tile, edgeAgainstOurRight, Normal) ->
                    yield! assembleLine matches placementNext { Tile = tile ; EdgeOnTop = rotateAnticlockwise edgeAgainstOurRight ; Flippage = Flipped }
                | Some (tile, edgeAgainstOurRight, Flipped) ->
                    yield! assembleLine matches placementNext { Tile = tile ; EdgeOnTop = rotateClockwise edgeAgainstOurRight ; Flippage = Normal }
            | Side.Left, Normal ->
                match matches.[placementNext.Tile].TopTiles |> Seq.tryHead with
                | None -> yield placementNext
                | Some (tile, edgeAgainstOurTop, Normal) ->
                    yield! assembleLine matches placementNext { Tile = tile ; EdgeOnTop = rotateClockwise edgeAgainstOurTop ; Flippage = Normal }
                | Some (tile, edgeAgainstOurTop, Flipped) ->
                    yield! assembleLine matches placementNext { Tile = tile ; EdgeOnTop = rotateAnticlockwise edgeAgainstOurTop ; Flippage = Flipped }
            | Side.Left, Flipped ->
                match matches.[placementNext.Tile].BottomTiles |> Seq.tryHead with
                | None -> yield placementNext
                | Some (tile, edgeAgainstOurBottom, Normal) ->
                    yield! assembleLine matches placementNext { Tile = tile ; EdgeOnTop = rotateAnticlockwise edgeAgainstOurBottom ; Flippage = Flipped }
                | Some (tile, edgeAgainstOurBottom, Flipped) ->
                    yield! assembleLine matches placementNext { Tile = tile ; EdgeOnTop = rotateClockwise edgeAgainstOurBottom ; Flippage = Normal }
            | Side.Right, Normal ->
                match matches.[placementNext.Tile].BottomTiles |> Seq.tryHead with
                | None -> yield placementNext
                | Some (tile, edgeAgainstOurBottom, Normal) ->
                    // definitely correct
                    yield! assembleLine matches placementNext { Tile = tile ; EdgeOnTop = rotateAnticlockwise edgeAgainstOurBottom ; Flippage = Normal }
                | Some (tile, edgeAgainstOurBottom, Flipped) ->
                    yield! assembleLine matches placementNext { Tile = tile ; EdgeOnTop = rotateClockwise edgeAgainstOurBottom ; Flippage = Flipped }
            | Side.Right, Flipped ->
                match matches.[placementNext.Tile].TopTiles |> Seq.tryHead with
                | None -> yield placementNext
                | Some (tile, edgeAgainstOurTop, Normal) ->
                    yield! assembleLine matches placementNext { Tile = tile ; EdgeOnTop = rotateClockwise edgeAgainstOurTop ; Flippage = Flipped }
                | Some (tile, edgeAgainstOurTop, Flipped) ->
                    yield! assembleLine matches placementNext { Tile = tile ; EdgeOnTop = rotateAnticlockwise edgeAgainstOurTop ; Flippage = Normal }
            }

    let part2 () =
        let tiles =
            Utils.readResource "Day19Input.txt"
            |> Seq.splitAt ((=) "")
            |> Seq.map parse
            |> Map.ofSeq

        let matches = matches tiles

        // Can it really be?
        matches
        |> Map.iter (fun _ matches ->
            if matches.BottomTiles.Count > 1 then failwith "Very sad, we're going to have to do some actual work"
            if matches.TopTiles.Count > 1 then failwith "Very sad, we're going to have to do some actual work"
            if matches.RightTiles.Count > 1 then failwith "Very sad, we're going to have to do some actual work"
            if matches.LeftTiles.Count > 1 then failwith "Very sad, we're going to have to do some actual work"
        )

        // Pick out the edges and build them all into rows. We'll duplicate a lot of work here, but I hate this
        // challenge so I don't care.
        let built =
            matches
            |> Map.toSeq
            |> Seq.choose (fun (i, m) ->
                if m.LeftTiles.IsEmpty then Some ({ Tile = i ; EdgeOnTop = Top ; Flippage = Normal }, Seq.exactlyOne m.RightTiles)
                elif m.RightTiles.IsEmpty then Some ({ Tile = i ; EdgeOnTop = Bottom ; Flippage = Normal }, Seq.exactlyOne m.LeftTiles)
                elif m.TopTiles.IsEmpty then Some ({ Tile = i ; EdgeOnTop = Right ; Flippage = Normal }, Seq.exactlyOne m.BottomTiles)
                elif m.BottomTiles.IsEmpty then Some ({ Tile = i ; EdgeOnTop = Left ; Flippage = Normal }, Seq.exactlyOne m.TopTiles)
                else None
            )
            |> Seq.map (fun (placement, (next, edgeAgainst, flippage)) ->
                placement,
                {
                    Tile = next
                    EdgeOnTop = match flippage with | Normal -> rotateClockwise edgeAgainst | Flipped -> rotateAnticlockwise edgeAgainst
                    Flippage = flippage
                }
            )
            |> Seq.map (fun (prev, next) -> assembleLine matches prev next |> Array.ofSeq)
            |> Seq.toArray

        0
