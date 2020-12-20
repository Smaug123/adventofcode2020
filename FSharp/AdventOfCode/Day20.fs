namespace AdventOfCode

open System.Collections.Generic
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

    /// The numbers you get by reading clockwise around the grid.
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
                        for i in 0..tile.Length-1 do
                            yield tile.[i].[tile.[i].Length - 1]
                    }
                    |> interpretBinary
                    |> fun i -> i * 1<hash>
                Top =
                    tile.[0]
                    |> interpretBinary
                    |> fun i -> i * 1<hash>
                Bottom =
                    tile.[tile.Length - 1]
                    |> Array.rev
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
                        for i in 1..tile.Length do
                            let pos = tile.Length - i
                            yield tile.[pos].[tile.[pos].Length - 1]
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
                    |> interpretBinary
                    |> fun i -> i * 1<hash>
            }
        normal, flipped

    type Side =
        | Left
        | Right
        | Top
        | Bottom
        override this.ToString () =
            match this with
            | Left -> "L"
            | Right -> "R"
            | Top -> "T"
            | Bottom -> "B"

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

    let flip f =
        match f with
        | Flippage.Normal -> Flippage.Flipped
        | Flippage.Flipped -> Flippage.Normal

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
            let leftTiles =
                edgesGrouped.[normal.Left] |> Set.filter (fun (x, _, _) -> x <> tile)
                |> Set.union (edgesGrouped.[flipped.Left] |> Seq.choose (fun (x, side, flippage) -> if x = tile then None else Some (x, side, flip flippage)) |> Set.ofSeq)
            let topTiles =
                edgesGrouped.[normal.Top] |> Set.filter (fun (x, _, _) -> x <> tile)
                |> Set.union (edgesGrouped.[flipped.Top] |> Seq.choose (fun (x, side, flippage) -> if x = tile then None else Some (x, side, flip flippage)) |> Set.ofSeq)
            let bottomTiles =
                edgesGrouped.[normal.Bottom] |> Set.filter (fun (x, _, _) -> x <> tile)
                |> Set.union (edgesGrouped.[flipped.Bottom] |> Seq.choose (fun (x, side, flippage) -> if x = tile then None else Some (x, side, flip flippage)) |> Set.ofSeq)
            let rightTiles =
                edgesGrouped.[normal.Right] |> Set.filter (fun (x, _, _) -> x <> tile)
                |> Set.union (edgesGrouped.[flipped.Right] |> Seq.choose (fun (x, side, flippage) -> if x = tile then None else Some (x, side, flip flippage)) |> Set.ofSeq)
            // Now need to invert all the flippages, because a piece fits against another piece if reading clockwise
            // along the shared edge on one piece matches up with reading *anti*clockwise along the shared edge on the
            // other piece.
            {
                LeftTiles = leftTiles |> Seq.map (fun (x, side, flippage) -> (x, side, flip flippage)) |> Set.ofSeq
                TopTiles = topTiles |> Seq.map (fun (x, side, flippage) -> (x, side, flip flippage)) |> Set.ofSeq
                BottomTiles = bottomTiles |> Seq.map (fun (x, side, flippage) -> (x, side, flip flippage)) |> Set.ofSeq
                RightTiles = rightTiles |> Seq.map (fun (x, side, flippage) -> (x, side, flip flippage)) |> Set.ofSeq
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

    let transformGrid (edgeOnTop : Side) (flippage : Flippage) (square : 'a [] []) =
        let result =
            [|
                for _ in 0..square.Length-1 do
                    yield Array.zeroCreate square.Length
            |]
        match edgeOnTop with
        | Top ->
            for i in 0..square.Length-1 do
                for j in 0..square.[i].Length-1 do
                    result.[i].[j] <- square.[i].[j]
        | Left ->
            for i in 0..square.Length-1 do
                for j in 0..square.[i].Length-1 do
                    result.[j].[square.[i].Length - i - 1] <- square.[i].[j]
        | Right ->
            for i in 0..square.Length-1 do
                for j in 0..square.[i].Length-1 do
                    result.[square.Length - j - 1].[i] <- square.[i].[j]
        | Bottom ->
            for i in 0..square.Length-1 do
                for j in 0..square.[i].Length-1 do
                    result.[square.Length - i - 1].[square.[i].Length - j - 1] <- square.[i].[j]

        match flippage with
        | Normal -> ()
        | Flipped ->
            for i in 0..square.Length-1 do
                result.[i] <- result.[i] |> Array.rev

        result

    let toGrid (tiles : Map<int<tile>, Tile>) (placement : Placement) : Status[][] =
        let (Tile tile) = tiles.[placement.Tile]
        transformGrid placement.EdgeOnTop placement.Flippage tile

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
                    yield! assembleLine matches placementNext { Tile = tile ; EdgeOnTop = rotateClockwise edgeAgainstOurBottom ; Flippage = Normal }
                | Some (tile, edgeAgainstOurBottom, Flipped) ->
                    yield! assembleLine matches placementNext { Tile = tile ; EdgeOnTop = rotateAnticlockwise edgeAgainstOurBottom ; Flippage = Flipped }
            | Side.Right, Flipped ->
                match matches.[placementNext.Tile].TopTiles |> Seq.tryHead with
                | None -> yield placementNext
                | Some (tile, edgeAgainstOurTop, Normal) ->
                    yield! assembleLine matches placementNext { Tile = tile ; EdgeOnTop = rotateAnticlockwise edgeAgainstOurTop ; Flippage = Flipped }
                | Some (tile, edgeAgainstOurTop, Flipped) ->
                    yield! assembleLine matches placementNext { Tile = tile ; EdgeOnTop = rotateClockwise edgeAgainstOurTop ; Flippage = Normal }
            }

    // A reflection/rotation so that toInsert matches existing.
    let transform (toInsert : Placement) (existing : Placement) : Placement -> Placement =
        let rec createRotation (rotation : Side -> Side) (toInsert : Side) =
            if toInsert = existing.EdgeOnTop then rotation else
            createRotation (rotateClockwise >> rotation) (rotateClockwise toInsert)

        let createRotation = createRotation id toInsert.EdgeOnTop
        let createFlip =
            match toInsert.Flippage, existing.Flippage with
            | Normal, Normal
            | Flipped, Flipped -> id
            | _, _ -> flip

        fun placement ->
            { placement with
                Flippage = createFlip placement.Flippage
                EdgeOnTop = createRotation placement.EdgeOnTop
            }

    /// Get the location of each tile, though not its orientation.
    let getTilePositions (built : Placement [][]) : int<tile>[][] =
        // `built` contains the rows and the columns of the tiles in the grid.
        // Remove duplicates.
        let rowsAndCols =
            built
            |> Array.groupBy (fun row -> List.sort [row.[0].Tile ; row.[row.Length - 1].Tile])
            |> Array.map (fun (_, rows) -> rows.[0])

        let size = rowsAndCols.[0].Length
        let placement =
            [|
                for i in 1..size do yield Array.zeroCreate<int<tile> option> size
            |]

        // Pick an arbitrary row, then find an intersecting column.
        let firstRow = rowsAndCols.[0] |> Array.map (fun i -> i.Tile) |> Set.ofArray
        let colNum, col, intersection =
            rowsAndCols
            |> Seq.skip 1
            |> Seq.mapi (fun i row -> i, row)
            |> Seq.choose (fun (i, row) ->
                let entries = row |> Array.map (fun i -> i.Tile) |> Set.ofArray
                let intersect = Set.intersect entries firstRow
                if intersect <> Set.empty then Some (i, row, intersect |> Seq.exactlyOne) else None
            )
            |> Seq.head

        let intersectionPointX = Array.findIndex (fun p -> p.Tile = intersection) rowsAndCols.[0]
        let intersectionPointY = Array.findIndex (fun p -> p.Tile = intersection) col

        for i in 0..size - 1 do
            placement.[i].[intersectionPointX] <- Some col.[i].Tile
            placement.[intersectionPointY].[i] <- Some rowsAndCols.[0].[i].Tile

        let rec go (reved : bool) (i : int) =
            if i = colNum + 1 then go false (i + 1)
            elif i >= rowsAndCols.Length then () else
            let rowOrCol = rowsAndCols.[i]
            let position =
                seq {
                    for i in 0..placement.Length - 1 do
                        // Would we be suitable as this row?
                        let isOk =
                            placement.[i]
                            |> Seq.mapi (fun j placement -> j, placement)
                            |> Seq.choose (fun (j, placement) ->
                                match placement with
                                | None -> None
                                | Some p ->
                                    if p = rowOrCol.[j].Tile then
                                        Some (Choice1Of2 i)
                                    else
                                        // Incompatible
                                        Some (Choice2Of2 ())
                            )
                            |> Seq.head
                        match isOk with
                        | Choice1Of2 output ->
                            yield Choice1Of2 output
                        | Choice2Of2 () -> ()
                    for j in 0..placement.[0].Length - 1 do
                        // Would we be suitable as this col?
                        let isOk =
                            placement
                            |> Seq.mapi (fun i row -> (i, row))
                            |> Seq.choose (fun (i, row) ->
                                match row.[j] with
                                | None -> None
                                | Some p ->
                                    if p = rowOrCol.[i].Tile then
                                        Some (Choice1Of2 j)
                                    else
                                        // Incompatible
                                        Some (Choice2Of2 ())
                            )
                            |> Seq.head
                        match isOk with
                        | Choice1Of2 output -> yield Choice2Of2 output
                        | Choice2Of2 () -> ()
                }
                |> Seq.tryHead
            match position with
            | None ->
                if reved then
                    printfn "Failed to place row: %+A" (rowOrCol |> Array.map (fun i -> i.Tile))
                    placement
                    |> Array.iter (fun row -> Array.map (fun i -> match i with | Some i -> string i | None -> "----") row |> String.concat "  " |> printfn "%s")
                    go false (i + 1)
                else
                    rowsAndCols.[i] <- Array.rev rowOrCol
                    go true i
            | Some (Choice1Of2 pos) ->
                for j in 0..placement.[pos].Length - 1 do
                    placement.[pos].[j] <- Some rowOrCol.[j].Tile
                go false (i + 1)
            | Some (Choice2Of2 j) ->
                for i in 0..placement.[j].Length - 1 do
                    placement.[i].[j] <- Some rowOrCol.[i].Tile
                go false (i + 1)

        go false 1

        placement
        |> Array.map (Array.map Option.get)

    let constructFlippages (matches : Map<int<tile>, Matches>) (placement : IReadOnlyList<IReadOnlyList<_>>) : Flippage[][] =
        let flippages =
            [|
                for i in 0..placement.Count - 1 do
                    Array.zeroCreate placement.[i].Count
            |]

        for i in 0..placement.Count - 1 do
            for j in 0..placement.[i].Count - 1 do
                let currentTile = placement.[i].[j]
                let expectedTraversal =
                    [
                        matches.[currentTile].RightTiles |> Seq.tryExactlyOne |> Option.map (fun (a, _, _) -> a)
                        matches.[currentTile].BottomTiles |> Seq.tryExactlyOne |> Option.map (fun (a, _, _) -> a)
                        matches.[currentTile].LeftTiles |> Seq.tryExactlyOne |> Option.map (fun (a, _, _) -> a)
                        matches.[currentTile].TopTiles |> Seq.tryExactlyOne |> Option.map (fun (a, _, _) -> a)
                    ]
                let actualTraversal =
                    [
                        if j < placement.[i].Count - 1 then Some placement.[i].[j+1] else None
                        if i < placement.Count - 1 then Some placement.[i+1].[j] else None
                        if 0 < j then Some placement.[i].[j - 1] else None
                        if 0 < i then Some placement.[i - 1].[j] else None
                    ]
                let rec rotations (original : _) (soFar : _ list) (l : _ list) =
                    let rotated =
                        match l with
                        | [] -> failwith "logic error"
                        | x :: xs -> xs @ [x]
                    if rotated.[0] = original then soFar else
                    rotations original (rotated :: soFar) rotated

                let rotations = rotations expectedTraversal.[0] [] expectedTraversal |> Set.ofList
                flippages.[i].[j] <- if rotations.Contains actualTraversal then Normal else Flipped

        flippages

    let constructOrientations (matches : Map<int<tile>, Matches>) (placement : int<tile>[][]) (flippages : Flippage[][]) =
        let orientations =
            [|
                for i in 0..placement.Length - 1 do Array.zeroCreate placement.[i].Length
            |]

        // Orient the first row.
        for col in 1..placement.[0].Length-2 do
            // The side is Up which has no entry.
            orientations.[0].[col] <-
                let matches = matches.[placement.[0].[col]]
                if matches.LeftTiles.IsEmpty then Left
                elif matches.RightTiles.IsEmpty then Right
                elif matches.TopTiles.IsEmpty then Top
                else Bottom

        // Orient the two top corners.
        do
            let matches = matches.[placement.[0].[0]]
            orientations.[0].[0] <-
                if matches.LeftTiles.IsEmpty && matches.TopTiles.IsEmpty then
                    match flippages.[0].[0] with
                    | Normal -> Top
                    | Flipped -> Left
                elif matches.TopTiles.IsEmpty && matches.RightTiles.IsEmpty then
                    match flippages.[0].[0] with
                    | Normal -> Right
                    | Flipped -> Top
                elif matches.RightTiles.IsEmpty && matches.BottomTiles.IsEmpty then
                    match flippages.[0].[0] with
                    | Normal -> Bottom
                    | Flipped -> Right
                else
                    match flippages.[0].[0] with
                    | Normal -> Left
                    | Flipped -> Bottom

        do
            let min1 = placement.[0].Length - 1
            let matches = matches.[placement.[0].[min1]]
            orientations.[0].[min1] <-
                if matches.LeftTiles.IsEmpty && matches.TopTiles.IsEmpty then
                    match flippages.[0].[min1] with
                    | Normal -> Left
                    | Flipped -> Top
                elif matches.TopTiles.IsEmpty && matches.RightTiles.IsEmpty then
                    match flippages.[0].[min1] with
                    | Normal -> Top
                    | Flipped -> Right
                elif matches.RightTiles.IsEmpty && matches.BottomTiles.IsEmpty then
                    match flippages.[0].[min1] with
                    | Normal -> Right
                    | Flipped -> Bottom
                else
                    match flippages.[0].[min1] with
                    | Normal -> Bottom
                    | Flipped -> Left

        // Orient the other rows.
        for row in 1..placement.Length - 1 do
            for col in 0..placement.[row].Length - 1 do
                // Find the orientation which will align entry (row-1, col) with (row, col).
                let immediatelyAbove = placement.[row-1].[col]
                let matches = matches.[placement.[row].[col]]
                orientations.[row].[col] <-
                    seq {
                        yield!
                            matches.TopTiles
                            |> Seq.tryExactlyOne
                            |> Option.bind (fun (t, _, _) -> if t = immediatelyAbove then Some Top else None)
                            |> Option.toList
                        yield!
                            matches.RightTiles
                            |> Seq.tryExactlyOne
                            |> Option.bind (fun (t, _, _) -> if t = immediatelyAbove then Some Right else None)
                            |> Option.toList
                        yield!
                            matches.BottomTiles
                            |> Seq.tryExactlyOne
                            |> Option.bind (fun (t, _, _) -> if t = immediatelyAbove then Some Bottom else None)
                            |> Option.toList
                        yield!
                            matches.LeftTiles
                            |> Seq.tryExactlyOne
                            |> Option.bind (fun (t, _, _) -> if t = immediatelyAbove then Some Left else None)
                            |> Option.toList
                    }
                    |> Seq.head

        orientations

    let findSeaMonsterSquares (s : Status[][]) =
        seq {
            for i in 0..s.Length - 1 do
                for j in 0..s.[i].Length - 1 do
                    // A sea monster looks like:
                    //                  #
                    //#    ##    ##    ###
                    // #  #  #  #  #  #
                    let monsterSquares =
                        [
                            i, j + 18
                            i+1, j
                            i+1, j+5
                            i+1, j+6
                            i+1, j+11
                            i+1, j+12
                            i+1, j+17
                            i+1, j+18
                            i+1, j+19
                            i+2, j+1
                            i+2, j+4
                            i+2, j+7
                            i+2, j+10
                            i+2, j+13
                            i+2, j+16
                        ]
                    if List.forall (fun (i, j) -> i < s.Length && j < s.[i].Length && s.[i].[j] = Status.On) monsterSquares then
                        yield! monsterSquares
        }
        |> Set.ofSeq

    let part2 () =
        let tiles =
            Utils.readResource "Day20Input.txt"
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
            |> Seq.collect (fun (i, m) ->
                [
                    if m.LeftTiles.IsEmpty then yield ({ Tile = i ; EdgeOnTop = Top ; Flippage = Normal }, Seq.exactlyOne m.RightTiles)
                    if m.RightTiles.IsEmpty then yield ({ Tile = i ; EdgeOnTop = Bottom ; Flippage = Normal }, Seq.exactlyOne m.LeftTiles)
                    if m.TopTiles.IsEmpty then yield ({ Tile = i ; EdgeOnTop = Right ; Flippage = Normal }, Seq.exactlyOne m.BottomTiles)
                    if m.BottomTiles.IsEmpty then yield ({ Tile = i ; EdgeOnTop = Left ; Flippage = Normal }, Seq.exactlyOne m.TopTiles)
                ]
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

        let placement = getTilePositions built

        // Find the flip status of each tile.
        let flippages =
            placement
            |> Array.map (fun i -> i :> IReadOnlyList<_>)
            :> IReadOnlyList<_>
            |> constructFlippages matches

        let orientations =
            constructOrientations matches placement flippages

        let placed =
            let arr = [| for i in 0..placement.Length - 1 do yield Array.zeroCreate placement.[i].Length |]
            for i in 0..arr.Length - 1 do
                for j in 0..arr.[i].Length - 1 do
                    arr.[i].[j] <- { Tile = placement.[i].[j] ; EdgeOnTop = orientations.[i].[j] ; Flippage = flippages.[i].[j] }
            arr

        let bigGrid =
            placed
            |> Array.map (Array.map (toGrid tiles >> Array.map (fun i -> i.[1..i.Length - 2]) >> fun i -> i.[1..i.Length - 2]))
            |> Array.map (Array.transpose >> Array.map Array.concat)
            |> Array.concat

        let allPositions =
            [ Left ; Right ; Top ; Bottom ]
            |> List.allPairs [Flipped ; Normal]

        let hashCount = bigGrid |> Array.sumBy (Array.sumBy (function | Status.On -> 1 | Status.Off -> 0))

        allPositions
        |> List.choose (fun (flip, side) ->
            let seaMonsterSquares = findSeaMonsterSquares (transformGrid side flip bigGrid)
            if seaMonsterSquares.IsEmpty then
                None
            else
                Some (hashCount - seaMonsterSquares.Count)
        )
        |> List.exactlyOne
