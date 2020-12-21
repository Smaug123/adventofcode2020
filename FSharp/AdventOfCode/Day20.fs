namespace AdventOfCode

open System.Collections.Generic
open AdventOfCode.Internals
open System

[<RequireQualifiedAccess>]
module Day20 =

    // Upsettingly large amounts of time are taken constructing the tiny type, even when it's a struct.
    type Status = bool

    [<RequireQualifiedAccess>]
    module Status =
        let parse (c : char) =
            match c with
            | '#' -> true
            | '.' -> false
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
                |> List.toArray
                |> Array.map (Seq.map Status.parse >> Seq.toArray)
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
        |> Seq.fold (fun s x -> match x with | true -> 2 * s + 1 | false -> 2 * s) 0

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

    let dictChange (key : 'k) (f : 'v option -> 'v) (dict : Dictionary<'k, 'v>) : Dictionary<'k, 'v> =
        match dict.TryGetValue key with
        | false, _ -> dict.[key] <- f None
        | true, v -> dict.[key] <- f (Some v)
        dict

    let matches (tiles : ArrayBackedMap<tile, Tile>) : ArrayBackedMap<tile, Matches> =
        let edges = tiles |> ArrayBackedMap.map (fun _ -> edges)

        let edgesGrouped = Dictionary<int<hash>, Set<int<tile> * Side * Flippage>> ()
        // Side-effectfully update the grouped edges and get a count at the same time.
        let maxSeen =
            edges
            |> ArrayBackedMap.fold (fun maxSeen tile (edges, flipped) ->
                edgesGrouped
                |> dictChange edges.Left ((function | None -> Set.singleton (tile, Left, Normal) | Some a -> Set.add (tile, Left, Normal) a))
                |> dictChange edges.Right ((function | None -> Set.singleton (tile, Right, Normal) | Some a -> Set.add (tile, Right, Normal) a))
                |> dictChange edges.Top ((function | None -> Set.singleton (tile, Top, Normal) | Some a -> Set.add (tile, Top, Normal) a))
                |> dictChange edges.Bottom ((function | None -> Set.singleton (tile, Bottom, Normal) | Some a -> Set.add (tile, Bottom, Normal) a))
                |> dictChange flipped.Left ((function | None -> Set.singleton (tile, Left, Flipped) | Some a -> Set.add (tile, Left, Flipped) a))
                |> dictChange flipped.Right ((function | None -> Set.singleton (tile, Right, Flipped) | Some a -> Set.add (tile, Right, Flipped) a))
                |> dictChange flipped.Top ((function | None -> Set.singleton (tile, Top, Flipped) | Some a -> Set.add (tile, Top, Flipped) a))
                |> dictChange flipped.Bottom ((function | None -> Set.singleton (tile, Bottom, Flipped) | Some a -> Set.add (tile, Bottom, Flipped) a))
                |> ignore
                max maxSeen tile
            ) 0<tile>

        edges
        |> Seq.map (fun (KeyValue(tile, (normal, flipped))) ->
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
            let result =
                {
                    LeftTiles = leftTiles |> Seq.map (fun (x, side, flippage) -> (x, side, flip flippage)) |> Set.ofSeq
                    TopTiles = topTiles |> Seq.map (fun (x, side, flippage) -> (x, side, flip flippage)) |> Set.ofSeq
                    BottomTiles = bottomTiles |> Seq.map (fun (x, side, flippage) -> (x, side, flip flippage)) |> Set.ofSeq
                    RightTiles = rightTiles |> Seq.map (fun (x, side, flippage) -> (x, side, flip flippage)) |> Set.ofSeq
                }
            tile, result
        )
        |> ArrayBackedMap.make maxSeen

    let part1 () =
        let tiles =
            Utils.readResource "Day20Input.txt"
            |> Seq.splitAt ((=) "")
            |> Seq.map parse
            |> ArrayBackedMap.ofSeq

        let matches = matches tiles
        // A match will show up twice: once with both flipped, and once with both normal.
        matches
        |> Seq.choose (fun (KeyValue(i, m)) ->
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

    let toGrid (tiles : IReadOnlyDictionary<int<tile>, Tile>) (placement : Placement) : Status[][] =
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
    let rec assembleLine (matches : IReadOnlyDictionary<int<tile>, Matches>) (placementLeft : Placement) (placementNext : Placement) : Placement seq =
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
                for _ in 1..size do yield Array.zeroCreate<int<tile> option> size
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
                        let isOk, isBad =
                            placement.[i]
                            |> Seq.mapi (fun j placement -> j, placement)
                            |> Seq.choose (fun (j, placement) ->
                                match placement with
                                | None -> None
                                | Some p ->
                                    if p = rowOrCol.[j].Tile then
                                        Some (Ok i)
                                    else
                                        // Incompatible
                                        Some (Error ())
                            )
                            |> List.ofSeq
                            // why does partitionChoice not exist
                            |> List.partition (function | Ok _ -> true | Error _ -> false)
                        match isBad with
                        | _ :: _ -> ()
                        | [] -> yield Choice1Of2 (List.head isOk |> function | Ok x -> x | _ -> failwith "logic error")
                    for j in 0..placement.[0].Length - 1 do
                        // Would we be suitable as this col?
                        let isOk, isBad =
                            placement
                            |> Seq.mapi (fun i row -> (i, row))
                            |> Seq.choose (fun (i, row) ->
                                match row.[j] with
                                | None -> None
                                | Some p ->
                                    if p = rowOrCol.[i].Tile then
                                        Some (Ok j)
                                    else
                                        // Incompatible
                                        Some (Error ())
                            )
                            |> List.ofSeq
                            // why does Result.partition not exist
                            |> List.partition (function | Ok _ -> true | Error _ -> false)
                        match isBad with
                        | _ :: _ -> ()
                        | [] -> yield Choice2Of2 (List.head isOk |> function | Ok x -> x | _ -> failwith "logic error")
                }
                |> Seq.tryHead
            match position with
            | None ->
                if reved then
                    placement
                    |> Array.iter (fun row -> Array.map (fun i -> match i with | Some i -> string i | None -> "----") row |> String.concat "  " |> printfn "%s")
                    failwithf "Failed to place row: %+A" (rowOrCol |> Array.map (fun i -> i.Tile))
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

    let constructFlippages (matches : IReadOnlyDictionary<int<tile>, Matches>) (placement : IReadOnlyList<IReadOnlyList<_>>) : Flippage[][] =
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

                let rotationsCalc = rotations expectedTraversal.[0] [expectedTraversal] expectedTraversal |> Set.ofList
                flippages.[i].[j] <-
                    if rotationsCalc.Contains actualTraversal then Normal else
                    // Assert consistency
                    let expectedTraversal =
                        match expectedTraversal with
                        | [x ; y ; z ; w] -> [x ; w ; z ; y]
                        | _ -> failwith "too small"
                    let rotationsCalc = rotations expectedTraversal.[0] [expectedTraversal] expectedTraversal |> Set.ofList
                    if not (rotationsCalc.Contains actualTraversal) then
                        printfn "eep: (%i, %i) %+A // %+A %+A" i j actualTraversal expectedTraversal rotationsCalc
                        failwith "assertion failed"
                    Flipped

        flippages

    let constructOrientations (matches : IReadOnlyDictionary<int<tile>, Matches>) (placement : int<tile>[][]) (flippages : Flippage[][]) =
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
                let s =
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
                orientations.[row].[col] <- s |> Seq.exactlyOne

        orientations

    let findSeaMonsterSquares (s : Status[][]) =
        // A sea monster looks like:
        //                  #
        //#    ##    ##    ###
        // #  #  #  #  #  #
        let monsterSquares =
            [|
                    0, 18
                    1, 0
                    1, 5
                    1, 6
                    1, 11
                    1, 12
                    1, 17
                    1, 18
                    1, 19
                    2, 1
                    2, 4
                    2, 7
                    2, 10
                    2, 13
                    2, 16
                |]
        let len = Array.length monsterSquares
        let mutable count = 0
        for i in 0..s.Length - 3 do
            for j in 0..s.[i].Length - 20 do
                if Array.forall (fun (offsetI, offsetJ) -> s.[i + offsetI].[j + offsetJ]) monsterSquares then
                    count <- count + 1
        // Implicitly we are asserting that sea monsters are disjoint, for the sake of efficiency
        count * len

    let part2 () =
        let tiles =
            Utils.readResource "Day20Input.txt"
            |> Seq.splitAt ((=) "")
            |> Seq.map parse
            |> ArrayBackedMap.ofSeq

        let matches = matches tiles

        // Can it really be?
        matches
        |> Seq.iter (fun (KeyValue(_, matches)) ->
            if matches.BottomTiles.Count > 1 then failwith "Very sad, we're going to have to do some actual work"
            if matches.TopTiles.Count > 1 then failwith "Very sad, we're going to have to do some actual work"
            if matches.RightTiles.Count > 1 then failwith "Very sad, we're going to have to do some actual work"
            if matches.LeftTiles.Count > 1 then failwith "Very sad, we're going to have to do some actual work"
        )

        // Pick out the edges and build them all into rows. We'll duplicate a lot of work here, but I hate this
        // challenge so I don't care.
        let built =
            matches
            |> Seq.collect (fun (KeyValue(i, m)) ->
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

        // Sanity check
        do
            let count =
                built
                |> Array.map Array.length
                |> Set.ofArray
                |> Set.count
            if count <> 1 then failwith "oh no!"

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

        let hashCount = bigGrid |> Array.sumBy (Array.sumBy (function | true -> 1 | false -> 0))

        allPositions
        |> List.choose (fun (flip, side) ->
            let seaMonsterSquares = findSeaMonsterSquares (transformGrid side flip bigGrid)
            if seaMonsterSquares = 0 then
                None
            else
                Some (hashCount - seaMonsterSquares)
        )
        |> List.exactlyOne
