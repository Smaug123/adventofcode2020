namespace AdventOfCode

open System
open AdventOfCode.Internals

[<RequireQualifiedAccess>]
module Day12 =

    type Direction =
        | North
        | South
        | East
        | West

    [<Measure>]
    type units

    type Absolute =
        {
            Direction : Direction
            Distance : int<units>
        }
        static member TryParse (s : string) =
            match s.[0] with
            | 'N' -> { Direction = North ; Distance = int s.[1..] * 1<units> } |> Some
            | 'S' -> { Direction = South ; Distance = int s.[1..] * 1<units> } |> Some
            | 'E' -> { Direction = East ; Distance = int s.[1..] * 1<units> } |> Some
            | 'W' -> { Direction = West ; Distance = int s.[1..] * 1<units> } |> Some
            | _ -> None

    [<Measure>]
    type deg

    type Relative =
        | Left of int<deg>
        | Right of int<deg>
        | Forward of int<units>
        static member TryParse (s : string) =
            match s.[0] with
            | 'L' -> Left (int s.[1..] * 1<deg>) |> Some
            | 'R' -> Right (int s.[1..] * 1<deg>) |> Some
            | 'F' -> Forward (int s.[1..] * 1<units>) |> Some
            | _ -> None

    type Movement =
        | Absolute of Absolute
        | Relative of Relative
        static member Parse (s : string) =
            match Absolute.TryParse s with
            | Some s -> Absolute s
            | None ->

            match Relative.TryParse s with
            | Some s -> Relative s
            | None -> failwithf "Unable to parse: '%s'" s

    let turnLeftOnce (dir : Direction) : Direction =
        match dir with
        | North -> West
        | South -> East
        | East -> North
        | West -> South

    let rec turnLeft (angle : int<deg>) (dir : Direction) : Direction =
        match angle with
        | 0<deg> -> dir
        | n when n > 0<deg> -> turnLeft (n - 90<deg>) (turnLeftOnce dir)
        | _ -> failwithf "Logic error: tried to turn left by %A" angle

    let turnRight (angle : int<deg>) (dir : Direction) : Direction =
        turnLeft (360<deg> - angle) dir

    let goForward (dir : Direction) (length : int<units>) ((x : int<units>, y : int<units>) as pos) =
        match dir with
        | North -> (x, y + length)
        | South -> (x, y - length)
        | East -> (x + length, y)
        | West -> (x - length, y)

    let part1 () =
        let rec go (direction : Direction) ((x : int<units>, y : int<units>) as pos) (instructions : Movement list) : int<units> * int<units> =
            match instructions with
            | [] -> pos
            | instr :: instructions ->
                match instr with
                | Absolute dir ->
                    let pos = goForward dir.Direction dir.Distance pos
                    go direction pos instructions
                | Relative rel ->
                    match rel with
                    | Left angle ->
                        go (turnLeft angle direction) pos instructions
                    | Right angle ->
                        go (turnRight angle direction) pos instructions
                    | Forward length ->
                        go direction (goForward direction length pos) instructions

        Utils.readResource "Day12Input.txt"
        |> List.map Movement.Parse
        |> go East (0<units>, 0<units>)
        |> fun (a, b) -> abs a + abs b

    /// Waypoint is expressed as a position relative to the ship.
    let rotateLeftOnce (wX : int<units>, wY : int<units>) =
        (-wY, wX)

    let rec rotateLeft angle waypoint =
        if angle = 0<deg> then waypoint
        elif angle > 0<deg> then rotateLeft (angle - 90<deg>) (rotateLeftOnce waypoint)
        else failwithf "Logic error: tried to rotate left negatively: %+A" angle

    let rotateRight angle waypoint =
        rotateLeft (360<deg> - angle) waypoint

    let part2 () =
        /// Waypoint is expressed as a position relative to the ship.
        let rec go ((x : int<units>, y : int<units>) as pos) ((wX : int<units>, wY : int<units>) as waypoint) (instructions : Movement list) =
            match instructions with
            | [] -> pos
            | instr :: instructions ->
                match instr with
                | Absolute a ->
                    // Move the waypoint only
                    go pos (goForward a.Direction a.Distance waypoint) instructions
                | Relative (Forward i) ->
                    // Sigh, they changed the domain for this part and `i` is no longer representing a `units`
                    let movementX = wX * i / 1<units>
                    let movementY = wY * i / 1<units>
                    go (x + movementX, y + movementY) waypoint instructions
                | Relative (Left i) ->
                    go pos (rotateLeft i waypoint) instructions
                | Relative (Right i) ->
                    go pos (rotateRight i waypoint) instructions

        Utils.readResource "Day12Input.txt"
        |> List.map Movement.Parse
        |> go (0<units>, 0<units>) (10<units>, 1<units>)
        |> fun (a, b) -> abs a + abs b
