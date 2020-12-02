namespace AdventOfCode

open System.IO
open System.Reflection
open AdventOfCode.Internals

[<RequireQualifiedAccess>]
module Day2 =

    type Policy =
        {
            Letter : char
            Min : int
            /// To do this in a truly type-safe way, Max would instead be
            /// a delta from Min, guaranteeing by construction that
            /// Max > Min.
            Max : int
        }

        /// For ease of typing (heh) I've used `string` for the error-case result.
        /// In Real Life (tm) I'd define a dedicated type for this.
        static member Parse (s : string) : Result<Policy * string, string> =
            let components = s.Split " "
            match components with
            | [| minMax ; letterAndColon ; password |] ->
                let minMax = minMax.Split "-"
                match minMax with
                | [| min ; max |] ->
                    // And if I were being actually careful, I'd Int32.TryParse `min` and `max`.
                    let policy =
                        {
                            Letter = letterAndColon.[0]
                            Min = int min
                            Max = int max
                        }
                    Ok (policy, password)
                | _ ->
                    Error (sprintf "Somehow got multiple entries in minMax for '%s'." s)
            | _ ->
                Error (sprintf "Somehow got an unexpected number of entries in components for '%s'." s)


    let isMatch1 (policy : Policy) (str : string) =
        let count =
            str
            |> Seq.fold (fun i chr ->
                if chr = policy.Letter then i + 1 else i
            ) 0
        policy.Min <= count && count <= policy.Max

    let private get () : (Policy * string) list =
        Utils.readResource "Day2Input.txt"
        |> List.map (Policy.Parse >> Result.get)

    let part1 () =
        get ()
        |> Seq.filter (fun (p, s) -> isMatch1 p s)
        |> Seq.length

    let isMatch2 (p : Policy) (s : string) =
        (s.[p.Min - 1] = p.Letter) <> (s.[p.Max - 1] = p.Letter)

    let part2 () =
        get ()
        |> Seq.filter (fun (p, s) -> isMatch2 p s)
        |> Seq.length


[<RequireQualifiedAccess>]
module Day2StateMachine =
    [<Struct>]
    type State =
        | Min of soFar : byte
        | Max of min' : byte * maxSoFar : byte
        | Seeking of min : byte * max : byte * seeking : char * count : byte

    let private chrToByte (c : char) : byte =
        match c with
        | '0' -> 0uy
        | '1' -> 1uy
        | '2' -> 2uy
        | '3' -> 3uy
        | '4' -> 4uy
        | '5' -> 5uy
        | '6' -> 6uy
        | '7' -> 7uy
        | '8' -> 8uy
        | '9' -> 9uy
        | _ -> failwith "oh no"

    let rec go (input : string) (passingCount : int) (s : State) (pos : int) : int =
        // Example:
        //1-4 m: mrfmmbjxr
        match s with
        | Min i ->
            // Consuming characters from the start.
            if input.[pos] = '-' then
                go input passingCount (Max (i, 0uy)) (pos + 1)
            else
                go input passingCount (Min (10uy * i + chrToByte (input.[pos]))) (pos + 1)
        | Max (min, max) ->
            // Consuming characters from the middle, the "4" in the example
            if input.[pos] = ' ' then
                go input passingCount (Seeking (min, max, input.[pos + 1], 0uy)) (pos + 4)
            else
                go input passingCount (Max (min, 10uy * max + chrToByte (input.[pos]))) (pos + 1)
        | Seeking (min, max, seek, count) ->
            if pos >= input.Length then
                if (min <= count) = (count <= max) then passingCount + 1 else passingCount
            else
            match input.[pos] with
            | '\r' ->
                // End of example.
                if (min <= count) <> (count <= max) then
                    // Assume \r\n, hence +2
                    go input (passingCount + 1) (Min 0uy) (pos + 2)
                else
                    go input passingCount (Min 0uy) (pos + 2)
            | '\n' ->
                if (min <= count) = (count <= max) then
                    go input (passingCount + 1) (Min 0uy) (pos + 1)
                else
                    go input passingCount (Min 0uy) (pos + 1)
            | x when x = seek ->
                go input passingCount (Seeking (min, max, seek, count + 1uy)) (pos + 1)
            | _ ->
                go input passingCount (Seeking (min, max, seek, count)) (pos + 1)

    let part1 () =
        let asm = Assembly.GetAssembly typeof<State>
        use stream = asm.GetManifestResourceStream "AdventOfCode.Day2Input.txt"
        use reader = new StreamReader(stream)
        let s = reader.ReadToEnd ()
        go s 0 (Min 0uy) 0
