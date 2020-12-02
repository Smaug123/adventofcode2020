namespace AdventOfCode

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

