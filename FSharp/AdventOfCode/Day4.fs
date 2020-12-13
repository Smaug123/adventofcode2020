namespace AdventOfCode

open System
open AdventOfCode.Internals

/// This problem is particularly grotty; it's an exercise purely in data validation. I've gone the whole hog and
/// represented the domain in a more type-safe way than the problem really requires; if we were going to do any
/// further processing with the passport type, it would be very helpful to have the types aligned in the way I've
/// done here.
[<RequireQualifiedAccess>]
module Day4 =

    type PassportId =
        private
        | PassportId of string
        static member Parse (s : string) : PassportId option =
            if s.Length = 9 && s |> Seq.forall (fun i -> '0' <= i && i <= '9') then Some (PassportId s) else None

    type EyeColour =
        | Amber
        | Blue
        | Brown
        | Grey
        | Green
        | Hazel
        | Other

        static member Parse (s : string) : EyeColour option =
            match s with
            | "amb" -> Some Amber
            | "blu" -> Some Blue
            | "brn" -> Some Brown
            | "gry" -> Some Grey
            | "grn" -> Some Green
            | "hzl" -> Some Hazel
            | "oth" -> Some Other
            | _ -> None

    type HairColour =
        private
        | HairColour of string

        static member Parse (s : string) : HairColour option =
            if s.Length = 7 && s.[0] = '#' && s.[1..] |> Seq.forall (fun i -> ('a' <= i && i <= 'f') || ('0' <= i && i <= '9')) then
                Some (HairColour s)
            else None

    type Height =
        | Inches of int
        | Cm of int

        static member Parse (s : string) : Height option =
            if s.EndsWith "cm" then
                match Int32.TryParse (s.[0..s.Length - 3]) with
                | true, v when 150 <= v && v <= 193 ->
                    Some (Cm v)
                | _, _ -> None
            elif s.EndsWith "in" then
                match Int32.TryParse (s.[0..s.Length - 3]) with
                | true, v when 59 <= v && v <= 76 ->
                    Some (Inches v)
                | _, _ -> None
            else None

    type Passport =
        {
            // I did consider life to be too short to encode the constraints in the types for the three 'year' fields.
            BYR : int
            IYR : int
            EYR : int
            HGT : Height
            HCL : HairColour
            ECL : EyeColour
            PID : PassportId
            CID : string option
        }

    type Field = Field of string

    type ParseFailure =
        | UnknownField
        | MissingField
        | FailedValidation

    /// Returns the missing fields in the case of failure.
    let tryParse (s : string seq) : Result<Passport, Map<Field, ParseFailure>> =
        let split =
            s
            |> Seq.map (fun i ->
                i.Split ':'
                |> function
                    | [| x ; y |] ->
                        (Field x, y)
                    | xs -> failwithf "Failed parse: %+A" xs
            )
            |> Map.ofSeq

        let getInt (key : Field) : Result<int, (Field * ParseFailure) list> =
            match Map.tryFind key split with
            | Some i ->
                match key with
                | Field "byr" ->
                    match Int32.TryParse i with
                    | true, v when 1920 <= v && v <= 2002 -> Ok v
                    | false, _
                    | true, _ -> Error [key, FailedValidation]
                | Field "iyr" ->
                    match Int32.TryParse i with
                    | true, v when 2010 <= v && v <= 2020 -> Ok v
                    | false, _
                    | true, _ -> Error [key, FailedValidation]
                | Field "eyr" ->
                    match Int32.TryParse i with
                    | true, v when 2020 <= v && v <= 2030 -> Ok v
                    | false, _
                    | true, _ -> Error [key, FailedValidation]
                | _ -> Error [key, UnknownField]

            | None -> Error [key, MissingField]

        result {
            let! byr = getInt (Field "byr")
            and! iyr = getInt (Field "iyr")
            and! eyr = getInt (Field "eyr")
            and! hgt =
                match Map.tryFind (Field "hgt") split with
                | None ->
                    Error [Field "hgt", MissingField]
                | Some hgt ->
                    match Height.Parse hgt with
                    | None -> Error [Field "hgt", FailedValidation]
                    | Some i -> Ok i
            and! hcl =
                match Map.tryFind (Field "hcl") split with
                | Some i ->
                    match HairColour.Parse i with
                    | None -> Error [Field "hcl", FailedValidation]
                    | Some i -> Ok i
                | None -> Error [Field "hcl", MissingField]

            and! ecl =
                match Map.tryFind (Field "ecl") split with
                | Some i ->
                    match EyeColour.Parse i with
                    | None -> Error [Field "ecl", FailedValidation]
                    | Some i -> Ok i
                | None -> Error [Field "ecl", MissingField]

            and! pid =
                match Map.tryFind (Field "pid") split with
                | None ->
                    Error [Field "pid", MissingField]
                | Some pid ->
                    match PassportId.Parse pid with
                    | None -> Error [Field "pid", FailedValidation]
                    | Some i -> Ok i

            let cid = Map.tryFind (Field "cid") split

            return
                {
                    BYR = byr
                    IYR = iyr
                    EYR = eyr
                    HGT = hgt
                    HCL = hcl
                    ECL = ecl
                    PID = pid
                    CID = cid
                }
        }
        |> Result.mapError Map.ofList

    let private get () =
        let split =
            Utils.readResource "Day4Input.txt"
            |> Seq.splitAt ((=) "")
        split
        |> Seq.map (Seq.collect (fun i -> i.Split ()))
        |> Seq.map tryParse

    let part1 () =
        get ()
        |> Seq.sumBy (
            function
            | Ok _ -> 1
            | Error errors ->
                // Validation errors are fine; it's only missing-field errors we care about.
                errors
                |> Map.toSeq
                |> Seq.map snd
                |> Seq.forall (function | FailedValidation -> true | _ -> false)
                |> function | true -> 1 | false -> 0
        )

    let part2 () =
        get ()
        |> Seq.sumBy (function | Ok _ -> 1 | Error _ -> 0)

