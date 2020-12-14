namespace AdventOfCode

open System.Text.RegularExpressions
open AdventOfCode.Internals
open System.Collections.Immutable

[<RequireQualifiedAccess>]
module Day14 =

    type Location = Location of uint64

    type Mask =
        {
            ZeroMask : uint64
            OneMask : uint64
            FloatingPositions : uint64 list
            FloatingMask : uint64
        }
        static member Parse (s : string) : Mask =
            let rec go (zeroMask : uint64) (oneMask : uint64) (floatingPos : uint64 list) (i : int) =
                if i = s.Length then
                    {
                        ZeroMask = zeroMask
                        OneMask = oneMask
                        FloatingPositions = floatingPos
                        FloatingMask = ~~~(List.sum floatingPos)
                    }
                else

                let zeroMask, oneMask, floatingPos =
                    match s.[i] with
                    | '0' -> (zeroMask <<< 1) + 1UL, oneMask <<< 1, floatingPos
                    | '1' -> zeroMask <<< 1, (oneMask <<< 1) + 1UL, floatingPos
                    | 'X' -> zeroMask <<< 1, oneMask <<< 1, (1UL <<< (s.Length - i - 1)) :: floatingPos
                    | c -> failwithf "Failed to parse char: '%c' in '%s'" c s

                go zeroMask oneMask floatingPos (i + 1)

            go 0UL 0UL [] 0

        static member Initial =
            {
                FloatingPositions = []
                FloatingMask = 0UL
                ZeroMask = 0UL
                OneMask = 0UL
            }

    let rec subsets (l : 'a list) =
        match l with
        | [] -> [[]]
        | x :: xs ->
            let subs = subsets xs
            subs
            |> List.collect (fun s -> [x :: s ; s])

    [<RequireQualifiedAccess>]
    module Mask =
        let applyNonFloating (m : Mask) (v : uint64) : uint64 =
            (v &&& (~~~m.ZeroMask)) ||| m.OneMask

        let applyFloating (m : Mask) (v : uint64) : uint64 list =
            let init = (v &&& m.FloatingMask) ||| m.OneMask
            subsets m.FloatingPositions
            |> List.map (fun subset -> List.sum subset + init)

    let private regex = Regex(@"mem\[([0-9]+)\] = ([0-9]+)")

    type Instruction =
        | UpdateMask of Mask
        | MemSet of Location * uint64

        static member Parse (s : string) : Instruction =
            if s.StartsWith "mask = " then
                UpdateMask (Mask.Parse s.[7..])
            else

            let matches = regex.Match s
            if not matches.Success then
                failwithf "Unexpected failure to parse: '%s'" s
            else
                MemSet (Location (uint64 matches.Groups.[1].Value), uint64 matches.Groups.[2].Value)

    let part1 () =
        let rec go (memory : Map<Location, uint64>) (mask : Mask) (instrs : Instruction list) : Map<Location, uint64> =
            match instrs with
            | [] -> memory
            | instr :: instrs ->
                match instr with
                | UpdateMask mask -> go memory mask instrs
                | MemSet (loc, value) ->
                    let memory = Map.add loc (Mask.applyNonFloating mask value) memory
                    go memory mask instrs

        Utils.readResource "Day14Input.txt"
        |> List.map Instruction.Parse
        |> go Map.empty Mask.Initial
        |> Map.toSeq
        |> Seq.sumBy (snd >> uint64)

    let part2 () =
        let rec go (memory :ImmutableDictionary<Location, uint64>) (mask : Mask) (instrs : Instruction list) : ImmutableDictionary<Location, uint64> =
            match instrs with
            | [] -> memory
            | instr :: instrs ->
                match instr with
                | UpdateMask mask -> go memory mask instrs
                | MemSet (Location loc, value) ->
                    let memory =
                        Mask.applyFloating mask loc
                        |> List.fold (fun (memory : ImmutableDictionary<_, _>) loc -> memory.Add (Location loc, value)) memory
                    go memory mask instrs

        Utils.readResource "Day14Input.txt"
        |> List.map Instruction.Parse
        |> go ImmutableDictionary.Empty Mask.Initial
        |> Seq.sumBy (fun (KeyValue(_, v)) -> uint64 v)
