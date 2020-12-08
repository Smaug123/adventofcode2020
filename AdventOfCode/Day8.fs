namespace AdventOfCode

open AdventOfCode.Internals
open System

[<RequireQualifiedAccess>]
module Day8 =

    [<Measure>]
    type ProgramCount

    [<Measure>]
    type Accumulated

    type Instruction =
        | Nop of int
        | Jmp of int<ProgramCount>
        | Acc of int<Accumulated>
        static member Parse (s : string) : Instruction =
            match s.Split () with
            | [| instr ; arg |] ->
                match instr, Int32.TryParse arg with
                | "nop", (true, v) -> Nop v
                | "jmp", (true, v) -> Jmp (v * 1<ProgramCount>)
                | "acc", (true, v) -> Acc (v * 1<Accumulated>)
                | instr, (true, v) -> failwithf "Unexpected instruction: '%s' with argument %i" instr v
                | instr, (false, _) -> failwithf "Unexpected instruction: '%s' with unparsed argument '%s'" instr arg
            | _ ->
                failwithf "Unexpected format: '%s'" s

    type State =
        {
            Accumulator : int<Accumulated>
            ProgramCounter : int<ProgramCount>
            SeenPreviously : int<ProgramCount> Set
        }
        static member Empty () : State =
            {
                Accumulator = 0<Accumulated>
                ProgramCounter = 0<ProgramCount>
                SeenPreviously = Set.empty
            }

    type MachineError =
        | InfiniteLoop of State

    let eval (state : State) (instr : Instruction) : Result<State, MachineError> =
        if Set.contains state.ProgramCounter state.SeenPreviously then Error (InfiniteLoop state) else

        match instr with
        | Nop _ ->
            { state with
                ProgramCounter = state.ProgramCounter + 1<ProgramCount>
                SeenPreviously = Set.add state.ProgramCounter state.SeenPreviously
            }
        | Jmp i ->
            { state with
                ProgramCounter = state.ProgramCounter + i
                SeenPreviously = Set.add state.ProgramCounter state.SeenPreviously
            }
        | Acc i ->
            {
                ProgramCounter = state.ProgramCounter + 1<ProgramCount>
                Accumulator = state.Accumulator + i
                SeenPreviously = Set.add state.ProgramCounter state.SeenPreviously
            }
        |> Ok

    let runToCompletion (instructions : Instruction array) : Result<State, MachineError> =
        let len = instructions.Length * 1<ProgramCount>
        let rec go (s : State) =
            if s.ProgramCounter < len then
                match eval s instructions.[s.ProgramCounter / 1<ProgramCount>] with
                | Ok next -> go next
                | Error e -> Error e
            else
                Ok s

        go (State.Empty ())

    let part1 () =
        Utils.readResource "Day8Input.txt"
        |> List.map Instruction.Parse
        |> List.toArray
        |> runToCompletion
        |> function | Error (MachineError.InfiniteLoop s) -> s.Accumulator | x -> failwithf "Unexpected result: %+A" x

    let part2 () =
        let instr =
            Utils.readResource "Day8Input.txt"
            |> List.map Instruction.Parse
            |> List.toArray

        let canChange =
            instr
            |> Seq.mapi (fun i instr ->
                match instr with
                | Nop x -> Some (i, Jmp (x * 1<ProgramCount>))
                | Jmp x -> Some (i, Nop (x / 1<ProgramCount>))
                | _ -> None
            )
            |> Seq.choose id
            |> Seq.toList

        canChange
        |> Seq.choose (fun (i, changed) ->
            let old = instr.[i]
            // cheeky
            instr.[i] <- changed
            let result = runToCompletion instr
            instr.[i] <- old

            match result with
            | Ok e -> Some e
            | _ -> None
        )
        |> Seq.exactlyOne
        |> fun i -> i.Accumulator
