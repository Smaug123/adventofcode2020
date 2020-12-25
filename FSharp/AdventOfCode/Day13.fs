namespace AdventOfCode

open AdventOfCode.Internals

[<RequireQualifiedAccess>]
module Day13 =

    let part1 () : int =
        let lines = Utils.readResource "Day13Input.txt"
        let earliestTime = int lines.[0]
        let busTimes =
            lines.[1].Split(',')
            |> Seq.choose (function | "x" -> None | x -> Some (int x))
            |> List.ofSeq

        busTimes
        |> List.map (fun t -> t, t - (earliestTime % t))
        |> List.minBy snd
        |> fun (id, t) -> id * t

    [<Struct>]
    type Modulus<'a> =
        | Modulus of 'a

    [<RequireQualifiedAccess>]
    module Modulus =
        let inline Times (Modulus (a : ^b)) (Modulus (b : ^b)) : Modulus< ^b> = Modulus (a * b)

    let inline private timesModTriple (modulus : ^a) (x1 : ^a) (x2 : ^a) (x3 : ^a) : ^a =
        Int.timesMod modulus x1 (Int.timesMod modulus x2 x3)

    /// Find the unique i, with 0 <= i < mod1 * mod2, such that i = x (mod mod1) and i = y (mod mod2).
    /// For simplicity, we assert that the mod1, mod2 are distinct primes.
    let inline chineseRemainder (Modulus (mod1 : ^a)) (Modulus (mod2 : ^a)) (x : ^a) (y : ^a) : ^a =
        assert (mod1 <> mod2)

        let euc = Int.euclideanAlgorithm mod1 mod2
        let t1 = timesModTriple (mod1 * mod2) x euc.B mod2
        let t2 = timesModTriple (mod1 * mod2) y euc.A mod1
        let result = Int.addMod (mod1 * mod2) t1 t2
        if result < LanguagePrimitives.GenericZero then result + (mod1 * mod2) else result

    let inline chineseRemainder' (xs : (^a * Modulus< ^a>) list) =
        // Curse the inability of FSharp to have genuinely recursive inline functions, or to recognise that this is
        // tail recursive
        let mutable xs = xs
        let mutable stop = false
        while not stop do
            match xs with
            | [] -> failwith "no empty lists pls"
            | [_] -> stop <- true
            | (x, mod1) :: (y, mod2) :: rest ->
                xs <- (chineseRemainder mod1 mod2 x y, Modulus.Times mod1 mod2) :: rest
        xs
        |> List.exactlyOne
        |> fst

    let parse (s : string) =
        s.Split(',')
        |> Seq.mapi (fun offset ->
            function
            | "x" -> None
            | x ->
                let toInt = int64 x
                if not (Int.isPrime toInt) then
                    failwithf "Assumption violated: expected prime bus times, got '%i'" toInt
                Some (((- (int64 offset)) % toInt) + toInt, Modulus toInt))
        |> Seq.choose id
        |> List.ofSeq

    let part2 () : int64 =
        let lines = Utils.readResource "Day13Input.txt"
        let busTimes = parse lines.[1]

        chineseRemainder' busTimes
