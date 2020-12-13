namespace AdventOfCode.Internals

open System.IO
open System.Reflection

[<RequireQualifiedAccess>]
module Utils =
    type private Dummy = class end

    let readResource' (name : string) : string array =
        let asm = Assembly.GetAssembly typeof<Dummy>
        use stream = asm.GetManifestResourceStream (sprintf "AdventOfCode.%s" name)
        let s =
            use reader = new StreamReader(stream)
            reader.ReadToEnd()
        s.Split('\r', '\n')

    let inline readResource (name : string) : string list =
        readResource' name |> List.ofArray

    let fixedPoint<'a when 'a : equality> (f : 'a -> 'a) (start : 'a) : 'a =
        seq {
            let mutable a1 = start
            let mutable a2 = f start
            yield a1
            while a1 <> a2 do
                yield a2
                a1 <- a2
                a2 <- f a2
        }
        |> Seq.last

[<RequireQualifiedAccess>]
module Seq =

    let tryMinAndMax<'a when 'a : comparison> (s : 'a seq) : ('a * 'a) option =
        use s = s.GetEnumerator ()
        if not <| s.MoveNext () then None else
        let mutable min = s.Current
        let mutable max = s.Current
        while s.MoveNext () do
            if s.Current < min then min <- s.Current else
            if s.Current > max then max <- s.Current

        Some (min, max)

    let splitAt (f : 'a -> bool) (x : 'a seq) : 'a list seq =
        seq {
            use i = x.GetEnumerator ()
            let mutable soFar = ResizeArray ()
            while i.MoveNext () do
                if f i.Current then
                    yield List.ofSeq soFar
                    soFar.Clear ()
                else
                    soFar.Add i.Current

            if soFar.Count > 0 then
                yield List.ofSeq soFar
        }

/// This should be in the standard library.
type ResultBuilder () =
    member __.MergeSources<'a, 'b, 'err> (a : Result<'a, 'err list>, b : Result<'b, 'err list>) : Result<'a * 'b, 'err list> =
        match a, b with
        | Ok a, Ok b -> Ok (a, b)
        | Error e, Ok _
        | Ok _, Error e -> Error e
        | Error e1, Error e2 -> Error (e1 @ e2)

    member __.BindReturn<'a, 'b, 'err> (a : Result<'a, 'err>, f : 'a -> 'b) : Result<'b, 'err> =
        match a with
        | Ok a -> Ok (f a)
        | Error e -> Error e

[<AutoOpen>]
module Builders =
    let result<'a, 'b, 'err> = ResultBuilder()

[<RequireQualifiedAccess>]
module Result =
    let get r =
        match r with
        | Ok i -> i
        | Error f -> failwithf "Assertion failure: result was error: %+A" f

[<RequireQualifiedAccess>]
module Int =
    let inline sqrt (i : ^a) =
        if i <= LanguagePrimitives.GenericOne then i else
        let rec go start =
            let next = start + LanguagePrimitives.GenericOne
            let sqr = next * next
            if sqr < LanguagePrimitives.GenericZero then
                // Overflow attempted, so the sqrt is between start and next
                start
            elif i < sqr then
                start
            elif i = sqr then next
            else go next
        go LanguagePrimitives.GenericOne

    let inline isPrime i =
        if i <= LanguagePrimitives.GenericOne then false else
        seq {
            for j in (LanguagePrimitives.GenericOne + LanguagePrimitives.GenericOne)..sqrt i do
                if i % j = LanguagePrimitives.GenericZero then yield false
            yield true
        }
        |> Seq.head

    /// Find Hcf, A, B s.t. A * a + B * b = Hcf, and Hcf is the highest common factor of a and b.
    let inline euclideanAlgorithm (a : ^a) (b : ^a) : {| Hcf : ^a ; A : ^a ; B : ^a |} =
        let rec go rMin1 r sMin1 s tMin1 t =
            if r = LanguagePrimitives.GenericZero then {| Hcf = rMin1 ; A = sMin1 ; B = tMin1 |} else
            let newQ = rMin1 / r
            go r (rMin1 - newQ * r) s (sMin1 - newQ * s) t (tMin1 - newQ * t)

        let maxA = max a b
        let minB = min a b
        let result = go maxA minB LanguagePrimitives.GenericOne LanguagePrimitives.GenericZero LanguagePrimitives.GenericZero LanguagePrimitives.GenericOne
        if a = maxA then result else {| Hcf = result.Hcf ; A = result.B ; B = result.A |}
