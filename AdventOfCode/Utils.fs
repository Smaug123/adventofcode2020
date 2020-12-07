namespace AdventOfCode.Internals

open System
open System.IO
open System.Reflection

[<RequireQualifiedAccess>]
module Utils =
    type private Dummy = class end

    let readResource (name : string) : string list =
        let asm = Assembly.GetAssembly typeof<Dummy>
        use stream = asm.GetManifestResourceStream (sprintf "AdventOfCode.%s" name)
        let s =
            use reader = new StreamReader(stream)
            reader.ReadToEnd()
        s.Split('\r', '\n')
        |> List.ofArray

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