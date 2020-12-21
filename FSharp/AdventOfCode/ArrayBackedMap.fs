namespace AdventOfCode.Internals

open System.Collections.Generic
open System

[<NoEquality ; NoComparison>]
type ArrayBackedMap<[<Measure>]'k, 'v> =
    private
        {
            Max : int
            Array : 'v voption array
            ElementCount : int
        }
    interface IReadOnlyDictionary<int<'k>, 'v> with
        member this.get_Item (key : int<'a>) : 'v =
            match this.Array.[key / LanguagePrimitives.Int32WithMeasure 1] with
            | ValueSome v -> v
            | ValueNone -> failwithf "could not find: %+A" key
        member this.get_Keys () =
            seq {
                for i in 0..this.Max+1 do
                    match this.Array.[i] with
                    | ValueNone -> ()
                    | ValueSome _ -> yield LanguagePrimitives.Int32WithMeasure i
            }
        member this.get_Values () =
            seq {
                for i in 0..this.Max+1 do
                    match this.Array.[i] with
                    | ValueNone -> ()
                    | ValueSome v -> yield v
            }
        member this.get_Count () = this.ElementCount
        member this.ContainsKey (k : int<'a>) =
            match this.Array.[k / LanguagePrimitives.Int32WithMeasure 1] with
            | ValueNone -> false
            | ValueSome _ -> true

        member this.TryGetValue (k : int<'a>, output : byref<_>) =
            match this.Array.[k / LanguagePrimitives.Int32WithMeasure 1] with
            | ValueNone -> false
            | ValueSome res ->
                output <- res
                true

    interface IEnumerable<KeyValuePair<int<'k>, 'v>> with
        member this.GetEnumerator () : IEnumerator<KeyValuePair<int<'a>, 'v>> =
            let mutable i = -1
            { new IEnumerator<_> with
                member __.get_Current () : KeyValuePair<int<'a>, 'v> =
                    KeyValuePair(LanguagePrimitives.Int32WithMeasure i, this.Array.[i] |> ValueOption.get)
                member __.get_Current () : obj =
                    KeyValuePair(LanguagePrimitives.Int32WithMeasure i, this.Array.[i] |> ValueOption.get)
                    |> box
                member __.MoveNext () =
                    if i >= this.Max then false else
                    i <- i + 1
                    let mutable stop = false
                    while stop = false && i < this.Max do
                        i <- i + 1
                        match this.Array.[i] with
                        | ValueNone -> i <- i + 1
                        | ValueSome _ -> stop <- true
                    true
                member __.Reset () =
                    i <- -1
                member __.Dispose () = ()
            }
    interface Collections.IEnumerable with
        member this.GetEnumerator () : Collections.IEnumerator =
            failwith "Don't use the non-generic IEnumerator, it's not the 2000's any more"

[<RequireQualifiedAccess>]
module ArrayBackedMap =
    let make<[<Measure>]'a, 'v> (biggest : int<'a>) (inputs : seq<int<'a> * 'v>) : ArrayBackedMap<'a,_> =
        let biggest = biggest / LanguagePrimitives.Int32WithMeasure 1

        let mutable count = 0
        let arr = Array.zeroCreate<'v voption> (biggest + 1)
        for (key, value) in inputs do
            count <- count + 1
            arr.[key / LanguagePrimitives.Int32WithMeasure 1] <- ValueSome value
        let count = count

        {
            Max = biggest
            Array = arr
            ElementCount = count
        }

    let map<[<Measure>]'a, 'v, 'v2> (f : int<'a> -> 'v -> 'v2) (m : ArrayBackedMap<'a, 'v>) : ArrayBackedMap<'a, 'v2> =
        {
            Max = m.Max
            Array = m.Array |> Array.mapi (fun i -> ValueOption.map (f (LanguagePrimitives.Int32WithMeasure i)))
            ElementCount = m.ElementCount
        }

    let choose<[<Measure>]'a, 'v, 'v2> (f : int<'a> -> 'v -> 'v2 option) (m : ArrayBackedMap<'a, 'v>) : ArrayBackedMap<'a, 'v2> =
        {
            Max = m.Max
            Array =
                let a = Array.zeroCreate m.Array.Length
                for i in 0..a.Length - 1 do
                    match m.Array.[i] with
                    | ValueNone -> ()
                    | ValueSome v ->
                        match f (LanguagePrimitives.Int32WithMeasure i) v with
                        | None -> ()
                        | Some v -> a.[i] <- ValueSome v
                a
            ElementCount = m.ElementCount
        }

    let fold<[<Measure>]'a, 'v, 'state> (f : 'state -> int<'a> -> 'v -> 'state) (s : 'state) (m : ArrayBackedMap<'a, 'v>) : 'state =
        let mutable s = s
        for i in 0..m.Array.Length - 1 do
            match m.Array.[i] with
            | ValueNone -> ()
            | ValueSome v -> s <- f s (LanguagePrimitives.Int32WithMeasure i) v
        s

    let iter<[<Measure>]'a, 'v, 'state> (f : int<'a> -> 'v -> unit) (m : ArrayBackedMap<'a, 'v>) : unit =
        for i in 0..m.Array.Length - 1 do
            match m.Array.[i] with
            | ValueNone -> ()
            | ValueSome v -> f (LanguagePrimitives.Int32WithMeasure i) v
