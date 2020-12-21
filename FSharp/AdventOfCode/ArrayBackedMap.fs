namespace AdventOfCode.Internals

open System.Collections.Generic
open System

[<NoEquality ; NoComparison>]
type ArrayBackedMap<[<Measure>]'k, 'v> =
    private
        {
            mutable Max : int
            Array : 'v voption array
            mutable ElementCount : int
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

    let ofSeq<[<Measure>]'a, 'v> (s : (int<'a> * 'v) seq) : ArrayBackedMap<'a, 'v> =
        let s = s |> Seq.cache
        let max = s |> Seq.map fst |> Seq.max
        make max s

    let lastWeaklyBefore<[<Measure>]'a, 'v> (k : int<'a>) (m : ArrayBackedMap<'a, 'v>) : (int<'a> * 'v) option =
        let k = k / LanguagePrimitives.Int32WithMeasure 1
        if k < 0 then None
        else
            let mutable i = min m.Max k
            let mutable stop = false
            while not stop && i >= 0 do
                match m.Array.[i / LanguagePrimitives.Int32WithMeasure 1] with
                | ValueNone -> i <- i - 1
                | ValueSome _ -> stop <- true

            if i < 0 then None else
            Some (LanguagePrimitives.Int32WithMeasure i, ValueOption.get m.Array.[i])

    let lastStrictlyAfter<[<Measure>]'a, 'v> (k : int<'a>) (m : ArrayBackedMap<'a, 'v>) : (int<'a> * 'v) option =
        let k = k / LanguagePrimitives.Int32WithMeasure 1
        if k >= m.Array.Length then None
        else
            let mutable i = max 0 k
            let mutable stop = false
            while not stop && i < m.Array.Length do
                match m.Array.[i / LanguagePrimitives.Int32WithMeasure 1] with
                | ValueNone -> i <- i + 1
                | ValueSome _ -> stop <- true

            if i >= m.Array.Length then None else
            Some (LanguagePrimitives.Int32WithMeasure i, ValueOption.get m.Array.[i])

    let tryFind<[<Measure>]'a, 'v> (k : int<'a>) (m : ArrayBackedMap<'a, 'v>) : 'v option =
        let k = k / LanguagePrimitives.Int32WithMeasure 1
        if k < 0 then None
        elif k > m.Max then None
        else
        match m.Array.[k] with
        | ValueNone -> None
        | ValueSome v -> Some v

    let find<[<Measure>]'a, 'v> (k : int<'a>) (m : ArrayBackedMap<'a, 'v>) : 'v =
        let k = k / LanguagePrimitives.Int32WithMeasure 1
        if k < 0 then failwithf "Attempted to find a negative index %i" k
        elif k > m.Max then failwithf "Attempted to find an index %i greater than the maximum size of this map, %i" k m.Max
        else m.Array.[k] |> ValueOption.get

    type AddFailure =
        | Negative of int
        | TooBig of tried : int * max : int

    [<RequireQualifiedAccess>]
    module Mutate =
        let addAsserting<[<Measure>]'a, 'v> (k : int<'a>) (v : 'v) (m : ArrayBackedMap<'a, 'v>) : unit =
            let k = k / LanguagePrimitives.Int32WithMeasure 1
            if k < 0 then failwithf "Attempted to add an index (%i) less than 0" k
            if k > m.Max then failwithf "Attempted to add at index '%i' to a map which is too small to hold it (max '%i')" k m.Max
            match m.Array.[k] with
            | ValueNone ->
                m.ElementCount <- m.ElementCount + 1
            | ValueSome _ -> ()
            m.Array.[k] <- ValueSome v

        let addSilently<[<Measure>]'a, 'v> (k : int<'a>) (v : 'v) (m : ArrayBackedMap<'a, 'v>) : unit =
            let k = k / LanguagePrimitives.Int32WithMeasure 1
            if k < 0 then ()
            elif k > m.Max then ()
            else
                match m.Array.[k] with
                | ValueNone ->
                    m.ElementCount <- m.ElementCount + 1
                | ValueSome _ -> ()
                m.Array.[k] <- ValueSome v

        let tryAdd<[<Measure>]'a, 'v> (k : int<'a>) (v : 'v) (m : ArrayBackedMap<'a, 'v>) : Result<unit, AddFailure> =
            let k = k / LanguagePrimitives.Int32WithMeasure 1
            if k < 0 then AddFailure.Negative k |> Error
            elif k > m.Max then AddFailure.TooBig (k, m.Max) |> Error
            else
                match m.Array.[k] with
                | ValueNone ->
                    m.ElementCount <- m.ElementCount + 1
                | ValueSome _ -> ()
                m.Array.[k] <- ValueSome v
                Ok ()
