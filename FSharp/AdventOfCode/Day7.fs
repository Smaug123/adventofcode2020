namespace AdventOfCode

open AdventOfCode.Internals
open System
open System.Collections.Generic

[<RequireQualifiedAccess>]
module Day7 =

    type Colour = Colour of string

    /// Parse a single line of puzzle input.
    let parse (s : string) : Colour * Map<Colour, int> =
        match s.Split " bags contain " with
        | [| colour ; rest |] ->
            Colour colour,
            if rest = "no other bags." then Map.empty else
            rest.Split ", "
            |> Seq.map (fun s ->
                let firstSpace = s.IndexOf ' '
                let num =
                    match Int32.TryParse s.[0..firstSpace] with
                    | true, v -> v
                    | false, _ ->
                        failwithf "Failed to parse '%s' as an int." s.[0..firstSpace]
                let lastSpace = s.LastIndexOf ' '
                let colour = s.[firstSpace + 1..lastSpace - 1]
                (Colour colour, num)
            )
            |> Map.ofSeq
        | _ -> failwithf "Could not parse: %s" s

    type Tree =
        {
            Colour : Colour
            Children : (int * Tree) list
        }

    /// A mutable object allowing memoisation in an invocation of a cata, or across invocations.
    type CataStore<'ret> =
        private
        | CataStore of Dictionary<Colour, 'ret>

    [<RequireQualifiedAccess>]
    module CataStore =
        let make<'ret> () : CataStore<'ret> = CataStore (Dictionary ())

    [<RequireQualifiedAccess>]
    module Tree =

        /// Construct the trees represented by the given rule set.
        /// We maintain a stack of not-yet-constructed nodes, represented as notConstructed.
        /// When we encounter a node, we check whether all its children have been constructed; the ones which have not, we
        /// push onto the stack.
        let rec private go (allRules : Map<Colour, Map<Colour, int>>) (notConstructed : Colour list) (soFar : Map<Colour, Tree>) : Map<Colour, Tree> =
            match notConstructed with
            | [] ->
                // Work queue consumed, no more work to do
                soFar
            | colour :: notConstructed ->

            match soFar.TryFind colour with
            | Some _ ->
                // We've already done the work for this colour. Ignore the work implied by it.
                go allRules notConstructed soFar
            | None ->

            // We haven't yet done the work. Iterate over the children, checking whether we've done the work for them;
            // for each child not yet done, add it to the queue.
            let changed, built, notConstructed =
                allRules.[colour]
                |> Map.fold (fun (changed, built, notConstructed) colour count ->
                    match soFar.TryFind colour with
                    | Some tree ->
                        changed, (count, tree) :: built, notConstructed
                    | None ->
                    true, built, (colour :: notConstructed)
                ) (false, [], colour :: notConstructed)

            if changed then
                // At least one child has had work pushed onto `notConstructed`.
                go allRules notConstructed soFar

            else

            // All the children have already been constructed.
            let subtree =
                {
                    Colour = colour
                    Children = built
                }

            Map.add colour subtree soFar
            |> go allRules notConstructed

        /// Construct trees for each of the input colours, given the global set of rules defining every tree.
        /// Trees are shared wherever possible.
        let ofRules (colours : Colour list) (rules : Map<Colour, Map<Colour, int>>) : Map<Colour, Tree> =
            let output = go rules colours Map.empty
            let colours = Set.ofList colours
            output
            |> Map.filter (fun colour _ -> colours.Contains colour)

        /// Warning: be careful which CataStore you pass into this function. We'll protect you at the type level only
        /// from using incompatible return types; we can't stop you reusing a store from a cata that was computing
        /// something different.
        let cata'<'ret> ((CataStore store) : CataStore<'ret>) (b : Colour -> (int * 'ret) list -> 'ret) (t : Tree) : 'ret =
            let rec go (t : Tree) =
                match store.TryGetValue t.Colour with
                | true, v -> v
                | false, _ ->
                    let result =
                        t.Children
                        |> List.map (fun (count, tree) ->
                            let ret = go tree
                            count, ret
                        )
                        |> b t.Colour
                    store.[t.Colour] <- result
                    result

            go t

        let cata<'ret> (b : Colour -> (int * 'ret) list -> 'ret) (t : Tree) : 'ret =
            cata'<'ret> (CataStore.make<'ret> ()) b t

    let part1 () =
        let rules =
            Utils.readResource "Day7Input.txt"
            |> List.map parse
            |> Map.ofList

        let cataStore = CataStore.make ()

        rules
        |> Tree.ofRules (rules |> Map.toSeq |> Seq.map fst |> List.ofSeq)
        |> Map.map (fun _ ->
            Tree.cata' cataStore (fun colour counts ->
                if colour = Colour "shiny gold" then 1
                elif counts |> Seq.map snd |> Seq.contains 1 then 1
                else 0
            )
        )
        |> Map.toSeq
        |> Seq.sumBy snd
        |> fun c -> c - 1 // the shiny gold bag can't contain itself

    let part2 () : int =
        Utils.readResource "Day7Input.txt"
        |> List.map parse
        |> Map.ofList
        |> Tree.ofRules [Colour "shiny gold"]
        |> fun i -> i.[Colour "shiny gold"]
        |> Tree.cata<int> (fun _ counts ->
            counts
            |> List.sumBy (fun (i, j) -> i * j)
            |> (+) 1 // for the node itself
        )
        |> fun c -> c - 1 // because the outermost bag doesn't count
