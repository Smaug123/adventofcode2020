namespace AdventOfCode

open AdventOfCode.Internals
open System
open System.Collections.Generic

[<RequireQualifiedAccess>]
module Day7 =

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

    type Colour = Colour of string

    type Rule =
        {
            Type : Colour
            Contains : Map<Colour, int>
        }

        static member Parse (s : string) : Rule =
            // e.g. shiny red bags contain 1 posh olive bag, 1 vibrant green bag, 4 muted purple bags.
            match s.Split " bags contain " with
            | [| colour ; rest |] ->
                {
                    Type = Colour colour
                    Contains =
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
                }
            | _ -> failwithf "Could not parse: %s" s

    let part1 () =
        let bags =
            Utils.readResource "Day7Input.txt"
            |> List.map Rule.Parse

        do
            let colours =
                bags
                |> List.map (fun i -> i.Type)
            let distincted = colours |> Set.ofList
            assert (List.length colours = Set.count distincted)

        let rec go (required : Colour) (canContain : Colour Set) (rules : Rule list) : Colour Set =
            match rules with
            | [] -> canContain
            | rule :: rules ->
                let canContain =
                    if Map.exists (fun colour _ -> colour = required || Set.contains colour canContain) rule.Contains then
                        Set.add rule.Type canContain
                    else canContain

                go required canContain rules

        fixedPoint (fun s -> go (Colour "shiny gold") s bags) Set.empty
        |> Set.count

    type Tree =
        {
            Colour : Colour
            Children : (int * Tree) list
        }

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

    let ofRules (colour : Colour) (rules : Map<Colour, Map<Colour, int>>) : Tree =
        go rules [colour] Map.empty
        |> Map.find colour

    let cata<'ret> (b : Colour -> (int * 'ret) list -> 'ret) (t : Tree) : 'ret =
        let memoize = Dictionary ()
        let rec go (t : Tree) =
            match memoize.TryGetValue t.Colour with
            | true, v -> v
            | false, _ ->
                let result =
                    t.Children
                    |> List.map (fun (count, tree) ->
                        let ret = go tree
                        count, ret
                    )
                    |> b t.Colour
                memoize.[t.Colour] <- result
                result

        go t

    let part2 () : int =
        Utils.readResource "Day7Input.txt"
        |> List.map Rule.Parse
        |> List.map (fun i -> i.Type, i.Contains)
        |> Map.ofList
        |> ofRules (Colour "shiny gold")
        |> cata<int> (fun _ counts ->
            counts
            |> List.map (fun (i, j) -> i * j)
            |> List.sum
            |> (+) 1 // for the node itself
        )
        |> fun c -> c - 1 // because the outermost bag doesn't count

