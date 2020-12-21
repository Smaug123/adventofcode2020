namespace AdventOfCode

open AdventOfCode.Internals

[<RequireQualifiedAccess>]
module Day21 =

    type Food = Food of string
    type Allergen = Allergen of string

    let parse (s : string) : Food Set * Allergen list =
        match s.Split " (contains " with
        | [| foods ; allergens |] ->
            let foods =
                foods.Split " "
                |> Array.map Food
                |> Set.ofArray
            let allergens = allergens.Split ", "
            allergens.[allergens.Length - 1] <- allergens.[allergens.Length - 1].TrimEnd(')')
            let allergens = allergens |> Array.map Allergen |> List.ofArray
            foods, allergens
        | _ -> failwithf "Could not parse: %s" s

    let pinDown ingredientsLists =
        let allergenPossibilities =
            ingredientsLists
            |> List.fold (fun possibles (foods, allergens) ->
                allergens
                |> List.fold (fun possibles allergen ->
                    possibles
                    |> Map.change allergen (function | None -> Some [foods] | Some otherFoods -> Some (foods :: otherFoods))
                ) possibles
            ) Map.empty

        // If the intersection of an allergen's foods is of size exactly 1, then we've found it!
        let rec go (pinnedDown : Map<Allergen, Food>) (allergenPossibilities : Map<Allergen, Food Set list>) =
            if allergenPossibilities.Count = 0 then Ok pinnedDown else
            let newlyPinned =
                allergenPossibilities
                |> Map.toSeq
                |> Seq.choose (fun (allergen, foods) ->
                    Set.intersectMany foods
                    |> Seq.tryExactlyOne
                    |> Option.map (fun food -> allergen, food)
                )
                |> Seq.tryHead
            match newlyPinned with
            | None -> Error (pinnedDown, allergenPossibilities)
            | Some (allergen, food) ->
                allergenPossibilities
                |> Map.remove allergen
                |> Map.map (fun _ -> List.map (Set.remove food))
                |> go (pinnedDown |> Map.add allergen food)

        let result = go Map.empty allergenPossibilities
        match result with
        | Error (pinnedDown, allergenPossibilities) ->
            pinnedDown
            |> Map.iter (fun (Allergen a) (Food f) -> printfn "%s: %s" a f)

            failwithf "The following are undetermined: %+A" allergenPossibilities

        | Ok fullyPinnedDown ->
            fullyPinnedDown

    let part1 () =
        let ingredientsLists =
            Utils.readResource "Day21Input.txt"
            |> List.map parse

        let fullyPinnedDown = pinDown ingredientsLists

        let knownAllergens = fullyPinnedDown |> Map.toSeq |> Seq.map snd |> Set.ofSeq

        ingredientsLists
        |> List.map (fun (foods, _) -> Set.difference foods knownAllergens)
        |> List.sumBy Set.count

    let part2 () =
        let ingredientsLists =
            Utils.readResource "Day21Input.txt"
            |> List.map parse

        let fullyPinnedDown = pinDown ingredientsLists

        fullyPinnedDown
        |> Map.toSeq
        |> Seq.map (fun (_, Food f) -> sprintf "%s" f)
        |> String.concat ","
