namespace AdventOfCode

open System
open System.Text.RegularExpressions
open AdventOfCode.Internals

[<RequireQualifiedAccess>]
module Day19 =

    type Rule =
        | RecursiveRule of int list list
        | Exactly of char

        static member Parse (s : string) : int * Rule =
            match s.Split ": " with
            | [| num ; rest |] ->
                match Int32.TryParse num with
                | false, _ -> failwithf "Unexpectedly not an int before the colon: %s" s
                | true, ruleNum ->
                    let parseChunk (s : string) : int list =
                        s.Split ' '
                        |> Array.map int
                        |> Array.toList

                    if rest.[0] = '"' then ruleNum, Rule.Exactly rest.[1] else
                    match rest.Split " | " with
                    | others ->
                        others
                        |> Array.map parseChunk
                        |> Array.toList
                        |> Rule.RecursiveRule
                    |> fun i -> ruleNum, i

            | _ -> failwithf "Unexpected number of colons in '%s'" s

    let rec toRegex (allRules : Map<int, Rule>) (rule : Rule) : string =
        match rule with
        | Rule.Exactly c -> c.ToString()
        | Rule.RecursiveRule alternatives ->
            alternatives
            |> List.map (List.map (fun i -> Map.find i allRules |> toRegex allRules) >> String.concat "")
            |> String.concat ")|("
            |> sprintf "((%s))"

    let part1 () =
        let input =
            Utils.readResource "Day19Input.txt"
            |> Seq.splitAt ((=) "")
            |> List.ofSeq
        let rules, inputs =
            match input with
            | [ rules ; inputs ] -> List.map Rule.Parse rules |> Map.ofList, inputs
            | _ -> failwith "unexpected number of groups"

        let regex = sprintf "^%s$" (toRegex rules rules.[0]) |> Regex

        inputs
        |> List.filter regex.IsMatch
        |> List.length

    let part2 () =
        let input =
            Utils.readResource "Day19Input.txt"
            |> Seq.splitAt ((=) "")
            |> List.ofSeq
        let rules, inputs =
            match input with
            | [ rules ; inputs ] -> List.map Rule.Parse rules |> Map.ofList, inputs
            | _ -> failwith "unexpected number of groups"

        // We've been told to be dumb here. So let's analyse it manually a bit first.
        //  8: 42     -->   8: 42 8
        // 11: 42 31  -->  11: 42 11 31
        // and rule 0 is unchanged:
        // 0: 8 11
        // So rule 8 is now start with "at least one 42", and rule 11 is now "start with at least one 42, then end with the same number of 31's".
        // This is not something that regexes support (you can prove this with the pumping lemma), so we'll just hack it
        // with n different regexes.

        let regex42 = sprintf "%s" (toRegex rules rules.[42])
        let regex31 = sprintf "%s" (toRegex rules rules.[31])
        let regexes =
            [1..10]
            |> List.map (fun i -> sprintf "^(%s)+(%s){%i}(%s){%i}$" regex42 regex42 i regex31 i |> Regex)

        inputs
        |> List.filter (fun i -> regexes |> List.exists (fun regex -> regex.IsMatch i))
        |> List.length
