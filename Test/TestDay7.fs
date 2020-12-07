namespace Test

open AdventOfCode
open FsUnitTyped
open NUnit.Framework

[<TestFixture>]
module TestDay7 =

    [<Test>]
    let ``Test parser`` () =
        Day7.Rule.Parse "shiny red bags contain 1 posh olive bag, 1 vibrant green bag, 4 muted purple bags."
        |> shouldEqual
            {
                Type = Day7.Colour "shiny red"
                Contains =
                    [
                        Day7.Colour "posh olive", 1
                        Day7.Colour "vibrant green", 1
                        Day7.Colour "muted purple", 4
                    ]
                    |> Map.ofList
            }

    [<Test>]
    let ``Test fixedPoint`` () =
        Day7.fixedPoint (fun i -> if i > 10 then i else i + 1) 0
        |> shouldEqual 11
        Day7.fixedPoint (fun i -> if i > 10 then i else i + 1) 10
        |> shouldEqual 11
        Day7.fixedPoint (fun i -> if i > 10 then i else i + 1) 11
        |> shouldEqual 11

    [<Test>]
    let ``Test part 1`` () =
        Day7.part1 () |> shouldEqual 139

    [<Test>]
    let ``Part 2 example`` () =
        [
            "light red bags contain 1 bright white bag, 2 muted yellow bags."
            "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
            "bright white bags contain 1 shiny gold bag."
            "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
            "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
            "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
            "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
            "faded blue bags contain no other bags."
            "dotted black bags contain no other bags."
        ]
        |> List.map Day7.Rule.Parse
        |> List.map (fun i -> i.Type, i.Contains)
        |> Map.ofList
        |> Day7.ofRules (Day7.Colour "shiny gold")
        |> Day7.cata<int> (fun _ counts ->
            counts
            |> List.map (fun (i, j) -> i * j)
            |> List.sum
            |> (+) 1
        )
        |> fun c -> c - 1
        |> shouldEqual 32

    [<Test>]
    let ``Test part 2`` () =
        Day7.part2 () |> shouldEqual 58175

