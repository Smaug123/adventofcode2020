namespace AdventOfCode.Test

open NUnit.Framework
open AdventOfCode
open FsUnitTyped

[<TestFixture>]
module TestDay18 =

    [<Test>]
    let ``Part 1 examples`` () =
        [
            "1 + (2 * 3) + (4 * (5 + 6))", 51L
            "2 * 3 + (4 * 5)", 26L
            "5 + (8 * 3 + 9 + 3 * 4 * 3)", 437L
            "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", 12240L
            "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", 13632L
        ]
        |> List.iter (fun (expr, expected) -> Day18.lex expr |> Day18.parse1 |> Day18.eval |> shouldEqual expected)

    [<Test>]
    let ``Part 2 examples`` () =
        [
            "1 + 2 * 3 + 4 * 5 + 6", 231L
            "1 + (2 * 3) + (4 * (5 + 6))", 51L
            "2 * 3 + (4 * 5)", 46L
            "5 + (8 * 3 + 9 + 3 * 4 * 3)", 1445L
            "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", 669060L
            "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", 23340L
        ]
        |> List.iter (fun (expr, expected) -> Day18.lex expr |> Day18.parse2 |> Day18.eval |> shouldEqual expected)

    [<Test>]
    let ``Part 1`` () =
        Day18.part1 ()
        |> shouldEqual 4491283311856L

    [<Test>]
    let ``Part 2`` () =
        Day18.part2 ()
        |> shouldEqual 68852578641904L

