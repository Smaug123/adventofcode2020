namespace AdventOfCode.Test

open NUnit.Framework
open AdventOfCode
open FsUnitTyped

[<TestFixture>]
module TestDay16 =

    [<Test>]
    let ``Part 1`` () =
        Day16.part1 ()
        |> shouldEqual 20231

    [<Test>]
    let ``Part 2`` () =
        Day16.part2 ()
        |> shouldEqual 1940065747861L
