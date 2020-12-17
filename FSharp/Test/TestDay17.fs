namespace AdventOfCode.Test

open NUnit.Framework
open AdventOfCode
open FsUnitTyped

[<TestFixture>]
module TestDay17 =

    [<Test>]
    let ``Part 1`` () =
        Day17.part1 ()
        |> shouldEqual 202

    [<Test>]
    let ``Part 2`` () =
        Day17.part2 ()
        |> shouldEqual 2028
