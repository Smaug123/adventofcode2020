namespace AdventOfCode.Test

open NUnit.Framework
open AdventOfCode
open FsUnitTyped

[<TestFixture>]
module TestDay15 =

    [<Test>]
    let ``Part 1`` () =
        Day15.part1 ()
        |> shouldEqual 1085

    [<Test>]
    let ``Part 2`` () =
        Day15.part2 ()
        |> shouldEqual 10652
