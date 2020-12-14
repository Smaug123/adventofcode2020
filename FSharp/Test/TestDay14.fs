namespace AdventOfCode.Test

open NUnit.Framework
open AdventOfCode
open FsUnitTyped

[<TestFixture>]
module TestDay14 =

    [<Test>]
    let ``Part 1`` () =
        Day14.part1 ()
        |> shouldEqual 6631883285184UL

    [<Test>]
    let ``Part 2`` () =
        Day14.part2 ()
        |> shouldEqual 3161838538691UL
