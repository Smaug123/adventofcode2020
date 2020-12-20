namespace AdventOfCode.Test

open NUnit.Framework
open AdventOfCode
open FsUnitTyped

[<TestFixture>]
module TestDay20 =

    [<Test>]
    let ``Part 1`` () =
        Day20.part1 ()
        |> shouldEqual 17148689442341L

    [<Test>]
    let ``Part 2`` () =
        Day20.part2 ()
        |> shouldEqual -1
