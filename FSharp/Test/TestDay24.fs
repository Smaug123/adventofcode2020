namespace AdventOfCode.Test

open NUnit.Framework
open AdventOfCode
open FsUnitTyped

[<TestFixture>]
module TestDay24 =

    [<Test>]
    let ``Part 1`` () =
        Day24.part1 ()
        |> shouldEqual 330

    [<Test>]
    let ``Part 2`` () =
        Day24.part2 ()
        |> shouldEqual 3711
