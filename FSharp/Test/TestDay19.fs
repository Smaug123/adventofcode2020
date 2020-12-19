namespace AdventOfCode.Test

open NUnit.Framework
open AdventOfCode
open FsUnitTyped

[<TestFixture>]
module TestDay19 =

    [<Test>]
    let ``Part 1`` () =
        Day19.part1 ()
        |> shouldEqual 173

    [<Test>]
    let ``Part 2`` () =
        Day19.part2 ()
        |> shouldEqual 367
