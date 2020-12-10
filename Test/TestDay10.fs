namespace AdventOfCode.Test

open NUnit.Framework
open AdventOfCode
open FsUnitTyped

[<TestFixture>]
module TestDay10 =

    [<Test>]
    let ``Part 1`` () =
        Day10.part1 ()
        |> shouldEqual 1917

    [<Test>]
    let ``Part 2`` () =
        Day10.part2 ()
        |> shouldEqual 113387824750592L

