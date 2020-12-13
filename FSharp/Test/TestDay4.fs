namespace Test

open FsUnitTyped
open NUnit.Framework
open AdventOfCode

[<TestFixture>]
module TestDay4 =

    [<Test>]
    let ``Part 1`` () =
        Day4.part1 () |> shouldEqual 226

    [<Test>]
    let ``Part 2`` () =
        Day4.part2 () |> shouldEqual 160
