namespace AdventOfCode.Test

open NUnit.Framework
open AdventOfCode
open FsUnitTyped

[<TestFixture>]
module TestDay12 =

    [<Test>]
    let ``Part 1`` () =
        Day12.part1 ()
        |> shouldEqual 1007<Day12.units>

    [<Test>]
    let ``Part 2`` () =
        Day12.part2 ()
        |> shouldEqual 41212<Day12.units>



