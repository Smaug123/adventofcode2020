namespace Test

open AdventOfCode
open FsUnitTyped
open NUnit.Framework

[<TestFixture>]
module TestDay8 =

    [<Test>]
    let ``Test part 1`` () =
        Day8.part1 () |> shouldEqual 2080<Day8.Accumulated>

    [<Test>]
    let ``Test part 2`` () =
        Day8.part2 () |> shouldEqual 2477<Day8.Accumulated>


