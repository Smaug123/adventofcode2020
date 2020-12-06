namespace Test

open AdventOfCode
open FsUnitTyped
open NUnit.Framework

[<TestFixture>]
module TestDay6 =

    [<Test>]
    let ``Test part 1`` () =
        Day6.part1 () |> shouldEqual 6885

    [<Test>]
    let ``Test part 2`` () =
        Day6.part2 () |> shouldEqual 3550

