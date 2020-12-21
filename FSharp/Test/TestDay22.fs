namespace AdventOfCode.Test

open NUnit.Framework
open AdventOfCode
open FsUnitTyped

[<TestFixture>]
module TestDay22 =

    [<Test>]
    let ``Part 1`` () =
        Day22.part1 ()
        |> shouldEqual -1

    [<Test>]
    let ``Part 2`` () =
        Day22.part2 ()
        |> shouldEqual -1

