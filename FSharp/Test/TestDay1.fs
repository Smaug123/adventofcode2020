namespace AdventOfCode.Test

open NUnit.Framework
open AdventOfCode
open FsUnitTyped

[<TestFixture>]
module TestDay1 =

    [<Test>]
    let ``Part 1`` () =
        Day1.part1 ()
        |> shouldEqual (Some 158916)

    [<Test>]
    let ``Part 2`` () =
        Day1.part2 ()
        |> shouldEqual (Some 165795564)
        Day1.part2' ()
        |> shouldEqual (Some 165795564)
