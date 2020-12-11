namespace AdventOfCode.Test

open NUnit.Framework
open AdventOfCode
open FsUnitTyped

[<TestFixture>]
module TestDay11 =

    [<Test>]
    let ``Part 1`` () =
        Day11.part1 ()
        |> shouldEqual 2483

    [<Test>]
    let ``Part 2`` () =
        let board = Day11.board ()
        Day11.part2 board
        |> shouldEqual 2285


