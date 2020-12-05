namespace Test

open FsUnitTyped
open NUnit.Framework
open AdventOfCode

[<TestFixture>]
module TestDay5 =

    [<Test>]
    let ``Test interpretBase2`` () =
        Day5.interpretBase2 [true ; false ; false ; true ; true]
        |> shouldEqual 0b10011

    [<Test>]
    let ``Part 1`` () =
        Day5.part1 ()
        |> shouldEqual 976

    [<Test>]
    let ``Part 2`` () =
        Day5.part2 ()
        |> shouldEqual 685
