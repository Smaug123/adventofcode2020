namespace AdventOfCode.Test

open NUnit.Framework
open FsUnitTyped
open AdventOfCode

[<TestFixture>]
module TestDay2 =

    [<Test>]
    let ``Part 1 examples`` () =
        Day2.isMatch1 { Min = 1 ; Max = 3 ; Letter = 'a' } "abcde"
        |> shouldEqual true
        Day2.isMatch1 { Min = 1 ; Max = 3 ; Letter = 'b' } "cdefg"
        |> shouldEqual false
        Day2.isMatch1 { Min = 2 ; Max = 9 ; Letter = 'c' } "ccccccccc"
        |> shouldEqual true

    [<Test>]
    let ``Part 1`` () =
        Day2.part1 ()
        |> shouldEqual 607

    [<Test>]
    let ``Part 1, super speedy`` () =
        Day2StateMachine.part1 ()
        |> shouldEqual 607

    [<Test>]
    let ``Part 2`` () =
        Day2.part2 ()
        |> shouldEqual 321
