namespace AdventOfCode.Test

open NUnit.Framework
open AdventOfCode
open FsUnitTyped

[<TestFixture>]
module TestDay25 =

    [<Test>]
    let ``Test powerMod`` () =
        Day25.toBinary 8
        |> shouldEqual [false ; false ; false ; true]
        Day25.toBinary 7
        |> shouldEqual [true ; true ; true]
        Day25.powerMod 7 8 20201227
        |> shouldEqual 5764801
        Day25.powerMod 7 11 20201227
        |> shouldEqual 17807724
        Day25.powerMod' 7 (Day25.toBinary 8 |> List.toArray) 20201227 |> shouldEqual (Day25.powerMod 7 8 20201227)

    [<Test>]
    let ``Test incr`` () =
        for i in 1..100 do
            let arr = Day25.toBinary i |> List.toArray
            match Day25.incr arr with
            | None -> Day25.ofBinary arr |> shouldEqual (i + 1)
            | Some arr -> Day25.ofBinary arr |> shouldEqual (i + 1)

    [<Test>]
    let ``Part 1`` () =
        Day25.part1 ()
        |> shouldEqual 1890859
