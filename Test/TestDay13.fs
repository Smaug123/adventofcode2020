namespace AdventOfCode.Test

open NUnit.Framework
open AdventOfCode
open FsUnitTyped

[<TestFixture>]
module TestDay13 =

    [<Test>]
    let ``Part 1`` () =
        Day13.part1 ()
        |> shouldEqual 8063

    [<Test>]
    let ``Test chineseRemainder`` () =
        Day13.chineseRemainder' [(0L, Day13.Modulus 3L) ; (3L, Day13.Modulus 4L) ; (4L, Day13.Modulus 5L)]
        |> shouldEqual 39L

    [<Test>]
    let ``Toy part 2`` () =
        Day13.parse "17,x,13,19" |> Day13.chineseRemainder' |> shouldEqual 3417L
        Day13.parse "67,7,59,61" |> Day13.chineseRemainder' |> shouldEqual 754018L
        Day13.parse "67,x,7,59,61" |> Day13.chineseRemainder' |> shouldEqual 779210L
        Day13.parse "67,7,x,59,61" |> Day13.chineseRemainder' |> shouldEqual 1261476L
        Day13.parse "1789,37,47,1889" |> Day13.chineseRemainder' |> shouldEqual 1202161486L
        Day13.parse "7,13,x,x,59,x,31,19" |> Day13.chineseRemainder' |> shouldEqual 1068781L

    [<Test>]
    let ``Part 2`` () =
        Day13.part2 ()
        |> shouldEqual 775230782877242L

[<TestFixture>]
[<Category "Exhaustive">]
[<Explicit "These tests are exhaustive tests of functions and may take a long time.">]
module ExhaustiveDay13 =

    [<Test>]
    let ``Test addModulus`` () =
        for i in 0..127 do
            for j in 0..127 do
                for modulus in 1..127 do
                    let result =
                        Day13.addMod (sbyte modulus) (sbyte i) (sbyte j)
                        |> int
                    if result <> (i + j) % modulus then
                        failwithf "oh no: %+A %+A %+A, got %A" i j modulus result
                    result
                    |> shouldEqual ((i + j) % modulus)

    [<Test>]
    let ``Test timesMod`` () =
        for i in 0..127 do
            for j in 0..127 do
                for modulus in 1..127 do
                    let result =
                        Day13.timesMod (sbyte modulus) (sbyte i) (sbyte j)
                        |> int
                    if result <> ((i * j) % modulus) then failwithf "oh no: %+A * %+A (mod %+A), got %+A" i j modulus result
                    result
                    |> shouldEqual ((i * j) % modulus)
