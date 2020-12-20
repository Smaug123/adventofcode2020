namespace AdventOfCode.Test

open NUnit.Framework
open AdventOfCode
open FsUnitTyped

[<TestFixture>]
module TestDay20 =

    [<Test>]
    let ``transformGrid test`` () =
        let grid =
            [|
                [| 1..5 |]
                [| 6..10 |]
                [| 11..15 |]
                [| 16..20 |]
                [| 21..25 |]
            |]

        Day20.transformGrid Day20.Side.Top Day20.Flippage.Normal grid
        |> shouldEqual grid
        Day20.transformGrid Day20.Side.Top Day20.Flippage.Flipped grid
        |> shouldEqual
            [|
                [|1..5|] |> Array.map (fun i -> 6 - i)
                [|1..5|] |> Array.map (fun i -> 11 - i)
                [|1..5|] |> Array.map (fun i -> 16 - i)
                [|1..5|] |> Array.map (fun i -> 21 - i)
                [|1..5|] |> Array.map (fun i -> 26 - i)
            |]

        Day20.transformGrid Day20.Side.Bottom Day20.Flippage.Flipped grid
        |> shouldEqual (Array.rev grid)
        Day20.transformGrid Day20.Side.Bottom Day20.Flippage.Normal grid
        |> shouldEqual (Array.rev grid |> Array.map Array.rev)

        Day20.transformGrid Day20.Side.Left Day20.Flippage.Flipped grid
        |> shouldEqual (Array.transpose grid)
        Day20.transformGrid Day20.Side.Left Day20.Flippage.Normal grid
        |> shouldEqual (Array.transpose grid |> Array.map Array.rev)

        Day20.transformGrid Day20.Side.Right Day20.Flippage.Normal grid
        |> shouldEqual (Array.transpose grid |> Array.rev)
        Day20.transformGrid Day20.Side.Right Day20.Flippage.Flipped grid
        |> shouldEqual (Array.transpose grid |> Array.rev |> Array.map Array.rev)

    [<Test>]
    let ``Part 1`` () =
        Day20.part1 ()
        |> shouldEqual 17148689442341L

    [<Test>]
    let ``Part 2`` () =
        Day20.part2 ()
        |> shouldEqual 2009
