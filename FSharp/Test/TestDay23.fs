namespace AdventOfCode.Test

open NUnit.Framework
open AdventOfCode
open FsUnitTyped

[<TestFixture>]
module TestDay23 =

    [<Test>]
    let ``Part 1`` () =
        Day23.part1 ()
        |> shouldEqual "25368479"

    [<Test>]
    let ``Part 2`` () =
        Day23.part2 ()
        |> shouldEqual 44541319250L


