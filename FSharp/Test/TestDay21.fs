namespace AdventOfCode.Test

open NUnit.Framework
open AdventOfCode
open FsUnitTyped

[<TestFixture>]
module TestDay21 =

    [<Test>]
    let ``Part 1`` () =
        Day21.part1 ()
        |> shouldEqual 2162

    [<Test>]
    let ``Part 2`` () =
        Day21.part2 ()
        |> shouldEqual "lmzg,cxk,bsqh,bdvmx,cpbzbx,drbm,cfnt,kqprv"
