namespace AdventOfCode.Test

open NUnit.Framework
open AdventOfCode
open FsUnitTyped

[<TestFixture>]
module TestDay10 =

    [<Test>]
    let ``Test runs`` () =
        Day10.runs (fun i j -> j - i = 3) [1;2;4;7;8]
        |> Seq.toList
        |> shouldEqual [3 ; 2]

    [<Test>]
    let ``Test permissible`` () =
        [1..10]
        |> List.map (Day10.permissible Map.empty >> snd)
        |> shouldEqual [ 1L ; 1L ; 2L ; 4L ; 7L ; 13L ; 24L ; 44L ; 81L ; 149L ]

    [<Test>]
    let ``Part 1`` () =
        Day10.part1 ()
        |> shouldEqual 1917

    [<Test>]
    let ``Part 2`` () =
        Day10.part2 ()
        |> shouldEqual 113387824750592L
