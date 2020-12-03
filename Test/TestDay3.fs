namespace Test

open AdventOfCode
open NUnit.Framework
open FsUnitTyped

[<TestFixture>]
module TestDay3 =

    let example =
        """..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#""".Split()
        |> Day3.parseBoard

    [<Test>]
    let ``Example 1`` () =
        Day3.countTrees 3 1 example
        |> shouldEqual 7

    [<Test>]
    let ``Example 2`` () =
        Day3.countTrees 1 1 example
        |> shouldEqual 2
        Day3.countTrees 3 1 example
        |> shouldEqual 7
        Day3.countTrees 5 1 example
        |> shouldEqual 3
        Day3.countTrees 7 1 example
        |> shouldEqual 4
        Day3.countTrees 1 2 example
        |> shouldEqual 2

    [<Test>]
    let ``Part 1`` () =
        Day3.part1 ()
        |> shouldEqual 257

    [<Test>]
    let ``Part 2`` () =
        Day3.part2 ()
        |> shouldEqual 1744787392
