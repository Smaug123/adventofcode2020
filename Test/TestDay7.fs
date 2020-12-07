namespace Test

open AdventOfCode
open FsUnitTyped
open NUnit.Framework

[<TestFixture>]
module TestDay7 =

    [<Test>]
    let ``Test parser`` () =
        let colour, contents =
            Day7.parse "shiny red bags contain 1 posh olive bag, 1 vibrant green bag, 4 muted purple bags."
        colour |> shouldEqual (Day7.Colour "shiny red")
        contents
        |> shouldEqual (
            [
                Day7.Colour "posh olive", 1
                Day7.Colour "vibrant green", 1
                Day7.Colour "muted purple", 4
            ]
            |> Map.ofList
        )

    [<Test>]
    let ``Test part 1`` () =
        Day7.part1 () |> shouldEqual 139

    [<Test>]
    let ``Test part 2`` () =
        Day7.part2 () |> shouldEqual 58175

