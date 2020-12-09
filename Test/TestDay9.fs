namespace Test

open AdventOfCode
open FsUnitTyped
open NUnit.Framework

[<TestFixture>]
module TestDay9 =

    [<Test>]
    let ``Test availableSums advancement`` () =
        let available = Day9.AvailableSums.create [ 30 ; 20 ; 15 ; 25 ; 47 ] 5
        available.Modulus |> shouldEqual 5
        available.Ptr |> shouldEqual 0
        available.Buffer
        |> shouldEqual
            [|
                // 30 plus everything
                [| 60 ; 50 ; 45 ; 55 ; 77 |]
                // 20 plus everything starting from 20 (the last element is junk)
                [| 40 ; 35 ; 45 ; 67 ; 0 |]
                // 15 plus everything starting from 15 (the last two elements are junk)
                [| 30 ; 40 ; 62 ; 0 ; 0 |]
                // 25 plus everything (the last three elements are junk)
                [| 50 ; 72 ; 0 ; 0 ; 0 |]
                [| 94 ; 0 ; 0 ; 0 ; 0 |]
            |]

        Day9.AvailableSums.advance 40 available

        available.Modulus |> shouldEqual 5
        available.Ptr |> shouldEqual 1
        available.Buffer
        |> shouldEqual
            [|
                // 40 plus everything; the last four elements are junk.
                [| 80 ; 50 ; 45 ; 55 ; 77 |]
                // 20 plus everything starting from 20 (the last element has the new 40 added to it)
                [| 40 ; 35 ; 45 ; 67 ; 60 |]
                // 15 plus everything starting from 15 (the last element is junk; the second-to-last has the new 40)
                [| 30 ; 40 ; 62 ; 55 ; 0 |]
                // 25 plus everything (the last two elements are junk; the third-to-last has the new 40)
                [| 50 ; 72 ; 65 ; 0 ; 0 |]
                // 47 plus everything (the last three elements are junk)
                [| 94 ; 87 ; 0 ; 0 ; 0 |]
            |]

        Day9.AvailableSums.advance 62 available

        available.Modulus |> shouldEqual 5
        available.Ptr |> shouldEqual 2
        available.Buffer
        |> shouldEqual
            [|
                // 40 plus everything; the last three elements are junk.
                [| 80 ; 102 ; 45 ; 55 ; 77 |]
                // 62 plus everything starting from 62 (the last four elements are junk)
                [| 124 ; 35 ; 45 ; 67 ; 60 |]
                // 15 plus everything starting from 15 (all elements are valid)
                [| 30 ; 40 ; 62 ; 55 ; 77 |]
                // 25 plus everything (the last element is junk)
                [| 50 ; 72 ; 65 ; 87 ; 0 |]
                // 47 plus everything (the last two elements are junk)
                [| 94 ; 87 ; 109 ; 0 ; 0 |]
            |]

    [<Test>]
    let ``Test availableSums toSeq`` () =
        let available = Day9.AvailableSums.create [ 30 ; 20 ; 15 ; 25 ; 47 ] 5

        Day9.AvailableSums.advance 40 available
        Day9.AvailableSums.advance 62 available

        Day9.AvailableSums.toSeq available
        |> List.ofSeq
        |> shouldEqual
            [
                // 15 plus everything-ahead-of-15
                30 ; 40 ; 62 ; 55 ; 77
                // 15 plus everything-ahead-of-15
                50 ; 72 ; 65 ; 87
                // 25 plus everything-ahead
                94 ; 87 ; 109
                // 40 plus everything-ahead
                80 ; 102
                124
            ]

    [<Test>]
    let ``Test part 1`` () =
        Day9.part1 () |> shouldEqual 373803594L

    [<Test>]
    let ``Test part 2`` () =
        Day9.part2 () |> shouldEqual 51152360L


