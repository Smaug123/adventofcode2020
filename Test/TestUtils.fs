namespace Test

open NUnit.Framework
open FsUnitTyped
open AdventOfCode.Internals

[<TestFixture>]
module TestUtils =

    [<Test>]
    let ``Test fixedPoint`` () =
        Utils.fixedPoint (fun i -> if i > 10 then i else i + 1) 0
        |> shouldEqual 11
        Utils.fixedPoint (fun i -> if i > 10 then i else i + 1) 10
        |> shouldEqual 11
        Utils.fixedPoint (fun i -> if i > 10 then i else i + 1) 11
        |> shouldEqual 11

    [<Test>]
    let ``Test sqrt`` () =
        let stupidSqrt (x : sbyte) =
            seq {
                for i in 0..127 do
                    if i * i > int x then
                        yield sbyte (i - 1)
            }
            |> Seq.head

        [ 0..127 ]
        |> List.map sbyte
        |> List.map Int.sqrt
        |> shouldEqual (
            [0..127]
            |> List.map (sbyte >> stupidSqrt)
        )

    [<Test>]
    let ``Test isPrime`` () =
        [ 0..36 ]
        |> List.filter Int.isPrime
        |> shouldEqual [ 2 ; 3 ; 5 ; 7 ; 11 ; 13 ; 17 ; 19 ; 23 ; 29 ; 31 ]

    /// Find Hcf, A, B s.t. A * a + B * b = Hcf, and Hcf is the highest common factor of a and b.
    let inline euclideanAlgorithm (a : ^a) (b : ^a) : {| Hcf : ^a ; A : ^a ; B : ^a |} =
        let rec go rMin1 r sMin1 s tMin1 t =
            if r = LanguagePrimitives.GenericZero then {| Hcf = rMin1 ; A = sMin1 ; B = tMin1 |} else
            let newQ = rMin1 / r
            go r (rMin1 - newQ * r) s (sMin1 - newQ * s) t (tMin1 - newQ * t)

        let maxA = max a b
        let minB = min a b
        let result = go maxA minB LanguagePrimitives.GenericOne LanguagePrimitives.GenericZero LanguagePrimitives.GenericZero LanguagePrimitives.GenericOne
        if a = maxA then result else {| Hcf = result.Hcf ; A = result.B ; B = result.A |}

    [<Test>]
    let ``Test euclidean`` () =
        Int.euclideanAlgorithm 240L 46L
        |> shouldEqual {| Hcf = 2L ; A = -9L ; B = 47L |}
        Int.euclideanAlgorithm 46L 240L
        |> shouldEqual {| Hcf = 2L ; A = 47L ; B = -9L |}
        Int.euclideanAlgorithm 4L 3L
        |> shouldEqual {| Hcf = 1L ; A = 1L ; B = -1L |}

