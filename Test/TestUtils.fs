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

