namespace AdventOfCode.Internals

open System
open System.IO
open System.Reflection

[<RequireQualifiedAccess>]
module Utils =
    type private Dummy = class end

    let readResource (name : string) : string list =
        let asm = Assembly.GetAssembly typeof<Dummy>
        use stream = asm.GetManifestResourceStream (sprintf "AdventOfCode.%s" name)
        use reader = new StreamReader(stream)
        reader.ReadToEnd().Split('\r', '\n')
        |> List.ofArray
        |> List.filter (String.IsNullOrEmpty >> not)

[<RequireQualifiedAccess>]
module Result =
    let get r =
        match r with
        | Ok i -> i
        | Error f -> failwithf "Assertion failure: result was error: %+A" f