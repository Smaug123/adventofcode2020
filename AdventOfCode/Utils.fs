namespace AdventOfCode.Internals

open System.IO
open System.Reflection

[<RequireQualifiedAccess>]
module Utils =
    type private Dummy = class end

    let readResource (name : string) : string list =
        let asm = Assembly.GetAssembly typeof<Dummy>
        use stream = asm.GetManifestResourceStream (sprintf "AdventOfCode.%s" name)
        use reader = new StreamReader(stream)
        reader.ReadToEnd().Split()
        |> List.ofArray
