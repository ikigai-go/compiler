module Ikigai.Compiler.Main

[<EntryPoint>]
let main argv =
    Array.tryHead argv
    |> Option.iter (fun path ->
        let res = Parser.parse path
        printfn "%A" res
        // Fable.Import.JS.console.log(res)
    )
    0