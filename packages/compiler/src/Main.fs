module Ikigai.Compiler.Main

open System
open Fable.Import
open Fable.Core
open Fable.Core.JsInterop

let transformFromAstSync(ast: obj, code: string): obj = importMember "@babel/core"

type NodePlatform() =
    let Fs: obj = importAll "fs"
    interface IPlatform with
        member __.ReadFile(path) =
            Promise.create(fun resolve reject ->
                Fs?readFile(path, Func<_,_,_>(fun (err: exn) (data: obj) ->
                    if isNull err then data?toString() |> resolve
                    else reject err)  ))

let transform (platform: IPlatform) (filepath: string): JS.Promise<string> =
    platform.ReadFile filepath
    |> Promise.map (fun txt ->
        let parsed = Parser.parse txt
        for error in parsed.errors do
            printfn "[PARSE ERROR] %O" error
        match parsed.ast with
        | Some ast ->
            let babelAst =
                Checker.check filepath ast
                |> Emitter.transform filepath
            transformFromAstSync(babelAst, null)?code
        | None -> "")

[<EntryPoint>]
let main argv =
    Array.tryHead argv
    |> Option.iter (fun path ->
        let platform = NodePlatform()
        transform platform path
        |> Promise.eitherEnd
            (fun code -> Console.WriteLine(code))
            (fun (er: exn) -> printfn "[ERROR] %s\n%s" er.Message er.StackTrace))
    0
