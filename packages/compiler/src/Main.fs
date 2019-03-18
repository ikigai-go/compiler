module Ikigai.Compiler.Main

open System
open Fable.Import
open Fable.Core
open Fable.Core.JsInterop

let chokidar: obj = importAll "chokidar"
let transformFromAstSync(ast: obj, code: string): obj = importMember "@babel/core"

type NodePlatform() =
    let Fs: obj = importAll "fs"
    interface IPlatform with
        member __.ReadFile(path) =
            Promise.create(fun resolve reject ->
                Fs?readFile(path, Func<_,_,_>(fun (err: exn) (data: obj) ->
                    if isNull err then data?toString() |> resolve
                    else reject err)  ))

let transform (platform: IPlatform) (filepath: string): unit =
    let separator = "==========================="
    platform.ReadFile filepath
    |> Promise.map (fun txt ->
        let parsed = Parser.parse txt
        for e in parsed.lexerErrors do
            printfn "[LEXER ERROR %i,%i] %s" e.line e.column e.message
        for e in parsed.parserErrors do
            printfn "[PARSER ERROR %i,%i] %s" e.token.startLine e.token.startColumn e.message
        match parsed.ast with
        | Some ast ->
            let babelAst =
                Checker.check filepath ast
                |> Emitter.transform filepath
            transformFromAstSync(babelAst, null)?code
        | None -> "")
    |> Promise.eitherEnd
        (fun code -> printfn "\n%s\n%s" code separator)
        (fun (er: exn) -> JS.console.error(er))

[<EntryPoint>]
let main argv =
    Array.tryHead argv
    |> Option.iter (fun path ->
        let platform = NodePlatform()
        transform platform path
        chokidar?watch(path)?on("change", fun _ ->
            transform platform path)
    )
    0
