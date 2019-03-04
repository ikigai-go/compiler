module Ikigai.Compiler.Parser.Tokens

open System.Text.RegularExpressions
open Fable.Core
open Fable.Core.JsInterop
open Chevrotain

[<Import("*", "chevrotain")>]
let chevrotain: IExports = jsNative

// Fable adds g flag by default, which is not supported by chevrotain
[<Emit("new RegExp($0)")>]
let inline regex pattern: U2<string,Regex> = jsNative

type Token =
    [<Import("createToken", "chevrotain")>]
    [<Emit("$0({ name: $1, pattern: $2, {{group: $3,}} {{longer_alt: $4,}} })")>]
    static member Create(name: string, pattern: U2<string,Regex>, ?group: string, ?longer_alt: TokenType): TokenType = jsNative

let WhiteSpace = Token.Create("WhiteSpace", regex "s+", chevrotain.Lexer.SKIPPED)
let SingleLineComment = Token.Create("SingleLineComment", regex (@"\/\/.*?\n"), chevrotain.Lexer.SKIPPED)
let Semicolon = Token.Create("Semicolon", !^";")
let Assignment = Token.Create("Assignment", !^"=")
let LParen = Token.Create("LParen", !^"(")
let RParen = Token.Create("RParen", !^")")

// Keywords
let Identifier = Token.Create("Identifier", regex "[a-zA-Z_][0-9a-zA-Z_]*")
let MutabilityModifier = Token.Create("MutabilityModifier", regex "const|mutable", longer_alt=Identifier)
let ExportModifier = Token.Create("ExportModifier", regex "export", longer_alt=Identifier)

// Operators
let AdditionOperator = Token.Create("AdditionOperator", regex (@"[+\-]"))
// ProductOperator: /[*\/]/,
// ExponentialOperator: /\^/,
// UnaryOperator: /[!\-]/,

// Literals
let Number = Token.Create("Number", regex @"-?(?:[0-9]|[1-9][0-9]+)(?:\.[0-9]+)?(?:[eE][-+]?[0-9]+)?\b")
let String = Token.Create("String", regex @"""(?:[^""\\]|.)*""")

let TOKENS =
    [|
    WhiteSpace
    SingleLineComment
    Semicolon
    Assignment
    LParen
    RParen
    MutabilityModifier
    ExportModifier
    Identifier
    AdditionOperator
    Number
    String
    |]