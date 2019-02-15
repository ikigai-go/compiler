module Ikigai.Compiler.Parser

open Ikigai.Compiler.AST
open Ikigai.Compiler.AST.Ikigai

let parse(txt: string) = Fable.Core.JsInterop.importMember "./parser"

// TODO: SourceLocation
let makeLiteral(name: string, value: obj) =
    let kind =
        match name with
        | "Null" -> NullLiteral
        | "Void" -> VoidLiteral
        | "Bool" -> BoolLiteral(unbox value)
        | "String" -> StringLiteral(unbox value)
        | "Number" -> NumberLiteral(unbox value)
        | name -> failwithf "Unknown literal: %s" name
    Untyped.Literal(kind, SourceLocation.Empty)

let makeValueDeclaration(isMutable: bool, ident: string, body: obj) =
    ValueDeclaration(false, isMutable, ident, unbox body)

let makeUntypedAst(decls: Declaration[]) =
    decls
