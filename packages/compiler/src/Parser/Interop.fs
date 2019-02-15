module Ikigai.Compiler.Parser

open Ikigai.Compiler.AST
open Ikigai.Compiler.AST.Ikigai

let parse(txt: string): Untyped.FileAst =
    Fable.Core.JsInterop.importMember "./parser"

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

let makeBinaryOperation(op: string, expr1: Untyped.Expr, expr2: Untyped.Expr) =
    let kind = Untyped.BinaryOperation(BinaryOperator.Parse op, expr1, expr2)
    Untyped.Operation(kind, SourceLocation.Empty)

let makeValueDeclaration(isMutable: bool, ident: string, body: obj) =
    Untyped.ValueDeclaration(false, isMutable, ident, SourceLocation.Empty, None, unbox body)

let makeUntypedAst(decls: Untyped.Declaration[]): Untyped.FileAst =
    { declarations = Array.toList decls }
