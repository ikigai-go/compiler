module Ikigai.Compiler.Parser

open Fable.Core.JsInterop
open Ikigai.Compiler.AST
open Ikigai.Compiler.AST.Ikigai

let parseToCst(txt: string): obj = import "parse" "./parser"
let toAst(cst: obj): Untyped.FileAst = import "toAst" "./visitor"

let parse(txt: string): Untyped.FileAst =
    parseToCst txt |> toAst

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

let makeBinaryOperation(expr1: Untyped.Expr, op: string, expr2: Untyped.Expr) =
    let kind = Untyped.BinaryOperation(BinaryOperator.Parse op, expr1, expr2)
    Untyped.Operation(kind, SourceLocation.Empty)

let makeValueDeclaration(mutabilityModifier: string, ident: string, body: obj) =
    let isMutable = mutabilityModifier = "mutable"
    Untyped.ValueDeclaration(false, isMutable, ident, SourceLocation.Empty, None, unbox body)

let makeProgram(decls: Untyped.Declaration[]): Untyped.FileAst =
    { declarations = Array.toList decls }
