module Ikigai.Compiler.Parser

open Fable.Core.JsInterop
open Ikigai.Compiler.AST
open Ikigai.Compiler.AST.Ikigai

type TokenType =
    interface end

type Terminal =
    abstract member image: string
    abstract member startOffset: int
    abstract member startLine: int
    abstract member startColumn: int
    abstract member tokenTypeIdx: int
    abstract member tokenType: TokenType

// type NonTerminal =
//     abstract member name: string
//     abstract member children: IDictionary<string, obj>

type Error = interface end

type ParseResult =
    abstract member ast: Untyped.FileAst
    abstract member errors: Error[]

let parse(txt: string): ParseResult = import "parse" "./parser"

// TODO: SourceLocation
let makeLiteral(name: string, value: string) =
    let kind =
        match name with
        | "Null" -> NullLiteral
        | "Void" -> VoidLiteral
        | "Bool" -> BoolLiteral(value = "true")
        | "String" -> StringLiteral(value)
        | "Number" -> NumberLiteral(float value)
        | name -> failwithf "Unknown literal: %s" name
    Untyped.Literal(kind, SourceLocation.Empty)

// TODO: SourceLocation
let makeIdent(name: string) =
    Untyped.Ident(name, SourceLocation.Empty)

let makeBinaryOperation(expr1: Untyped.Expr, op: string, expr2: Untyped.Expr) =
    let kind = Untyped.BinaryOperation(BinaryOperator.Parse op, expr1, expr2)
    Untyped.Operation(kind, SourceLocation.Empty)

let makeValueDeclaration(mutabilityModifier: string, ident: string, body: obj) =
    let isMutable = mutabilityModifier = "mutable"
    Untyped.ValueDeclaration(false, isMutable, ident, SourceLocation.Empty, None, unbox body)

let makeProgram(decls: Untyped.Declaration[]): Untyped.FileAst =
    { declarations = Array.toList decls }
