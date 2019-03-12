module Ikigai.Compiler.Parser

open System
open Fable.Core.JsInterop
open Ikigai.Compiler.AST
open Ikigai.Compiler.AST.Ikigai

type TokenType =
    interface end

type IToken =
    abstract member image: string
    abstract member startOffset: int
    abstract member startLine: int
    abstract member startColumn: int
    abstract member endOffset: int
    abstract member endLine: int
    abstract member endColumn: int
    abstract member tokenTypeIdx: int
    abstract member tokenType: TokenType

type Error = interface end

type ParseResult =
    abstract member ast: Untyped.FileAst option
    abstract member errors: Error[]

let parse(txt: string): ParseResult = import "parse" "./Parser.js"

let makeRange (ter: IToken): SourceLocation =
    { start = { line = ter.startLine; column = ter.startColumn }
      ``end`` = { line = ter.endLine; column = ter.endColumn } }

let makeLiteral(kind: string, tok: IToken) =
    let kind =
        match kind with
        | "null" -> NullLiteral
        | "void" -> VoidLiteral
        | "boolean" -> BoolLiteral(tok.image = "true")
        | "string" -> StringLiteral(tok.image)
        | "number" -> NumberLiteral(float tok.image)
        | kind -> failwithf "Unknown literal: %s" kind
    Untyped.Literal(kind, makeRange tok)

let makeIdent(tok: IToken) =
    Untyped.Ident(tok.image, makeRange tok)

// TODO: Logical operations
let makeBinaryOperation(expr1: Untyped.Expr, op: IToken, expr2: Untyped.Expr) =
    let kind = Untyped.BinaryOperation(BinaryOperator.Parse op.image, expr1, expr2)
    Untyped.Operation(kind, expr1.Range + expr2.Range)

let makeUnaryOperation(op: IToken, expr: Untyped.Expr) =
    let kind = Untyped.UnaryOperation(UnaryOperator.Parse op.image, expr)
    Untyped.Operation(kind, makeRange op + expr.Range)

let makeValueDeclaration(mutabilityModifier: string, ident: IToken, body: Untyped.Expr) =
    let isMutable = mutabilityModifier = "mutable"
    Untyped.ValueDeclaration(false, isMutable, ident.image, makeRange ident, None, body)

let makeProgram(decls: Untyped.Declaration[]): Untyped.FileAst =
    { declarations = Array.toList decls }

let makeLambdaExpression(args: Untyped.Argument[], hasSpread, returnAnnotation, body: Untyped.Expr): Untyped.Expr =
    Untyped.Function(Array.toList args, hasSpread, returnAnnotation, Untyped.Expr body)

let makeArgument(ident: IToken, annotation, defaultValue): Untyped.Argument =
    { name = ident.image
      annotation = annotation
      defaultValue = defaultValue
      range = makeRange ident }

let rec makeType(ident: IToken, genericArgs: Untyped.Type[]): Untyped.Type =
    let r = makeRange ident
    match genericArgs, Primitive.TryParse ident.image with
    | [||], Some prim -> Untyped.Primitive(prim, r)
    | genArgs, _ -> Untyped.DeclaredType(ident.image, r, Array.toList genArgs)
