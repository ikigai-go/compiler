module Ikigai.Compiler.Parser

open System
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

let parse(txt: string): ParseResult = import "parse" "./Parser.js"

// TODO: SourceLocation
let makeLiteral(kind: string, value: string) =
    let kind =
        match kind with
        | "null" -> NullLiteral
        | "void" -> VoidLiteral
        | "boolean" -> BoolLiteral(value = "true")
        | "string" -> StringLiteral(value)
        | "number" -> NumberLiteral(float value)
        | kind -> failwithf "Unknown literal: %s" kind
    Untyped.Literal(kind, SourceLocation.Empty)

// TODO: SourceLocation
let makeIdent(name: string) =
    Untyped.Ident(name, SourceLocation.Empty)

// TODO: Logical operations
let makeBinaryOperation(expr1: Untyped.Expr, op: string, expr2: Untyped.Expr) =
    let kind = Untyped.BinaryOperation(BinaryOperator.Parse op, expr1, expr2)
    Untyped.Operation(kind, SourceLocation.Empty)

let makeUnaryOperation(op: string, expr: Untyped.Expr) =
    let kind = Untyped.UnaryOperation(UnaryOperator.Parse op, expr)
    Untyped.Operation(kind, SourceLocation.Empty)

let makeValueDeclaration(mutabilityModifier: string, ident: string, body: Untyped.Expr) =
    let isMutable = mutabilityModifier = "mutable"
    Untyped.ValueDeclaration(false, isMutable, ident, SourceLocation.Empty, None, body)

let makeProgram(decls: Untyped.Declaration[]): Untyped.FileAst =
    { declarations = Array.toList decls }

let makeLambdaExpression(args: Untyped.Argument[], hasSpread, returnAnnotation, body: Untyped.Expr): Untyped.Expr =
    Untyped.Function(Array.toList args, hasSpread, returnAnnotation, Untyped.Expr body, SourceLocation.Empty)

let makeArgument(name, annotation, defaultValue): Untyped.Argument =
    { name = name
      annotation = annotation
      defaultValue = defaultValue
      range = SourceLocation.Empty }

let makeAnnotation(ident): Annotation =
    match Primitive.TryParse ident with
    | Some prim -> Annotation.Primitive prim
    | None -> Annotation.DeclaredType(ident, []) // TODO
