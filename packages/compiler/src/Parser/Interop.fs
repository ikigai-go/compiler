﻿module Ikigai.Compiler.Parser

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

let addRanges (r1: SourceLocation) r2 =  r1 + r2

let rangeFromExpr (e: Untyped.Expr) = e.Range

let rangeFromToken (ter: IToken): SourceLocation =
    { start = { line = ter.startLine; column = ter.startColumn }
      ``end`` = { line = ter.endLine; column = ter.endColumn } }

let makeLiteral(kind: string, tok: IToken) =
    let kind =
        match kind with
        | "null" -> NullLiteral
        | "void" -> VoidLiteral
        | "boolean" -> BoolLiteral(tok.image = "true")
        | "string" -> StringLiteral(tok.image.Trim('"'))
        | "number" -> NumberLiteral(float tok.image)
        | kind -> failwithf "Unknown literal: %s" kind
    Untyped.Literal(kind, rangeFromToken tok)

let makeIdent(tok: IToken) =
    Untyped.Ident(tok.image, rangeFromToken tok)

// TODO: Logical operations
let makeBinaryOperation(expr1: Untyped.Expr, op: IToken, expr2: Untyped.Expr) =
    let kind = Untyped.BinaryOperation(BinaryOperator.Parse op.image, expr1, expr2)
    Untyped.Operation(kind, expr1.Range + expr2.Range)

let makeUnaryOperation(op: IToken, expr: Untyped.Expr) =
    let kind = Untyped.UnaryOperation(UnaryOperator.Parse op.image, expr)
    Untyped.Operation(kind, rangeFromToken op + expr.Range)

let makeCallOperation(baseExpr: Untyped.Expr, args: Untyped.Expr[], isConstructor, hasSpread, range) =
    let kind = Untyped.Call(baseExpr, Array.toList args, isConstructor, hasSpread)
    Untyped.Operation(kind, range)

let makeGetExpression(baseExpr: Untyped.Expr, memberExpr: Untyped.Expr) =
    Untyped.Get(baseExpr, memberExpr)

let makeValueDeclaration(mutabilityModifier: string, ident: IToken, body: Untyped.Expr) =
    let isMutable = mutabilityModifier = "mutable"
    Untyped.ValueDeclaration(isMutable, (ident.image, rangeFromToken ident), None, body)

let makeSkillDeclaration(name: IToken, genericParam: IToken, signatures: Untyped.Signature[]) =
    Untyped.SkillDeclaration((name.image, rangeFromToken name), genericParam.image, Array.toList signatures)

let makeTrainDeclaration(skillName: IToken, trainedType: Untyped.Type, members: Untyped.Member[]) =
    Untyped.TrainDeclaration((skillName.image, rangeFromToken skillName), trainedType, Array.toList members)

let makeMethod(name: IToken, args, hasSpread, returnType, body): Untyped.Member =
    Untyped.Method((name.image, rangeFromToken name), Array.toList args, hasSpread, returnType, Untyped.Expr body)

let makeDeclaration(export: bool, decl: Untyped.DeclarationKind): Untyped.Declaration =
    { kind = decl; export = export }

let makeMethodSignature(name: IToken, args, hasSpread, returnType): Untyped.Signature =
    Untyped.MethodSignature((name.image, rangeFromToken name), Array.toList args, hasSpread, returnType)

let makeArgumentSignature(name: IToken, isOptional, argType): Untyped.ArgumentSignature =
    { name = name.image
      annotation = argType
      isOptional = isOptional }

let makeProgram(decls: Untyped.Declaration[]): Untyped.FileAst =
    { declarations = Array.toList decls }

let makeLambdaExpression(args: Untyped.Argument[], hasSpread, returnAnnotation, body: Untyped.Expr): Untyped.Expr =
    Untyped.Function(Array.toList args, hasSpread, returnAnnotation, Untyped.Expr body)

let makeArgument(ident: IToken, annotation, defaultValue): Untyped.Argument =
    { name = ident.image
      annotation = annotation
      defaultValue = defaultValue
      range = rangeFromToken ident }

let rec makeType(ident: IToken, genericArgs: Untyped.Type[]): Untyped.Type =
    let r = rangeFromToken ident
    match genericArgs, Primitive.TryParse ident.image with
    | [||], Some prim -> Untyped.Primitive(prim, r)
    | genArgs, _ -> Untyped.DeclaredType(ident.image, r, Array.toList genArgs)
