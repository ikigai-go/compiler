module rec Ikigai.Compiler.Parser.Parser

open System
open System.Text.RegularExpressions
open Fable.Core
open Fable.Core.JsInterop
open Chevrotain

open Ikigai.Compiler.AST
open Ikigai.Compiler.AST.Ikigai
module Tok = Ikigai.Compiler.Parser.Tokens

type Rules =
    abstract Program: unit->Untyped.FileAst
    abstract ValueDeclaration: unit->Untyped.Declaration
    abstract LiteralExpression: unit->Untyped.Expr

let inline R(p: Parser): Rules = unbox p

type Parser with
    member inline P.MANY_(f: unit->'T): 'T[] =
        let items = ResizeArray()
        P.MANY(fun () ->
            f() |> items.Add)
        items.ToArray()

let makeRules (P: Parser) =
    { new Rules with
        member __.Program() =
            P.MANY_(fun () ->
                P.SUBRULE(R(P).ValueDeclaration))
            |> makeProgram

         member __.ValueDeclaration() =
            P.OPTION(fun () ->
                P.CONSUME(Tok.ExportModifier) |> ignore
            )
            let mut =   P.CONSUME(Tok.MutabilityModifier)
            let id =    P.CONSUME(Tok.Identifier)
            let _ =     P.CONSUME(Tok.Assignment)
            let exp =   P.SUBRULE(R(P).LiteralExpression)
            let _ =     P.CONSUME(Tok.Semicolon)
            makeValueDeclaration(mut, id, exp)

         member __.LiteralExpression() =
            P.OR [|
                {| ALT = fun () ->
                    let t = P.CONSUME(Tok.Number)
                    let r = makeRange t
                    Double.Parse(t.image) |> NumberLiteral |> makeLiteral r |}
            |]
    }

let makeRange (t: IToken) =
    { start =   { line  = t.startLine; column = t.startColumn }
      ``end`` = { line  = t.endLine; column = t.endColumn } }

let makeLiteral (r: SourceLocation) kind =
    Untyped.Literal(kind, r)

// TODO: SourceLocation
let makeIdent(name: string) =
    Untyped.Ident(name, SourceLocation.Empty)

let makeBinaryOperation(expr1: Untyped.Expr, op: string, expr2: Untyped.Expr) =
    let kind = Untyped.BinaryOperation(BinaryOperator.Parse op, expr1, expr2)
    Untyped.Operation(kind, SourceLocation.Empty)

let makeValueDeclaration(mutabilityModifier: IToken, ident: IToken, body: Untyped.Expr) =
    let r1 = makeRange mutabilityModifier
    let isMutable = mutabilityModifier.image = "mutable"
    Untyped.ValueDeclaration(false, isMutable, ident.image, r1 + body.Range, None, body)

let makeProgram(decls: Untyped.Declaration[]): Untyped.FileAst =
    { declarations = Array.toList decls }

type Error = interface end

type ParseResult =
    abstract member ast: Untyped.FileAst
    abstract member errors: Error[]

let private makeLexer(tokens: TokenType[]): Lexer = importMember "./parser"
let private makeParser(tokens: TokenType[], makeRules: Parser -> Rules): Parser = importMember "./parser"
let private parsePrivate(lexer: Lexer, parser: Parser, txt: string): ParseResult = import "parse" "./parser"

let private lexer = makeLexer(Tok.TOKENS)
let private parser = makeParser(Tok.TOKENS, makeRules)

let parse txt = parsePrivate(lexer, parser, txt)
