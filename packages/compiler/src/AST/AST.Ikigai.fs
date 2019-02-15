namespace rec Ikigai.Compiler.AST.Ikigai

open System
open Ikigai.Compiler
open Ikigai.Compiler.AST

type Entity = Entity

type ArgumentType =
    { argType: Type; isOptional: bool }

type ArgumentAnnotation =
    { annotation: Annotation; isOptional: bool }

type Type =
    | Any
    | Void // Undefined
    | Null
    | Boolean
    | String
    | Number
    | FunctionType of argTypes: ArgumentType list * hasSpread: bool * returnType: Type
    | GenericParam of name: string
    | DeclaredType of Entity * genericArgs: Type list
    | Union of Type list // Typescript-like unions
    | Enum

[<RequireQualifiedAccess>]
type Annotation =
    | Any
    | Void
    | Null
    | Boolean
    | String
    | Number
    | FunctionType of argTypes: ArgumentAnnotation list * hasSpread: bool * returnType: Annotation
    | GenericParam of name: string
    member this.Type: Type =
        match this with
        | Any -> Type.Any
        | Void -> Type.Void
        | Null -> Type.Null
        | Boolean -> Type.Boolean
        | String -> Type.String
        | Number -> Type.Number
        | FunctionType(args, hasSpread, ret) ->
            let args = args |> List.map (fun a -> { argType = a.annotation.Type
                                                    isOptional = a.isOptional })
            Type.FunctionType(args, hasSpread, ret.Type)
        | GenericParam name -> Type.GenericParam name

type LiteralKind =
    | NullLiteral
    | VoidLiteral
    | BoolLiteral of bool
    | StringLiteral of string
    | NumberLiteral of float
    // | RegexLiteral of source: string * flags: RegexFlag list
    member this.Type =
        match this with
        | NullLiteral -> Null
        | VoidLiteral -> Void
        | BoolLiteral _ -> Boolean
        | StringLiteral _ -> String
        | NumberLiteral _ -> Number
        // | RegexLiteral _ -> failwith "TODO: Native types"

[<RequireQualifiedAccess>]
module Untyped =
    type Declaration =
        | ValueDeclaration of isExport: bool * isMutable: bool * name: string * body: Expr

    type OperationKind =
        | Call of baseExpr: Expr * args: Expr list * isConstructor: bool * hasSpread: bool
        | UnaryOperation of UnaryOperator * Expr
        | BinaryOperation of BinaryOperator * left:Expr * right:Expr
        | LogicalOperation of LogicalOperator * left:Expr * right:Expr
        | TernaryOperation of condition: Expr * thenExpr:Expr * elseExpr:Expr

    type Argument =
        { name: string
          annotation: Annotation option
          defaultValue: Expr option
          range: SourceLocation }

    type Block =
        { statements: Statemement list
          returnStatement: ReturnStatement option }
        member __.Range: SourceLocation =
            failwith "TODO: Add statements' ranges"

    type BlockOrExpr =
        | Block of Block
        | Expr of Expr

    type ElseIfOrBlock =
        | ElseIf of guardExpr: Expr * thenBlock: Block * elseBlock: ElseIfOrBlock
        | ElseBlock of Block

    type FlowControl =
        | TryCatch of body: Block * catch: (string * SourceLocation * Block) option * finalizer: Block option
        | IfThenElse of guardExpr: Expr * thenBlock: Block * elseBlock: ElseIfOrBlock option

    type ReturnStatement =
        | Return of Expr
        | FlowControlReturn of FlowControl

    type Statemement =
        | CallStatement of baseExpr: Expr * args: Expr list * isConstructor: bool * hasSpread: bool * range: SourceLocation
        | Binding of ident: (string * SourceLocation) * isMutable: bool * annotation: Annotation option * value: Expr * range: SourceLocation
        | Assignment of baseExpr: Expr * valueExpr: Expr * range: SourceLocation
        | WhileLoop of guard: Expr * body: Block * range: SourceLocation
        // | ForLoop
        // | Break
        // | Debugger of range: SourceLocation
        // | Throw of Expr * range: SourceLocation
        | FlowControlStatement of FlowControl

    type Expr =
        | Literal of LiteralKind * range: SourceLocation
        | Ident of name: string * range: SourceLocation
        | Function of args: Argument list * hasSpread: bool * returnAnnotation: Annotation option * body: BlockOrExpr * range: SourceLocation

        | Operation of OperationKind * range: SourceLocation
        | Get of baseExpr: Expr * indexExpr: Expr * range: SourceLocation

        member this.Range =
            match this with
            | Literal(_,r)
            | Ident(_,r)
            | Function(_,_,_,_,r)
            | Operation(_,r)
            | Get(_,_,r) -> r

type Declaration =
    | ValueDeclaration of isExport: bool * isMutable: bool * name: string * body: Expr

type OperationKind =
    | Call of baseExpr: Expr * args: Expr list * isConstructor: bool * hasSpread: bool
    | UnaryOperation of UnaryOperator * Expr
    | BinaryOperation of BinaryOperator * left:Expr * right:Expr
    | LogicalOperation of LogicalOperator * left:Expr * right:Expr
    | TernaryOperation of condition: Expr * thenExpr:Expr * elseExpr:Expr

type Reference =
    { name: string
      refType: Type
      isMutable: bool
      isCompilerGenerated: bool
      declarationLocation: SourceLocation option }

type Argument =
    { reference: Reference
      defaultValue: Expr option }

type Block =
    { statements: Statemement list
      returnStatement: ReturnStatement option }
    member this.Type =
        match this.returnStatement with
        | Some r -> r.Type
        | None -> Void

type BlockOrExpr =
    | Block of Block
    | Expr of Expr
    member this.Type =
        match this with
        | Block b -> b.Type
        | Expr e -> e.Type

type ElseIfOrBlock =
    | ElseIf of guardExpr: Expr * thenBlock: Block * elseBlock: ElseIfOrBlock
    | ElseBlock of Block

type FlowControl =
    // TODO: Enforce either catch or finalizer or both
    | TryCatch of body: Block * catch: (Reference * Block) option * finalizer: Block option
    // TODO: Else block can also be IfThenElse
    | IfThenElse of guardExpr: Expr * thenBlock: Block * elseBlock: ElseIfOrBlock option
    member this.Type =
        match this with
        | TryCatch(body,_,_) -> body.Type
        | IfThenElse(_,thenExpr,_) -> thenExpr.Type

type ReturnStatement =
    | Return of Expr
    | FlowControlReturn of FlowControl
    member this.Type =
        match this with
        | Return e -> e.Type
        // TODO: This shouldn't be Void, check?
        | FlowControlReturn c -> c.Type

type Statemement =
    | CallStatement of baseExpr: Expr * args: Expr list * isConstructor: bool * hasSpread: bool * range: SourceLocation
    | Binding of ident: Reference * value: Expr * range: SourceLocation
    | Assignment of baseExpr: Expr * valueExpr: Expr * range: SourceLocation
    | WhileLoop of guard: Expr * body: Block * range: SourceLocation
    // | ForLoop
    // | Break
    // | Debugger of range: SourceLocation option
    // | Throw of Expr * typ: Type * range: SourceLocation option
    | FlowControlStatement of FlowControl

type Expr =
    | Literal of LiteralKind
    | Ident of Reference * SourceLocation option
    | Function of args: Argument list * hasSpread: bool * body: BlockOrExpr

    | Operation of OperationKind * typ: Type * range: SourceLocation option
    | Get of baseExpr: Expr * indexExpr: Expr * typ: Type * range: SourceLocation option

    // | DecisionTree of Expr * targets: (Ident list * Expr) list
    // | DecisionTreeSuccess of targetIndex: int * boundValues: Expr list * Type

    member this.Type =
        match this with
        | Literal k -> k.Type
        | Ident(ref, _) -> ref.refType
        | Function(args, hasSpread, body) ->
            let argTypes = args |> List.map (fun a ->
                { argType = a.reference.refType; isOptional = Option.isSome a.defaultValue })
            FunctionType(argTypes, hasSpread, body.Type)
        | Operation(_,t,_) -> t
        | Get(_,_,t,_) -> t

    member this.Range =
        match this with
        | Literal _ -> None
        | Ident(_,r) -> r
        | Function _ -> None // TODO: Include range?
        | Operation(_,_,r) -> r
        | Get(_,_,_,r) -> r
