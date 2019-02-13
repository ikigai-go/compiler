namespace rec Ikigai.Compiler.AST.Ikigai

open System
open Ikigai.Compiler
open Ikigai.Compiler.AST

type Entity = Entity

type ArgumentType =
    { argType: Type; isOptional: bool }

type Type =
    | Any
    | Void // Undefined
    | Null
    | Boolean
    | String
    | Number
    | FunctionType of argTypes: ArgumentType list * returnType: Type * hasSpread: bool
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
    | FunctionType of argTypes: Annotation list * returnType: Annotation
    | GenericParam of name: string

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
        { name: string; annotation: Annotation option }

    type Block =
        { statements: Statemement list; returnStatement: ReturnStatement option }

    type BlockOrExpr =
        | Block of Block
        | Expr of Expr

    type FlowControl =
        // TODO: Enforce either catch or finalizer or both
        | TryCatch of body: Block * catch: (string * Block) option * finalizer: Block option
        | IfThenElse of guardExpr: Expr * thenExpr: Block * elseExpr: Block option

    type ReturnStatement =
        | Return of Expr
        | FlowControlReturn of FlowControl

    type Statemement =
        | CallStatement of baseExpr: Expr * args: Expr list * isConstructor: bool * hasSpread: bool * range: SourceLocation
        | Binding of ident: string * annotation: Annotation option * value: Expr * body: Expr * range: SourceLocation
        | Assignment of baseExpr: Expr * valueExpr: Expr * range: SourceLocation
        | WhileLoop of guard: Expr * body: Block * range: SourceLocation
        // | ForLoop
        // | Break
        // | Debugger of range: SourceLocation
        // | Throw of Expr * range: SourceLocation
        | FlowControlStatement of FlowControl

    type Expr =
        | Literal of LiteralKind
        | IdentExpr of name: string * range: SourceLocation
        | Function of args: Argument list * returnAnnotation: Annotation option * body: BlockOrExpr

        | Operation of OperationKind * range: SourceLocation
        | Get of baseExpr: Expr * indexExpr: Expr * range: SourceLocation

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
      /// Only used for arguments
      isOptional: bool
      isCompilerGenerated: bool }

type Block =
    { statements: Statemement list; returnStatement: ReturnStatement option }

type BlockOrExpr =
    | Block of Block
    | Expr of Expr

type FlowControl =
    // TODO: Enforce either catch or finalizer or both
    | TryCatch of body: Block * catch: (string * Block) option * finalizer: Block option
    | IfThenElse of guardExpr: Expr * thenExpr: Block * elseExpr: Block option

type ReturnStatement =
    | Return of Expr
    | FlowControlReturn of FlowControl

type Statemement =
    | CallStatement of baseExpr: Expr * args: Expr list * isConstructor: bool * hasSpread: bool * range: SourceLocation
    | Binding of ident: string * annotation: Annotation option * value: Expr * body: Expr * range: SourceLocation
    | Assignment of baseExpr: Expr * valueExpr: Expr * range: SourceLocation
    | WhileLoop of guard: Expr * body: Block * range: SourceLocation
    // | ForLoop
    // TODO: Some expressions, like TryCatch and IfThenElse can also appear in statement position
    // | Debugger of range: SourceLocation option
    // | Throw of Expr * typ: Type * range: SourceLocation option

type Expr =
    | Literal of LiteralKind
    | IdentExpr of Reference * SourceLocation option
    | Function of args: Reference list * body: Expr * name: string option * hasSpread: bool

    | Operation of OperationKind * typ: Type * range: SourceLocation option
    | Get of baseExpr: Expr * indexExpr: Expr * typ: Type * range: SourceLocation option

    // | DecisionTree of Expr * targets: (Ident list * Expr) list
    // | DecisionTreeSuccess of targetIndex: int * boundValues: Expr list * Type

    member this.Type =
        match this with
        | Literal k -> k.Type
        | IdentExpr(ref, _) -> ref.refType
        | Function(args, body, _, hasSpread) ->
            let argTypes = args |> List.map (fun r ->
                { argType = r.refType; isOptional = r.isOptional })
            FunctionType(argTypes, body.Type, hasSpread)
        | Operation(_,t,_) -> t
        | Get(_,_,t,_) -> t

