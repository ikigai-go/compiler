namespace rec Ikigai.Compiler.AST.Ikigai

open System
open Ikigai.Compiler
open Ikigai.Compiler.AST

type ArgumentType =
    { argType: Type; isOptional: bool }

type Primitive =
    | Any
    | Void // Undefined
    | Null
    | Boolean
    | String
    | Number
    member this.Name =
        match this with
        | Any -> "any"
        | Void -> "void"
        | Null -> "null"
        | Boolean -> "boolean"
        | String -> "string"
        | Number -> "number"
    static member TryParse = function
        | "any" -> Some Any
        | "void" -> Some Void
        | "null" -> Some Null
        | "boolean" -> Some Boolean
        | "string" -> Some String
        | "number" -> Some Number
        | _ -> None

type Type =
    | Primitive of Primitive
    | FunctionType of argTypes: ArgumentType list * hasSpread: bool * returnType: Type
    | GenericParam of name: string
    | DeclaredType of Reference * genericArgs: Type list
    // | Union of Type list // Typescript-like unions
    // | Enum
    member this.Name: string =
        match this with
        | Primitive p -> p.Name
        | FunctionType _ -> "function" // Not a named type
        | GenericParam name -> name
        | DeclaredType(ref,_) -> ref.name // Generic args are not included in the reference name

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

type RangedName = string * SourceLocation

[<RequireQualifiedAccess>]
module Untyped =
    type ArgumentType =
        { annotation: Type; isOptional: bool }

    type Type =
        | Primitive of Primitive * SourceLocation
        | FunctionType of argTypes: ArgumentType list * hasSpread: bool * returnType: Type * SourceLocation
        | GenericParam of name: string * SourceLocation
        | DeclaredType of name: string * SourceLocation * genericArgs: Type list
        // | Union of Type list // Typescript-like unions
        // | Enum
        member this.Name: string =
            match this with
            | Primitive(p,_) -> p.Name
            | FunctionType _ -> "function" // Not a named type
            | GenericParam(name,_) -> name
            | DeclaredType(name,_,_) -> name // Generic args are not included in the reference name

    type FileAst =
        { declarations: Declaration list }

    type DeclarationKind =
        | ValueDeclaration of isMutable: bool * name: RangedName * annotation: Type option * body: Expr
        | SkillDeclaration of name: RangedName * generic: string * Signature list
        | TrainDeclaration of skillName: RangedName * trainedType: Type * Member list

    type Declaration =
        { kind: DeclarationKind; export: bool }

    type Signature =
        | MethodSignature of name: RangedName * args: ArgumentSignature list * hasSpread: bool * returnType: Type

    type Member =
        | Method of name: RangedName * args: Argument list * hasSpread: bool * returnType: Type option * body: BlockOrExpr

    type OperationKind =
        | Call of baseExpr: Expr * args: Expr list * isConstructor: bool * hasSpread: bool
        | UnaryOperation of UnaryOperator * Expr
        | BinaryOperation of BinaryOperator * left:Expr * right:Expr
        | LogicalOperation of LogicalOperator * left:Expr * right:Expr
        | TernaryOperation of condition: Expr * thenExpr:Expr * elseExpr:Expr

    type ArgumentSignature =
        { name: string
          annotation: Type
          isOptional: bool }

    type Argument =
        { name: string
          annotation: Type option
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
        member this.Range =
            match this with
            | Block x -> x.Range
            | Expr x -> x.Range

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
        | Binding of ident: RangedName * isMutable: bool * annotation: Type option * value: Expr * range: SourceLocation
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
        | Function of args: Argument list * hasSpread: bool * returnAnnotation: Type option * body: BlockOrExpr

        | Operation of OperationKind * range: SourceLocation
        | Get of baseExpr: Expr * indexExpr: Expr

        member this.Range =
            match this with
            | Literal(_,r)
            | Ident(_,r)
            | Operation(_,r) -> r
            | Get(e1,e2) -> e1.Range + e2.Range
            | Function(_,_,_,body) -> body.Range

// Typed AST

type FileAst =
    { declarations: Declaration list }

type Signature =
    | MethodSignature of name: RangedName * args: ArgumentSignature list * hasSpread: bool * returnType: Type

type Member =
    | Method of name: RangedName * args: Argument list * hasSpread: bool * returnType: Type * body: BlockOrExpr

type Declaration =
    | ValueDeclaration of ident: Reference * body: Expr
    // | SkillDeclaration of name: RangedName * generic: string * Signature list
    | TrainDeclaration of isExport: bool * skill: Reference * trainedType: Type * Member list

type OperationKind =
    | Call of baseExpr: Expr * args: Expr list * isConstructor: bool * hasSpread: bool
    | UnaryOperation of UnaryOperator * Expr
    | BinaryOperation of BinaryOperator * left:Expr * right:Expr
    | LogicalOperation of LogicalOperator * left:Expr * right:Expr
    | TernaryOperation of condition: Expr * thenExpr:Expr * elseExpr:Expr

type ReferenceKind =
    | ValueRef of valType: Type * isMutable: bool * isCompilerGenerated: bool
    | SkillRef of generic: string * members: Signature list
    | TrainRef of skillRef: Reference * trainedType: Type

type Reference =
    { name: string
      kind: ReferenceKind
      isExport: bool
      declarationLocation: SourceLocation option }
    member this.IsMutable =
        match this.kind with
        | ValueRef(_,isMutable,_) -> isMutable
        | _ -> false
    member this.Type =
        match this.kind with
        | ValueRef(t,_,_) -> t
        // TODO: How to type type references when used as expressions?
        | _ -> Primitive Any

type ArgumentSignature =
    { name: string
      sigType: Type
      isOptional: bool }

type Argument =
    { reference: Reference
      defaultValue: Expr option }

type Block =
    { statements: Statemement list
      returnStatement: ReturnStatement option }
    member this.Type =
        match this.returnStatement with
        | Some r -> r.Type
        | None -> Primitive Void

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

    member this.Type: Type =
        match this with
        | Literal k -> Primitive k.Type
        | Ident(ref, _) -> ref.Type
        | Function(args, hasSpread, body) ->
            let argTypes = args |> List.map (fun a ->
                { argType = a.reference.Type; isOptional = Option.isSome a.defaultValue })
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
