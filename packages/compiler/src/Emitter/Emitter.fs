module rec Ikigai.Compiler.Emitter

open Ikigai.Compiler.AST
open Ikigai.Compiler.AST.Ikigai
open Ikigai.Compiler.AST.Babel

let memberFromExpr memberExpr: Expression * bool =
    match memberExpr with
    | Literal (StringLiteral name) ->
        if Naming.hasIdentForbiddenChars name
        then upcast StringLiteral name, true
        else upcast Identifier name, false
    | e -> transformExpr e, true

let ident range (id: Reference) =
    Identifier(id.name, ?loc=range)

let identDeclaration (id: Reference) =
    Identifier(id.name, ?loc=id.declarationLocation)

let toPattern (e: PatternExpression): Pattern =
    Choice2Of2 e

let transformLiteral kind: Expression =
    match kind with
    | NullLiteral -> upcast NullLiteral()
    | VoidLiteral -> upcast (UnaryExpression(UnaryVoid, NumericLiteral 0.))
    | BoolLiteral b -> upcast BooleanLiteral b
    | StringLiteral s -> upcast StringLiteral s
     | NumberLiteral x ->
        if x < 0.
        // Negative numeric literals can give issues in Babel AST, see #1186
        then upcast UnaryExpression(UnaryMinus, NumericLiteral(x * -1.))
        else upcast NumericLiteral x

let transformExpr = function
    | Literal kind ->
        transformLiteral kind
    | Ident(ref, r) ->
        upcast ident r ref
    | Function(args, hasSpread, body) ->
        // TODO: Check default values, spread
        let args = args |> List.map (fun a ->
            identDeclaration a.reference |> toPattern)
        upcast ArrowFunctionExpression(Array.ofList args, transformBlockOrExpr body)
    | Operation(kind, _, range) ->
        match kind with
        | Call(baseExpr, args, isCons, hasSpread) ->
            // TODO: Check spread
            let args = args |> List.map transformExpr |> List.toArray
            let baseExpr = transformExpr baseExpr
            if isCons then upcast NewExpression(baseExpr, args, ?loc=range)
            else upcast CallExpression(baseExpr, args, ?loc=range)
        | UnaryOperation(op, e) ->
            upcast UnaryExpression(op, transformExpr e, ?loc=range)
        | BinaryOperation(op, e1, e2) ->
            upcast BinaryExpression(op, transformExpr e1, transformExpr e2, ?loc=range)
        | LogicalOperation(op, e1, e2) ->
            upcast LogicalExpression(op, transformExpr e1, transformExpr e2, ?loc=range)
        | TernaryOperation(cond, thenExpr, elseExpr) ->
            upcast ConditionalExpression(transformExpr cond, transformExpr thenExpr, transformExpr elseExpr, ?loc=range)
    | Get(baseExpr, indexExpr, _, range) ->
        let baseExpr = transformExpr baseExpr
        let indexExpr, computed = memberFromExpr indexExpr
        upcast MemberExpression(baseExpr, indexExpr, computed, ?loc=range)

let transformElseIfOrBlock = function
    | ElseBlock b -> transformBlock b :> Babel.Statement
    | ElseIf(guardExpr, thenBlock, elseBlock) ->
        let thenBlock = transformBlock thenBlock
        let elseBlock = transformElseIfOrBlock elseBlock
        upcast IfStatement(transformExpr guardExpr, thenBlock, elseBlock)

let transformFlowControl control: Statement =
    match control with
    | TryCatch(body, catch, finalizer) ->
        let handler =
            catch |> Option.map (fun (ref, body) ->
                CatchClause (identDeclaration ref |> toPattern, transformBlock body))
        let finalizer =
            finalizer |> Option.map transformBlock
        upcast TryStatement(transformBlock body, ?handler=handler, ?finalizer=finalizer)
    | IfThenElse(guardExpr, thenBlock, elseBlock) ->
        let thenBlock = transformBlock thenBlock
        let elseBlock = elseBlock |> Option.map transformElseIfOrBlock
        upcast IfStatement(transformExpr guardExpr, thenBlock, ?alternate=elseBlock)

let transformStatement statement: Statement =
    match statement with
    | CallStatement _
    | Assignment _
    | WhileLoop _ -> failwith "TODO"
    | Binding(ref, value, range) ->
        let kind = if ref.isMutable then Let else Const
        upcast VariableDeclaration(identDeclaration ref |> toPattern, transformExpr value, kind)
    | FlowControlStatement control ->
        transformFlowControl control

let transformBlock (block: Block): BlockStatement =
    [|for statement in block.statements do
        yield transformStatement statement
      match block.returnStatement with
      | None -> ()
      | Some(Return e) ->
        yield ReturnStatement(transformExpr e, ?loc=e.Range) :> Statement
      | Some(FlowControlReturn c) ->
        yield transformFlowControl c
    |] |> BlockStatement

let transformBlockOrExpr boe: Choice<BlockStatement, Expression> =
    match boe with
    | Block block -> transformBlock block |> Choice1Of2
    | Expr expr -> transformExpr expr |> Choice2Of2
