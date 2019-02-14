module rec Ikigai.Compiler.Emitter

open Ikigai.Compiler.AST
open Ikigai.Compiler.AST.Ikigai
open Ikigai.Compiler.AST.Babel

let ident range (id: Reference) =
    Identifier(id.name, ?loc=range)

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
            ident a.reference.declarationLocation a.reference |> toPattern)
        upcast ArrowFunctionExpression(Array.ofList args, transformBlockOrExpr body)
    | Operation(kind, t, range) ->
        match kind with
        | Call(baseExpr, args, isCons, hasSpread) ->
            failwith "TODO"
        | UnaryOperation(op, e) ->
            upcast UnaryExpression(op, transformExpr e, ?loc=range)
        | BinaryOperation(op, e1, e2) ->
            upcast BinaryExpression(op, transformExpr e1, transformExpr e2, ?loc=range)
        | LogicalOperation(op, e1, e2) ->
            upcast LogicalExpression(op, transformExpr e1, transformExpr e2, ?loc=range)
        | TernaryOperation(cond, thenExpr, elseExpr) ->
            upcast ConditionalExpression(transformExpr cond, transformExpr thenExpr, transformExpr elseExpr, ?loc=range)
    | Get(baseExpr, indexExpr, t, range) ->
        failwith "TODO"

let transformFlowControl control: Statement =
    match control with
    | TryCatch(body, catch, finalizer) ->
        let handler =
            catch |> Option.map (fun (ref, body) ->
                CatchClause (ident ref.declarationLocation ref |> toPattern, transformBlock body))
        let finalizer =
            finalizer |> Option.map transformBlock
        upcast TryStatement(transformBlock body, ?handler=handler, ?finalizer=finalizer)
    | IfThenElse(guardExpr, thenBlock, elseBlock) ->
        let thenBlock = transformBlock thenBlock
        let elseBlock = elseBlock |> Option.map (fun x -> transformBlock x :> Babel.Statement)
        upcast IfStatement(transformExpr guardExpr, thenBlock, ?alternate=elseBlock)

let transformStatement statement: Statement =
    match statement with
    | CallStatement _
    | Assignment _
    | WhileLoop _ -> failwith "TODO"
    | Binding(ref, value, range) ->
        failwith "TODO"
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
