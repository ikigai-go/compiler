module rec Ikigai.Compiler.Emitter

open Ikigai.Compiler.AST
open Ikigai.Compiler.AST.Ikigai
open Ikigai.Compiler.AST.Babel

let memberFromName name: Expression * bool =
    if Naming.hasIdentForbiddenChars name
    then upcast StringLiteral name, true
    else upcast Identifier name, false

let memberFromExpr memberExpr: Expression * bool =
    match memberExpr with
    | Literal (StringLiteral name) -> memberFromName name
    | e -> transformExpr e, true

let ident range (id: Reference) =
    Identifier(id.name, ?loc=range)

let identFromName range name =
    Identifier(name, ?loc=range)

let identDeclaration (id: Reference) =
    Identifier(id.name, ?loc=id.declarationLocation)

let toPattern (e: PatternExpression): Pattern =
    Choice2Of2 e

let nullStatement() =
    NullLiteral() |> ExpressionStatement :> Babel.Statement

let varDeclaration range (ref: Reference) value =
    let kind = if ref.IsMutable then Let else Const
    VariableDeclaration(identDeclaration ref |> toPattern, value, kind)

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

// TODO: Check default values, spread
let transformArgIdents hasSpread (args: Argument list) =
    args |> Seq.map (fun a ->
        identDeclaration a.reference |> toPattern) |> Seq.toArray

let transformExpr = function
    | Binding _ -> failwith "TODO"
    | Literal kind ->
        transformLiteral kind
    | Ident(ref, r) ->
        upcast ident r ref
    | Function(args, hasSpread, body) ->
        let args = transformArgIdents hasSpread args
        upcast ArrowFunctionExpression(args, transformExpr body |> Choice2Of2)
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

// let transformElseIfOrBlock = function
//     | ElseBlock b -> transformBlock b :> Babel.Statement
//     | ElseIf(guardExpr, thenBlock, elseBlock) ->
//         let thenBlock = transformBlock thenBlock
//         let elseBlock = transformElseIfOrBlock elseBlock
//         upcast IfStatement(transformExpr guardExpr, thenBlock, elseBlock)

// let transformFlowControl control: Statement =
//     match control with
//     | TryCatch(body, catch, finalizer) ->
//         let handler =
//             catch |> Option.map (fun (ref, body) ->
//                 CatchClause (identDeclaration ref |> toPattern, transformBlock body))
//         let finalizer =
//             finalizer |> Option.map transformBlock
//         upcast TryStatement(transformBlock body, ?handler=handler, ?finalizer=finalizer)
//     | IfThenElse(guardExpr, thenBlock, elseBlock) ->
//         let thenBlock = transformBlock thenBlock
//         let elseBlock = elseBlock |> Option.map transformElseIfOrBlock
//         upcast IfStatement(transformExpr guardExpr, thenBlock, ?alternate=elseBlock)

// let transformStatement statement: Statement =
//     match statement with
//     | CallStatement _
//     | Assignment _
//     | WhileLoop _ -> failwith "TODO"
//     | Binding(ref, value, range) ->
//         transformExpr value |> varDeclaration range ref :> _
//     | FlowControlStatement control ->
//         transformFlowControl control

// let transformBlock (block: Block): BlockStatement =
//     [|for statement in block.statements do
//         yield transformStatement statement
//       match block.returnStatement with
//       | None -> ()
//       | Some(Return e) ->
//         yield ReturnStatement(transformExpr e, ?loc=e.Range) :> Statement
//       | Some(FlowControlReturn c) ->
//         yield transformFlowControl c
//     |] |> BlockStatement

// let transformBlockOrExpr boe: Choice<BlockStatement, Expression> =
//     match boe with
//     | Block block -> transformBlock block |> Choice1Of2
//     | Expr expr -> transformExpr expr |> Choice2Of2

// let transformBlockOrExprAsBlock boe: BlockStatement =
//     match boe with
//     | Block block -> transformBlock block
//     | Expr expr ->
//         transformExpr expr
//         |> ReturnStatement :> Babel.Statement
//         |> Array.singleton
//         |> BlockStatement

let declareModuleVar isExport isMutable varName r (value: Expression) =
    let kind = if isMutable then Let else Const
    let var = identFromName r varName |> toPattern
    let varDeclaration = VariableDeclaration(var, value, kind)
    if not isExport then
        varDeclaration :> Babel.Statement |> Choice1Of2
    else
        ExportNamedDeclaration(varDeclaration)
        :> ModuleDeclaration |> Choice2Of2

let transformDeclaration decl: Choice<Babel.Statement, ModuleDeclaration> =
    match decl with
    | TrainDeclaration(isExport, skill, trained, members) ->
        let name = Naming.trainName skill.name trained.Name
        members |> Seq.map (function
            | Method((name,_), args, hasSpread, _, body) ->
                let args = transformArgIdents hasSpread args
                let body = [|transformExpr body |> ReturnStatement :> Statement|] |> BlockStatement
                let prop, computed = memberFromName name
                ObjectMethod(ObjectMeth, prop, args, body, computed_=computed) |> Choice2Of3)
        |> Seq.toArray
        |> ObjectExpression
        |> declareModuleVar isExport false name None
    | ValueDeclaration(i, body) ->
        transformExpr body
        |> declareModuleVar i.isExport i.IsMutable i.name i.declarationLocation

let transform file (ast: FileAst): Babel.Program =
    let decls = ast.declarations |> List.map transformDeclaration
    Program(file, Array.ofList decls)
