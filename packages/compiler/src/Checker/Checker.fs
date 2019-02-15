module rec Ikigai.Compiler.Checker

open Ikigai.Compiler.AST
open Ikigai.Compiler.AST.Ikigai

// TODO: Ensure normalized path?
type File = string

type Error(msg: string, ?range: SourceLocation) =
    member __.Message = msg
    member __.Range = range

type FileCompiler(file: File) =
    let errors = ResizeArray()
    member __.AddError(msg, ?range) =
        Error(msg, ?range=range) |> errors.Add
    member this.AddErrorAndReturnNull(msg, ?range) =
        this.AddError(msg, ?range=range)
        NullLiteral |> Literal

type Scope =
    { references: Map<string, Reference>
      parent: Scope option }
    member this.TryFind(name: string) =
        match Map.tryFind name this.references with
        | Some ref -> Some ref
        | None -> this.parent |> Option.bind (fun s -> s.TryFind name)

let addReferenceToScope (com: FileCompiler) (scope: Scope) name typ isMutable r =
    let ref =
        { name = name
          refType = typ
          isMutable = isMutable
          isCompilerGenerated = false
          declarationLocation = Some r }
    // TODO: Check if there's already a ref with same name in current scope
    ref, { scope with references = Map.add name ref scope.references }

let checkExpr (com: FileCompiler) (scope: Scope) (expected: Type option) e =
    match e with
    | Untyped.Literal(kind,_) -> Literal kind
    | Untyped.Ident(name, r) ->
        match scope.TryFind name with
        | Some ref -> Ident(ref, Some r)
        | None -> com.AddErrorAndReturnNull(Error.cannotFindIdent name, r)
    | Untyped.Function(args, hasSpread, returnAnnotation, body, range) ->
        let scope, args =
            ((scope, []), args) ||> List.fold (fun (scope, acc) arg ->
                let t =
                    match arg.annotation with
                    | Some x -> x.Type
                    // TODO: Infer type if function is passed as argument (expected type)
                    // or add an error
                    | None -> Any
                let ref, scope = addReferenceToScope com scope arg.name t false arg.range
                // Using the accumulated scope here is intentional, in JS optional arguments
                // can refer to previous arguments as default value `add(x, y=x)`
                let defValue = arg.defaultValue |> Option.map (checkExpr com scope (Some t))
                scope, { reference = ref; defaultValue = defValue }::acc)
        let retType =
            match returnAnnotation with
            | Some a -> a.Type
            | None -> Any // TODO: Infer from expression or expected type
        let body = checkBlockOrExpr com scope retType body
        Function(List.rev args, hasSpread, body)
    | Untyped.Operation(kind, range) ->
        let typ, kind =
            match kind with
            | Untyped.Call(baseExpr, args, isCons, hasSpread) ->
                let baseExpr = checkExpr com scope expected baseExpr
                let args = args |> List.map (checkExpr com scope expected)
                let t = Any // TODO: Check baseExpr
                t, Call(baseExpr, args, isCons, hasSpread)
            | Untyped.UnaryOperation(op, e) ->
                // TODO: Check the expected types according to the operator
                let t = defaultArg expected Any
                let e = checkExpr com scope expected e
                t, UnaryOperation(op, e)
            | Untyped.BinaryOperation(op, e1, e2) ->
                // TODO: Check the expected types according to the operator
                let t = defaultArg expected Any
                let e1 = checkExpr com scope expected e1
                let e2 = checkExpr com scope expected e2
                t, BinaryOperation(op, e1, e2)
            | Untyped.LogicalOperation(op, e1, e2) ->
                // TODO: Check the expected types according to the operator
                let e1 = checkExpr com scope expected e1
                let e2 = checkExpr com scope expected e2
                Boolean, LogicalOperation(op, e1, e2)
            | Untyped.TernaryOperation(cond, thenExpr, elseExpr) ->
                // TODO: Check condition is boolean and thenExpr/elseExpr have same type
                let cond = checkExpr com scope expected cond
                let thenExpr = checkExpr com scope expected thenExpr
                let elseExpr = checkExpr com scope expected elseExpr
                thenExpr.Type, TernaryOperation(cond, thenExpr, elseExpr)
        Operation(kind, typ, Some range)
    | Untyped.Get(baseExpr, indexExpr, range) ->
        // TODO: Check type of baseExpr to infer type
        let t = defaultArg expected Any
        let baseExpr = checkExpr com scope expected baseExpr
        let indexExpr = checkExpr com scope expected indexExpr
        Get(baseExpr, indexExpr, t, Some range)

let checkElseIfOrBlock (com: FileCompiler) (scope: Scope) expected (eseIfOrBlock: Untyped.ElseIfOrBlock): ElseIfOrBlock =
    match eseIfOrBlock with
    | Untyped.ElseBlock b -> checkBlock com scope expected b |> ElseBlock
    | Untyped.ElseIf(cond, thenBlock, elseIfOrBlock) ->
        let cond = checkExpr com scope (Some Boolean) cond
        let thenBlock = checkBlock com scope expected thenBlock
        let elseIfOrBlock = checkElseIfOrBlock com scope expected elseIfOrBlock
        ElseIf(cond, thenBlock, elseIfOrBlock)

let checkFlowControl (com: FileCompiler) (scope: Scope) expected (control: Untyped.FlowControl): FlowControl =
    match control with
    // TODO: Enforce either catch or finalizer or both
    | Untyped.TryCatch(body, catch, finalizer) ->
        let body = checkBlock com scope expected body
        let catch = catch |> Option.map (fun (name, r, block) ->
            let ref, scope = addReferenceToScope com scope name Any false r
            ref, checkBlock com scope expected block)
        let finalizer = finalizer |> Option.map (checkBlock com scope expected)
        TryCatch(body, catch, finalizer)
    | Untyped.IfThenElse(cond, thenBlock, elseBlock) ->
        let cond = checkExpr com scope (Some Boolean) cond
        let thenBlock = checkBlock com scope expected thenBlock
        // TODO: If expected is not Void and elseBlock is None, add error
        let elseBlock = elseBlock |> Option.map (checkElseIfOrBlock com scope expected)
        IfThenElse(cond, thenBlock, elseBlock)

let checkStatement (com: FileCompiler) (scope: Scope) (statement: Untyped.Statemement): Scope * Statemement =
    match statement with
    | Untyped.CallStatement _
    | Untyped.Assignment _
    | Untyped.WhileLoop _ -> failwith "TODO"
    | Untyped.Binding((ident, identRange), isMutable, annotation, value, range) ->
        // TODO: Infer expected type from annotation if present
        let value = checkExpr com scope None value
        let ref, scope = addReferenceToScope com scope ident value.Type isMutable identRange
        scope, Binding(ref, value, range)
    | Untyped.FlowControlStatement control ->
        let control = checkFlowControl com scope Void control
        scope, FlowControlStatement control

let checkBlock (com: FileCompiler) (scope: Scope) expected (block: Untyped.Block): Block =
    let statements, ret =
        match expected, block.returnStatement with
        | Void, Some ret ->
            match ret with
            | Untyped.Return e ->
                com.AddError(Error.unexpectedReturningBlock, e.Range)
                block.statements, None
            | Untyped.FlowControlReturn c ->
                block.statements @ [Untyped.FlowControlStatement c], None
        | Void, None ->
            block.statements, None
        | _, None  ->
            com.AddError(Error.unexpectedVoidBlock, block.Range)
            block.statements, Untyped.Literal(NullLiteral, SourceLocation.Empty) |> Untyped.Return |> Some
        | _, Some r ->
            block.statements, Some r
    let scope, statements =
        ((scope, []), statements) ||> List.fold (fun (scope, acc) stmnt ->
            let scope, stmnt = checkStatement com scope stmnt
            scope, stmnt::acc)
    let ret =
        ret |> Option.map (function
            | Untyped.Return e ->
                checkExpr com scope (Some expected) e |> Return
            | Untyped.FlowControlReturn c ->
                checkFlowControl com scope expected c |> FlowControlReturn)
    { statements = List.rev statements
      returnStatement = ret }

let checkBlockOrExpr (com: FileCompiler) (scope: Scope) expected boe: BlockOrExpr =
    match boe with
    | Untyped.Block block -> checkBlock com scope expected block |> Block
    | Untyped.Expr expr -> checkExpr com scope (Some expected) expr |> Expr

let check file (ast: Untyped.FileAst): FileAst =
    // TODO: Scope starts with: global values, imports, values declared in the file
    let scope = { parent = None; references = Map.empty }
    let com = FileCompiler(file)
    let _scope, decls =
        ((scope, []), ast.declarations) ||> List.fold (fun (scope, acc) decl ->
            match decl with
            // TODO: Exported values cannot be mutable
            | Untyped.ValueDeclaration(isExport, isMutable, name, range, annotation, body) ->
                let t =
                    match annotation with
                    | Some x -> x.Type
                    | None -> Any // TODO: Infer type from body
                let ref, scope = addReferenceToScope com scope name t isMutable range
                let body = checkExpr com scope (Some t) body
                scope, ValueDeclaration(isExport, ref, body)::acc)
    { declarations = List.rev decls }
