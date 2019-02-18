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

// TODO: Enforce scope naming rules
// - Check if there's already a ref with same name in current scope
// - Check no reference starts with <skillname>$
type Scope =
    { references: Map<string, Reference>
      parent: Scope option }
    member this.TryFind(name: string) =
        match Map.tryFind name this.references with
        | Some ref -> Some ref
        | None -> this.parent |> Option.bind (fun s -> s.TryFind name)
    member this.Find(name: string) =
        match this.TryFind name with
        | Some ref -> ref
        | None -> failwithf "Cannot find reference in scope: %s" name

let makeReference name r isExport kind: Reference =
    { name = name
      kind = kind
      isExport = isExport
      declarationLocation = Some r }

let addLocalValueRefToScope (scope: Scope) name r typ isMutable =
    let ref = ValueRef(typ, isMutable, false) |> makeReference name r false
    ref, { scope with references = Map.add name ref scope.references }

let rec getTypeFromAnnotation (findRef: string->Reference) (annotation: Annotation) =
    match annotation with
    | Annotation.Any -> Type.Any
    | Annotation.Void -> Type.Void
    | Annotation.Null -> Type.Null
    | Annotation.Boolean -> Type.Boolean
    | Annotation.String -> Type.String
    | Annotation.Number -> Type.Number
    | Annotation.FunctionType(args, hasSpread, ret) ->
        let args = args |> List.map (fun a ->
            { argType = getTypeFromAnnotation findRef a.annotation
              isOptional = a.isOptional })
        Type.FunctionType(args, hasSpread, getTypeFromAnnotation findRef ret)
    | Annotation.GenericParam name -> GenericParam name
    | Annotation.DeclaredType(name, genArgs) ->
        DeclaredType(findRef name, genArgs |> List.map GenericParam)

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
                    | Some x -> getTypeFromAnnotation scope.Find x
                    // TODO: Infer type if function is passed as argument (expected type)
                    // or add an error
                    | None -> Any
                let ref, scope = addLocalValueRefToScope scope arg.name arg.range t false
                // Using the accumulated scope here is intentional, in JS optional arguments
                // can refer to previous arguments as default value `add(x, y=x)`
                let defValue = arg.defaultValue |> Option.map (checkExpr com scope (Some t))
                scope, { reference = ref; defaultValue = defValue }::acc)
        let retType =
            match returnAnnotation with
            | Some a -> getTypeFromAnnotation scope.Find a
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
            let ref, scope = addLocalValueRefToScope scope name r Any false
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
        let ref, scope = addLocalValueRefToScope scope ident identRange value.Type isMutable
        scope, Binding(ref, value, range)
    | Untyped.FlowControlStatement control ->
        let control = checkFlowControl com scope Void control
        scope, FlowControlStatement control

let checkBlock (com: FileCompiler) (scope: Scope) expected (block: Untyped.Block): Block =
    let scope = { parent = Some scope; references = Map.empty }
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
    let com = FileCompiler(file)
    let scope = getGlobalScope com ast
    let decls =
        ([], ast.declarations) ||> List.fold (fun acc decl ->
            match decl with
            | Untyped.TypeDeclaration _ -> acc
            | Untyped.ValueDeclaration(_isExport, _isMutable, name, range, annotation, body) ->
                // TODO: Find reference in scope and get type instead of checking the annotation
                let ref =
                    match Map.tryFind name scope.references with
                    | Some ref -> ref
                    | None -> failwithf "Unexpected: reference not found in scope %s (%A)" name range
                let body = checkExpr com scope (Some ref.Type) body
                ValueDeclaration(ref, body)::acc)
    { declarations = List.rev decls }

let getGlobalScope (com: FileCompiler) (ast: Untyped.FileAst): Scope =
    let rec resolveReference declMap scope decl: Reference option =
        let findRef declMap (scope: Scope) name =
            match Map.tryFind name scope.references with
            | Some ref -> ref
            | None ->
                Map.tryFind name declMap
                |> Option.bind (resolveReference declMap scope)
                |> Option.defaultWith (fun () -> failwithf "Cannot find reference in scope %s" name)
        let checkSignature declMap scope = function
            | Untyped.MethodSignature(name, args, hasSpread, returnType) ->
                let args = args |> List.map (fun a ->
                    { name = a.name
                      sigType = getTypeFromAnnotation (findRef declMap scope) a.annotation
                      isOptional = a.isOptional })
                MethodSignature(name, args, hasSpread, getTypeFromAnnotation (findRef declMap scope) returnType)
        match decl with
        | Untyped.TypeDeclaration(isExport, decl) ->
            match decl with
            | Untyped.SkillDeclaration((name, range), generic, signatures) ->
                let signatures = signatures |> List.map (checkSignature declMap scope)
                SkillRef(generic, signatures)
                |> makeReference name range isExport |> Some
            | Untyped.TrainDeclaration _ -> None
        // TODO: Exported values cannot be mutable
        | Untyped.ValueDeclaration(isExport, isMutable, name, range, annotation, _) ->
            let t =
                match annotation with
                | Some x -> getTypeFromAnnotation (findRef declMap scope) x
                | None -> Any // TODO: Infer type from body
            ValueRef(t, isMutable, false)
            |> makeReference name range isExport |> Some
    let declMap =
        (Map.empty, ast.declarations) ||> List.fold (fun acc decl ->
            match decl with
            | Untyped.TypeDeclaration(_,typeDecl) ->
                match typeDecl with
                | Untyped.SkillDeclaration((name,_),_,_) -> Map.add name decl acc
                | Untyped.TrainDeclaration _ -> acc
            | Untyped.ValueDeclaration(_,_,name,_,_,_) -> Map.add name decl acc)
    // TODO: Scope starts with: global values, imports
    ({ parent = None; references = Map.empty }, ast.declarations)
    ||> List.fold (fun scope decl ->
        match resolveReference declMap scope decl with
        | None -> scope
        | Some ref -> { scope with references = Map.add ref.name ref scope.references })
