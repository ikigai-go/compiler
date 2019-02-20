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
    | Annotation.Primitive p -> Primitive p
    | Annotation.GenericParam name -> GenericParam name
    | Annotation.FunctionType(args, hasSpread, ret) ->
        let args = args |> List.map (fun a ->
            { argType = getTypeFromAnnotation findRef a.annotation
              isOptional = a.isOptional })
        Type.FunctionType(args, hasSpread, getTypeFromAnnotation findRef ret)
    | Annotation.DeclaredType(name, genArgs) ->
        DeclaredType(findRef name, genArgs |> List.map GenericParam)

// TODO: If hasSpread check last argument is an array
let checkFunction (com: FileCompiler) (scope: Scope) (args: Untyped.Argument list) returnType body =
    let scope, args =
        ((scope, []), args) ||> List.fold (fun (scope, acc) arg ->
            let t =
                match arg.annotation with
                | Some x -> getTypeFromAnnotation scope.Find x
                // TODO: Infer type if function is passed as argument (expected type)
                // or add an error
                | None -> Primitive Any
            let ref, scope = addLocalValueRefToScope scope arg.name arg.range t false
            // Using the accumulated scope here is intentional, in JS optional arguments
            // can refer to previous arguments as default value `add(x, y=x)`
            let defValue = arg.defaultValue |> Option.map (checkExpr com scope (Some t))
            scope, { reference = ref; defaultValue = defValue }::acc)
    let body = checkBlockOrExpr com scope returnType body
    List.rev args, body

let injectSkillArgs (scope: Scope) appliedType (argExprs: Expr list) =
    let resolveGeneric argTypesAndExprs generic =
        argTypesAndExprs |> List.pick (fun (argType: ArgumentType, argExpr: Expr) ->
            // TODO: The generic can be nested
            match argType.argType with
            | GenericParam g when g = generic ->
                Some argExpr.Type
            | _ -> None)
    match appliedType with
    | FunctionType(argTypes,_,_) ->
        let mutable finished = false
        let skillArgs, restArgTypes =
            argTypes |> List.choosePartition (fun a ->
                if finished then None
                else
                    match a.argType with
                    | DeclaredType(ref,_) ->
                        match ref.kind with
                        | SkillRef(generic,_) -> Some(ref.name, generic)
                        | _ -> finished <- true; None
                    | _ -> finished <- true; None)
        match skillArgs with
        | [] -> argExprs
        | skillArgs ->
            let argTypesAndExprs = List.zip restArgTypes argExprs
            let injectedArgs =
                skillArgs |> List.map (fun (skillName, generic) ->
                    let expectedType = resolveGeneric argTypesAndExprs generic
                    let ident = Naming.trainName skillName expectedType.Name |> scope.Find
                    Ident(ident, None))
            injectedArgs @ argExprs
    | _ -> argExprs

let checkExpr (com: FileCompiler) (scope: Scope) (expected: Type option) e =
    match e with
    | Untyped.Literal(kind,_) -> Literal kind
    | Untyped.Ident(name, r) ->
        match scope.TryFind name with
        | Some ref -> Ident(ref, Some r)
        | None -> com.AddErrorAndReturnNull(Error.cannotFindIdent name, r)
    | Untyped.Function(args, hasSpread, returnAnnotation, body, _range) ->
        let retType =
            match returnAnnotation with
            | Some a -> getTypeFromAnnotation scope.Find a
            | None -> Primitive Any // TODO: Infer from expression or expected type
        let args, body = checkFunction com scope args retType body
        Function(List.rev args, hasSpread, body)
    | Untyped.Operation(kind, range) ->
        let typ, kind =
            match kind with
            | Untyped.Call(baseExpr, args, isCons, hasSpread) ->
                let baseExpr = checkExpr com scope expected baseExpr
                // TODO: Type check arguments ignoring skill ones
                let args =
                    args |> List.map (checkExpr com scope expected)
                    |> injectSkillArgs scope baseExpr.Type
                let t = Primitive Any // TODO: Check baseExpr
                t, Call(baseExpr, args, isCons, hasSpread)
            | Untyped.UnaryOperation(op, e) ->
                // TODO: Check the expected types according to the operator
                let t = defaultArg expected (Primitive Any)
                let e = checkExpr com scope expected e
                t, UnaryOperation(op, e)
            | Untyped.BinaryOperation(op, e1, e2) ->
                // TODO: Check the expected types according to the operator
                let t = defaultArg expected (Primitive Any)
                let e1 = checkExpr com scope expected e1
                let e2 = checkExpr com scope expected e2
                t, BinaryOperation(op, e1, e2)
            | Untyped.LogicalOperation(op, e1, e2) ->
                // TODO: Check the expected types according to the operator
                let e1 = checkExpr com scope expected e1
                let e2 = checkExpr com scope expected e2
                Primitive Boolean, LogicalOperation(op, e1, e2)
            | Untyped.TernaryOperation(cond, thenExpr, elseExpr) ->
                // TODO: Check condition is boolean and thenExpr/elseExpr have same type
                let cond = checkExpr com scope expected cond
                let thenExpr = checkExpr com scope expected thenExpr
                let elseExpr = checkExpr com scope expected elseExpr
                thenExpr.Type, TernaryOperation(cond, thenExpr, elseExpr)
        Operation(kind, typ, Some range)
    | Untyped.Get(baseExpr, indexExpr, range) ->
        // TODO: Check type of baseExpr to infer type
        let t = defaultArg expected (Primitive Any)
        let baseExpr = checkExpr com scope expected baseExpr
        let indexExpr = checkExpr com scope expected indexExpr
        Get(baseExpr, indexExpr, t, Some range)

let checkElseIfOrBlock (com: FileCompiler) (scope: Scope) expected (eseIfOrBlock: Untyped.ElseIfOrBlock): ElseIfOrBlock =
    match eseIfOrBlock with
    | Untyped.ElseBlock b -> checkBlock com scope expected b |> ElseBlock
    | Untyped.ElseIf(cond, thenBlock, elseIfOrBlock) ->
        let cond = checkExpr com scope (Primitive Boolean |> Some) cond
        let thenBlock = checkBlock com scope expected thenBlock
        let elseIfOrBlock = checkElseIfOrBlock com scope expected elseIfOrBlock
        ElseIf(cond, thenBlock, elseIfOrBlock)

let checkFlowControl (com: FileCompiler) (scope: Scope) expected (control: Untyped.FlowControl): FlowControl =
    match control with
    // TODO: Enforce either catch or finalizer or both
    | Untyped.TryCatch(body, catch, finalizer) ->
        let body = checkBlock com scope expected body
        let catch = catch |> Option.map (fun (name, r, block) ->
            let ref, scope = addLocalValueRefToScope scope name r (Primitive Any) false
            ref, checkBlock com scope expected block)
        let finalizer = finalizer |> Option.map (checkBlock com scope expected)
        TryCatch(body, catch, finalizer)
    | Untyped.IfThenElse(cond, thenBlock, elseBlock) ->
        let cond = checkExpr com scope (Primitive Boolean |> Some) cond
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
        let control = checkFlowControl com scope (Primitive Void) control
        scope, FlowControlStatement control

let checkBlock (com: FileCompiler) (scope: Scope) (expected: Type) (block: Untyped.Block): Block =
    let scope = { parent = Some scope; references = Map.empty }
    let statements, ret =
        match expected, block.returnStatement with
        | Primitive Void, Some ret ->
            match ret with
            | Untyped.Return e ->
                com.AddError(Error.unexpectedReturningBlock, e.Range)
                block.statements, None
            | Untyped.FlowControlReturn c ->
                block.statements @ [Untyped.FlowControlStatement c], None
        | Primitive Void, None ->
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
    let decls = ast.declarations |> List.choose (function
            // TODO: Add skill declarations to typed tree too?
            | Untyped.SkillDeclaration _ -> None
            | Untyped.TrainDeclaration(isExport, skill, trained, range, members) ->
                let trained = getTypeFromAnnotation scope.Find trained
                let members = members |> List.map (function
                    | Untyped.Method(name, args, hasSpread, returnAnnotation, body) ->
                        let retType =
                            match returnAnnotation with
                            | Some a -> getTypeFromAnnotation scope.Find a
                            | None -> Primitive Any // TODO: Infer from expression
                        let args, body = checkFunction com scope args retType body
                        Method(name, args, hasSpread, retType, body))
                TrainDeclaration(isExport, scope.Find skill, trained, members) |> Some
            | Untyped.ValueDeclaration(_isExport, _isMutable, name, range, annotation, body) ->
                let ref =
                    match Map.tryFind name scope.references with
                    | Some ref -> ref
                    | None -> failwithf "Unexpected: reference not found in scope %s (%A)" name range
                let body = checkExpr com scope (Some ref.Type) body
                ValueDeclaration(ref, body) |> Some)
    { declarations = decls }

let getGlobalScope (com: FileCompiler) (ast: Untyped.FileAst): Scope =
    let rec resolveReference declMap scope decl: Reference =
        let findRef declMap (scope: Scope) name =
            match Map.tryFind name scope.references with
            | Some ref -> ref
            | None ->
                match Map.tryFind name declMap with
                | Some decl -> resolveReference declMap scope decl
                | None -> failwithf "Cannot find reference in scope %s" name
        let checkSignature declMap scope = function
            | Untyped.MethodSignature(name, args, hasSpread, returnType) ->
                let args = args |> List.map (fun a ->
                    { name = a.name
                      sigType = getTypeFromAnnotation (findRef declMap scope) a.annotation
                      isOptional = a.isOptional })
                MethodSignature(name, args, hasSpread, getTypeFromAnnotation (findRef declMap scope) returnType)
        match decl with
        | Untyped.TrainDeclaration(isExport, skill, trained, range, _) ->
            TrainRef(findRef declMap scope skill, getTypeFromAnnotation (findRef declMap scope) trained)
            |> makeReference (Naming.trainName skill trained.Name) range isExport
        | Untyped.SkillDeclaration(isExport, (name, range), generic, signatures) ->
            let signatures = signatures |> List.map (checkSignature declMap scope)
            SkillRef(generic, signatures)
            |> makeReference name range isExport
        // TODO: Exported values cannot be mutable
        | Untyped.ValueDeclaration(isExport, isMutable, name, range, annotation, _) ->
            let t =
                match annotation with
                | Some x -> getTypeFromAnnotation (findRef declMap scope) x
                | None -> Primitive Any // TODO: Infer type from body
            ValueRef(t, isMutable, false)
            |> makeReference name range isExport
    let declMap =
        (Map.empty, ast.declarations) ||> List.fold (fun acc decl ->
            match decl with
            | Untyped.TrainDeclaration(_,skill,trained,_,_) ->
                Map.add (Naming.trainName skill trained.Name) decl acc
            | Untyped.SkillDeclaration(_,(name,_),_,_) ->
                Map.add name decl acc
            | Untyped.ValueDeclaration(_,_,name,_,_,_) ->
                Map.add name decl acc)
    // TODO: Scope starts with: global values, imports
    ({ parent = None; references = Map.empty }, ast.declarations)
    ||> List.fold (fun scope decl ->
        let ref = resolveReference declMap scope decl
        { scope with references = Map.add ref.name ref scope.references })
