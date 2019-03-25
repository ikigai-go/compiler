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
    member this.FindOrNullRef(com: FileCompiler, name: string, ?range) =
        match this.TryFind(name) with
        | Some ref -> ref
        | None ->
            com.AddError(Error.cannotFindRef name, ?range=range)
            NULL_REF

let (|RefKind|) (ref: Reference) = ref.kind

let makeReference name r isExport kind: Reference =
    { name = name
      kind = kind
      isExport = isExport
      declarationLocation = Some r }

let NULL_REF =
    ValueRef(Primitive Any, false, true)
    |> makeReference "null" SourceLocation.Empty false

let addLocalValueRefToScope (scope: Scope) name r typ isMutable =
    let ref = ValueRef(typ, isMutable, false) |> makeReference name r false
    ref, { scope with references = Map.add name ref scope.references }

let inferTypeFromExpr com scope = function
    | Untyped.Literal(kind,_) ->
        Primitive kind.Type
    | Untyped.Function(args, hasSpread, returnType, body) ->
        let argTypes = args |> List.map (fun arg ->
            let t =
                match arg.annotation with
                | Some a -> getTypeFromAnnotation com scope a
                | None -> Primitive Any
            { argType = t; isOptional = Option.isSome arg.defaultValue })
        let returnType =
            getTypeFromAnnotationOrInferFromExpr com scope returnType body
        FunctionType(argTypes, hasSpread, returnType)
    // TODO
    | _ -> Primitive Any

let getTypeFromAnnotation (com: FileCompiler) (scope: Scope) (annotation: Untyped.Type) =
    match annotation with
    | Untyped.Primitive(p,_) -> Primitive p
    | Untyped.GenericParam(name,_,_) -> GenericParam name
    | Untyped.FunctionType(args, hasSpread, ret, _) ->
        let args = args |> List.map (fun a ->
            { argType = getTypeFromAnnotation com scope a.annotation
              isOptional = a.isOptional })
        Type.FunctionType(args, hasSpread, getTypeFromAnnotation com scope ret)
    | Untyped.DeclaredType(name, r, genArgs) ->
        match scope.TryFind(name) with
        | Some ref -> DeclaredType(ref, genArgs |> List.map (getTypeFromAnnotation com scope))
        | None ->
            com.AddError(Error.cannotFindType name, r)
            Primitive Any

let getTypeFromAnnotationOrInferFromExpr com scope annotation expr =
    match annotation with
    | Some a -> getTypeFromAnnotation com scope a
    | None -> inferTypeFromExpr com scope expr

// TODO: If hasSpread check last argument is an array
let checkFunction (com: FileCompiler) (scope: Scope) (args: Untyped.Argument list) returnType body =
    let scope, args =
        ((scope, []), args) ||> List.fold (fun (scope, acc) arg ->
            let t =
                match arg.annotation with
                | Some x -> getTypeFromAnnotation com scope x
                // TODO: Infer type if function is passed as argument (expected type) or add an error
                | None -> Primitive Any
            let ref, scope = addLocalValueRefToScope scope arg.name arg.location t false
            // Using the accumulated scope here is intentional, in JS optional arguments
            // can refer to previous arguments as default value `add(x, y=x)`
            let defValue = arg.defaultValue |> Option.map (checkExpr com scope (Some t))
            scope, { reference = ref; defaultValue = defValue }::acc)
    let body = checkExpr com scope (Some returnType) body
    List.rev args, body

let injectSkillArgs com (scope: Scope) appliedType (argExprs: Expr list) =
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
                    let name = Naming.trainName skillName expectedType.Name
                    // TODO: Better error if training is not found
                    Ident(scope.FindOrNullRef(com, name), None))
            injectedArgs @ argExprs
    | _ -> argExprs

let checkExpr (com: FileCompiler) (scope: Scope) (expected: Type option) e =
    match e with
    | Untyped.Binding _ -> failwith "TODO"
    | Untyped.Literal(kind,_) -> Literal kind
    | Untyped.Ident(name, r) ->
        match scope.TryFind name with
        | Some ref -> Ident(ref, Some r)
        | None -> com.AddErrorAndReturnNull(Error.cannotFindValue name, r)
    | Untyped.Function(args, hasSpread, returnAnnotation, body) ->
        // TODO: Infer also from expected type
        let retType = getTypeFromAnnotationOrInferFromExpr com scope returnAnnotation body
        let args, body = checkFunction com scope args retType body
        Function(List.rev args, hasSpread, body)
    | Untyped.Operation(kind, range) ->
        let makeOp typ kind =
            Operation(kind, typ, Some range)
        match kind with
        | Untyped.Call(baseExpr, args, isCons, hasSpread) ->
            let baseExpr = checkExpr com scope expected baseExpr
            match baseExpr with
            // TODO: Accept case name alone when expecting the enum
            | Get(Ident(RefKind(EnumRef(_,cases)) as r1,_),Literal(StringLiteral caseName),_,_) ->
                match cases |> List.tryFind (fun c -> c.name = caseName) with
                | None ->
                    com.AddError(Error.cannotFindEnumCase caseName r1.name, range)
                    Call(baseExpr, [], false, false) |> makeOp (Primitive Any)
                | Some c ->
                    let genArs = [] // TODO
                    NewEnum(r1, genArs, c, args |> List.map (checkExpr com scope expected), Some range)
            | _ ->
                // TODO: Type check arguments ignoring skill ones, fix expected
                let args =
                    args |> List.map (checkExpr com scope expected)
                    |> injectSkillArgs com scope baseExpr.Type
                let t = Primitive Any // TODO: Check baseExpr
                Call(baseExpr, args, isCons, hasSpread) |> makeOp t
        | Untyped.UnaryOperation(op, e) ->
            // TODO: Check the expected types according to the operator
            let t = defaultArg expected (Primitive Any)
            let e = checkExpr com scope expected e
            UnaryOperation(op, e) |> makeOp t
        | Untyped.BinaryOperation(op, e1, e2) ->
            // TODO: Check the expected types according to the operator
            let t = defaultArg expected (Primitive Any)
            let e1 = checkExpr com scope expected e1
            let e2 = checkExpr com scope expected e2
            BinaryOperation(op, e1, e2) |> makeOp t
        | Untyped.LogicalOperation(op, e1, e2) ->
            // TODO: Check the expected types according to the operator
            let e1 = checkExpr com scope expected e1
            let e2 = checkExpr com scope expected e2
            LogicalOperation(op, e1, e2) |> makeOp (Primitive Boolean)
        | Untyped.TernaryOperation(cond, thenExpr, elseExpr) ->
            // TODO: Check condition is boolean and thenExpr/elseExpr have same type
            let cond = checkExpr com scope expected cond
            let thenExpr = checkExpr com scope expected thenExpr
            let elseExpr = checkExpr com scope expected elseExpr
            TernaryOperation(cond, thenExpr, elseExpr) |> makeOp thenExpr.Type
    | Untyped.Get(baseExpr, indexExpr) ->
        // TODO: Check type of baseExpr to infer type
        let t = defaultArg expected (Primitive Any)
        let baseExpr = checkExpr com scope expected baseExpr
        let indexExpr = checkExpr com scope expected indexExpr
        Get(baseExpr, indexExpr, t, Some e.Range)

// let checkElseIfOrBlock (com: FileCompiler) (scope: Scope) expected (eseIfOrBlock: Untyped.ElseIfOrBlock): ElseIfOrBlock =
//     match eseIfOrBlock with
//     | Untyped.ElseBlock b -> checkBlock com scope expected b |> ElseBlock
//     | Untyped.ElseIf(cond, thenBlock, elseIfOrBlock) ->
//         let cond = checkExpr com scope (Primitive Boolean |> Some) cond
//         let thenBlock = checkBlock com scope expected thenBlock
//         let elseIfOrBlock = checkElseIfOrBlock com scope expected elseIfOrBlock
//         ElseIf(cond, thenBlock, elseIfOrBlock)

// let checkFlowControl (com: FileCompiler) (scope: Scope) expected (control: Untyped.FlowControl): FlowControl =
//     match control with
//     // TODO: Enforce either catch or finalizer or both
//     | Untyped.TryCatch(body, catch, finalizer) ->
//         let body = checkBlock com scope expected body
//         let catch = catch |> Option.map (fun (name, r, block) ->
//             let ref, scope = addLocalValueRefToScope scope name r (Primitive Any) false
//             ref, checkBlock com scope expected block)
//         let finalizer = finalizer |> Option.map (checkBlock com scope expected)
//         TryCatch(body, catch, finalizer)
//     | Untyped.IfThenElse(cond, thenBlock, elseBlock) ->
//         let cond = checkExpr com scope (Primitive Boolean |> Some) cond
//         let thenBlock = checkBlock com scope expected thenBlock
//         // TODO: If expected is not Void and elseBlock is None, add error
//         let elseBlock = elseBlock |> Option.map (checkElseIfOrBlock com scope expected)
//         IfThenElse(cond, thenBlock, elseBlock)

// let checkStatement (com: FileCompiler) (scope: Scope) (statement: Untyped.Statemement): Scope * Statemement =
//     match statement with
//     | Untyped.CallStatement _
//     | Untyped.Assignment _
//     | Untyped.WhileLoop _ -> failwith "TODO"
//     | Untyped.Binding((ident, identRange), isMutable, annotation, value, range) ->
//         let expected = annotation |> Option.map (getTypeFromAnnotation com scope)
//         let value = checkExpr com scope expected value
//         let ref, scope = addLocalValueRefToScope scope ident identRange value.Type isMutable
//         scope, Binding(ref, value, range)
//     | Untyped.FlowControlStatement control ->
//         let control = checkFlowControl com scope (Primitive Void) control
//         scope, FlowControlStatement control

// let checkBlock (com: FileCompiler) (scope: Scope) (expected: Type) (block: Untyped.Block): Block =
//     let scope = { parent = Some scope; references = Map.empty }
//     let statements, ret =
//         match expected, block.returnStatement with
//         | Primitive Void, Some ret ->
//             match ret with
//             | Untyped.Return e ->
//                 com.AddError(Error.unexpectedReturningBlock, e.Range)
//                 block.statements, None
//             | Untyped.FlowControlReturn c ->
//                 block.statements @ [Untyped.FlowControlStatement c], None
//         | Primitive Void, None ->
//             block.statements, None
//         | _, None  ->
//             com.AddError(Error.unexpectedVoidBlock, block.Range)
//             block.statements, Untyped.Literal(NullLiteral, SourceLocation.Empty) |> Untyped.Return |> Some
//         | _, Some r ->
//             block.statements, Some r
//     let scope, statements =
//         ((scope, []), statements) ||> List.fold (fun (scope, acc) stmnt ->
//             let scope, stmnt = checkStatement com scope stmnt
//             scope, stmnt::acc)
//     let ret =
//         ret |> Option.map (function
//             | Untyped.Return e ->
//                 checkExpr com scope (Some expected) e |> Return
//             | Untyped.FlowControlReturn c ->
//                 checkFlowControl com scope expected c |> FlowControlReturn)
//     { statements = List.rev statements
//       returnStatement = ret }

// let checkBlockOrExpr (com: FileCompiler) (scope: Scope) expected boe: BlockOrExpr =
//     match boe with
//     | Untyped.Block block -> checkBlock com scope expected block |> Block
//     | Untyped.Expr expr -> checkExpr com scope (Some expected) expr |> Expr

let check file (ast: Untyped.FileAst): FileAst =
    let com = FileCompiler(file)
    let scope = getGlobalScope com ast
    let decls = ast.declarations |> List.choose (fun decl ->
        match decl.kind with
        // TODO: Add enum/skill declarations to typed tree too?
        | Untyped.EnumDeclaration _
        | Untyped.SkillDeclaration _ -> None
        | Untyped.TrainDeclaration((skill, range), trained, members) ->
            let trained = getTypeFromAnnotation com scope trained
            let members = members |> List.map (function
                | Untyped.Method(name, args, hasSpread, returnAnnotation, body) ->
                    let retType = getTypeFromAnnotationOrInferFromExpr com scope returnAnnotation body
                    let args, body = checkFunction com scope args retType body
                    Method(name, args, hasSpread, retType, body))
            let skill = scope.FindOrNullRef(com, skill, range)
            TrainDeclaration(decl.export, skill, trained, members) |> Some
        | Untyped.ValueDeclaration(_isMutable, (name, range), annotation, body) ->
            let ref =
                match Map.tryFind name scope.references with
                | Some ref -> ref
                | None -> failwithf "Unexpected: reference not found in scope %s (%A)" name range
            let body = checkExpr com scope (Some ref.Type) body
            ValueDeclaration(ref, body) |> Some)
    { declarations = decls }

let getGlobalScope (com: FileCompiler) (ast: Untyped.FileAst): Scope =
    let rec resolveReference declMap scope (decl: Untyped.Declaration): Reference =
        let findRef declMap (scope: Scope) name =
            match Map.tryFind name scope.references with
            | Some ref -> ref
            | None ->
                match Map.tryFind name declMap with
                | Some decl -> resolveReference declMap scope decl
                | None -> failwithf "Cannot find reference in scope %s" name
        let checkSignature scope = function
            | Untyped.MethodSignature(name, args, hasSpread, returnType) ->
                let args = args |> List.map (fun a ->
                    { name = a.name
                      sigType = getTypeFromAnnotation com scope a.annotation
                      isOptional = a.isOptional })
                MethodSignature(name, args, hasSpread, getTypeFromAnnotation com scope returnType)
        match decl.kind with
        | Untyped.EnumDeclaration((name, range), generic, cases) ->
            // TODO: Check case names start with upper case
            let cases = cases |> List.mapi (fun i ((caseName, caseRange), fields) ->
                let fields = fields |> List.map (fun (fieldName, typ) ->
                    fieldName, getTypeFromAnnotation com scope typ)
                { name = caseName
                  index = i
                  location = caseRange
                  fields = fields })
            EnumRef(generic, cases)
            |> makeReference name range decl.export
        | Untyped.TrainDeclaration((skill, range), trained, _) ->
            TrainRef(findRef declMap scope skill, getTypeFromAnnotation com scope trained)
            |> makeReference (Naming.trainName skill trained.Name) range decl.export
        | Untyped.SkillDeclaration((name, range), generic, signatures) ->
            let signatures = signatures |> List.map (checkSignature scope)
            SkillRef(generic, signatures)
            |> makeReference name range decl.export
        // TODO: Exported values cannot be mutable
        | Untyped.ValueDeclaration(isMutable, (name, range), annotation, body) ->
            let t = getTypeFromAnnotationOrInferFromExpr com scope annotation body
            ValueRef(t, isMutable, false)
            |> makeReference name range decl.export
    let declMap =
        (Map.empty, ast.declarations) ||> List.fold (fun acc decl ->
            match decl.kind with
            | Untyped.EnumDeclaration((name,_),_,_) ->
                Map.add name decl acc
            | Untyped.TrainDeclaration((skill,_),trained,_) ->
                Map.add (Naming.trainName skill trained.Name) decl acc
            | Untyped.SkillDeclaration((name,_),_,_) ->
                Map.add name decl acc
            | Untyped.ValueDeclaration(_,(name,_),_,_) ->
                Map.add name decl acc)
    // TODO: Scope starts with: global values, imports
    ({ parent = None; references = Map.empty }, ast.declarations)
    ||> List.fold (fun scope decl ->
        let ref = resolveReference declMap scope decl
        { scope with references = Map.add ref.name ref scope.references })
