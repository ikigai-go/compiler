module Ikigai.Compiler.Checker

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

let rec checkExpr (com: FileCompiler) (scope: Scope) = function
    | Untyped.Literal kind ->
        Literal kind
    | Untyped.IdentExpr(name, r) ->
        match scope.TryFind name with
        | Some ref -> IdentExpr(ref, Some r)
        | None -> com.AddErrorAndReturnNull(Error.cannotFindIdent name, r)
    | Untyped.Function _ ->
        failwith "TODO: Function"
    | Untyped.Operation(kind, range) ->
        let typ, kind =
            match kind with
            | Untyped.Call(baseExpr, args, isCons, hasSpread) ->
                let baseExpr = checkExpr com scope baseExpr
                let args = args |> List.map (checkExpr com scope)
                let t = Any // TODO: Check baseExpr
                t, Call(baseExpr, args, isCons, hasSpread)
            | Untyped.UnaryOperation(op, e) ->
                // TODO: Check the expected types according to the operator
                let t = Any
                let e = checkExpr com scope e
                t, UnaryOperation(op, e)
            | Untyped.BinaryOperation(op, e1, e2) ->
                // TODO: Check the expected types according to the operator
                let t = Any
                let e1 = checkExpr com scope e1
                let e2 = checkExpr com scope e2
                t, BinaryOperation(op, e1, e2)
            | Untyped.LogicalOperation(op, e1, e2) ->
                // TODO: Check the expected types according to the operator
                let e1 = checkExpr com scope e1
                let e2 = checkExpr com scope e2
                Boolean, LogicalOperation(op, e1, e2)
            | Untyped.TernaryOperation(cond, thenExpr, elseExpr) ->
                // TODO: Check condition is boolean and thenExpr/elseExpr have same type
                let cond = checkExpr com scope cond
                let thenExpr = checkExpr com scope thenExpr
                let elseExpr = checkExpr com scope elseExpr
                thenExpr.Type, TernaryOperation(cond, thenExpr, elseExpr)
        Operation(kind, typ, Some range)
    | Untyped.Get(baseExpr, indexExpr, range) ->
        // TODO: Check type of baseExpr to infer type
        let t = Any
        let baseExpr = checkExpr com scope baseExpr
        let indexExpr = checkExpr com scope indexExpr
        Get(baseExpr, indexExpr, t, Some range)

let checkBlock (com: FileCompiler) (scope: Scope) (block: Untyped.Block): Block =
    failwith "TODO"

let checkBlockOrExpr (com: FileCompiler) (scope: Scope) boe: BlockOrExpr =
    match boe with
    | Untyped.Block block -> checkBlock com scope block |> Block
    | Untyped.Expr expr -> checkExpr com scope expr |> Expr
