declare module "*Interop.fs" {
    import { IToken } from "chevrotain";

    interface Type {}
    interface Argument {}
    interface Expr {}
    interface Declaration {}
    interface FileAst {}
    interface Position { line: number, column: number  }
    interface SourceLocation { start: Position, end: Position }

    function makeRange(token: IToken): SourceLocation;
    function makeLiteral(kind: string, tok: IToken): Expr;
    function makeIdent(tok: IToken): Expr;
    function makeBinaryOperation(e1: Expr, op: IToken, e2: Expr): Expr;
    function makeUnaryOperation(op: IToken, e: Expr): Expr;
    function makeArgument(ident: IToken, annotation: Type|null, defaultValue: Expr|null): Argument;
    function makeType(ident: IToken, genericArgs: Type[]): Type;
    function makeLambdaExpression(args: Argument[], hasSpread: boolean, returnAnnotation: Type|null, body: Expr): Expr;
    function makeValueDeclaration(mut: string, ident: IToken, body: Expr): Declaration;
    function makeProgram(decls: Declaration[]): FileAst;

    export {
        Type,
        Argument,
        Expr,
        Declaration,
        FileAst,
        makeRange,
        makeLiteral,
        makeIdent,
        makeBinaryOperation,
        makeUnaryOperation,
        makeArgument,
        makeType,
        makeLambdaExpression,
        makeValueDeclaration,
        makeProgram,
    }
}