declare module "*Interop.fs" {
    import { IToken } from "chevrotain";

    interface Annotation {}
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
    function makeArgument(ident: IToken, annotation: Annotation|null, defaultValue: Expr|null): Argument;
    function makeAnnotation(ident: string): Annotation;
    function makeLambdaExpression(args: Argument[], hasSpread: boolean, returnAnnotation: Annotation|null, body: Expr): Expr;
    function makeValueDeclaration(mut: string, ident: IToken, body: Expr): Declaration;
    function makeProgram(decls: Declaration[]): FileAst;

    export {
        Annotation,
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
        makeAnnotation,
        makeLambdaExpression,
        makeValueDeclaration,
        makeProgram,
    }
}