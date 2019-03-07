declare module "*Interop.fs" {
    interface Annotation {}
    interface Argument {}
    interface Expr {}
    interface Declaration {}
    interface FileAst {}

    function makeLiteral(kind: string, value: string): Expr;
    function makeIdent(name: string): Expr;
    function makeBinaryOperation(e1: Expr, op: string, e2: Expr): Expr;
    function makeUnaryOperation(e1: Expr, op: string): Expr;
    function makeArgument(name: string, annotation: Annotation|null, defaultValue: Expr|null): Argument;
    function makeAnnotation(ident: string): Annotation;
    function makeLambdaExpression(args: Argument[], hasSpread: boolean, returnAnnotation: Annotation|null, body: Expr): Expr;
    function makeValueDeclaration(mut: string, ident: string, body: Expr): Declaration;
    function makeProgram(decls: Declaration[]): FileAst;

    export {
        Annotation,
        Argument,
        Expr,
        Declaration,
        FileAst,
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