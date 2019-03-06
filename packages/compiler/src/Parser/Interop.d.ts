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
    function makeValueDeclaration(mut: string, ident: string, body: Expr): Declaration;
    function makeProgram(decls: Declaration[]): FileAst;
    function makeArgument(name: string, annotation: Annotation|null, defaultValue: Expr|null): Argument;
    function makeAnnotation(ident: string): Annotation;

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
        makeValueDeclaration,
        makeProgram,
        makeArgument,
        makeAnnotation,
    }
}