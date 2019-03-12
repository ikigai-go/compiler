declare module "*Interop.fs" {
    import { IToken } from "chevrotain";

    interface Position { line: number, column: number  }
    interface SourceLocation { start: Position, end: Position }

    // Using strings here only as placeholders for the types
    type Type = "Type"
    type Argument = "Argument"
    type Expr = "Expr"
    type Declaration = "Declaration"
    type DeclarationKind = "DeclarationKind"
    type Signature = "Signature"
    type ArgumentSignature = "ArgumentSignature"
    type FileAst = "FileAst"

    function makeRange(token: IToken): SourceLocation;
    function makeLiteral(kind: string, tok: IToken): Expr;
    function makeIdent(tok: IToken): Expr;
    function makeBinaryOperation(e1: Expr, op: IToken, e2: Expr): Expr;
    function makeUnaryOperation(op: IToken, e: Expr): Expr;
    function makeArgument(ident: IToken, annotation: Type|null, defaultValue: Expr|null): Argument;
    function makeType(ident: IToken, genericArgs: Type[]): Type;
    function makeLambdaExpression(args: Argument[], hasSpread: boolean, returnAnnotation: Type|null, body: Expr): Expr;
    function makeMethodSignature(name: IToken, args: ArgumentSignature[], hasSpread: boolean, returnType: Type): Signature;
    function makeArgumentSignature(name: IToken, isOptional: boolean, argType: Type): ArgumentSignature;
    function makeValueDeclaration(mut: string, ident: IToken, body: Expr): DeclarationKind;
    function makeSkillDeclaration(name: IToken, genericParam: IToken, signatures: Signature[]): DeclarationKind;
    function makeDeclaration(isExport: boolean, decl: DeclarationKind): Declaration;
    function makeProgram(decls: Declaration[]): FileAst;
}