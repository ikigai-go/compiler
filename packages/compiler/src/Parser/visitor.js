import { parser } from "./parser"
import * as I from "./Interop.fs"

const BaseVisitor = parser.getBaseCstVisitorConstructor()

export class IkigaiVisitor extends BaseVisitor {
    constructor() {
        super()
        // The "validateVisitor" method is a helper utility which performs static analysis
        // to detect missing or redundant visitor methods
        this.validateVisitor()
    }

    // The Ctx argument is the current CSTNode's children.
    Program(ctx) {
        return I.makeProgram(ctx.ValueDeclaration.map(d => this.visit(d)));
    }

    ValueDeclaration(ctx) {
        return I.makeValueDeclaration(
            ctx.MutabilityModifier[0].image,
            ctx.Identifier[0].image,
            this.visit(ctx.Expression)
        )
    }

    Expression(ctx) {
        if (ctx.Literal) {
            return this.visit(ctx.Literal);
        } else {
            return this.visit(ctx.BinaryOperation);
        }
    }

    Literal(ctx) {
        if (ctx.Number) {
            return I.makeLiteral("Number", parseFloat(ctx.Number[0].image))
        } else {
            return I.makeLiteral("String", JSON.parse(ctx.String[0].image))
        }
    }

    BinaryOperation(ctx) {
        return I.makeBinaryOperation(
            this.visit(ctx.left[0]),
            ctx.BinaryOperator[0].image,
            this.visit(ctx.right[0])
        )
    }

    /* Visit methods go here */
}

// Our visitor has no state, so a single instance is sufficient.
const toAstVisitor = new IkigaiVisitor()
export function toAst(cst) {
    return toAstVisitor.visit(cst)
}