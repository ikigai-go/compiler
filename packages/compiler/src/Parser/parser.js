import { Lexer, Parser } from "chevrotain"
import Tok from "./tokens"
import * as I from "./Interop.fs"
import { R_OK } from "constants";

class IkigaiParser extends Parser {
    constructor() {
        // we have to explicitly disable the CST building for embedded actions to work.
        super(Tok, { outputCst: false, maxLookahead: 10 })
        const $ = this

        $.RULE("Program", () => {
            let decls = [];
            $.MANY(() => {
                decls.push($.SUBRULE($.ValueDeclaration))
            })
            return I.makeProgram(decls)
        })

        // DECLARATIONS ----------------------------------

        $.RULE("ValueDeclaration", () => {
            $.OPTION(() => {
                $.CONSUME(Tok.ExportModifier)
            })
            const mut = $.CONSUME(Tok.MutabilityModifier).image
            const id = $.CONSUME(Tok.Identifier).image
            $.CONSUME(Tok.Assignment)
            const exp = $.SUBRULE($.Expression)
            $.CONSUME(Tok.Semicolon)
            return I.makeValueDeclaration(mut, id, exp)
        })

        // EXPRESSION GROUPS ----------------------------------

        $.RULE("Expression", () => {
            let expr;
            $.OR([
                { ALT: () => expr = $.SUBRULE($.AdditionExpression) },
            ])
            return expr;
        })

        // Addition has lowest precedence thus it is first in the rule chain
        // The precedence of binary expressions is determined by how far down int the Parse Tree appear
        $.RULE("AdditionExpression", () => {
            let items = [];
            items.push($.SUBRULE($.ProductExpression))
            $.MANY(() => {
                items.push($.CONSUME(Tok.AdditionOperator).image)
                items.push($.SUBRULE2($.ProductExpression))
            })
            return associateBinaryRight(items)
        })

        $.RULE("ProductExpression", () => {
            let items = [];
            items.push($.SUBRULE($.ExponentialExpression))
            $.MANY(() => {
                items.push($.CONSUME(Tok.ProductOperator).image)
                items.push($.SUBRULE2($.ExponentialExpression))
            })
            return associateBinaryRight(items)
        })

        $.RULE("ExponentialExpression", () => {
            let items = [];
            items.push($.SUBRULE($.UnaryExpression))
            $.MANY(() => {
                items.push($.CONSUME(Tok.ExponentialOperator).image)
                items.push($.SUBRULE2($.UnaryExpression))
            })
            return associateBinaryRight(items)
        })

        $.RULE("UnaryExpression", () => {
            let op = null;
            $.OPTION(() => {
                op = $.CONSUME(Tok.UnaryOperator);
            });
            const expr = $.SUBRULE($.PrimaryExpression)
            return op ? I.makeUnaryOperation(op, expr) : expr;
        })

        $.RULE("PrimaryExpression", () => {
            let expr;
            $.OR([
                { ALT: () => expr = $.SUBRULE($.LiteralExpression) },
                { ALT: () => expr = $.SUBRULE($.IdentExpression) },
            ])
            return expr;
        })

        $.RULE("ParensExpression", () => {
            $.CONSUME(Tok.LParen)
            const expr = $.SUBRULE($.Expression)
            $.CONSUME(Tok.RParen)
            return expr;
        })

        $.RULE("LambdaExpression", () => {
            const args = [];
            $.CONSUME(Tok.LParen)
            $.OPTION(() => {
                args.push($.SUBRULE($.Argument))
                $.MANY(() => {
                    $.CONSUME(t.Comma)
                    args.push($.SUBRULE2($.Argument))
                })
            })
            $.CONSUME(Tok.Arrow)
            $.OR([
                { ALT: () => expr = $.SUBRULE($.Block) },
                { ALT: () => expr = $.SUBRULE($.Expression) },
            ])
            return; // TODO
        })

        $.RULE("Block", () => {
            // TODO
        })

        $.RULE("Argument", () => {
            let annotation;
            const id = $.CONSUME(Tok.Identifier)
            $.OPTION(() => {
                annotation = $.SUBRULE($.Annotation);
            });
            // TODO: default value
            return I.makeArgument(id, annotation)
        })

        $.RULE("Annotation", () => {
            $.CONSUME(Tok.Colon)
            const id = I.makeIdent(id); // TODO: Generic args, ad-hoc signatures
            return I.makeAnnotation(id);
        })

        $.RULE("IdentExpression", () => {
            const id = $.CONSUME(Tok.Identifier)
            return I.makeIdent(id)
        })

        // LITERALS --------------------------------------

        // TODO: Booleans, null...
        $.RULE("LiteralExpression", () => {
            let tup;
            $.OR([
                { ALT: () => tup = ["Number", $.CONSUME(Tok.Number).image] },
                { ALT: () => tup = ["String", $.CONSUME(Tok.String).image] }
            ])
            return I.makeLiteral(tup[0], tup[1]);
        })

        this.performSelfAnalysis()
    }
}

// Items must be in the format: [expr, op, expr, op, expr...]
function associateBinaryRight(items) {
    // TODO: Throw error if length is 0;
    if (items.length === 1) {
        return items[0];
    } else {
        let lastExpr = items[items.length - 1];
        for (let i = items.length - 2; i > 0; i -= 2) {
            lastExpr = I.makeBinaryOperation(items[i - 1], items[i], lastExpr);
        }
        return lastExpr;
    }
}

const parser = new IkigaiParser()
const lexer = new Lexer(Object.keys(Tok).map(k => Tok[k]));

export function parse(text) {
    const lexingResult = lexer.tokenize(text)
    // "input" is a setter which will reset the parser's state.
    parser.input = lexingResult.tokens
    const ast = parser.Program()
    return {
        ast,
        errors: parser.errors
    }
}
