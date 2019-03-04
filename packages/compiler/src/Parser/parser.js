import { Lexer, Parser } from "chevrotain"

/*
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
            const id =  $.CONSUME(Tok.Identifier)
                        $.CONSUME(Tok.Assignment)
            const exp = $.SUBRULE($.Expression)
                        $.CONSUME(Tok.Semicolon)
            return I.makeValueDeclaration(mut, id.image, exp)
        })

// EXPRESSION GROUPS ----------------------------------

        $.RULE("Expression", () => {
            let expr =
                $.OR([
                    // ATTENTION: Order is important, because BinaryExpression
                    // can be prefixed by Identifier, Literal...
                    { ALT: () => $.SUBRULE($.BinaryExpression) },
                ])
            return expr;
        })

        $.RULE("IdentExpression", () => {
            const id = $.CONSUME(Tok.Identifier)
            return I.makeIdent(id)
        })

        $.RULE("BinaryAtomicExpression", () => {
            let expr;
            $.OR([
                // { ALT: () => expr = $.SUBRULE($.UnaryExpression) },
                { ALT: () => expr = $.SUBRULE($.LiteralExpression) },
                { ALT: () => expr = $.SUBRULE($.IdentExpression) },
            ])
            return expr;
        })

        // $.RULE("UnaryAtomicExpression", () => {
        //     let expr;
        //     $.OR([
        //         { ALT: () => expr = $.SUBRULE($.LiteralExpression) },
        //         { ALT: () => expr = $.SUBRULE($.IdentExpression) },
        //     ])
        //     return expr;
        // })

// OPERATIONS --------------------------------------

      // Addition has lowest precedence thus it is first in the rule chain
      // The precedence of binary expressions is determined by how far down int the Parse Tree appear
        $.RULE("BinaryExpression", () => {
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
            items.push($.SUBRULE($.BinaryAtomicExpression))
            $.MANY(() => {
                items.push($.CONSUME(Tok.ExponentialOperator).image)
                items.push($.SUBRULE2($.BinaryAtomicExpression))
            })
            return associateBinaryRight(items)
        })

        // $.RULE("UnaryExpression", () => {
        //     $.CONSUME(Tok.UnaryOperator)
        //     $.SUBRULE($.UnaryAtomicExpression)
        // })

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
            lastExpr = I.makeBinaryOperation(items[i-1], items[i], lastExpr);
        }
        return lastExpr;
    }
}
*/

export function makeLexer(tokens) {
    return new Lexer(tokens);
}

export function makeParser(tokens, makeRules) {
    const ParserClass = class extends Parser {
        constructor() {
            // we have to explicitly disable the CST building for embedded actions to work.
            super(tokens, { outputCst: false })
            const $ = this

            let rules = makeRules($);
            for (let ruleName of Object.keys(rules)) {
                $.RULE(ruleName, rules[ruleName]);
            }

            this.performSelfAnalysis()
        }
    }

    return new ParserClass();
}

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
