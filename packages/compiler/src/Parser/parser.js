import { Parser } from "chevrotain"
import * as L from "./lexer"

/*
Program
   : (ValueDeclaration)*

ValueDeclaration
   : (ExportModifier)? MutabilityModifier Identifier Assignment Expression NewLine

Expression
   : BinaryOperation | Literal

Literal
   : Number | String

BinaryOperation
   : Literal BinaryOperator Literal
*/

class IkigaiParser extends Parser {
    constructor() {
        super(L.allTokens)

        const $ = this

        $.RULE("Program", () => {
            $.MANY(() => {
                $.SUBRULE($.ValueDeclaration)
            })
        })

        $.RULE("ValueDeclaration", () => {
            $.OPTION(() => {
                $.CONSUME(L.ExportModifier)
            })
            $.CONSUME(L.MutabilityModifier)
            $.CONSUME(L.Identifier)
            $.CONSUME(L.Assignment)
            $.SUBRULE($.Expression)
            $.CONSUME(L.NewLine)
        })

        $.RULE("Expression", () => {
            $.OR([
                // ATTENTION: Order is important, because BinaryOperation
                // can be prefixed by Literal
                { ALT: () => $.SUBRULE($.BinaryOperation) },
                { ALT: () => $.SUBRULE($.Literal) },
            ])
        })

        // TODO: Booleans, null...
        $.RULE("Literal", () => {
            $.OR([
                { ALT: () => $.CONSUME(L.Number) },
                { ALT: () => $.CONSUME(L.String) }
            ])
        })

        // TODO: Using Literal to avoid left recursion, enable multiple operations
        $.RULE("BinaryOperation", () => {
            $.SUBRULE($.Literal, { LABEL: "left" })
            $.CONSUME(L.BinaryOperator)
            $.SUBRULE2($.Literal, { LABEL: "right" })
        })

        this.performSelfAnalysis()
    }
}

export const parser = new IkigaiParser()

export function parse(text) {
    const lexingResult = L.lexer.tokenize(text)
    // "input" is a setter which will reset the parser's state.
    parser.input = lexingResult.tokens
    const cst = parser.Program()

    if (parser.errors.length > 0) {
        throw new Error("sad sad panda, Parsing errors detected")
    }
    return cst
}
