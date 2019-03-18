import { createToken, createTokenInstance, IToken, Lexer } from "chevrotain";

const _ = {
    last<T>(xs: T[]) {
        return xs[xs.length - 1];
    },
    isEmpty<T>(xs: T[]) {
        return xs.length === 0;
    },
    findLastIndex<T>(xs: T[], predicate: (x: T) => boolean) {
        for (let i = xs.length - 1; i >= 0; i--) {
            if (predicate(xs[i]))
                return i;
        }
        return -1;
    }
}

function tok(name: string, value: string | RegExp | object) {
    return createToken(Object.assign({ name },
        typeof value === "string" || value instanceof RegExp
            ? { pattern: value } : value));
}

// State required for matching the indentations
let indentStack = [0]

function matchIndent(test: string, offset: number, matchedTokens?: IToken[], groups?: { [name: string]: IToken[] }): RegExpExecArray | null {
    matchedTokens = matchedTokens || [];
    let lastNL: IToken
    const noTokensMatchedYet = _.isEmpty(matchedTokens)
    const newLines = groups ? groups.newline || [] as IToken[] : [];
    const noNewLinesMatchedYet = _.isEmpty(newLines)
    const isFirstLine = noTokensMatchedYet && noNewLinesMatchedYet
    const isStartOfLine =
        // only newlines matched so far
        (noTokensMatchedYet && !noNewLinesMatchedYet) ||
        // Both newlines and other Tokens have been matched AND the last matched Token is a newline
        (!noTokensMatchedYet && !noNewLinesMatchedYet && (lastNL = _.last(newLines), lastNL.startOffset + lastNL.image.length === offset))

    // indentation can only be matched at the start of a line.
    if (isFirstLine || isStartOfLine) {
        let match: RegExpExecArray|null = null
        let currIndentLevel = 0
        const wsRegExp = / +/y
        wsRegExp.lastIndex = offset
        match = wsRegExp.exec(test)
        // possible non-empty indentation
        if (match !== null) {
            currIndentLevel = match[0].length
        }
        // "empty" indentation means indentLevel of 0.

        const prevIndentLevel = _.last(indentStack)
        // deeper indentation
        if (currIndentLevel > prevIndentLevel) {
            indentStack.push(currIndentLevel)
            return match
        }
        // shallower indentation
        else if (currIndentLevel < prevIndentLevel) {
            const matchIndentIndex = _.findLastIndex(
                indentStack,
                // TODO: Require exact indentation? See commented lines below
                stackIndentDepth => stackIndentDepth <= currIndentLevel
            )
            // if (matchIndentIndex === -1) {
            //     throw Error(`invalid outdent at offset: ${offset}`)
            // }

            while (indentStack.length - 1 > matchIndentIndex) {
                indentStack.pop()
                matchedTokens.push(createTokenInstance(Outdent, "", NaN, NaN, NaN, NaN, NaN, NaN))
            }
            return null
        } else {
            return null // same indent, this should be lexed as simple whitespace and ignored
        }
    } else {
        // indentation cannot be matched under other circumstances
        return null
    }
}

// function matchIndent(test: string, offset: number, matchedTokens?: IToken[], groups?: { [name: string]: IToken[] }) {
//     return matchIndentBase(text, offset, matchedTokens, groups);
// }

// function matchOutdent(text: string, offset: number, matchedTokens?: IToken[], groups?: { [name: string]: IToken[] }) {
//     return matchIndentBase(text, offset, matchedTokens, groups, "outdent");
// }

// CustomPatterns
const Indent = createToken({
    name: "Indent",
    pattern: matchIndent,
    // custom token patterns should explicitly specify the line_breaks option
    line_breaks: false
})
const Outdent = createToken({
    name: "Outdent",
    pattern(_test: string, _offset: number) {
        return null;
    },
    line_breaks: false
})

const Identifier = tok("Identifier", /[a-zA-Z_][0-9a-zA-Z_]*/);

function keyword(name: string, pattern: string | RegExp) {
    return createToken({ name, pattern, longer_alt: Identifier })
}

export const Tokens = {
    Newline: tok("Newline", {
        pattern: /\n|\r\n?/,
        group: "newline"
    }),
    // indentation tokens must appear before Spaces, otherwise all indentation will always be consumed as spaces.
    // Outdent must appear before Indent for handling zero spaces outdents.
    Outdent,
    Indent,
    // Skipped
    Spaces: tok("Spaces", {
        pattern: / +/,
        group: Lexer.SKIPPED
    }),
    SingleLineComment: tok("SingleLineComment", {
        pattern: /\/\/.*?\n/,
        group: Lexer.SKIPPED
    }),
    MultiLineComment: tok("MultiLineComment", {
        pattern: /\/\*[^*]*\*+([^\/*][^*]*\*+)*\//,
        group: Lexer.SKIPPED
    }),
    // Symbols
    Colon: tok("Colon", ":"),
    Semicolon: tok("Semicolon", ";"),
    Comma: tok("Comma", ","),
    Dot: tok("Dot", "."),
    Arrow: tok("Arrow", "=>"),
    Assignment: tok("Assignment", "="),
    LParen: tok("LParen", "("),
    RParen: tok("RParen", ")"),
    LBrace: tok("LBrace", "{"),
    RBrace: tok("RBrace", "}"),
    LBracket: tok("LBracket", "["),
    RBracket: tok("RBracket", "]"),
    LAngleBracket: tok("LAngleBracket", "<"),
    RAngleBracket: tok("RAngleBracket", ">"),
    // Keywords
    Let: keyword("Let", "let"),
    Mutable: keyword("Mutable", "mutable"),
    Export: keyword("Export", "export"),
    Skill: keyword("Skill", "skill"),
    Train: keyword("Train", "train"),
    New: keyword("New", "new"),
    Bool: keyword("Bool", /true|false/),
    Null: keyword("Null", /null/),
    // Must appear after keywords
    Identifier,
    // Complex patterns
    UnaryOperator: tok("UnaryOperator", /[!\-]/),
    AdditionOperator: tok("AdditionOperator", /[+\-]/),
    ProductOperator: tok("ProductOperator", /[*\/]/),
    ExponentialOperator: tok("ExponentialOperator", /\^/),
    Number: tok("Number", /-?(?:[0-9]|[1-9][0-9]+)(?:\.[0-9]+)?(?:[eE][-+]?[0-9]+)?\b/),
    String: tok("String", /".*?"/), // TODO: Escaped quotes
}

export function tokenize(lexer: Lexer, text: string) {
    // have to reset the indent stack between processing of different text inputs
    indentStack = [0]

    const lexResult = lexer.tokenize(text)

    //add remaining Outdents
    while (indentStack.length > 1) {
        lexResult.tokens.push(
            createTokenInstance(Outdent, "", NaN, NaN, NaN, NaN, NaN, NaN)
        )
        indentStack.pop()
    }

    // if (lexResult.errors.length > 0) {
    //     throw new Error("sad sad panda lexing errors detected")
    // }
    return lexResult
}
