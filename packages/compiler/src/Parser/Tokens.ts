import { createToken, createTokenInstance, IToken, Lexer, TokenType } from "chevrotain";

const SPACES_REGEX = / +/y;
const NEWLINE_PATTERN = "\\n|\\r\\n?";
const START_COMMENT_PATTERN = "\\/[\\/*]";
const NEWLINE_OR_COMMENT_REGEX = new RegExp(`(?:${NEWLINE_PATTERN})|(?:${START_COMMENT_PATTERN})`, "y")

const _ = {
    last<T>(xs: T[]) {
        return xs[xs.length - 1];
    },
    isEmpty<T>(xs: T[]) {
        return xs.length === 0;
    },
    mapNullOrDefault<TIn, TOut>(x: TIn|null|undefined, f: (x:TIn)=>TOut, def: TOut): TOut {
        return x == null ? def : f(x);
    },
    pipe(x: any, ...fs: Function[]) {
        let result = x;
        for (const f of fs) {
            result = f(result);
        }
        return result;
    }
}

function execSticky(regex: RegExp, index: number, test: string) {
    regex.lastIndex = index;
    return regex.exec(test);
}

function tok(name: string, value: string | RegExp | object) {
    return createToken(Object.assign({ name },
        typeof value === "string" || value instanceof RegExp
            ? { pattern: value } : value));
}

class Stack<T> {
    private stack: T[];
    private baseValues: T[];
    constructor(...baseValues: T[]) {
        this.stack = this.baseValues = baseValues;
    }
    public reset() {
        this.stack = this.baseValues;
    }
    public push(x: T) {
        this.stack.push(x);
    }
    public pop() {
        return this.stack.pop();
    }
    public get length() {
        return this.stack.length;
    }
    public last() {
        return this.stack[this.stack.length - 1];
    }
    public findLastIndex(predicate: (x: T) => boolean) {
        for (let i = this.stack.length - 1; i >= 0; i--) {
            if (predicate(this.stack[i]))
                return i;
        }
        return -1;
    }
}

// State required for matching the indentations
let INDENT_STACK = new Stack<number>(0);

function insertEmptyToken(matchedTokens: IToken[], newLines: IToken[], offset: number, token: TokenType ) {
    const lastNewLine = _.last(newLines);
    const line = lastNewLine ? (lastNewLine.endLine || 0) + 1 : 1;
    matchedTokens.push(createTokenInstance(token, "", offset, offset, line, line, 1, 1))
}

function popIndent(matchedTokens: IToken[], newLines: IToken[], offset: number, matchIndentIndex = 0) {
    while (INDENT_STACK.length - 1 > matchIndentIndex) {
        INDENT_STACK.pop()
        insertEmptyToken(matchedTokens, newLines, offset, PopIndent);
    }
}

function matchIndent(test: string, offset: number, matchedTokens?: IToken[], groups?: { [name: string]: IToken[] }): RegExpExecArray | null {
    matchedTokens = matchedTokens || [];
    const noTokensMatchedYet = _.isEmpty(matchedTokens);
    const newLines = groups ? groups.newline || [] as IToken[] : [];
    const noNewLinesMatchedYet = _.isEmpty(newLines);
    const isFirstLine = noTokensMatchedYet && noNewLinesMatchedYet;

    const isStartOfLine =
        // only newlines matched so far
        (noTokensMatchedYet && !noNewLinesMatchedYet) ||
        // Both newlines and other Tokens have been matched AND the last matched Token is a newline
        (!noTokensMatchedYet && !noNewLinesMatchedYet
            && (_.pipe(_.last(newLines),
                       lastNL => lastNL.startOffset + lastNL.image.length === offset)))

    // indentation can only be matched at the start of a line.
    if (!(isFirstLine || isStartOfLine)) {
        return null;
    } else {
        let currIndentLevel = 0;
        const match = execSticky(SPACES_REGEX, offset, test);
        if (match !== null) { // "empty" indentation means indentLevel of 0.
            currIndentLevel = match[0].length;
        }

        if (execSticky(NEWLINE_OR_COMMENT_REGEX, offset + currIndentLevel, test)) {
            return null; // Discard empty lines
        }

        const prevIndentLevel = INDENT_STACK.last();
        if (currIndentLevel > prevIndentLevel) {
            INDENT_STACK.push(currIndentLevel)
            return match
        } else {
            const matchIndentIndex = INDENT_STACK.findLastIndex(stackIndentDepth => stackIndentDepth === currIndentLevel);
            // TODO: Add lexer error
            if (matchIndentIndex === -1) {
                throw Error(`invalid outdent at offset: ${offset}`)
            }

            popIndent(matchedTokens, newLines, offset, matchIndentIndex);
        }
        return null
    }
}

// CustomPatterns
const PushIndent = createToken({
    name: "PushIndent",
    pattern: matchIndent,
    // custom token patterns should explicitly specify the line_breaks option
    line_breaks: false
})
const PopIndent = createToken({
    name: "PopIndent",
    // We will insert the PopIndent tokens manually in matchIndent
    pattern(_test: string, _offset: number) { return null },
    line_breaks: false
})

const Identifier = tok("Identifier", /[a-zA-Z_][0-9a-zA-Z_]*/);

function keyword(name: string, pattern: string | RegExp) {
    return createToken({ name, pattern, longer_alt: Identifier })
}

export const Tokens = {
    Newline: tok("Newline", {
        pattern: new RegExp(NEWLINE_PATTERN),
        group: "newline"
    }),
    // indentation tokens must appear before Spaces, otherwise all indentation will always be consumed as spaces.
    PushIndent,
    PopIndent,
    // Skipped
    Spaces: tok("Spaces", {
        pattern: new RegExp(SPACES_REGEX.source),
        group: Lexer.SKIPPED
    }),
    SingleLineComment: tok("SingleLineComment", {
        pattern: new RegExp(`\\/\\/.*?(?=${NEWLINE_PATTERN})`),
        group: Lexer.SKIPPED
    }),
    MultiLineComment: tok("MultiLineComment", {
        pattern: /\/\*[^*]*\*+([^\/*][^*]*\*+)*\//,
        group: Lexer.SKIPPED
    }),
    // Symbols that can open new indentation
    // TODO: Use chevrotain lexer modes to forbid new lines
    // if there's no indentation immediately after these symbols?
    // Or just make a formatter available for that? Example
    // let foo x y = x
    // + y
    Arrow: tok("Arrow", "=>"),
    Assignment: tok("Assignment", "="),
    LParen: tok("LParen", "("),
    LBrace: tok("LBrace", "{"),
    LBracket: tok("LBracket", "["),
    // Other symbols
    RParen: tok("RParen", ")"),
    RBrace: tok("RBrace", "}"),
    RBracket: tok("RBracket", "]"),
    LAngleBracket: tok("LAngleBracket", "<"),
    RAngleBracket: tok("RAngleBracket", ">"),
    Pipe: tok("Pipe", "|"),
    Colon: tok("Colon", ":"),
    Semicolon: tok("Semicolon", ";"),
    Comma: tok("Comma", ","),
    Dot: tok("Dot", "."),
    // Keywords
    Enum: keyword("Enum", "enum"),
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
    INDENT_STACK.reset();
    const lexResult = lexer.tokenize(text)
    //add remaining Outdents
    popIndent(lexResult.tokens, lexResult.groups.newline, text.length);
    return lexResult
}
