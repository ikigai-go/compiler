import { createToken, Lexer } from "chevrotain";

function tok(name: string, value: string | RegExp | object) {
    return createToken(Object.assign({ name },
        typeof value === "string" || value instanceof RegExp
            ? { pattern: value } : value));
}

const Identifier = tok("Identifier", /[a-zA-Z_][0-9a-zA-Z_]*/);

export default {
    // note we are placing WhiteSpace first as it is very common thus it will speed up the lexer.
    WhiteSpace: tok("WhiteSpace", {
        pattern: /\s+/,
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
    Colon: tok("Colon", ':'),
    Semicolon: tok("Semicolon", ';'),
    Comma: tok("Comma", ','),
    Arrow: tok("Arrow", '=>'),
    Assignment: tok("Assignment", '='),
    LParen: tok("LParen", '('),
    RParen: tok("RParen", ')'),
    LBrace: tok("LBrace", '{'),
    RBrace: tok("RBrace", '}'),
    LAngleBracket: tok("LAngleBracket", '<'),
    RAngleBracket: tok("RAngleBracket", '>'),
    // "keywords" appear before the Identifier
    MutabilityModifier: tok("MutabilityModifier", {
        pattern: /const|mutable/,
        longer_alt: Identifier
    }),
    ExportModifier: tok("ExportModifier", {
        pattern: /export/,
        longer_alt: Identifier
    }),
    Skill: tok("Skill", 'skill'),
    Bool: tok("Bool", /true|false/),
    Null: tok("Null", /null/),
    Identifier,
    UnaryOperator: tok("UnaryOperator", /[!\-]/),
    AdditionOperator: tok("AdditionOperator", /[+\-]/),
    ProductOperator: tok("ProductOperator", /[*\/]/),
    ExponentialOperator: tok("ExponentialOperator", /\^/),
    Number: tok("Number", /-?(?:[0-9]|[1-9][0-9]+)(?:\.[0-9]+)?(?:[eE][-+]?[0-9]+)?\b/),
    String: tok("String", /"(?:[^"\\]|.)*"/)
};
