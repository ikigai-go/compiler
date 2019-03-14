import { createToken, Lexer } from "chevrotain";

function tok(name: string, value: string | RegExp | object) {
    return createToken(Object.assign({ name },
        typeof value === "string" || value instanceof RegExp
            ? { pattern: value } : value));
}

const Identifier = tok("Identifier", /[a-zA-Z_][0-9a-zA-Z_]*/);

function keyword(name: string, pattern: string | RegExp) {
    return createToken({ name, pattern, longer_alt: Identifier })
}

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
    MutabilityModifier: keyword("MutabilityModifier", /const|mutable/),
    Export: keyword("Export", "export"),
    Skill: keyword("Skill", "skill"),
    Train: keyword("Train", "train"),
    New: keyword("New", "new"),
    If: keyword("If", "if"),
    Else: keyword("Else", "else"),
    Try: keyword("Try", "try"),
    Catch: keyword("Catch", "catch"),
    Finally: keyword("Finally", "finally"),
    Bool: keyword("Bool", /true|false/),
    Null: keyword("Null", "null"),
    // Identifier must appear after keywords
    Identifier,
    // Complex patterns
    UnaryOperator: tok("UnaryOperator", /[!\-]/),
    AdditionOperator: tok("AdditionOperator", /[+\-]/),
    ProductOperator: tok("ProductOperator", /[*\/]/),
    ExponentialOperator: tok("ExponentialOperator", /\^/),
    Number: tok("Number", /-?(?:[0-9]|[1-9][0-9]+)(?:\.[0-9]+)?(?:[eE][-+]?[0-9]+)?\b/),
    String: tok("String", /".*?"/) // TODO: Escaped quotes
};
