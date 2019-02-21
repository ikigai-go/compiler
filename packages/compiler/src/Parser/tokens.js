import { createToken, Lexer } from "chevrotain";

const WORDS = ["Identifier"];

export default processTokens({
    // note we are placing WhiteSpace first as it is very common thus it will speed up the lexer.
    WhiteSpace: {
        pattern: /\s+/,
        group: Lexer.SKIPPED
    },
    SingleLineComment: {
        pattern: /\/\/.*?\n/,
        group: Lexer.SKIPPED
    },
    Semicolon: /;/,
    Assignment: /=/, // longer_alt: AssignmentOperator
    LParen: /\(/,
    RParen: /\)/,
    // "keywords" appear before the Identifier
    MutabilityModifier: {
        pattern: /const|mutable/,
        longer_alt: "Identifier"
    },
    ExportModifier: {
        pattern: /export/,
        longer_alt: "Identifier"
    },
    Identifier: /[a-zA-Z_][0-9a-zA-Z_]*/,
    AdditionOperator: /[+\-]/,
    ProductOperator: /[*\/]/,
    ExponentialOperator: /\^/,
    UnaryOperator: /[!\-]/,
    Number: /-?(?:[0-9]|[1-9][0-9]+)(?:\.[0-9]+)?(?:[eE][-+]?[0-9]+)?\b/,
    String: /"(?:[^"\\]|.)*"/
});

function processTokens(tokenObj) {
    function tok(name, value) {
        return createToken(Object.assign({ name },
            value instanceof RegExp ? { pattern: value } : value));
    }

    // Deal with words first to replace longer_alt fields
    const wordsObj = {};
    for (const w of WORDS) {
        // TODO: Error if doesn't exist
        wordsObj[w] = tok(w, tokenObj[w]);
    }

    for (const key of Object.keys(tokenObj)) {
        if (key in wordsObj) {
            tokenObj[key] = wordsObj[key]
        } else {
            const value = tokenObj[key];
            if (value.longer_alt) { // TODO: Error if doesn't exist
                value.longer_alt = wordsObj[value.longer_alt];
            }
            tokenObj[key] = tok(key, value);
        }
    }
    return tokenObj;
}
