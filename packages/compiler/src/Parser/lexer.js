import { createToken, Lexer } from "chevrotain";

export const WhiteSpace = createToken({
    name: "WhiteSpace",
    pattern: /\s+/,
    group: Lexer.SKIPPED
})

export const NewLine = createToken({
    name: "NewLine",
    pattern: /\n/,
    longer_alt: WhiteSpace
})

export const SingleLineComment = createToken({
    name: "SingleLineComment",
    pattern: /\/\/.*?\n/,
    group: Lexer.SKIPPED
})

// export const AssignmentOperator = createToken({
//     name: "AssignmentOperator",
//     pattern: /=/,
// })

export const Assignment = createToken({
    name: "Assignment",
    pattern: /=/,
    // longer_alt: AssignmentOperator
})

export const Identifier = createToken({
    name: "Identifier",
    pattern: /[a-zA-Z_][0-9a-zA-Z_]*/
})

export const MutabilityModifier = createToken({
    name: "MutabilityModifier",
    pattern: /const|mutable/,
    longer_alt: Identifier
})

export const ExportModifier = createToken({
    name: "ExportModifier",
    pattern: /export/,
    longer_alt: Identifier
})

// Other keywords

export const BinaryOperator = createToken({
    name: "BinaryOperator",
    pattern: /[+\-*\/]/
})

export const Number = createToken({
    name: "Number",
    pattern: /-?(?:[0-9]|[1-9][0-9]+)(?:\.[0-9]+)?(?:[eE][-+]?[0-9]+)?\b/,
})

export const String = createToken({
    name: "String",
    pattern: /"(?:[^"\\]|.)*"/, // TODO: template strings
})

// note we are placing WhiteSpace first as it is very common thus it will speed up the lexer.
export const allTokens = [
    NewLine,
    WhiteSpace,
    // "keywords" appear before the Identifier
    ExportModifier,
    MutabilityModifier,
    // The Identifier must appear after the keywords because all keywords are valid identifiers.
    Identifier,
    Number,
    String,
    Assignment,
    BinaryOperator,
    SingleLineComment
]

export const lexer = new Lexer(allTokens)
