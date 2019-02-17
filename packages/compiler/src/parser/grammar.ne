# http://www.json.org/
# http://www.asciitable.com/
@{%

const moo = require('moo')
const interop = require('./Interop')

const keyword = ["true", "false", "null", "const", "mutable", "skill", "train", "for"];

const lexer = moo.compile({
    '{': '{',
    '}': '}',
    '[': '[',
    ']': ']',
    '<': '<',
    '>': '>',
    ',': ',',
    ':': ':',
    ';': ';',
    '=': '=',
    identifier: {
        match: /[a-zA-Z_][0-9a-zA-Z_]+/,
        type: moo.keywords({ keyword })
    },
    endLine: /[\t ]*(;|\r?\n)/,
    space: { match: /\s+/, lineBreaks: true },
    number: /-?(?:[0-9]|[1-9][0-9]+)(?:\.[0-9]+)?(?:[eE][-+]?[0-9]+)?\b/,
    string: /"(?:[^"\\]|.)*"/, // TODO: template strings
    binaryOperator: /[+\-*\/]/,
})

%}

@lexer lexer

module -> (_ declaration):* _
    {% d => interop.makeUntypedAst(d[0].map(x => x[1])) %}

declaration ->
      typeDeclaration {% id %}
    | valueDeclaration {% id %}

typeDeclaration ->
      skillDeclaration {% id %}
    | trainDeclaration {% id %}

skillDeclaration -> "skill" __ identifier _ "{" _ "}"

valueDeclaration -> "const" __ identifier _ "=" _ expression %endLine
    {% d => interop.makeValueDeclaration(false, d[2], d[6]) %}

identifier -> %identifier {% d => d[0].value %}

expression ->
      literal {% id %}
    | binaryOperation {% id %}

literal ->
      %number {% d => interop.makeLiteral("Number", parseFloat(d[0].value)) %}
    | %string {% d => interop.makeLiteral("String", JSON.parse(d[0].value)) %}
    | "true" {% d => interop.makeLiteral("Boolean", d[0].value) %}
    | "false" {% d => interop.makeLiteral("Boolean", d[0].value) %}
    | "null" {% d => interop.makeLiteral("Null", d[0].value) %}

binaryOperation -> expression _ %binaryOperator _ expression
     {% d => interop.makeBinaryOperation(d[2].value, d[0], d[4]) %}

_ -> null | %space {% d => null %}
__ ->  %space {% d => null %}