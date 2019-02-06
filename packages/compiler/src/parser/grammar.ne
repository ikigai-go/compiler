# http://www.json.org/
# http://www.asciitable.com/
@{%

const moo = require('moo')

const keyword = ["true", "false", "null", "const", "mutable"];

const lexer = moo.compile({
    '{': '{',
    '}': '}',
    '[': '[',
    ']': ']',
    ',': ',',
    ':': ':',
    ';': ';',
    '=': '=',
    identifier: {
        match: /[a-zA-Z_][0-9a-zA-Z_]+/,
        type: moo.keywords({ keyword })
    },
    space: { match: /\s+/, lineBreaks: true },
    number: /-?(?:[0-9]|[1-9][0-9]+)(?:\.[0-9]+)?(?:[eE][-+]?[0-9]+)?\b/,
    string: /"(?:[^"\\]|.)*"/, // TODO: template strings
})

%}

@lexer lexer

module -> (_ valueDeclaration):* _
    {% d => { return { declarations: d[0].map(x => x[1]) } } %}

valueDeclaration -> "const" _ identifier _ "=" _ expression ";":?
    {% d => makeValueDeclaration(false, d[2], d[6]) %}

identifier -> %identifier {% d => d[0].value %}

expression ->
      %number {% d => makeLiteralExpression("Number", parseFloat(d[0].value)) %}
    | %string {% d => makeLiteralExpression("String", JSON.parse(d[0].value)) %}
    | "true" {% d => makeLiteralExpression("Boolean", d[0].value) %}
    | "false" {% d => makeLiteralExpression("Boolean", d[0].value) %}
    | "null" {% d => makeLiteralExpression("Null", d[0].value) %}

_ -> null | %space {% d => null %}

@{%

function makeUntypedAst(declarations) {
    return { declarations }
}

function makeValueDeclaration(mutable, identifier, value) {
    return {
        type: "ValueDeclaration",
        mutable,
        identifier,
        value,
        // TODO loc
    }
}

function makeLiteralExpression(kind, value) {
    return {
        kind,
        value
        // TODO loc
    }
}

%}