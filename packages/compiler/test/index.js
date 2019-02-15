const fs = require("fs");
const path = require("path");
const nearley = require("nearley");
const grammar = require("../build/Parser/grammar.js");

function readFile(relativePath) {
    return fs.readFileSync(path.join(__dirname, relativePath)).toString();
}

function pipe(value, ...operations) {
    for (const op of operations) {
        value = op(value);
    }
    return value;
}

function parse(text) {
    const parser = new nearley.Parser(nearley.Grammar.fromCompiled(grammar));
    try {
        parser.feed(text);
        return parser.results[0];
    } catch (error) {
        return error.message;
    }
}

pipe("./test.iki",
    readFile,
    parse,
    console.log);
