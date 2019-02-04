const fs = require("fs");
const path = require("path");
const nearley = require("nearley");
const grammar = require("../build/grammar.js");

function readFile(relativePath) {
    return fs.readFileSync(path.join(__dirname, relativePath)).toString();
}

// Create a Parser object from our grammar.
const parser = new nearley.Parser(nearley.Grammar.fromCompiled(grammar));

// Parse something!
parser.feed(readFile("./test.iki"));

// parser.results is an array of possible parsings.
console.log(parser.results);

