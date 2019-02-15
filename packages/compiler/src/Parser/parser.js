import * as Fs from "fs"
import * as Nearley from "nearley"
import Grammar from "./grammar"

function readFile(path) {
    return Fs.readFileSync(path).toString();
}

export function parse(path) {
    const text = readFile(path);
    const parser = new Nearley.Parser(Nearley.Grammar.fromCompiled(Grammar));
    try {
        parser.feed(text);
        return parser.results[0];
    } catch (error) {
        return error.message;
    }
}