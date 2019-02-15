import * as Nearley from "nearley"
import Grammar from "./grammar"

export function parse(text) {
    const parser = new Nearley.Parser(Nearley.Grammar.fromCompiled(Grammar));
    try {
        parser.feed(text);
        return parser.results[0];
    } catch (error) {
        return error.message;
    }
}