# Flow control

In the MVP there will be only a few constructs for flow control, things like collection iteration, etc will be done with utility functions for now.

## Conditional ternary operator

Same as in JS: `const foo = aCondition ? 5 : 3`

## Pipeline operator

[As proposed in JS](https://github.com/tc39/proposal-pipeline-operator).

> Discussion: We'll probably combine this later with some placeholder strategy, either as proposed in JS `[5] |> #.filter(...)` or using a keyword like `it` in Kotlin.

## While statement

Same as in JS. However, if we finally have `return` we may want to forbid it in `while` bloc.

## Switch expression

Syntax will probably be similar to ReasonML and behaviour very similar to F#, OCaml, etc.

```
const foo =
    switch (aValue) {
        5 => "something"
        _ => "something else"
    }
```

Probably semicolon will be optional at the end of each case, but not commas. When using Void expressions it behaves the same as a switch statement.

> Discussion: I'm not familiar with the algorithms to deal with pattern matching so this will be very simple at the beginning (no guards, no nested pattern matching). After MVP we should enable custom patterns as with F# active patterns.
