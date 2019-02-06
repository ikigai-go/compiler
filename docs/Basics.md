# Basics

Ikigai will follow the file module structure as in modern JS.

## Imports/Exports

Should work more or less as in modern Javascript.

## Actions

I'm inclined to allow side-effects on module loading only in a single place within a block. Maybe using `do` but if we decide to force this block to be put between imports and other values, we may use `onload` so users understand the block will be run after all the other values have been assigned.

```js
import { runEffect } from "./util"

onload {
    runEffect(myValue)
}

const myValue = 5;
```

## Values

Use `const` for immutable values (unlike JS this shouldn't allow editing a field of the value).

> Discussion: We have to stress semantic differences whenever possible, so we won't allow `let`. To define mutable values we will use the `mutable` keyword or similar.

## Comments

Same as in JS, `//` and `/* .. */`


