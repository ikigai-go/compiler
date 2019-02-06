# Functions

Functions in Ikigai will be very similar to TypeScript. Optional and spread arguments are allowed. There will be **no implicit currying**. For the MVP at least we will restrict the syntaxt to lambda-style:

```
const add = (x: number, y: number): number => x + y;
const cubic = (x: number): number => {
    const temp = x * x;
    return temp * x;
}
```

Notes:

- What about labeled arguments? Do we allow them or make them . I'd rather not use a symbol like `~` in OCaml/ReasonML.

- We will likely try to infer the return type for simple functions.

- How should generics be typed? If we use an apostrophe (Ocaml, F#) we probably can omit the generic parameters after the function name, without the apostrophe we will need them: `const apply<T1,T2> = (x: T1, f: T1=>T2): T2 => f(x)`

- Destructuring will likely not be available for MVP.

- Document comments: we should do it after MVP but maybe not using JsDoc format.