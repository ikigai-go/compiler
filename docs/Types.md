# Types

The "general" types in Ikigai will be:

- Any
- Void
- Null
- Boolean
- String
- Number
- Object
- Enum

Objects can have **Shapes**. These will be a form of structural typing and equivalent to Typescript interfaces.

Notes for discussion:

- I want to avoid classes and inheritance. Let's see if we can have good Typescript interaction without them.

- In the MVP we will work mainly with object literals `{ foo: 5, bar: "abc" }` but later we will probably add some kind of constructors. This will enable concrete types for Objects as for Enums.

```
constructor Foo(foo: number, bar: string) {
    this.foo = foo + 5;
    this.bar = bar;
}

constructor Foo(foo: number, bar: string)
// Auto-body. compiles as `function Foo(foo, bar) { this.foo = foo; this.bar = bar }

// Will we force `new` when calling these?
```

- Should we make Void same as `undefined` for JS interaction?

- I've used "enums" (as in rust or kotlin) because we will soon need to add union types à-la Typescript to consume d.ts declarations. Other alternatives: sum types, variant (reason, ocaml).

- Instead of promoting option types, we'll likely align with Typescript and use `xxx | null` to indicate the abscence of value.