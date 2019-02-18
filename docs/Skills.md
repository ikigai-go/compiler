# Skills

Skills will be the cornerstone of Ikigai, a new attempt to make Haskell typeclasses mainstream. The success of the language depends on the correct design of this feature.

Ikigai encourages the separation of data and functionality. Skills are set of functions that can be trained for a specific type or shape (data container).

```js
skill Mappable<T> {
    map<A,B>(x: T<A>, f: A=>B): T<B>
    // We can have implementations
    select<A,B>(x: T<A>, f: A=>B): T<B> => this.map(x, f);
}

// skill Applicable<T>: Mappable<T> {
//     lift(x: A): T<A>;
//     apply(x: T<A>, f: T<A=>B>): T<B>
// }

shape Wrapper1: T {
    value: T
}

shape Wrapper2: T {
    value1: T
    value2: T
}

train Wrapper1 for Mappable {
    map<A,B>(x: Wrapper1<A>, f: A=>B) {
        return { value: f(x.value) }
    }
}

train Wrapper2 for Mappable {
    map<A,B>(x: Wrapper2<A>, f: A=>B) {
        return { value1: x.value2, value2: f(x.value1) }
    }
}

// train Set: Mappable {
//     map<A,B>(x: Set<A>, f: A=>B) {
//         return new Set(x.values().map(f));
//     }
// }
//
// train Map for Mappable {
//     map<K,A,B>(x: Map<K,A>, f: A=>B) {
//         return new Map(x.entries().map((kv) => [kv[0], f[kv[1]]]));
//     }
// }
```

When you need a skill in a function, place them as arguments **always in first place**:

```js
const map<T,A,B> = (s: Mappable<T>, x: T<A>, f: A=>B) => {
    return s.map(x, f);
}
```

When calling such a function, you don't need to pass the skills by yourself, the compiler will pass the appropriate training if declared (or imported) in the current scope.

```js
const test = () => {
    const object1: Wrapper1 = { value: 5 };
    const object2: Wrapper2 = { value1: "a", value2: "b" }
    return [
        map(object1, x => x + x), // Use Wrapper1 training
        map(object2, x => x + x), // Use Wrapper2 training
    ];
}
```
