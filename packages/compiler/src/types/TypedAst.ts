import { Location } from "./Common"
import { NumberExpression, StringExpression } from "./UntypedAst"

// TYPES

export interface Skill { kind: "Skill" }
export interface Training { kind: "Training" }
export interface DiscriminatedUnion { kind: "DiscriminatedUnion" }
export interface Any { kind: "Any" }
export interface Null { kind: "Null" }
export interface Boolean { kind: "Boolean" }
export interface Number { kind: "Number" }
export interface String { kind: "String" }

export interface Argument {
    kind: "Argument",
    type: Type,
    spread: boolean,
    optional: boolean,
}

export interface Function {
    kind: "Function",
    arguments: Argument[],
    returnType: Type,
}

export interface Field {
    kind: "Field",
    key: string,
    type: Type,
}

export interface Indexer {
    kind: "Indexer",
    index: Type,
    returnType: Type,
}

export interface Invocation {
    kind: "Invocation",
    arguments: Argument[],
    returnType: Type,
    withNew: boolean
}

export interface Shape {
    kind: "Shape",
    properties: Field | Indexer | Invocation,
}

export type Primitive =
    | Null
    | Boolean
    | Number
    | String
    | Function
    | Shape

export type Type =
    | Any
    | Primitive
    | Skill
    | DiscriminatedUnion

// EXPRESSIONS

export interface Value {
    identifier: string,
    mutable: boolean,
    type: Type,
}

export interface ValueExpression {
    value: Value,
    loc: Location,
}

export type Expression =
    | NumberExpression
    | StringExpression
    | ValueExpression

export interface ValueDeclaration {
    type: "ValueDeclaration",
    loc: Location,
    value: Value,
    binding: Expression,
    body: Expression,
}

export interface ShapeDeclaration {
    type: "ShapeDeclaration",
    loc: Location,
}

export type Declaration =
    | ValueDeclaration
    | ShapeDeclaration

export interface Scope {
    parent?: Scope,
    values: Map<string, Value>;
}

export default interface TypedAst {
    scope: Scope,
    declarations: Declaration[],
}
