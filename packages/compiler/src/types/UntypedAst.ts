import { Location } from "./Common"

export interface NumberExpression {
    kind: "Number",
    value: number,
    loc: Location,
}

export interface StringExpression {
    kind: "String",
    value: string,
    loc: Location,
}

export interface BooleanExpression {
    kind: "Boolean",
    value: boolean,
    loc: Location,
}

export interface NullExpression {
    kind: "Null",
    loc: Location,
}

export interface ValueExpression {
    kind: "Value",
    identifier: string,
    loc: Location,
}

export type Expression =
    | NumberExpression
    | StringExpression
    | ValueExpression
    | BooleanExpression
    | NullExpression

export interface ValueDeclaration {
    type: "ValueDeclaration",
    loc: Location,
    mutable: boolean,
    identifier: string,
    value: Expression,
}

export interface ShapeDeclaration {
    type: "ShapeDeclaration",
    loc: Location,
}

export type Declaration =
    | ValueDeclaration
    | ShapeDeclaration

export default interface UntypedAst {
    declarations: Declaration[],
}
