import UntypedAst from "../types/UntypedAst"
import TypedAst from "../types/TypedAst"

export default function (ast: UntypedAst): TypedAst {
    return ast as TypedAst
}