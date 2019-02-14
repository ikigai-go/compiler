[<RequireQualifiedAccess>]
module Ikigai.Compiler.Error

let cannotFindIdent name =
    sprintf "Cannot find ident with name %s in scope" name

let unexpectedReturningBlock =
    // "The block is not expected to return a value"
    "Early returns are not allowed"

let unexpectedVoidBlock =
    "The block is expected to return a value"