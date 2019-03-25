[<RequireQualifiedAccess>]
module Ikigai.Compiler.Error

let private cannotFind kind name =
    sprintf "Cannot find %s with name %s in scope" kind name

let cannotFindValue name = cannotFind "value" name
let cannotFindType name = cannotFind "type" name
let cannotFindRef name = cannotFind "reference" name

let cannotFindEnumCase caseName enumName =
    sprintf "Cannot find case %s in enum %s" caseName enumName

let unexpectedReturningBlock =
    // "The block is not expected to return a value"
    "Early returns are not allowed"

let unexpectedVoidBlock =
    "The block is expected to return a value"