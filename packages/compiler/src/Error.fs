[<RequireQualifiedAccess>]
module Ikigai.Compiler.Error

let private cannotFind kind name =
    sprintf "Cannot find %s with name %s in scope" kind name

let cannotFindValue name = cannotFind "value" name
let cannotFindType name = cannotFind "type" name
let cannotFindRef name = cannotFind "reference" name

let earlyReturn = "Early returns are not allowed"
let unexpectedReturningBlock = "The block is not expected to return a value"
let unexpectedVoidBlock = "The block is expected to return a value"