[<RequireQualifiedAccess>]
module Ikigai.Compiler.Error

let cannotFindIdent name =
    sprintf "Cannot find ident with name %s in scope" name