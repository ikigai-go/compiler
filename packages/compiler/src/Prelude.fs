namespace Ikigai.Compiler

open Fable.Import

type IPlatform =
    abstract ReadFile: path: string -> JS.Promise<string>

[<RequireQualifiedAccess>]
module List =
    let isSingle = function
        | [_] -> true
        | _ -> false

    /// Same as List.length xs > 1
    let isMultiple = function
        | [] | [_] -> false
        | _ -> true

    let rec sameLength xs1 xs2 =
        match xs1, xs2 with
        | [], [] -> true
        | [_], [_] -> true
        | _::xs1, _::xs2 -> sameLength xs1 xs2
        | _ -> false

    let splitLast (xs: 'a list) =
        let rec splitListInner acc = function
            | [] -> failwith "List is empty"
            | [x] -> List.rev acc, x
            | x::xs -> splitListInner (x::acc) xs
        splitListInner [] xs

    let replaceLast f (xs: 'a list) =
        let xs = List.toArray xs
        xs.[xs.Length - 1 ] <- f xs.[xs.Length - 1 ]
        List.ofArray xs

    let mapToArray (f: 'a -> 'b) (xs: 'a list) =
        let ar: 'b[] = List.length xs |> Array.zeroCreate
        xs |> List.iteri (fun i x -> ar.[i] <- f x)
        ar

    let mapiToArray (f: int -> 'a -> 'b) (xs: 'a list) =
        let ar: 'b[] = List.length xs |> Array.zeroCreate
        xs |> List.iteri (fun i x -> ar.[i] <- f i x)
        ar

    let choosePartition (f: 'a -> 'b option) (xs: 'a list): 'b list * 'a list =
        let mutable lucky = []
        let mutable unlucky = []
        xs |> List.iter (fun x ->
            match f x with
            | Some x -> lucky <- x::lucky
            | None -> unlucky <- x::unlucky)
        List.rev lucky, List.rev unlucky

[<RequireQualifiedAccess>]
module Naming =
    let isIdentChar index (c: char) =
        let code = int c
        c = '_'
        || c = '$'
        || (65 <= code && code <= 90)   // a-z
        || (97 <= code && code <= 122)  // A-Z
        // Digits are not allowed in first position, see #1397
        || (index > 0 && 48 <= code && code <= 57) // 0-9

    let hasIdentForbiddenChars (ident: string) =
        let mutable i = 0
        while i < ident.Length && (isIdentChar i ident.[i]) do i <- i + 1
        i < ident.Length

    let trainName skillName typeName =
        skillName + "$" + typeName
