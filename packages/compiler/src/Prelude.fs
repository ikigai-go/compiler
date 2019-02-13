namespace Ikigai.Compiler

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


