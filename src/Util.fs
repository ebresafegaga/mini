[<AutoOpen>]
module Util

type Either<'a, 'b> = Left of 'a | Right of 'b

let (++) list a = list @ [ a ]

let ($) = (<|)

let const' x = fun _ -> x

let uncurry f a b = f (a, b)

module String =
    let toList (s: string) = 
        s.ToCharArray () 
            |> List.ofArray

    let foldBack f s b =
        let s = toList s
        List.foldBack f s b

// Simulating GHC's type holes in F# with good ol' ML Value restriction 
// And (VS Code or Vim) Ionide Extention for seeing the type signatures right in the editor
// let _f = failwith ""

// let data = List.foldBack (fun x -> _f) ["e", 3; "r", 4] 13


// let f x = 
//     let f _ = 10 
//     f 10


let rec Y f = f (fun x -> (Y f) x)
 
// let Y' =
//     fun le -> 
//         (fun f -> f f) (fun f -> le (fun x -> (f f) x))

module Map =
    let union m1 m2 =
        let folder k v s =
            match Map.tryFind k s with
            | Some _ -> s
            | None -> Map.add k v s

        Map.foldBack folder m1 m2

type ResultBuilder () = 
    member x.Return a = Ok a
    member x.Bind (r, f) = 
        match r with 
        | Ok a -> f a
        | Error s -> Error s
    member x.ReturnFrom a = a

let result = ResultBuilder ()

module Result =
    let sequenceA list =
        let folder a s =
            match a, s with
            | Ok (), Ok () -> Ok ()
            | Error s1, Error s2 -> Error (List.concat [s1;s2])
            | Error s, _ | _, Error s -> Error s
        List.foldBack folder list (Ok ())
    
    let sequenceA' list =
        let folder a s =
            match a, s with
            | Ok x, Ok xs -> Ok (x :: xs)
            | Error s1, Error s2 -> Error (List.concat [s1;s2])
            | Error s, _ | _, Error s -> Error s
        List.foldBack folder list (Ok [])
