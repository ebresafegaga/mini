[<AutoOpen>]
module Util

let (++) list a = list @ [ a ]

let ($) = (<|)

let const' x = fun _ -> x

module String =
    let toList (s: string) = 
        s.ToCharArray () 
            |> List.ofArray

    let foldBack f s b =
        let s = toList s
        List.foldBack f s b

// Simulating GHC's type holes in F#
// let _f = failwith ""

// let data = List.foldBack (fun x -> _f) ["e", 3; "r", 4] 13


// let f x = 
//     let f _ = 10 
//     f 10

module Map =
    let union m1 m2 =
        let folder k v s =
            match Map.tryFind k s with
            | Some _ -> s
            | None -> Map.add k v s

        Map.foldBack folder m1 m2
