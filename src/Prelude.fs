[<AutoOpen>]
module Prelude

let rec (|Snoc|_|) = function
    | [x] -> Some ([], x)
    | x :: Snoc (xs, last) -> Some (x::xs, last)
    | _ -> None

let rec (|Consf|_|) f = function
    | a :: Consf f (b, xs) when f a -> Some (a, b :: xs)
    | a :: Consf f (b, xs) -> Some (b, xs)
    | a :: _ when f a -> Some (a, []) 
    | a :: _ -> None
    | [] -> None

// let (|ConsSnoc|_|) = function 
//     | first :: Snoc (middle, last) -> Some (first, middle, last)
//     | _ -> None

let rec (|ConsSnoc|Only|Empty|) = function
    | [] -> Empty
    | only :: Empty -> Only only
    | first :: Only last -> ConsSnoc (first, [], last) 
    | first :: ConsSnoc (y, ys, last) -> ConsSnoc (first, y :: ys, last)

let rec (|Half|) xs =
    let rec s xs ys =
        match xs, ys with
        | _ :: _ :: xs, y :: ys ->
            let left, right = s xs ys
            y :: left, right
        | _, xs -> ([], xs)
    Half (s xs xs)

type Either<'a, 'b> = Left of 'a | Right of 'b

let (++) list a = list @ [a]

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
    // 
    //  Note that this two instances of seqA can be generalized by a (<*>)
    //  given that what the error contains is a Monoid

    let (<*>) rf ra = 
        match rf, ra with
        | Ok f, Ok a -> Ok (f a)
        | _, Error s | Error s, _ -> Error s
 
    let sequenceA list =
        let folder a s =
            match a, s with
            | Ok (), Ok () -> Ok ()
            | Error s1, Error s2 -> Error (List.concat [s1;s2])
            | Error s, _ | _, Error s -> Error s
        List.foldBack folder list (Ok ())

    // This is the sequenceA you *really* want
    // I'm sorry for the poor naming 
    // TODO: rename and refactor references
    let sequenceA' list =
        let folder a s =
            match a, s with
            | Ok x, Ok xs -> Ok (x :: xs)
            | Error s1, Error s2 -> Error (List.concat [s1;s2])
            | Error s, _ | _, Error s -> Error s
        List.foldBack folder list (Ok [])

module List = 
    let rec zipWith f xs ys = 
        match xs, ys with
        | x :: xs, y :: ys ->  f x y :: zipWith f xs ys 
        | _ -> []