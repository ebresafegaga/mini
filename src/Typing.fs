module Typing

open Ast

type Index = int 

/// Types 
type Type =
    | TyUnit
    | TyVar of Either<Index, Type> ref
    | TyNumber 
    | TyString 
    | TyBool
    | TyCon of Name * Type list // A type constructor e.g  TyCon ("List", TyNumber) 
    | TyFunc of Type * Type


// 
// Get the free variables in a type 
//
let rec ftv = function 
    | TyVar {contents = Left idx} -> Set.singleton idx
    | TyCon (_, types) -> 
        types 
        |> List.map ftv
        |> Set.unionMany
    | TyFunc (a, r) -> Set.union (ftv a) (ftv r)
    | _ -> Set.empty

//
// pretty print a type 
//
let rec prettyPrint = function 
    | TyUnit -> "()"
    | TyString -> "string"
    | TyNumber -> "number"
    | TyBool -> "bool"
    | TyCon (name, args) -> 
        // printing a type like this List<bool> 
        let folder a s = 
            if s = ">"
            then  prettyPrint a + s 
            else prettyPrint a + "," + s
        let t = List.foldBack folder args ">"
        name + "<" + t 
    | TyFunc (a, r) -> 
        "(" + prettyPrint a + " -> " + prettyPrint r + ")"
    | TyVar {contents=Left idx} -> sprintf "a%d" idx
    | TyVar {contents=Right ty} -> prettyPrint ty
    


//  To create a new free variable 
let freevar =
    let v = ref 0
    fun () -> 
        v := !v + 1
        TyVar $ ref (Left !v)

// 
// The unifier 
//

let occurs t1 t2 = 
    match t1, t2 with 
    | TyVar {contents=Left idx}, t -> ftv t |> Set.contains idx
    | _ -> false

let bind t1 t2 = 
    match t1, t2 with 
    | TyVar {contents=Left idx}, t when t1 = t2 -> 
        t1


let rec unify t1 t2 = 
    match t1, t2 with 
    | TyUnit, TyUnit
    | TyNumber, TyNumber 
    | TyString, TyString
    | TyBool, TyBool -> Ok () 
    | TyFunc (a1, r1), TyFunc (a2, r2) -> 
        let a = unify a1 a2
        let b = unify r1 r2 
        match a, b with 
        | Ok _, Ok _ -> Ok ()
        | Error s1, Error s2 -> Error (List.concat [s1;s2]) 
        | Error s, _ -> Error s 
        | _, Error s -> Error s 
    | TyCon (n1, a1), TyCon (n2, a2) -> 
        // Note: This can throw an exn if a1 and a2 aren't equal in length
        let r = 
            List.zip a1 a2
            |> List.map (fun (x, y) -> unify x y) 
            |> Result.sequenceA
        match n1=n2, r with 
        | true, Ok () -> Ok ()
        | false, Ok () -> Error [ sprintf "Type Contructor names do not match %s %s" n1 n2 ]
        | true, Error s -> 
            Error ["The type arguments of type contructors do not match"]
        | false, Error s -> 
            let a = [ sprintf "Type Contructor names do not match %s %s" n1 n2 ]
            let b =  ["The type arguments of type contructors do not match"]
            Error (List.concat [a;b])
    // | TyVar (Left i1), TyVar (Left i2) -> 
    //     // Var bind 
    //     failwith ""

    