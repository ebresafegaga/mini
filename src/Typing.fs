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

/// TypeEnv is a finite mapping from terms to their respective types 
type TypeEnv = Map<string, Type>
let emptyTypeEnv : TypeEnv = Map.empty

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

let chng value ty = 
    match ty, value with 
    | TyVar t, value -> t := value 
    | _ -> ()

let occurs t1 t2 = 
    match t1, t2 with 
    | TyVar {contents=Left idx}, t -> ftv t |> Set.contains idx
    | _ -> false

let bind t1 t2 = 
    match t1, t2 with 
    | TyVar {contents=Left idx}, TyVar {contents=Left i} ->
        chng (Left idx) t2
        Ok ()
    | TyVar {contents=Left idx}, TyVar { contents=Right ty} -> 
        // occurs check on ty
        if occurs t1 t2 
        then 
            Error 
                [ sprintf "Occurs check failed: Cannot construct the infinite type %s ~ %s" 
                          (prettyPrint t1) 
                          (prettyPrint t2) ]
        else 
            chng (Right ty) t1
            Ok ()
    | TyVar {contents=Left _}, ty -> 
        chng (Right ty) t1
        Ok ()
    | _ -> Error ["Cannot bind to an already bound type variable or a concrete type"]

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
    | TyVar {contents=Left _}, ty -> if t1=t2 then Ok () else bind t1 ty
    | TyVar {contents=Right ty}, _ -> unify ty t2
    | _ -> Error [ sprintf "Cannot unify %s with %s" (prettyPrint t1) (prettyPrint t2) ]

let tyConst = function
    | ConstBool _ -> Ok TyBool
    | ConstNumber _ -> Ok TyNumber
    | ConstString _ -> Ok TyString

let rec infer (tenv : TypeEnv) = function 
    | Unit -> Ok TyUnit, tenv
    | Const c -> tyConst c, tenv
    | Variable name -> 
        match Map.tryFind name tenv with
        | Some ty -> Ok ty, tenv
        | None -> Error [sprintf "Unbound variable %s" name], tenv
    | List list -> 
        let result = fst (infer tenv $ List.head list)
        match result with
        | Ok ty -> 
            let res = 
                List.tail list 
                |> List.map (infer tenv >> fst)
                |> Result.sequenceA'
            match res with 
            | Ok types ->
                let a =
                    List.map (unify ty) types
                    |> Result.sequenceA
                match a with 
                | Ok () -> Ok (TyCon ("List", [ty])), tenv
                | Error s -> Error s, tenv
            | Error s -> Error s, tenv
        | Error s -> Error s, tenv