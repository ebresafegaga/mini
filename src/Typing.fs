module Typing

open Ast

/// Types 
type Type =
    | TyUnit
    | TyVar of string
    | TyNumber 
    | TyString 
    | TyBool
    | TyList of Type 
    | TyFunc of Type * Type

/// A Finite map from type variables to types.
type Subst = Subst of Map<string, Type>
let unSubst (Subst x) = x

/// A product of type variables used in a type and the actual type.
type TypeScheme = Scheme of string list * Type
let unScheme (Scheme (x, y)) = x, y

/// Γ : A finite map from terms to their type schemes 
type TypeEnv = TypeEnv of Map<string, TypeScheme>
let unTypeEnv (TypeEnv x) = x


type Type with 
    member x.FreeTypeVariables = 
        match x with
        | TyVar a -> Set.singleton a
        | TyBool | TyNumber 
        | TyUnit | TyString -> Set.empty
        | TyFunc (a, b) -> Set.union a.FreeTypeVariables b.FreeTypeVariables
        | TyList t -> t.FreeTypeVariables

    member x.Apply (Subst s) =
        match x with 
        | TyVar a -> 
            match Map.tryFind a s with 
            | Some t -> t
            | None -> TyVar a
        | TyFunc (t1, t2) -> 
            let s = Subst s 
            TyFunc (t1.Apply s, t2.Apply s)
        | t -> t

type TypeScheme with
    member x.FreeTypeVariables = 
        match x with
        | Scheme (vars, t) -> 
            Set.difference t.FreeTypeVariables (set vars)
    
    member x.Apply (Subst s) = 
        match x with 
        | Scheme (vars, t) ->
            let s' = Subst (List.foldBack Map.remove vars s)
            Scheme (vars, t.Apply s')

let nullSubt = Subst Map.empty
let composeSubst s1 (Subst s2) =
    s2
    |> Map.map (fun _ t -> t.Apply s1)
    |> Map.union (unSubst s1)
    |> Subst

let remove (TypeEnv env) var = TypeEnv (Map.remove var env)

type TypeEnv with 
    member x.FreeTypeVariables =
        let result = 
            unTypeEnv x
            |> Map.toList
            |> List.map (fun x -> (snd x).FreeTypeVariables)
        List.foldBack Set.union result Set.empty
    
    member x.Apply s =
        (unTypeEnv x)
        |> Map.map (fun k v -> v.Apply s)
        |> TypeEnv

// Free variables in the Type t that are not free in the TypeEnv env 
let generalize (env: TypeEnv) (t: Type) = 
    let vars =
        t.FreeTypeVariables
        |> Set.difference env.FreeTypeVariables
        |> Set.toList
    Scheme (vars, t)