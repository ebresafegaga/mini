module Eval

open Ast

//
// Enviroment is an f.
// Where f is a function that when given a name returns a value or not.
//
type Thunk = Thunk of Expression

type Function =
    | Defined of string list * Env * Thunk
    | Builtin of (Expression list -> Value)

and Env = Env of (string -> Value option)

and Value =
    | ConstValue of Const 
    | ListValue of Value list
    | UnitValue
    | FuncValue of string * Env * Thunk

let unThunk (Thunk thunk) = thunk

let mapContext f (Env g) = 
    let k s = Option.map f (g s)
    Env k

let unEnv (Env f) = f

let empty = Env (const' None)

let addVar (var, expr) (Env ctx) =
    let f s =
        if s = var then (Some expr)
        else ctx s
    Env f

let addCtx (Env f) (Env g) = 
    let k s =
        match f s with
        | None -> g s
        | Some _ as v -> v
    Env k

// create a context from a list of string and corresponding values
let assoc sl vs =
    (sl, vs)
    ||> List.fold2 (fun s arg value -> addVar (arg, value) s) empty

/// Is this value a function? 
let churchable = function
    | FuncValue _ -> true
    | ConstValue _
    | UnitValue | FuncValue _
    | ListValue _ -> false

// let countParams = function
//     | FuncValue (ps, _, _) -> List.length ps
//     | ConstValue _
//     | UnitValue | FuncValue _
//     | ListValue _ -> 0

let getFunc = function
    | FuncValue (arg, env, expr) -> arg, env, expr
    | ConstValue _
    | UnitValue | FuncValue _
    | ListValue _ -> "", empty, Thunk Unit

let isThunkFunc = function
    | Thunk (Lambda _) | Thunk (Lambda _) -> true 
    | _ -> false

let rec eval ctx expr =
    let eval' = eval ctx
    match expr with
    | Unit -> UnitValue, ctx
    | Const x -> ConstValue x, ctx
    | Variable v ->
        let (Env f) = ctx
        match f v with
        | Some x -> x, ctx
        | None ->
            // TODO: Handle built-in operators
            failwithf "Unbound value %s" v
    | If (exp, if', else') -> 
        match fst $ eval' exp with 
        | ConstValue (ConstBool v) -> 
            if v then eval ctx if' else eval ctx else'
        | _ -> failwith "invalid if expression"
    | Binding (a, expr) ->
        let value, newCtx = eval ctx expr 
        UnitValue, addVar (a, value) newCtx
    | Lambda (a, expr) ->
        FuncValue (a, ctx, Thunk expr), ctx
    | List exprs ->
        let v = 
            exprs
            |> List.map (eval' >> fst)
        ListValue v, ctx
    | Application (f, args) -> apply ctx f args
    | Binary (l, op, r) -> 
        // This is just a hack to get this working, for now!
        let l', r' = fst (eval' l), fst (eval' r)
        match l', r' with 
        | ConstValue (ConstNumber l), ConstValue (ConstNumber r) -> 
            match op with 
            | "<" -> ConstValue (ConstBool (l < r)), ctx
            | ">" -> ConstValue (ConstBool (l > r)), ctx
            | _ -> failwith "not yet implemented"
        | _ -> failwith "not yet implemented"

// FUNCTION Application 101: 
// Given an expression f
// 1 : Determine if f is a churchable - Yes that's a real thing!
// 2 : let args = < number of arguments given to f >
// 3 : If args > 1
//         check if f returns a function
//         if it does, apply arguments one by one 
//         if it doesn't fail
// 5: This function is being applied with one argument (as it should be), just eval the body

and apply ctx f args =
    let eval' = eval ctx 
    let func, _ = eval ctx f // do I need a ctx from this call?
    if not $ churchable func
    then failwith "This value is not a function and cannot be applied." 
    else
        let aLen = List.length args   
        if aLen > 1 // More args than required? - Yes, functions can have only 1 arguments. (Thank you, Alonzo Church!)
        then
            // Check if this "function value" returns a function
            // if it does, apply arguments one by one 
            let _, _, Thunk thunk = getFunc func
            if  not $ isThunkFunc (Thunk thunk)
            then failwith "This value is not a function and cannot be applied."
            else
                let _, env, _ = 
                    apply ctx f [List.head args] 
                    |> fst 
                    |> getFunc
                apply (addCtx env ctx) thunk (List.tail args)
        else
            let argument, context, thunk = getFunc func
            let value = (eval' >> fst) $ List.head args

            let enviroment =
                (addCtx context ctx)
                |> addVar (argument, value)          

            eval enviroment (unThunk thunk)

let id' =
    Builtin $ function [x] -> fst $ eval empty x | _ -> failwith ""

let map' = 
    Builtin $ function
              | [f; List l] ->
                l 
                |> List.map (List.singleton >> apply empty f >> fst)
                |> ListValue
              | _ -> failwith ""