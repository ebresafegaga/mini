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
    | FuncValue of string list * Env * Thunk

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

let countParams = function
    | FuncValue (ps, _, _) -> List.length ps
    | ConstValue _
    | UnitValue | FuncValue _
    | ListValue _ -> 0

let getFunc = function
    | FuncValue (arg, env, expr) -> arg, env, expr
    | ConstValue _
    | UnitValue | FuncValue _
    | ListValue _ -> [], empty, Thunk Unit

let isThunkFunc = function
    | Thunk (Function _) | Thunk (Lambda _) -> true 
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
    | Lambda (vars, expr) ->
        // let value, _ = eval ctx expr
        FuncValue (vars, ctx, Thunk expr), ctx
    | Function (name, vars, expr) ->
        // Remove the current name from this function
        let ctx = Env $ fun x -> if x = name then None else unEnv ctx x
        let func = FuncValue (vars, ctx, Thunk expr)
        let ctx' = addVar (name, func) ctx 
        func, ctx'
    | RecFunction (name, vars, expr) ->
        let func = FuncValue (vars, ctx, Thunk expr)
        let ctx' = addVar (name, func) ctx 
        func, ctx'
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
// 1 : Determine if f is a churchable (if it evaluates to a function)
// 2 : Check how many args f takes - params 
// 3 : Check how many args given - args 
// 4 : If args > params
//         check if f returns a function
//         if it does, apply arguments one by one 
//         if it doesn't fail
// 5 : If args < parms, return a curried function  **
// 6 : If equal, *apply* 
// NOTE : if f returns a lambda - g, add args to g's context **
//        Finally, when args = params, make sure we eval the thunk with the "deepest" context 
//        which will hold variables of previously curried functions (if any) - This is how to APPLY. 

and apply ctx f args =
    let eval' = eval ctx 
    let func, _ = eval ctx f // do I need a ctx from this call?
    if not $ churchable func
    then failwith "This value is not a function and cannot be applied." 
    else
        let aLen = List.length args
        let pLen = countParams func
        
        if aLen > pLen // more args than required?
        then
            // Check if this "function value" returns a function
            // if it does, apply arguments one by one 
            let _, _, Thunk thunk = getFunc func
            // let v = eval context e |> fst  // v |> churchable |> not // Noooooooooooo
            if  not $ isThunkFunc (Thunk thunk)
            then failwith "This value is not a function and cannot be applied."
            else
                let _, env, _ = 
                    apply ctx f [List.head args] 
                    |> fst 
                    |> getFunc
                apply (addCtx env ctx) thunk (List.tail args)
        else
            // Curry this application.
            // TODO: Fail when args have the same name
            let curry = aLen < pLen
            let arguments, context, thunk = getFunc func
            let values = 
                args
                |> List.map (eval' >> fst) // Eval all arguments eagerly
            let names, rest = List.splitAt aLen arguments
            let context' = assoc names values
            let enviroment =
                // 
                (addCtx context ctx)
                |> addCtx context'             

            assert (List.isEmpty >> not $ rest)
            let value = FuncValue (rest, enviroment, thunk), ctx
            
            if curry then value
            (* 
               aLen = pLen, is a simple function application, see step 6. 
               But it must be applied with the right env. 
            *)
            else eval enviroment (unThunk thunk)

let id' =
    Builtin $ function [x] -> fst $ eval empty x | _ -> failwith ""

let map' = 
    Builtin $ function
              | [f; List l] ->
                l 
                |> List.map (List.singleton >> apply empty f >> fst)
                |> ListValue
              | _ -> failwith ""