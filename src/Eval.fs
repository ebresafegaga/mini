module Eval

open Ast

exception NoRuleApplies // TODO: Values are Expressions (a la TAPL)
 
type Env = Env of (Name -> Value option) // Yes, the enviroment is a function

and Value =
    | ConstValue of Const 
    | ListValue of Value list
    | UnitValue
    | FuncValue of Function

and Thunk = Thunk of Expression

and Function =
    | Defined of 
        argument : Expression * 
        enviroment : Env * 
        body : Thunk
    | Builtin of (Env -> Expression list -> Value)

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

/// Is this value a function? 
let churchable = function
    | FuncValue _ -> true
    | ConstValue _
    | UnitValue | FuncValue _
    | ListValue _ -> false

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
            match v with 
            | GetBuiltin v -> v, ctx
            | _ -> failwithf "Unbound value %s" v
    | If (exp, if', else') -> 
        match fst $ eval' exp with 
        | ConstValue (ConstBool v) -> 
            if v then eval ctx if' else eval ctx else'
        | _ -> failwith "invalid if expression"
    | Binding (a, expr) | RecBinding (a, expr) ->
        let value, newCtx = eval ctx expr 
        UnitValue, addVar (a, value) newCtx
    | Lambda (a, expr) ->
        let lambda = Defined (a, ctx, Thunk expr) |> FuncValue
        lambda, ctx
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
// 2 : let args = **number of arguments given to f**
// 3 : If args > 1
//         check if f returns a function
//         if it does, apply arguments one by one 
//         if it doesn't fail
// 5: This function is being applied with one argument (as it should be), just eval the body

and apply ctx f arg =
    let eval' = fst << eval ctx 
    let func, _ = eval ctx f // do I need a ctx from this call?
    if not (churchable func)
    then failwith "This value is not a function and cannot be applied." 
    else
        match func with 
        | FuncValue (Defined (Variable argument, context, Thunk thunk)) -> 
            let value = eval' arg
            let enviroment =
                (addCtx context ctx)
                |> addVar (argument, value)
            eval enviroment thunk
        | FuncValue (Builtin f) -> 
            // f ctx arg, ctx
            failwith "not yet adapted for currying"
        | _ -> failwith "This value is not a function and cannot be applied"

and (|GetBuiltin|_|) = function 
    | "car" -> trans builtinCar
    | "cdr" -> trans builtinCdr
    | "null" -> trans builtinNull
    | "cons" -> trans builtinCons
    | _ -> None

and builtinCar env values =
    match values with 
    | [l] -> 
        match fst $ eval env l with 
        | ListValue (x :: xs) -> x 
        | ListValue [] -> failwith "Attempted to get head of empty list"
        | _ -> failwith "Invalid arguments"
    | _ -> failwith "invalid number of arguments"

and builtinCdr env values =
    match values with 
    | [l] -> 
        match fst $ eval env l with
        | ListValue (x :: xs) -> ListValue xs
        | ListValue _ -> failwith "Attempted to get the cdr of an empty list"
        | _ -> failwith "Invalid arguments"
    | _ -> failwith "invalid number of arguments"

and builtinNull env values = 
    match values with 
    | [x] -> 
        match fst $ eval env x with 
        | ListValue [] -> ConstValue (ConstBool true)
        | ListValue _ -> ConstValue (ConstBool false)
        | _ -> failwith "invalid arguments"
    | _ -> failwith "invalid arguments"

and builtinCons env values = 
    match values with
    | [x; xs] ->
        let x', xs' = fst (eval env x), fst (eval env xs)
        match xs' with 
        | ListValue xs'' -> ListValue $ x' :: xs''
        | _ -> failwithf "Expected a list but got a %A" xs
    | _ -> failwith "invalid number arguments"

and trans x = Some $ FuncValue (Builtin x)
