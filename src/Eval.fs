module Eval

open Ast

//
// enviroment is a function f, that when given a name (string) returns a (Some value) or None
//
type Env = Context of (string -> Value option)

and Value =
    | NumericValue of float
    | StringValue of string
    | ListValue of Value list
    | UnitValue
    | FuncValue of string list * Env * Expression

// id function 
let m () = 
    FuncValue (["x"], Context (fun x -> None), Variable "x")

let mapContext f (Context g) = 
    let k s = Option.map f (g s)
    Context k

let unwrapContext (Context f) = f

let empty =
    let f _ = None
    Context f

// TODO: implement builtin functions, operator precedence and custom operators
let fromF f = 
    ()
    
let builtin x = 
    Syntax.builtinKeywords 
    |> List.contains x

let addVar (var, expr) (Context ctx) =
    let f s =
        if s = var then (Some expr)
        else ctx s
    Context f

let addCtx (Context f) (Context g) = 
    let  k s =
        match f s with
        | None -> g s
        | Some _ as v -> v
    Context k

// create a context from a list of string and corresponding values
let assoc sl vs =
    (sl, vs)
    ||> List.fold2 (fun s arg value -> addVar (arg, value) s) empty

let churchable =
    function
    | FuncValue _ -> true
    | NumericValue _ | StringValue _
    | UnitValue | FuncValue _
    | ListValue _ -> false

let countParams =
    function
    | FuncValue (ps, _, _) -> List.length ps
    | NumericValue _ | StringValue _
    | UnitValue | FuncValue _
    | ListValue _ -> 0

let getFunc =
    function
    | FuncValue (arg, env, expr) -> arg, env, expr
    | NumericValue _ | StringValue _
    | UnitValue | FuncValue _
    | ListValue _ -> [], empty, Unit

let rec eval ctx expr =
    match expr with
    | Unit -> UnitValue, ctx
    | Number n -> NumericValue n, ctx
    | String s -> StringValue s, ctx
    | Variable v ->
        let (Context f) = ctx
        match f v with
        | Some x -> x, ctx
        | None -> 
            // Handle built-in operators
            failwithf "Unbound value %s" v
    // There's a BUG that doesn't allow binding to get the context of functions given to it
    | Binding (a, expr) -> 
        let value, newCtx = eval ctx expr 
        UnitValue, addVar (a, value) newCtx
    | Lambda (vars, expr) ->
        // let value, _ = eval ctx expr
        FuncValue (vars, ctx, expr), empty // ????????????
    | Function (name, vars, expr) ->
        let func = FuncValue (vars, ctx, expr)
        let newCtx = addVar (name, func) empty // ??????????????
        func, newCtx
    | List exprs ->
        let v = exprs |> List.map (eval ctx >> fst)
        ListValue v, ctx
    | Application (f, args) -> apply ctx f args
    | Binary _ -> invalidOp "Not yet implemented"

// FUNCTION Application 101:
// 1 : determine if f is a churchable
// 2 : Check how many args f takes - params 
// 3 : Check how many args given - args 
// 4 : if args > params, fail 
// 5 : if args < parms, return a curried function 
// 6 : if equal, *apply* 
// 7 : if f returns a lambda - g, add args to g's context

//  BUG: apply should return the env passed to it and not the env of the function given to it.
and apply ctx f args =
    let g = fst >> getFunc
    let func, _ = eval ctx f // do I need a ctx from this call?
    if churchable func |> not
    then failwith "This value is not a function and cannot be applied." 
    else
        let aLen = List.length args
        let pLen = countParams func
        
        if aLen > pLen // more args than required?
        then
            // Check if this "function value" returns a function
            // if it does, apply arguments one by one 
            let _, context, e = getFunc func
            let v = eval context e |> fst
            if  v |> churchable |> not
            then failwith "This value is not a function and cannot be applied."
            else
                let _, env, _ = 
                    apply ctx f [List.head args] 
                    |> fst 
                    |> getFunc
                apply (addCtx env ctx) e (List.tail args)
        else
            // TODO: Fail when args have the same name
            let toCurry = aLen < pLen
            let as', context, returnValue = getFunc func
            let values = args |> List.map (eval ctx >> fst)
            let names, rest = List.splitAt aLen as'
            let context' = assoc names values
            let r = FuncValue (rest, addCtx context context', returnValue), ctx
            if toCurry then r
            else eval context' returnValue