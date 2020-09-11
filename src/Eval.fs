module Eval

open Ast

//
// Enviroment is an f.
// Where f is a function that when given a name returns a value or not.
//
type Thunk = Thunk of Expression 

type Env = Context of (string -> Value option)

and Value =
    | NumericValue of float
    | StringValue of string
    | ListValue of Value list
    | UnitValue
    | FuncValue of string list * Env * Thunk

let unThunk = function Thunk thunk -> thunk

// id function 
let m () = 
    FuncValue (["x"], Context (fun x -> None), Thunk $ Variable "x")

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
    let k s =
        match f s with
        | None -> g s
        | Some _ as v -> v
    Context k

// create a context from a list of string and corresponding values
let assoc sl vs =
    (sl, vs)
    ||> List.fold2 (fun s arg value -> addVar (arg, value) s) empty

let churchable = function
    | FuncValue _ -> true
    | NumericValue _ | StringValue _
    | UnitValue | FuncValue _
    | ListValue _ -> false

let countParams = function
    | FuncValue (ps, _, _) -> List.length ps
    | NumericValue _ | StringValue _
    | UnitValue | FuncValue _
    | ListValue _ -> 0

let getFunc = function
    | FuncValue (arg, env, expr) -> arg, env, expr
    | NumericValue _ | StringValue _
    | UnitValue | FuncValue _
    | ListValue _ -> [], empty, Thunk Unit

let isThunkFunc = function 
    | Thunk (Function _) |Thunk (Lambda _) -> true 
    | _ -> false 

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
    | Binding (a, expr) -> 
        let value, newCtx = eval ctx expr 
        UnitValue, addVar (a, value) newCtx
    | Lambda (vars, expr) ->
        // let value, _ = eval ctx expr
        FuncValue (vars, ctx, Thunk expr), ctx
    | Function (name, vars, expr) ->
        let func = FuncValue (vars, ctx, Thunk expr)
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
// 4 : if args > params
//         check if f returns a function
//         if it does, apply arguments one by one 
//         if it doesn't fail
// 5 : if args < parms, return a curried function  **
// 6 : if equal, *apply* 
// NOTE : if f returns a lambda - g, add args to g's context **
//        Finally, when args = params, make sure we eval the thunk with the "deepest" context 
//        which will hold variables of previously curried functions (if any) - This is how to APPLY. 

and apply ctx f args =
    let g = fst >> getFunc
    let func, _ = eval ctx f // do I need a ctx from this call?
    if not $ churchable func
    then failwith "This value is not a function and cannot be applied." 
    else
        let aLen = List.length args
        let pLen = countParams func
        
        if aLen > pLen // more args than required?
        then
            // printfn "here o"
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
                |> List.map (eval ctx >> fst) // Eval args 
            let names, rest = List.splitAt aLen arguments
            let context' = assoc names values
            let enviroment = 
                addCtx context' context
                |> addCtx ctx

            let value = FuncValue (rest, enviroment, thunk), ctx
            
            if curry then value
            (* 
               aLen = pLen, is a simple function application, see step 6. 
               But it must be applied with the right env. 
            *)
            else eval enviroment (unThunk thunk) 