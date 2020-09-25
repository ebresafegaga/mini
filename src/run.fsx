#load "Util.fs"
#load "Ast.fs"
#load "Syntax.fs"
#load "Eval.fs"
#load "Typing.fs"

// Typing 
let tenv = ref Typing.emptyTypeEnv
let infer' expr =
    let result, tenv' = Typing.infer !tenv expr 
    tenv := tenv'
    result 
    |> Result.map Typing.prettyPrint

// Evaluations 
let env = ref Eval.empty 
let eval expr = 
    let v, e = Eval.eval !env expr
    env := e
    v

let tok = Syntax.tokenize >> Syntax.filter 
let ps = tok >> Syntax.parse
