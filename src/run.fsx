#load "Util.fs"
#load "Ast.fs"
#load "Syntax.fs"
#load "Eval.fs"

let env = ref Eval.empty 
let tok = Syntax.tokenize >> Syntax.filter 
let ps = tok >> Syntax.parse
let eval expr = 
    let v, e = Eval.eval !env expr
    env := e
    v