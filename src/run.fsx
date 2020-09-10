#load "Util.fs"
#load "Ast.fs"
#load "Syntax.fs"
#load "Eval.fs"


let tok = Syntax.tokenize >> Syntax.filter 
let ps = tok >> Syntax.parse
