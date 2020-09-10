module Ast

// operators -> sin, cos, tan, +, -, **, *, /, pow, x nRoot y, %
// variables = x = 23, yyy = edf
// numbers -> floats
// string -> "eded"
// function -> f x = x * 2, pow x y = x ** y
// infix functions (also operators) -> 2 mod 4
// list -> [], [1..10], [1, 2, 4]: l = [1, 3, 5]; l[9]

// keywords: fn -> g = fn a v => a + v + g
// f x = x + 2
// g x = fn x y => 45

type Operator =
    | Addition
    | Substraction
    | Multiplication
    | Division
    | Compose

type Var = string

type Expression =
    | Unit
    | Number of float
    | String of string
    | Variable of Var
    | Binding of Var * Expression
    | Binary of Expression * Var * Expression
    | List of Expression list
    | Lambda of Var list * Expression
    | Function of Var * Var list * Expression
    | Application of Expression (* Var Exp list *)  * Expression list (* Var Exp *)