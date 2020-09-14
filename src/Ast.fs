module Ast

// operators -> sin, cos, tan, +, -, **, *, /, pow, x nRoot y, %
// variables = let x = 23, let yyy = edf
// numbers -> floats
// string -> "eded"
// function -> let f x = x * 2, pow x y = x ** y
// infix functions (also operators) -> 2 mod 4
// list -> [], [1..10], [1, 2, 4]: let l = [1, 3, 5]

// keywords: fn -> let g = fn a v => a + v + g
// let f x = x + 2
// let g x = fn x y => 45

type Operator =
    | Addition
    | Substraction
    | Multiplication
    | Division
    | Compose

type Var = string

type Const = 
    | ConstString of string
    | ConstNumber of float
    | ConstBool of bool

type Expression =
    | Unit
    | If of Expression * Expression * Expression
    | Const of Const
    | Variable of Var
    | Binding of Var * Expression
    | Binary of Expression * Var * Expression
    | List of Expression list
    | Lambda of Var list * Expression
    | Function of Var * Var list * Expression
    | RecFunction of Var * Var list * Expression
    | Application of Expression * Expression list
