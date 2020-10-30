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

type Name = string

type Const = 
    | ConstString of string
    | ConstNumber of float
    | ConstBool of bool

type Expression =
    | Unit
    | If of Expression * Expression * Expression
    | Const of Const
    | Variable of Name
    | Binding of Name * Expression
    | RecBinding of Name * Expression
    | Binary of Expression * Name * Expression
    | List of Expression list
    | Lambda of Name * Expression
    | Application of Expression * Expression

/// Transform a list of variable names and an expression body to a lambda calculus style function
/// e.g {x = ["a"; "b"; "c"]}, {body = Unit} -> Lambda (a, (Lambda (b, Lambda (c, Unit))))
let transformLambda x body =
    List.foldBack (uncurry Lambda) x body

/// Transform converts an expression and a list of expression
/// to a left associative application of the first expression to the rest 
/// since applications take a single argument  
/// e.g
/// let expr = Lambda ("x", Variable "x")
/// let args = [1..10] |> List.map (const' Unit)
/// (App ((App ((App (f, Unit)), Unit)), Unit))
let transformApp expr args = 
    List.fold (uncurry Application) expr args


