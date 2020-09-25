#load "Syntax.fs"

let rec (<*>) fs xs = 
    match fs, xs with 
    | f :: fs, x :: xs -> f x :: (fs <*> xs)
    | _ -> []

let pure' f = [f]

let cons x xs = x :: xs 

let rec seqA xs =
    match xs with
    | [] -> [[]]
    | xs :: xss -> pure' cons <*> xs <*> seqA xss

let m = 
    [
        [1;2;3]
        [3;4;5]
        [6;7;8]
    ]

// Peano numbers 
let (|S|Z|) = function 
    | 0 -> Z
    | n -> S (n-1)

let rec repeat f = function 
    | Z   -> []
    | S k -> f :: repeat f k


let rec fib = function 
    | Z       -> 0
    | S 0     -> 0
    | S (S k) -> fib k + fib (k+1)


let (|Zero|Even|Odd|) = function 
    | 0 -> Zero
    | n when n%2 = 0 -> Even (n/2)
    | n -> Odd ((n-1)/2) 

let rec power x = function 
    | Zero -> 1
    | Even n -> power (x*x) n
    | Odd n -> x * power (x*x) n

let (|As|) x = As (x, x)

let a = function 
    | As (Some {contents = 10}, v) -> "some"
    | _ -> ""
 
let rec factorial = function 
    // | Zero as n -> 1
    // | S k as n -> n * factorial k  
    | As (Zero, n) -> 1
    | As (S k, n) -> n * factorial k

let rec (|Zip|) = function 
    | [] -> Zip ([], [])
    | (a, b) :: Zip (l, r) -> Zip (a::l, b::r)


type JoinList<'a> = 
    | JNil 
    | Unit of 'a 
    | Join of JoinList<'a> * JoinList<'a> 

let rec (|Cons|_|) = function 
    | Unit x -> Some (x, JNil)
    | JNil -> None
    | Join (Cons (a, b), JNil) -> Some (a, b)
    | Join (JNil, Cons (a, b)) -> Some (a, b)
    | Join (Unit x, xs) -> Some (x, xs)
    | Join (Join (xs, ys), zs) -> 
        match Join (xs, Join (ys, zs)) with 
        | Cons (a, b) -> Some (a, b)
        | _ -> None


let rec merge xs ys = 
    match xs, ys with 
    | x :: xs, y :: _ when x < y -> x :: merge xs ys
    | _ :: _, y :: ys -> y :: merge xs ys
    | xs, [] -> xs
    | [], ys -> ys

let rec merge' xs = List.foldBack merge xs [] 

let a = merge' [ [4;6;8]; [34; 56; 68]; [1;3;7]; [9;29;67]; ]

let e = merge [1;3;5] [2;6;8]

let splitEven xs = 
    let rec s xs ys = 
        match xs, ys with
        | _ :: _ :: xs, y :: ys ->
            let left, right = s xs ys
            y :: left, right
        | _, xs -> ([], xs)
    s xs xs
 
let edd = splitEven [1;2;3;3;4;5]


let rec foldr f i l = 
    match l with 
    | [] -> i 
    | x :: xs -> f x (foldr f i xs)

let rec foldl f i l = 
    match l with 
    | [] -> i
    | x :: xs -> foldl f (f i x) xs

let l = [2.;3.]

foldr (fun x f -> fun a -> f (a ** x)) id l 2.
foldl (fun f x -> fun a -> f (x ** a)) id l 2.

foldr ( ** ) 2. l
foldl ( ** ) 2. l

type [<Measure>] Degree
let angle = 100.0<Degree>

// type RoundComputationBuilder (digits: int) = 
//     let round (x:decimal) = Math.Round (x, digits)

//     member x.Bind (a, rest) =
//         rest (round a)
    
//     member x.Return a = round a

// let round = RoundComputationBuilder

// let w = round 23 {
//     let! a = 34.545m
    
//     return a
// }

type MaybeBuilder () =  
    member x.Bind (a, f) = 
        match a with 
        | Some y -> f y 
        | None -> None
    member x.Return a = Some a
let maybe = MaybeBuilder ()

let ($) = (<|)

type Parser<'a> = Parser of (string -> ('a * string) option)

let charP c =
    fun l ->
        match Syntax.sr l with 
        | x :: xs when x = c -> Some (x, Syntax.ch xs)
        | _ -> None
    |> Parser

let apply (Parser f) (Parser a) = 
    fun l -> maybe {
        let! g, input = f l 
        let! a, rest = a input
        return (g a, rest)
    }
    |> Parser

let cons x list = x :: list

let map g (Parser f) =
    fun l -> 
        let r = f l
        match r with 
        | Some (a, str) -> Some (g a, str)
        | None -> None  
    |> Parser

let pf () = 
    fun l -> Some ((+), l)
    |> Parser

let (<!>) = map 
let (<*>) = apply 

let ap x y z = [x;y;z]

let (<*) (Parser f) (Parser g) =
    fun l -> maybe {
        let! c, input = f l
        let! _, _ = g input
        return (c, input)
    }
    |> Parser

let ( *> ) (Parser f) (Parser g) =
    fun l -> maybe {
        let! _, input = f l
        let! c, input' = g input
        return (c, input')
    }
    |> Parser

let aParser = charP 'a'

let anParser = aParser |> map int

let f = ap <!> aParser


let empty () = Parser $ fun _ -> None
let rec sequenceA lop = 
    match lop with 
    | [] -> empty ()
    | x :: xs -> cons <!> x <*> sequenceA xs 
    
let seqA l = List.foldBack (fun x s -> cons <!> x <*> s) l (empty ())
let seqA' l = l |> List.fold (fun f x -> fun a -> f (cons <!> x <*> a) ) id
let s = "dede"
let ss = Syntax.sr s |> List.map charP

// let sum l = l |> List.fold (fun f x -> fun a -> f (a + x)) id 

let parse = Syntax.ch <!> sequenceA ss


module SymbolicDiff =
    type Fexpr = 
        | Const of float
        | X
        | Add of Fexpr * Fexpr 
        | Sub of Fexpr * Fexpr 
        | Mul of Fexpr * Fexpr 
        | Div of Fexpr * Fexpr 
        | Sin of Fexpr 
        | Cos of Fexpr
        | Log of Fexpr 
        | Exp of Fexpr
    
    let e = Sin (Mul (X, X)) // sin (x^2)

    let rec D = function 
        | Const _ -> Const 0.
        | X -> Const 1.
        | Add (fe, ge) -> Add (D fe, D ge)
        | Sub (fe, ge) -> Sub (D fe, D ge)
        | Mul (fe, ge) -> Add (Mul (ge, D fe), Mul (fe, D ge))
        | Div (fe, ge) -> Div (Sub (Mul (D fe,ge), Mul (fe,D ge)), Mul (ge,ge))
        | Sin fe -> Mul (Cos fe, D fe)
        | Cos fe -> Mul (Const -1., Mul (Sin fe, D fe))
        | Log fe -> Div (D fe, fe)
        | Exp fe as e -> Mul (D fe, e)

    D (Mul (X, X)) 

    let rec toString = function 
        | Const x -> string x
        | X -> "x"
        | Add (e1, e2) -> sprintf "((%A) + (%A))" (toString e1) (toString e2) 
        | Sub (e1, e2) -> sprintf "((%A) - (%A))" (toString e1) (toString e2)
        | Mul (e1, e2) -> sprintf "((%A) * (%A))" (toString e1) (toString e2)
        | Div (e1, e2) -> sprintf "((%A) / (%A))" (toString e1) (toString e2)
        | Sin e -> sprintf "sin (%A)" (toString e) 
        | Cos e -> sprintf "cos (%A)" (toString e)
        | Log e -> sprintf "log (%A)" (toString e)
        | Exp e -> sprintf "exp (%A)" (toString e)

    // fsi.AddPrinter toString


let rec bigList = 
    function 
    | 0 -> []
    | n -> 1 :: bigList (n - 1)

let rec bigListC c = 
    function 
    | 0 -> c []
    | n -> bigListC (fun l -> c (1::l)) (n - 1)

bigList 10000
bigListC id 1000000000

type BinTree<'a> = Leaf | Node of 'a BinTree * 'a * 'a BinTree

let rec count = function 
    | Leaf -> 0
    | Node (a, n, b) -> count a + count b + 1

let rec countC c = function 
    | Leaf -> c 0
    | Node (a, n, b) -> countC (fun ares -> countC (fun bres -> c (ares+bres+1)) b) a 

let rec countA acc = function 
    | Leaf -> 0
    | Node (a, n, b) -> countA (countA 0 b + 1) a

let rec countAC aa c = function 
    | Leaf -> c (1+aa)
    | Node (a, n, b) -> countAC aa (fun acc -> countAC acc c b) a 

let sum l = List.fold (+) 0
// let s = List.map sum >> sum 

type expr =
    | Lam of string * expr 
    | App of expr * expr 
    | Var of string

let (|Lambda|_|) = 
    function 
    | Lam (a, b) -> Some (a, b)
    | _ -> None

let qZeroOrMore (|L|_|) inp = 
    let rec go acc e = 
        match e with 
        | L (v, body) -> go (v::acc) body
        | _ -> List.rev acc, e
    go [] inp

let e = Lam ("lamm", Var "x")

let (|Lambdas|) = qZeroOrMore (|Lambda|_|) 

let bbb (Lambdas _) = 0


let rec fib n = 
    match n with 
    | 0 | 1 -> 1 
    | n -> fib (n-1) + fib (n-2)

let rec fibT n (x, y) = 
    match n with 
    | 0 -> y 
    | n -> fibT (n - 1) (y, x+y)

let rec fibC n k = 
    match n with 
    | 0 | 1 -> k 1
    | n -> fibC (n-1) (fun one -> fibC (n-2) (fun two -> k (one+two)))

let rec fibC' n k =
    match n with 
    | 0 -> k (0, 1)
    | n -> fibC' (n-1) (fun (a, b) -> k (b, a+b))

[1 .. 10] |> List.map fib
[1 .. 10] |> List.map (fibT >> fun f -> f (0, 1))
[1 .. 10] |> List.map (fibC >> fun f -> f id)
[1 .. 10] |> List.map (fibC' >> fun f -> f id)

// let map f list cont = 
//     let rec loop acc list cont = 
//         match list with 
//         | [] -> cont (List.rev acc)
//         | x :: xs -> f x (fun x' -> loop (x'::acc) xs cont)

//     loop [] list 
    
type BinomialTree<'a> = Nd of int * 'a * BinomialTree<'a> list


let foldHeapStep1 h = 

    let rec loop h cont = 
        match h with 
        | [ Nd (_, a, []) ] -> cont [a]
        | [ Nd (_, a, h') ] -> loop h' (fun acc -> cont (a :: acc))
        | Nd (_, a, []) :: tail -> loop tail (fun acc -> cont (a :: acc))
        | Nd (_, a, h') :: tail -> loop h' (fun r1 -> loop tail (fun r2 -> cont (a :: (r1 @ r2))))

    loop h id