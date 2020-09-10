

type Book = { title: string; price: decimal }

type ChocolateType = Dark | Milk | SeventyPercent 
type Choclate = { chocType : ChocolateType; price: decimal}

type WrappingPaperStyle = 
    | HappyBirthday
    | HappyHolidays
    | SolidColor

type Gift = 
    | Book of Book
    | Choclate of Choclate
    | Wrapped of Gift * WrappingPaperStyle
    | Boxed of Gift
    | WithACard of Gift * message:string

let rec description gift = 
    match gift with 
    | Book book -> sprintf "'%s'" book.title
    | Choclate c -> sprintf "A choclate of type %A" c.chocType
    | Wrapped (gift, style) -> 
        sprintf "%s wrapped in %A" (description gift) style


let rec cataGift fBook fChocolate fWrapped fBox fCard gift =
    match gift with
    | Book book -> fBook book
    | Choclate choc -> fChocolate choc
    | Wrapped (innerGift,style) ->
        let innerGiftResult = cataGift fBook fChocolate fWrapped fBox fCard innerGift
        fWrapped (innerGiftResult,style)
    | Boxed innerGift ->
        let innerGiftResult = cataGift fBook fChocolate fWrapped fBox fCard innerGift
        fBox innerGiftResult
    | WithACard (innerGift,message) ->
        let innerGiftResult = cataGift fBook fChocolate fWrapped fBox fCard innerGift
        fCard (innerGiftResult,message)

let rec foldGift fBook fChocolate fWrapped fBox fCard acc gift =
    let recurse = foldGift fBook fChocolate fWrapped fBox fCard
    match gift with
    | Book book ->
        let finalAcc = fBook acc book
        finalAcc // final result
    | Choclate choc ->
        let finalAcc = fChocolate acc choc
        finalAcc // final result
    | Wrapped (innerGift, style) ->
        let newAcc = fWrapped acc style
        recurse newAcc innerGift
    | Boxed innerGift ->
        let newAcc = fBox acc
        recurse newAcc innerGift
    | WithACard (innerGift, message) ->
        let newAcc = fCard acc message
        recurse newAcc innerGift

let descriptionUsingFold gift =
    let fBook descriptionSoFar (book:Book) =
        sprintf "'%s' %s" book.title descriptionSoFar
    let fChocolate descriptionSoFar (choc:Choclate) =
        sprintf "%A chocolate %s" choc.chocType descriptionSoFar
    let fWrapped descriptionSoFar style =
        sprintf "%s wrapped in %A paper" descriptionSoFar style
    let fBox descriptionSoFar =
        sprintf "%s in a box" descriptionSoFar
    let fCard descriptionSoFar message =
        sprintf "%s with a card saying '%s'" descriptionSoFar message // initial accumulator
    let initialAcc = ""
    // main call
    foldGift fBook fChocolate fWrapped fBox fCard initialAcc gift

let gift = Wrapped (Boxed (Choclate { price = 23m; chocType = Milk}), SolidColor)

descriptionUsingFold gift

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

open System
open System.Text.RegularExpressions

type UnvalidatedUser = {
    Name : string
    Email : string
    DateOfBirth : string
}

type ValidatedUser = {   
    Name : string
    Email : string
    DateOfBirth : DateTime
}

type ValidationFailure =
    | NameIsInvalidFailure
    | EmailIsInvalidFailure
    | DateOfBirthIsInvalidFailure

let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

let (|IsValidName|_|) input =
    if input <> String.Empty then Some () else None

let (|IsValidEmail|_|) input =
    match input with
    | ParseRegex ".*?@(.*)" [ _ ] -> Some input
    | _ -> None

let (|IsValidDate|_|) (input:string) =
    let (success, value) = DateTime.TryParse(input)
    if success then Some value else None

let validateName input =
    match input with
    | IsValidName -> Ok input
    | _ -> Error [ NameIsInvalidFailure ]

let validateEmail input = 
    match input with
    | IsValidEmail email -> Ok email
    | _ -> Error [ EmailIsInvalidFailure ]

let validateDateOfBirth input = 
    match input with
    | IsValidDate dob -> Ok dob //Add logic for DOB
    | _ -> Error [ DateOfBirthIsInvalidFailure ]

let apply fResult xResult =
    match fResult,xResult with
    | Ok f, Ok x -> Ok (f x)
    | Error ex, Ok _ -> Error ex
    | Ok _, Error ex -> Error ex
    | Error ex1, Error ex2 -> Error (List.concat [ex1; ex2])

let (<!>) = Result.map
let (<*>) = apply
let ($) = (<|)

let create name email dateOfBirth =
    { Name = name; Email = email; DateOfBirth = dateOfBirth }


let validate (input:UnvalidatedUser) =
    let validatedName = input.Name |> validateName
    let validatedEmail = input.Email |> validateEmail
    let validatedDateOfBirth = input.DateOfBirth |> validateDateOfBirth

    let v = 
        create 
        |> Result.map $ validatedName
        |> apply $ validatedEmail
        |> apply $ validatedDateOfBirth
    
    // let another = 
    //    (validatedName, validatedEmail, validateDateOfBirth)
    //    |> (fun (x, y, z) -> create x y z)
    create <!> validatedName <*> validatedEmail <*> validatedDateOfBirth

type Tree<'a> = TreeNode of Tree<'a> list | LeafNode of 'a 

let fringeTR tr = 
    let rec collect tr acc = 
        match tr with 
        | TreeNode subts -> List.foldBack collect subts acc
        | LeafNode x -> x :: acc
    
    collect tr []

[1] |> List.reduce (+)



type LoggingBuilder () = 
    let log p = printfn "expression is %A" p

    member x.Bind (a, f) = 
        log a
        f a
    
    member x.Return a = a

let logger = LoggingBuilder ()

let workflow = logger {
    let! x = 45
    let! y = 34
    return x + y 
}

type MaybeBuilder () =  
    
    member x.Bind (a, f) = 
        match a with 
        | Some y -> f y 
        | None -> None
    
    member x.Return a = Some a


let maybe = MaybeBuilder ()

let comp = maybe {
    let! a = Some 23
    let! b = Some 34
    return a + b
}

type OrElseBuilder () =

    member x.ReturnFrom a = a 

    member x.Combine (a, b) =
        match a with 
        | Some _ -> a 
        | None -> b 
    
    member x.Delay f = f ()
    

let orElse = OrElseBuilder ()

let map1 = [ ("1","One"); ("2","Two") ] |> Map.ofList
let map2 = [ ("A","Alice"); ("B","Bob") ] |> Map.ofList
let map3 = [ ("Delta", "Asaba"); ("Rivers", "PH") ] |> Map.ofList

let multiLookUp key = orElse {
    return! map1.TryFind key
    return! map2.TryFind key
    return! map3.TryFind key
} 

// let (>>=) m f = 
//     printfn "Expression is %A" m
//     f m 
 
// let log = 1 >>= (+) >>= id

let strToInt (s:string) = try Some (int s) with e -> None

type StringWorkflow () = 
    member x.Bind (s, f) = maybe {
        let! a = s 
        return f a
    }

    member x.Return a = Some a

let mworkflow = StringWorkflow ()

let stringAddWorkflow x y z = mworkflow {
    let! a = strToInt x
    let! b = strToInt y
    let! c = strToInt z
    return a + b + c
}

let result = stringAddWorkflow "1" "3" "ef4"

let strAdd s i = mworkflow {
    let! a = strToInt s
    return a + i
}

let (>>=) m f =
    match m with 
    | None -> None
    | Some x -> f x

type DbResult<'a> = Success of 'a | Error of string 

let getCustomerId name =
    if name = ""
    then Error "getCustomerId failed"
    else Success "Cust42"

let getLastOrderForCustomer custId =
    if custId = ""
    then Error "getLastOrderForCustomer failed"
    else Success "Order123"

let getLastProductForOrder orderId =
    if orderId = ""
    then Error "getLastProductForOrder failed"
    else Success "Product456"

type DbResultBuilder () =

    member x.Bind (a, f) = 
        match a with 
        | Error e -> a
        | Success s -> f s 

    member x.Return a = Success a 

    member x.ReturnFrom a = a 

let dbresult = DbResultBuilder ()

let product = dbresult {
    let! custId = getCustomerId "Alice"
    let! orderId = getLastOrderForCustomer custId
    let! productId = getLastProductForOrder orderId
    printfn "Product is %s" productId
    return productId
}

printfn "Product %A" product

type RoundComputationBuilder (digits: int) = 
    let round (x:decimal) = Math.Round (x, digits)

    member x.Bind (a, rest) =
        rest (round a)
    
    member x.Return a = round a

let round = RoundComputationBuilder

let w = round 23 {
    let! a = 34.545m
    
    return a
}

#load "Syntax.fs"

module Parser = 
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
    
    let sum l = l |> List.fold (fun f x -> fun a -> f (a + x)) id 

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
        |  X -> "x"
        | Add (e1, e2) -> sprintf "((%A) + (%A))" (toString e1) (toString e2) 
        | Sub (e1, e2) -> sprintf "((%A) - (%A))" (toString e1) (toString e2)
        | Mul (e1, e2) -> sprintf "((%A) * (%A))" (toString e1) (toString e2)
        | Div (e1, e2) -> sprintf "((%A) / (%A))" (toString e1) (toString e2)
        | Sin e -> sprintf "sin (%A)" (toString e) 
        | Cos e -> sprintf "cos (%A)" (toString e)
        | Log e -> sprintf "log (%A)" (toString e)
        | Exp e -> sprintf "exp (%A)" (toString e)

    fsi.AddPrinter toString


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
let _ = List.map sum >> sum // rite or passage , hind sight, unilateral



type OptionBuilder () = 

    member x.Return a = Some a

    member x.ReturnFrom a = a

    member x.Bind (v, f) = 
        match v with 
        | None -> None 
        | Some x -> f x
    
    member x.Delay f = f()

    member x.Zero () = None

let option = OptionBuilder ()

let result' =
    option {
        let! a = option { return 10 }
        let! b = option { return "dee" } 
        if 2 > 10 then return String.length b + a
    }
    |> Option.map string

let a = async { "" }

type TraceBuilder () = 
    member x.Bind (m, f) =
        match m with
        | None ->
            printfn "Binding with None. Exiting."
        | Some a ->
            printfn "Binding with Some(%A). Continuing" a
        Option.bind f m
    
    member x.Return a =    
        printfn "Returning a unwrapped %A as an option" a
        Some a
    
    member x.ReturnFrom a = 
        printfn "Returning an option (%A) directly" a
        a
    
    member x.Zero () = 
        printfn "Zero"
        None
    
    member x.Yield a =
        printfn "Yield %A" a
        Some a

    member x.YieldFrom a =
        printfn "Yield an option (%A) directly" a
        a 
    member x.Combine (a, b) =
        // match a,b with
        // | Some a', Some b' ->
        //     printfn "combining %A and %A" a' b'
        //     Some (a' + b')
        // | Some a', None ->
        //     printfn "combining %A with None" a'
        //     Some a'
        // | None, Some b' ->
        //     printfn "combining None with %A" b'
        //     Some b'
        // | None, None ->
        //     printfn "combining None with None"
        //     None
        // printfn "Combining %A with %A" a b
        // x.Bind (a, fun () -> b)
        a
    
    member this.Delay(funcToDelay) =
        let delayed = fun () ->
            printfn "%A - Starting Delayed Fn." funcToDelay
            let delayedResult = funcToDelay()
            printfn "%A - Finished Delayed Fn. Result is %A" funcToDelay delayedResult
            delayedResult
            // return the result
        printfn "%A - Delaying using %A" funcToDelay delayed
        delayed
    
    member this.Run (funcToRun) =
        printfn "%A - Run Start." funcToRun
        let runResult = funcToRun()
        printfn "%A - Run End. Result is %A" funcToRun runResult
        runResult

let trace = TraceBuilder ()

trace {
    return 1
} |> printfn "Result 1 %A" 

trace {
    return! Some 1
} |> printfn "Result 1 %A" 

trace {
    let! a = Some 1 
    let! b = Some 45
    return a + b
} |> printfn "Result 1 %A" 


trace {
    let! a = None 
    let! b = Some 45
    return a + b
} |> printfn "Result 1 %A" 

trace {
    do! Some (printfn "")
    do! Some ()
    let! x = Some 1
    return x
} |> printfn "Result 1 %A" 


trace {
    printfn "m"
} |> printfn "Result 1 %A" 

trace {
    let! x = Some 45
    yield 2
} |> printfn "Result 1 %A" 


type ListBuilder () =
    member this.Bind (m, f) =
        m |> List.collect f
    member this.Zero () =
        printfn "Zero"
        []
    member this.Return x =
        printfn "Return an unwrapped %A as a list" x
        [x]
    member this.Yield x =
        printfn "Yield an unwrapped %A as a list" x
        [x]
    member this.For (m, f) =
        printfn "For %A" m
        this.Bind (m, f)
    
    member x.Combine (a, b) = 
        printfn "combining %A and %A" a b
        List.concat [a;b]

    member x.Delay f = 
        printfn "Delay"
        f ()

let lb = ListBuilder ()


lb {
    let! x = [1;2;3;4]
    let! y = [4;5;6]
    return x + y
}

lb {
    yield 1 
    yield 2
    yield 3
    yield 4
}

trace {
    printfn "Nooo"
    return 20
    printfn "ffrrr" 
}

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

let map f list cont = 
    let rec loop acc list cont = 
        match list with 
        | [] -> cont (List.rev acc)
        | x :: xs -> f x (fun x' -> loop (x'::acc) xs cont)

    loop [] list 
    
type BinomialTree<'a> = Nd of int * 'a * BinomialTree<'a> list


let foldHeapStep1 h = 

    let rec loop h cont = 
        match h with 
        | [ Nd (_, a, []) ] -> cont [a]
        | [ Nd (_, a, h') ] -> loop h' (fun acc -> cont (a :: acc))
        | Nd (_, a, []) :: tail -> loop tail (fun acc -> cont (a :: acc))
        | Nd (_, a, h') :: tail -> loop h' (fun r1 -> loop tail (fun r2 -> cont (a :: (r1 @ r2))))

    loop h id