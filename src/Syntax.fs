module Syntax

type Kind =
    | Kwd of string 
    | Id of string 
    | Num of float
    | QuotedId of string
    | Plus
    | Minus 
    | Star
    | Slash
    | GreaterThan 
    | LessThan 
    | Equals
    | FuncArrow
    | Builtin of string  
    | OpenSquareBracket
    | CloseSquareBraket
    | OpenParenthesis
    | CloseParenthesis
    | Comma
    | Dot
    | DotDot
    | Ws
    | Bad of char

type Location = int

type Range = Location * Location 

type Token = 
    { Range: Range 
      Kind : Kind }

let keywords = ["fn"; "let"; "rec"; "in"; "#t"; "#f"]
let builtinKeywords =
    ["sin"; "cos"; "tan"; "map"; "head"; "tail"; "print"; "iterList"; "iterNat"]

// kwd?
let kwd w = 
    keywords 
    |> List.contains w

// builtin func?
let builtin w = builtinKeywords |> List.contains w 

let ch l = 
    let a = Array.ofList l
    System.String (a, 0, Array.length a)

let sr (s : string) = 
    s.ToCharArray () 
    |> List.ofArray

let (|Cond|_|) f p t =
    if List.length t <= p then Some (List.length t, List.last t)
    else 
        match t with 
        | c when f c -> Some (p+1, t.[p])
        | _ -> None

let rec gopher f p text = 
    match text with 
    | Cond f p (np, item) -> gopher f np (item :: text)
    | _ -> p, List.rev text

// predicates on chars 
let ws = System.Char.IsWhiteSpace
let letter = System.Char.IsLetter
let digit = System.Char.IsDigit
let symbol x = System.Char.IsLetter x || x = '#' // A hack, fix later
 
let rec (|WS|_|) p t = 
    let len = List.length t
    let rec go c =
        if c >= len || not (ws t.[c]) then c
        else go (c + 1)
    if p >= len then None 
    elif (go p) = p then None 
    else Some (go p) 

// match t with 
// | Cond Char.IsLetter 0 t -> t 
// | Cond Char.IsDigit 0 t -> t 

// let text = sr "      "
// match text with 
// | WS 0 x -> x

let (|Letter|_|) p t =
    if List.length t <= p then None
    else 
        match t.[p] with 
        | c when letter c -> Some (p+1, c)
        | _ -> None

let (|LetterOrSymbol|_|) p t =
    if List.length t <= p then None
    else 
        match t.[p] with 
        | c when symbol c -> Some (p+1, c)
        | _ -> None

let (|Keyword|_|) p t =
    let rec go np acc = 
        match t with 
        | LetterOrSymbol np (pos, ch) -> go pos (ch :: acc)
        | _ -> np, List.rev acc
    let pos, chs = go p []
    if List.isEmpty chs then None
    elif kwd (ch chs) then Some (pos, ch chs)
    else None

let (|Identifier|_|) p t =
    let rec go np acc = 
        match t with 
        | Letter np (pos, ch) -> go pos (ch :: acc)
        | _ -> np, List.rev acc
    let pos, chs = go p []
    if List.isEmpty chs then None
    elif kwd (ch chs) then None 
    else Some (pos, ch chs)

let (|BuiltinId|_|) p t =
    let rec go np acc = 
        match t with 
        | Letter np (pos, ch) -> go pos (ch :: acc)
        | _ -> np, List.rev acc
    let pos, chs = go p []
    if List.isEmpty chs then None
    elif builtin (ch chs) then Some (pos, ch chs) 
    else None

let (|Digit|_|) p t = 
    if List.length t <= p then None
    else
        match t.[p] with
        | c when digit c -> Some (p+1, c.ToString ())
        | _ -> None 

let (|Number|_|) p t =
    let rec go np acc = 
        match t with 
        | Digit np (pos, ch) -> go pos (ch :: acc)
        | _ -> np, List.rev acc
    let pos, nums = go p []
    let f = String.concat "" >> float
    if List.isEmpty nums then None
    else Some (pos, f nums)

// NOTE: conflict with the >>> bit operator, but idc
// f KNOWS the position to start pattern matching
// g DOES NOT know the position to start; it waits for f to tell
// >>> composes f and g to produces a function k
// where k is a func that EXACTLY matches the two patterns
let (>>>) f g =
    fun l ->
        match f l with 
        | Some (p1, a) -> 
            if List.length l <= p1 then None // sanity check
            else
                match g p1 l with
                | Some (p2, b) -> Some (p2, a.ToString () + b.ToString ())
                | None -> None (* Some (p1, a.ToString ()) -- to & or to |? *) 
        | None -> None

// A replacement for the above operator 
let (>>|) (|F|_|) (|G|_|) =
    fun l ->
        match l with 
        | F (p1, a) -> 
            match l with 
            | G p1 (p2, b) -> Some (p2,  [a;b])
            | _ -> None
        | _ -> None

let (|Quote|_|) p t =
    if List.length t <= p then None
    else 
        match t.[p] with
        | c when c = '"' -> Some (p+1, string c)
        | _ -> None       

let (|Lit|_|) p t =
    let rec go np acc =
        match t with
        | Letter np (pos, ch) -> go pos (ch :: acc)
        | _ -> np, List.rev acc
    let pos, chs = go p []
    if List.isEmpty chs then None
    else Some (pos, ch chs)

let (|QuotedIdentifier|_|) p t =
    let f = (|Quote|_|) p >>> (|Lit|_|) >>> (|Quote|_|)
    f t 

// let s = sr """"no" """
// match s with 
// | QuotedIdentifier 0 x -> x

let (|Operator|_|) op p t =
    if List.isEmpty t then None
    else
        match t.[p] with
        | c when c = op -> Some (p+1, c.ToString ())
        | _ -> None

let (|Symbol|_|) = (|Operator|_|) 

let strFromChar (x: char) = string x // .ToString ()

let (|Symbols|_|) (sep: char) (ops: string) p text =
    let os = ops.Split (sep) |> List.ofArray
    let hd = List.head os 
    let text = text |> List.map strFromChar
    let f = (((|Symbol|_|) hd p), List.tail os) 
            ||> List.fold (fun f op -> f >>> ((|Symbol|_|) op))
    f text

// let text = sr "=>>" 
// let ops = sr "=>>" |> List.map string
// match text with 
// | Operators ops 0 (x, y) -> x, y
// | Operator '=' 0 (x, y) -> x, y
// | _ -> 0, ""

let lex pos t =
    match sr t with 
    | Keyword pos (p, word) ->
        let range = pos, p - 1
        { Range = range; Kind = Kwd word }, p
    | Identifier pos (p, word) ->
        let range = pos, p - 1 
        { Range = range; Kind = Id word }, p
    | Number pos (p, num) -> 
        let range = pos,  p - 1
        { Range = range; Kind = Num num }, p
    | QuotedIdentifier pos (p, word) -> 
        let range = pos,  p - 1
        { Range = range; Kind = QuotedId word }, p
    | Operator '+' pos (p, _) -> 
        let range = pos, p - 1
        { Range = range; Kind = Plus }, p
    | Operator '-' pos (p, _) -> 
        let range = pos, p - 1
        { Range = range; Kind = Minus }, p
    | Operator '*' pos (p, _) -> 
        let range = pos, p - 1
        { Range = range; Kind = Star }, p
    | Operator '/' pos (p, _) -> 
        let range = pos, p - 1
        { Range = range; Kind = Slash }, p
    | Operator '>' pos (p, _) -> 
        let range = pos, p - 1
        { Range = range; Kind = GreaterThan }, p
    | Operator '<' pos (p, _) -> 
        let range = pos, p - 1
        { Range = range; Kind = LessThan }, p
    | Symbol '[' pos (p, _) -> 
        let range = pos, p - 1
        { Range = range; Kind = OpenSquareBracket }, p
    | Symbol ']' pos (p, _) -> 
        let range = pos, p - 1
        { Range = range; Kind = CloseSquareBraket }, p
    | Symbol '(' pos (p, _) -> 
        let range = pos, p - 1
        { Range = range; Kind = OpenParenthesis }, p
    | Symbol ')' pos (p, _) ->
        let range = pos, p - 1
        { Range = range; Kind = CloseParenthesis }, p
    | Symbol ',' pos (p, _) -> 
        let range = pos, p - 1
        { Range = range; Kind = Comma }, p
    | Symbols '|' ".|." pos (p, _) ->
        let range = pos, p - 1
        { Range = range; Kind = DotDot }, p
    | Symbol '.' pos (p, _) -> 
        let range = pos, p - 1
        { Range = range; Kind = Dot }, p
    | Symbols ',' "=,>" pos (p, _) ->
        let range = pos, p - 1
        { Range = range; Kind = FuncArrow }, p
    | Operator '=' pos (p, _) -> 
        let range = pos, p - 1
        { Range = range; Kind = Equals }, p
    | WS pos p -> 
        let range = pos, p - 1
        { Range = range; Kind = Ws }, p
    | BuiltinId pos (p, func) ->
        let range = pos, p - 1
        { Range = range; Kind = Builtin func }, p - 1
    | c -> 
        { Range = (pos, pos); Kind = Bad c.[pos] }, pos + 1

let tokenize text =
    let rec aux pos my k = // cps vs. normal tail call?
        match my with
        | [] -> k []
        | _ ->
            let token, next = lex pos text
            let nl = (sr text).[next..]
            aux next nl (fun result -> k (token :: result))

    let rec go pos my tokens =  
        match my with 
        | [] -> List.rev tokens
        | _ -> 
            let token, next = lex pos text
            let nl = (sr text).[next..]
            go next nl (token :: tokens)
            
    go 0 (sr text) []

// token helpers 
let isOp = function 
    | { Kind = Plus | Minus | Star | Slash | Dot } -> true
    | _ -> false

let opStr = function 
    | Plus -> "+" 
    | Minus -> "-" 
    | Star -> "*" 
    | Slash -> "/" 
    | Dot -> "." 
    |_ -> "<nil>"

let isWs = function
    | { Kind = Ws } -> true
    | _ -> false 

let isTermOp = function 
    | { Kind = Plus | Minus } -> true 
    | _ -> false

let isFactorOp = function 
    | { Kind = Star | Slash } -> true 
    | _ -> false

//
// Combinators 
//

// P+ 
let oneOrMore pattern = 
    let rec go p tokens result = 
        match pattern p tokens with
        | None -> p, List.rev result 
        | Some (np, expr) -> 
            printfn "Got %A at %A" expr np
            go np tokens (expr :: result) 

    fun position tokens ->
        let pos, result = go position tokens []
        if List.isEmpty result then None // must match at least one 
        else Some (pos, result)

// Fix bug - probably reimplement
// (P sep P)+
let oneOrMoreSep pattern sep = 
    let rec go p tokens result = // P sep P
        match pattern p tokens with
        | Some (p1, e1) ->
            printfn "Matches pattern %A" e1;
            match sep p1 tokens with 
            | Some (p2, sep) -> // match sep but don't include it.
                printfn "Matches sep %A" sep;
                match pattern p2 tokens with 
                | Some (p3, e2) -> go2 p3 tokens (result @ [e1] @ [e2])
                | _ -> Some (p2, (result @ [e1]))  //temp
            | _ -> printfn "Couldn't match %A sep func %A" (tokens, p1) sep; None // Some (p1, result @ [e1]) 
        | _ -> Some (p, result) // None 
    and go2 p tokens result = // sep P sep
        match sep p tokens with 
        | Some (p1, _) -> 
            match pattern p1 tokens with 
            | Some (p2, expr) -> // match sep but don't include it.
                match sep p2 tokens with
                | Some (p3, _) -> go p3 tokens (result @ [expr])
                | _ -> Some (p2, result @ [expr])
            | _ -> Some (p1, result)
        | _ -> Some (p, result)

    fun position tokens ->
        let result = go position tokens []
        match result with
        | Some _ -> result
        | None -> None


let oneOrMoreSepStrict pattern sep =
    let rec aux p ts result = 
        match pattern p ts with
        | None -> None // 1, 3, 4
        | Some (p, a) -> 
            match sep p ts with
            | None -> Some (p, result ++ a)
            | Some (p, _) -> aux p ts (result ++ a)

    fun position tokens -> 
        let result = aux position tokens []
        match result with
        | Some _ -> result
        | None -> None


// P1 || P2
let thisOrThat p1 p2 = 
    fun position tokens -> 
        match p1 position tokens with 
        | Some _ as result -> result 
        | None ->
            match p2 position tokens with 
            | Some _ as result -> result
            | None -> None 

// P1 && P2
let thisAndThat p1 p2 =
    fun position tokens -> 
        match p1 position tokens with 
        | Some (np, x) -> 
            match p2 np tokens with 
            | Some (fp, y) -> Some (fp, [x;y])
            | None -> None  
        | None -> None

let (|Map|_|) (|Pat|_|) f p tokens =
        match tokens with 
        | Pat p (np, list) -> Some (np, f list)
        | _ -> None

let (<!>) = (|Map|_|)

let (|Token|_|) value position tokens = 
    let len = List.length tokens 
    if position >= len then None 
    else 
        match tokens.[position] with 
        | { Kind = k } when k = value -> Some (position+1, tokens.[position])
        | _ -> None

// TODO: use disjunctive patterns
let rec (|Expression|_|) p tokens = 
    match tokens with 
    | BindingExpression p result -> Some result 
    | FunctionExpression p result -> Some result 
    | LambdaExpression p result -> Some result  
    | BinaryExpression p result -> Some result
    | ListExpression p result -> Some result
    | ParenthesizedExpression p result -> Some result  
    | ConstExpression p result
    | VariableExpression p result -> Some result
    | UnitExpression p result -> Some result
    | ApplicationExpression p result -> Some result
    | _ -> None

and (|PrimaryExpression|_|) p tokens = 
    match tokens with
    | LambdaExpression p result -> Some result 
    | ApplicationExpression p result -> Some result // this might not be good
    | ConstExpression p result 
    | UnitExpression p result -> Some result 
    | VariableExpression p result -> Some result
    | ListExpression p result -> Some result
    | ParenthesizedExpression p result -> Some result 
    | _ -> None

and (|ParenthesizedExpression|_|) p tokens =
    match tokens with
    | Token OpenParenthesis p (p1, tOpen) -> 
        // printfn "start" 
        match tokens with
        | Expression p1 (p2, expr) ->
            printfn "got %A" expr
            match tokens with
            | Token CloseParenthesis p2 (p3, tClose) -> 
                // printfn "end"
                Some (p3, expr) 
            | _ ->  None
        | _ ->  None
    | _ -> None

and (|NumberExpression|_|) p tokens =
    let len = List.length tokens
    if p >= len then None
    else  
        match tokens.[p] with 
        | { Kind = Num num } -> Some (p+1, Ast.Const (Ast.ConstNumber num))
        | _ -> None

and (|BoolLiteral|_|) p tokens = 
    let t = Kwd "#t" 
    let f = Kwd "#f" 
    match tokens with 
    | Token t p (p, _) -> Some (p, Ast.Const (Ast.ConstBool true))
    | Token f p (p, _) -> Some (p, Ast.Const (Ast.ConstBool false))
    | _ -> None 

and (|StringLiteral|_|) p tokens = 
    let len = List.length tokens 
    if p >= len then None
    else 
        match tokens.[p] with 
        | { Kind = QuotedId id } -> Some (p+1, Ast.Const (Ast.ConstString id))
        | _ -> None 

and (|ConstExpression|_|) p tokens = 
    match tokens with 
    | StringLiteral p res | NumberExpression p res 
    | BoolLiteral p res -> Some res
    | _ -> None

// variable:   x, y, z
and (|VariableExpression|_|) p tokens = 
    let len = List.length tokens
    if p >= len then None
    else
        match tokens.[p] with 
        | { Kind = Id id | Builtin id } -> Some (p+1, Ast.Variable id) // hmm?
        | _ -> None

// binding: v = <expr>
and (|BindingExpression|_|) p tokens = 
    let l = Kwd "let"
    match tokens with 
    | Token l p (p, tok) -> 
        match tokens with
        | VariableExpression p (np, Ast.Variable var) ->
            if np >= List.length tokens then None // guard
            else
                match tokens.[np] with
                |  { Kind = Equals } ->
                    let p = np + 1 // re bind 
                    if p >= List.length tokens then None // guard
                    else
                        match tokens with
                        | Expression p (np, expr) -> Some (np, Ast.Binding (var, expr)) 
                        | _ -> None
                | _ -> None 
        | _ -> None 
    | _ -> None 

and (|BinaryExpression|_|) p tokens = 
    let len = List.length tokens
    match tokens with
    | PrimaryExpression p (np, e1) ->
        if np >= len then Some (np, e1) 
        else 
            match tokens.[np] with 
            | { Kind = k } as op when isOp op ->
                // printfn "Matched token %A" k;
                let p = np + 1 // re bind
                if p >= len then None
                else 
                    match tokens with 
                    | BinaryExpression p (fp, e2) -> Some (fp, Ast.Binary (e1, opStr k, e2))
                    | _ -> None
            | _ -> Some (np, e1) 
    | _ -> None

// [] || ([ &&  expr, expr && ])
and (|ListExpression|_|) p tokens =
    let numbers = oneOrMoreSep (|PrimaryExpression|_|) ((|Token|_|) Comma) // TODO: make this a pattern
    let (|Comprehension|_|) = oneOrMoreSepStrict (|NumberExpression|_|) ((|Token|_|) DotDot)

    match tokens with
    | Token OpenSquareBracket p (np, opn) -> 
        match numbers np tokens with
        | Some (tp, list) -> 
            match tokens with 
            | Token CloseSquareBraket tp (fp, cls) -> Some (fp, Ast.List list) 
            | _ -> None 
        | _ -> 
            match tokens with 
            | Token CloseSquareBraket np (fp, _) ->  Some (fp, Ast.List [])
            | Comprehension np (fp, list) -> 
                match list with
                | [Ast.Const (Ast.ConstNumber start); Ast.Const (Ast.ConstNumber ed)] ->
                    match tokens with 
                    | Token CloseSquareBraket fp (fp', _) -> 
                        let l = 
                            [int start .. int ed] 
                            |> List.map (float >> Ast.ConstNumber >> Ast.Const) 
                            |> Ast.List
                        Some (fp', l)
                    | _ -> None 
                | _ -> None 
            | _ -> None 
    | _ -> None 

// () 
and (|UnitExpression|_|) p tokens =
    match tokens with
    | Token OpenParenthesis p (p1, _) -> 
        match tokens with
        | Token CloseParenthesis p1 (p2, _) -> Some (p2, Ast.Unit)
        | _ -> None
    | _ -> None
    // let k = ((|Token|_|) OpenParenthesis)
    // let k' = ((|Token|_|) CloseParenthesis)
    // let g = fun _ -> Ast.Unit
    // thisAndThat k k' <!> g

and (|FunctionExpression|_|) p tokens = 
    let pattern = oneOrMore (|VariableExpression|_|)
    let l = Kwd "let"
    match tokens with 
    | Token l p (p, _) -> 
        match pattern p tokens with 
        | Some (p1, Ast.Variable name :: ps) -> 
            match tokens with 
            | Token Equals p1 (p2, eq) -> 
                match tokens with 
                | Expression p2 (p3, expr) -> 
                    let l = 
                        ps 
                        |> List.map (function
                                    | Ast.Variable n -> n 
                                    | _ -> failwith "(|FunctionExpression|_|): Internal error")
                    Some (p3, Ast.Function (name, l, expr))
                | _ -> None
            | _ ->  None
        | _ -> None
    | _ -> None 

// alow unit params 
and (|LambdaExpression|_|) p tokens =
    let pattern = oneOrMore (|VariableExpression|_|)
    let fn = Kwd "fn" 
    
    match tokens with 
    | Token fn p (p1, tok) -> 
        match pattern p1 tokens with 
        | Some (p2, ps) ->
            match tokens with 
            | Token FuncArrow p2 (p3, arrow) -> 
                match tokens with 
                | Expression p3 (p4, expr) -> 
                    let l = ps 
                            |> List.map (function 
                                        | Ast.Variable n -> n 
                                        | _ -> failwith "(|FunctionExpression|_|): Internal error")
                    Some (p4, Ast.Lambda (l, expr))
                | _ -> None 
            | _ -> None 
        | _ -> None 
    | _ -> None 

and (|SimpleExpression|_|) p tokens = 
    match tokens with 
    | ListExpression p result | ParenthesizedExpression p result
    | ConstExpression p result
    | VariableExpression p result 
    | UnitExpression p result -> Some result
    | _ -> None

// TODO: allow call with unit 
// cos 12, (fn x => x + 1) 20, (f x y = x + y) 10 10
and (|ApplicationExpression|_|) p tokens =
    let pattern = oneOrMore (|SimpleExpression|_|)
    // let b = thisAndThat ((|VariableExpression|_|) <!> fun x -> [x]) pattern <!> List.concat
    // let c = thisAndThat ((|ParenthesizedExpression|_|) <!> fun x -> [x]) pattern <!> List.concat
    // let d = thisOrThat b c
    let p1 p tokens = 
        match tokens with 
        | VariableExpression p (p1, name) -> 
            printfn "Parsing arguments now"
            match pattern p1 tokens with 
            | Some (p3, ps) -> 
                printfn "got %A from mutiple simple exps" ps
                Some (p3, Ast.Application (name, ps))
            | _ -> None
        | _ -> None
    let p2 p tokens =
        match tokens with
        | ParenthesizedExpression p (p1, (Ast.Lambda _ as f)) -> 
            match pattern p1 tokens with 
            | Some (p2, ps) -> Some (p2, Ast.Application (f, ps))
            | _ -> None
        | _ -> None

    thisOrThat p1 p2 p tokens 

let filter = List.filter (isWs >> not)

let parse tokens = 
    match tokens with 
    | Expression 0 result -> snd result 
    | _ -> failwith "Syntax error!"

let parse2 tokens = 
    match tokens with 
    | Expression 0 result -> result 
    | _ -> failwith "Syntax error!"

// To add 

let (|Tok|_|) value position tokens = 
    let len = List.length tokens
    if position >= len then None 
    else
        match value position tokens with 
        | Some _ as v -> v 
        | _ -> None

let tokToOp = 
    function
    | { Kind = Plus } -> Some Ast.Addition
    | { Kind = Minus } -> Some Ast.Substraction
    | { Kind = Star } -> Some Ast.Multiplication
    | { Kind = Slash } -> Some Ast.Division
    | { Kind = Dot } -> Some Ast.Compose
    | _ -> None

let (|AnyOp|_|) p tokens =
    let len = List.length tokens
    if p >= len then None
    else
        match tokens.[p] with
        | { Kind = Plus | Minus | Star | Slash } as tok ->
            match tokToOp tok with
            | Some x -> Some (p+1, x)
            | None -> None
        | _ -> None

let (|Op|_|) kind p tokens =
    match tokens with
    | Token kind p (np, op) -> Some (np, tokToOp op)
    | _ -> None


let (|A|_|) =  oneOrMoreSepStrict (|PrimaryExpression|_|) ((|Token|_|) Comma) <!> id
let (|B|_|) = oneOrMoreSepStrict (|PrimaryExpression|_|) ((|Token|_|) DotDot)

let rec flatten expr =
    match expr with 
    | Ast.Binary (e1, op, e2) -> flatten e1 @ [Ast.Variable op] @ flatten e2
    | _ -> [expr]

let table = []

let precedence key =
    let map = Map.ofList table
    let d = map |> Map.tryFind key
    match d with 
    | Some x -> x
    | None -> 0

let rec binaryExpr tokens = 
    // Start 
    
    ()