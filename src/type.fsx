#load "Util.fs"


// 
// Basic Expresions
//
type Name = string

type Const = 
    | ConstString of string
    | ConstNumber of float
    | CanstBool of bool

type Expression =
    | If of Expression * Expression * Expression
    | Const of Const
    | Variable of Name
    | Binding of Name * Expression
    | Lambda of Name * Expression
    | App of Expression * Expression

//
// Types 
//
type Tvar = string 

type Type =
    | TyVar of string
    | TyNumber 
    | TyString
    | TyBool
    | TyFunc of Type * Type
        override x.ToString () = 
            match x with 
            | TyVar a -> a 
            | TyNumber -> "number"
            | TyString -> "string"
            | TyBool -> "bool"
            | TyFunc (arg, ret) -> "(" + arg.ToString () + " -> " + ret.ToString () + ")"


// A List of equations between type variables and types 
// Can also be represented by a function from type variables to types 
type Subst = Map<Tvar, Type>
let emptySubst : Subst = Map.empty


let rec applyOneSubst ty0 (tvar: Tvar) ty1 = 
    match ty0 with 
    | TyNumber -> TyNumber 
    | TyBool -> TyBool 
    | TyString -> TyString 
    | TyFunc (argtyp, rettyp) -> 
        TyFunc (applyOneSubst argtyp tvar ty1, applyOneSubst rettyp tvar ty1)
    | TyVar _ -> if ty0 = TyVar tvar then ty1 else ty0

let rec applySubtToType ty (subst: Subst) = 
    match ty with 
    | TyNumber -> TyNumber 
    | TyBool -> TyBool
    | TyString -> TyString
    | TyVar a -> 
        match Map.tryFind a subst with 
        | Some x -> x 
        | None -> ty
    | TyFunc (arg, ret) ->
        TyFunc (applySubtToType arg subst, applySubtToType ret subst)

let extendSubst (subst: Subst) (tvar: Tvar) ty = 
    let subst' = Map.ofList [tvar, ty]
    subst
    |> Map.map (fun _ v -> applySubtToType v subst') 
    |> Map.union subst'


// 
// THE UNIFIER 
//


// Is tvar in ty? 
let rec noOccursCheck (tvar: Tvar) ty = 
    match ty with 
    | TyNumber | TyString
    | TyBool -> true 
    | TyFunc (arg, ret) -> 
        noOccursCheck tvar arg && noOccursCheck tvar ret
    | TyVar a -> a <> tvar

let rec unifier ty1 ty2 subst (exp: Expression) =
    let ty1 = applySubtToType ty1 subst
    let ty2 = applySubtToType ty2 subst 
    if ty1 = ty2 then (* a trivial equation *) subst
    else 
        match ty1, ty2 with
        | TyVar t1, _ ->
            if noOccursCheck t1 ty2
            then extendSubst subst t1 ty2 
            else failwithf "occurs check failed t1: %A, t2: %A in %A"  ty1 ty2 exp
        | _, TyVar t2 ->
            if noOccursCheck t2 ty1
            then extendSubst subst t2 ty1 
            else failwithf "occurs check failed t1: %A, t2: %A in %A"  ty2 ty1 exp
        | TyFunc (a1, r1), TyFunc (a2, r2) -> 
            let s = unifier a1 a2 subst exp // Do we really need to give the next function our substituition???
            let s' = unifier r1 r2 s exp 
            s'
        | _ -> failwithf "Cannot unify type %A with type %A in %A" ty1 ty2 exp

type Answer = Type * Subst

type Tenv = Map<string, Type>
let emptyTenv : Tenv = Map.empty

let extendTypeEnv  name ty (tenv: Tenv) : Tenv =
    tenv 
    |> Map.add name ty 

let freshTvarType = 
    let sn = ref 0 
    fun () -> 
        sn := !sn + 1 
        TyVar $ "a" + (string !sn)

let rec getType exp (tenv: Tenv) (subst: Subst) = 
    match exp with
    | Const (ConstNumber _) -> TyNumber, subst
    | Const (ConstBool _) -> TyBool, subst
    | Const (ConstString _) -> TyString, subst
    | Variable n -> 
        match Map.tryFind n tenv with 
        | Some x -> x, subst
        | None -> failwithf "unbound variable %A" n
    | If (e1, e2, e3) ->
        let ty1, s1 = getType e1 tenv subst    
        let s' = unifier ty1 TyBool s1 e1

        let ty2, s2 = getType e2 tenv s'
        let ty3, s3 = getType e3 tenv s2

        let subst = unifier ty2 ty3 s3 exp
        ty2, subst
    | Binding (name, exp) -> 
        // Add name to type envriroment with typ1
        let ty1, s1 = getType exp tenv subst
        ty1, s1
    | Lambda (n, expr) -> 
        let nty = freshTvarType ()
        let tenv' = 
            tenv 
            |> extendTypeEnv n nty
        let ty', subst' = getType expr tenv' subst
        TyFunc (nty, ty'), subst'
    | App (rator, rand) -> 
        let returnTy = freshTvarType ()
        let ratorTy, subst' = getType rator tenv subst
        let randTy, subst'' = getType rand tenv subst'
        let s = unifier ratorTy (TyFunc (randTy, returnTy)) subst'' exp
        returnTy, s

let e = Lambda ("n", Lambda ("x", Const (ConstBool true)))
let ty = getType e emptyTenv emptySubst