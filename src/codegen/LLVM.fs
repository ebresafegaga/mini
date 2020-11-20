module Codegen.LLVM


open LLVMSharp

let context = LLVM.GetGlobalContext ()
let M = uncurry LLVM.ModuleCreateWithNameInContext "mini" context 
let builder = LLVM.CreateBuilderInContext context

let env : Map<string, LLVMValueRef> = Map.empty

let llvmDouble = LLVM.DoubleType ()
let llfalse = LLVMBool (0)

let ofllvmBool (b : LLVMBool) =
    b.Value > 0

let phi = LLVM.BuildPhi

let lookupFunction name m = 
    let v = LLVM.GetNamedFunction (m, name)
    if ofllvmBool (LLVM.IsNull v) then None 
    else Some v


let codegenConst = function 
    | Ast.ConstNumber n -> Ok (LLVM.ConstReal (llvmDouble, n))
    | Ast.ConstString s -> Ok (LLVM.ConstString (s, uint32 s.Length, llfalse))
    | Ast.ConstBool b -> failwith ""
    

let rec codegen = function 
    | Ast.Const x -> codegenConst x
    | Ast.Variable name -> 
        match Map.tryFind name env with 
        | Some x -> Ok x 
        | None -> Error [ sprintf "unbound variable %s" name ]
    | Ast.Binary (left, op, right) -> 
        result { let! l = codegen left 
                 let! r = codegen right 
                 match op with 
                 | "+" -> return LLVM.BuildFAdd (builder, l, r, "addtmp") 
                 | "-" -> return LLVM.BuildFSub (builder, l, r, "subtmp") 
                 | "*" -> return LLVM.BuildFMul (builder, l, r, "multmp") 
                 | "/" -> return LLVM.BuildFAdd (builder, l, r, "divtmp")
                 // TODO : other operators
                 | _ -> return! Error [ sprintf "Invalid binary operator %s" op ] }
    | Ast.Application (Ast.Variable callee, arg) -> // Handle lambda by creating a global function 
        result {let callee' = 
                    match lookupFunction callee M with 
                    | Some callee -> Ok callee
                    | None -> Error [ sprintf "Undefined function %s" callee] 
                let! callee' = callee' 
                let args = LLVM.GetParams callee' 
                if Array.length args = Array.length [| arg |]
                then return LLVM.BuildCall (builder, callee', args, "calltmp") 
                else return! Error [ sprintf "Invalid number of arguments applied to %s" callee ] }
        



// let test () = 
//     let paramTypes : LLVMTypeRef array = 
//         [  LLVM.Int32Type () 
//            LLVM.Int32Type () ]
//         |> Array.ofList
//     let retType : LLVMTypeRef = 
//         LLVM.FunctionType (LLVM.Int32Type (), paramTypes, false)
//     let sum : LLVMValueRef = 
//         LLVM.AddFunction (M, "add", retType)
//     let entry : LLVMBasicBlockRef = 
//         LLVM.AppendBasicBlock (sum, "entry")
    
//     let builder : LLVMBuilderRef = 
//         LLVM.CreateBuilder ()
    
//     LLVM.PositionBuilderAtEnd (builder, entry)

//     let fst, snd = LLVM.GetParam (sum, 0u), LLVM.GetParam (sum, 1u) 
//     let tmp = LLVM.BuildAdd (builder, fst, snd, "tmp")
//     LLVM.BuildRet (builder, tmp) 
//         |> ignore

//     LLVM.WriteBitcodeToFile (M, "sum.bc")
//     |> function
//         | 0 -> printfn "File written succesfully"
//         | _ -> printfn "An error occured while wtiting bitcode to file" 

//     ()