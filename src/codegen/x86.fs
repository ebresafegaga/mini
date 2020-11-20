module Codegen.x86

type Reg = 
    | EAX 
    | EDX

type Value = 
    | Const of int 
    | Reg of 
        Register : Reg * 
        Displacement : int 

type Instr = 
    | Mov of Reg * Value 
    | Push of Reg 
    | RET


let emit c = 
    [ Mov (EAX, Reg (EDX, 4)) 
      Push EAX 
      RET ]