module Typing

let typ = 
    { new FSharpTypeFunc () with 
          member x.Specialize () = failwith "" }