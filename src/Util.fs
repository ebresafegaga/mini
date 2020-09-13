[<AutoOpen>]
module Util 

let (++) list a = list @ [a]

let ($) = (<|)

type Map'<'a, 'b> = ('a * 'b) list 

module Map = 
    let union m1 m2 = 
        let folder k v s = 
            match Map.tryFind k s with 
            | Some _ -> s
            | None -> Map.add k v s
        Map.foldBack folder m1 m2