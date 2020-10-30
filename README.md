# Mini

![Build](https://github.com/ebresafegaga/mini/workflows/Build/badge.svg)

A purely functional ML like language with Hindley-Milner type inference.

WIP.


# Basic syntax 

Let bindings 

```sml
let a = 10
a : number

let b = [] 
b : List<a>

```

---

Function bindings 

```sml
let f a b = [a, b]

f : a -> a -> List<a>
```
This is also the same as: 
```sml
let f = fn a b => [a, b] 

let f = fn a => fn b => [a, b]

f : a -> a -> List<a>
```

---

Currying 

```sml
let g = f 10 

g : number -> List<number>
```

---

Recursive definitions 

```sml
let rec fact n = 
  if n <= 0 then 1 else n * fact (n-1)
  
fact : number -> number 
```



