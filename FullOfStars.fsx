
type fruit = 
    | Peach 
    | Apple 
    | Pear 
    | Lemon
    | Fig 

type tree = 
    | Bud 
    | Flat of fruit * tree
    | Split of tree * tree 

let rec flatOnly = function 
    | Bud -> true 
    | Flat (f, t) -> flatOnly t
    | Split (a, b) -> false

let rec splitOnly = function 
    | Bud -> true 
    | Flat (f, t) -> false 
    | Split (a, b) -> splitOnly a && splitOnly b

let a = Split (Split (Bud, Flat (Fig, Split (Bud, Bud))), 
               Split (Flat (Apple, Flat (Pear, Bud)), Bud))
let b = Split (Bud, Bud)
splitOnly b

let rec containsFruit = function
    | Bud -> false
    | Flat (f, t) -> true 
    | Split (a, b) -> containsFruit a || containsFruit b

let containsFruit2 x = splitOnly x |> not 

let rec height = function
    | Bud -> 0 
    | Flat (f, t) -> 1 + height t
    | Split (x, y) -> 1 + max (height x) (height y)

let z = Split (Bud, Flat (Fig, Flat (Fig, Bud)))
height z

let eqFruit = function 
    | Peach, Peach -> true
    | Apple, Apple -> true
    | Pear, Pear -> true
    | Lemon, Lemon -> true
    | Fig, Fig -> true 
    | a, b -> false 

let rec substInTree = function
    | n, a, Bud -> Bud 
    | n, a, Flat (f, t) ->
        if eqFruit (f, a) 
        then Flat (n, substInTree (n, a, t))
        else Flat (f, substInTree (n, a, t))
    | n, a, Split (x, y) -> 
        Split (substInTree (n, a, x), substInTree (n, a, y))

let rec occurs = function
    | a, Bud -> 0
    | a, Flat (f, t) -> 
        if eqFruit (f, a) 
        then 1 + occurs (a, t)
        else occurs (a, t)
    | a, Split (x, y) -> occurs (a, x) + occurs (a, y)  

type 'a slist = 
    | Empty 
    | Scons of 'a sexp * 'a slist
and 'a sexp = 
    | Atom of 'a 
    | Slist of 'a slist

let rec occursInList = function 
    | a, Empty -> 0
    | a, Scons (x, y) -> occursInSexp (a, x) + occursInList (a, y)
and occursInSexp = function 
    | a, Atom b -> 
        if eqFruit (a, b) then 1 else 0
    | a, Slist l -> occursInList (a, l)

let rec substInSlist = function 
    | n, a, Empty -> Empty
    | n, a, Scons (x, y) -> Scons (substInSexp (n, a, x), substInSlist (n, a, y))
and substInSexp = function
    | n, a, Atom c -> 
        if eqFruit (a, c) 
        then Atom n 
        else Atom c
    | n, a, Slist l -> Slist (substInSlist (n, a, l))

(*
let rec remFromSlist = function 
    | a, Empty -> Empty
    | a, Scons (b, l) ->
        let p = remFromSexp (a, b)
        match p with 
        | Empty -> remFromSlist (a, l)
        | Scons (a', e) -> Scons (a', remFromSlist (a, l))
and remFromSexp = function 
    | a, Atom x -> 
        if eqFruit (a, x) 
        then Empty
        else Scons (Atom x, Empty)
    | a, Slist l -> remFromSlist (a, l)
*) 

let eqFruitInAtom = function 
    | a, Atom s -> eqFruit (a, s)
    | a, Slist l -> false 

let rec remFromSlist = function
    | a, Empty -> Empty
    | a, Scons (Atom x, l) ->    
        if eqFruit (a, x) then remFromSlist (a, l)
        else Scons (Atom x, remFromSlist (a, l))
    | a, Scons (Slist l, rest) -> 
        Scons (Slist (remFromSlist (a, l)), remFromSlist (a, rest))
and remFromSexp = function
    | a, Atom x -> Atom x 
    | a, Slist l -> Slist (remFromSlist (a, l))