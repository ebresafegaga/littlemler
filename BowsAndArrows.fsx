#load "CouplesAreMagnificentToo.fsx"

type orapl = 
    | Orange 
    | Apple 

let eqOrapl = function 
    | Orange, Orange -> true 
    | Apple, Apple -> true
    | a, b -> false 

let rec substOrapl = function
    | n, a, [] -> [] 
    | n, a, x :: xs -> 
        if eqOrapl (x, a) 
        then n :: substOrapl (n, a, xs)
        else x :: substOrapl (n, a, xs)

let rec subst = function
    | rel, n, a, [] -> [] 
    | rel, n, a, x :: xs -> 
        if rel (x, a) 
        then n :: subst (rel, n, a, xs)
        else x :: subst (rel, n, a, xs)

open CouplesAreMagnificentToo

let l = subst (eqInt, 5, 7, [1;7;5;7;6;7;8])

let lessThan (x, y) = x < y 


let inRange (x, (small, large)) = 
    if small < x 
    then x < large
    else false

let a = subst (inRange, 22, (11, 16), [])

let rec substWithPredicate = function 
    | f, n, [] -> []
    | f, n, x :: xs -> 
        if f x then n :: substWithPredicate (f, n, xs)
        else x :: substWithPredicate (f, n, xs)

let inRangeCurry (small, large) x = 
    if small < x 
    then x < large
    else false

// g is a predicate
let rec substCurry g (n, l) = 
    match l with 
    | [] -> []
    | x :: xs -> 
        if g x then n :: substCurry g (n, xs)
        else x :: substCurry g (n, xs)

let rec combine = function
    | [], l -> l
    | x :: xs, l -> x :: combine (xs, l)

let combineCurry x y = combine (x, y) 

let prefixer123 = combineCurry [1;2;3]

prefixer123 []

let waitingPrefix123 l2 = 
    1 :: combineCurry [2;3] l2


let rec combineStaged = function
    | [] -> id
    | x :: xs -> makeCons (x, combineStaged xs)
and makeCons (a, f) xs =
    a :: f xs

// [1, 2]
// 1 :: [2]
// makeCons (1, combineStaged [2]) -> 
                                    // 2 :: []
                                    // makeCons (2, combineStaged []) -> 
                                                                      // [] -> id
                                    // makeCons (2, id)
                                    // fun xs -> 2 :: id xs
// makeCons (1, fun xs -> 2 :: id xs)
// fun nl -> 1 :: ((fun xs -> 2 :: id xs) nl)

// (fun nl -> 1 :: ((fun xs -> 2 :: id xs) nl)) [3;4]
// 1 :: ((fun xs -> 2 :: id xs) [2;3])
// 1 :: 2 :: [2;3] -- wtf!