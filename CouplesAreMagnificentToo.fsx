#load "BuildingBlocks.fsx" (* For Num type *)

type 'a pizza = 
    | Bottom 
    | Topping of 'a * 'a pizza

type fish = 
    | Anchovy 
    | Lox 
    | Tuna 

let rec remAnchovy = function 
    | Bottom -> Bottom
    | Topping (Anchovy, p) -> remAnchovy p
    | Topping (f, p) -> Topping (f, remAnchovy p)

let rec remTuna = function 
    | Bottom -> Bottom
    | Topping (Anchovy, p) -> Topping (Anchovy, remTuna p)
    | Topping (Lox, p) -> Topping (Lox, remTuna p)
    | Topping (Tuna, p) -> remTuna p

let rec remFish = function 
    | x, Bottom -> Bottom 
    | Tuna, Topping (Tuna, p) -> remFish (Tuna, p) 
    | Tuna, Topping (nt, p) -> Topping (nt, remFish (Tuna, p))
    | Anchovy, Topping (Anchovy, p) -> remFish (Anchovy, p) 
    | Anchovy, Topping (na, p) -> Topping (na, remFish (Anchovy, p))
    | Lox, Topping (Lox, p) -> remFish (Lox, p) 
    | Lox, Topping (nl, p) -> Topping (nl, remFish (Lox, p))

let eqFish = function 
    | Anchovy, Anchovy -> true 
    | Lox, Lox -> true 
    | Tuna, Tuna -> true 
    | x, y -> false 

eqFish (Anchovy, Tuna)
eqFish (Anchovy, Anchovy)

// abridged
let rec remFish2 = function 
    | x, Bottom -> Bottom
    | x, Topping (t, p) -> 
        if eqFish (t, x) then remFish2 (x, p)
        else Topping (t, remFish2 (x, p))


let eqInt (x: int, y: int) = x = y

let rec remInt = function 
    | x, Bottom -> Bottom
    | x, Topping (t, p) -> 
        if eqInt (t, x) then remInt (x, p)
        else Topping (t, remInt (x, p))

let rec substFish = function 
    | n, a, Bottom -> Bottom 
    | n, a, Topping (t, p) -> 
        if eqFish (t, a) 
        then Topping (n, substFish (n, a, p))
        else Topping (t, substFish (n, a, p))

let rec substInt = function 
    | n, a, Bottom -> Bottom 
    | n, a, Topping (t, p) -> 
        if eqInt (t, a) 
        then Topping (n, substInt (n, a, p))
        else Topping (t, substInt (n, a, p))

open BuildingBlocks

let rec eqNum = function
    | Zero, Zero -> true
    | OneMoreThan x, OneMoreThan y -> eqNum (x, y)
    | x, y -> false