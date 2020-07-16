#load "CouplesAreMagnificentToo.fsx"


let identity a = a

let trueMaker x = true 

type bool_or_int =
    | Hot of bool 
    | Cold of int 

let hotMaker x = Hot

let help f = Hot (trueMaker (if trueMaker 5 then f else trueMaker))

type chain = Link of int * (int -> chain)

let rec ints x = Link (x + 1, ints)

let rec skip x = Link (x + 2, skip)

let dividesEvenly x y = (x % y) = 0

(* does 5 have more multiples than 7 or are they equal numbers? *)
let isMod5Or7 x = (dividesEvenly x 5) || (dividesEvenly x 7)

let rec someInts x = 
    if isMod5Or7 (x + 1) 
    then Link (x + 1, someInts)
    else someInts (x + 1)

open CouplesAreMagnificentToo

let rec chainItem = function
    | n, Link (i, f) -> 
        if eqInt (n, 1) 
        then i
        else chainItem (n - 1, f i)

chainItem (2, someInts 0)

let rec isPrime n = hasNoDivisors (n, n - 1)
and hasNoDivisors (n, c) = 
    if eqInt (c, 1) then true (* can make this O(sqrt(n)) by replacing 1 with sqrt(n)  *)
    elif dividesEvenly n c then false
    else hasNoDivisors (n, c - 1)

let rec primes n = 
    if isPrime (n + 1) then Link (n + 1, primes)
    else primes (n + 1)

chainItem (12, primes 1) 

for i in 1 .. 10 do printf "%d, " (chainItem (i, primes 1))

let rec fibs n m = Link (n + m, fibs m)

for i in 1 .. 10 do printf "%d, " (chainItem (i, fibs 0 1))