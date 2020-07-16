
module type N = sig
    type number 

    exception Too_small

    val succ: number -> number 
    val pred: number -> number 
    val is_zero: number -> bool 
end 

module NumberAsNum () : N = struct 
    type number = Zero | One_more_than of number

    exception Too_small 

    let is_zero = function
        | Zero -> true 
        | One_more_than x -> false

    let pred = function
        | Zero -> raise Too_small
        | One_more_than x -> x

    let succ x = One_more_than x

end

module NumberAsInt () : N = struct 
    type number = int
    exception Too_small

    let is_zero n = n = 0

    let pred n = 
        if is_zero n then raise Too_small
        else n - 1

    let succ n = n + 1

    let rec plus (n, m) = 
        if is_zero n then m 
        else succ (plus(pred (n), m))
end 

module IntStruct = NumberAsInt ()

module NumStruct = NumberAsNum ()

let f = IntStruct.pred

module type P = sig 
    type number 
    val plus: (number * number) -> number
end


module PlusOverNumber (A_N: N) : P with type number = A_N.number = struct 
    type number = A_N.number

    let rec plus (n, m) = 
        if A_N.is_zero n then m 
        else A_N.succ (plus (A_N.pred (n), m))
end 

module IntArith = PlusOverNumber (IntStruct)
module NumArith = PlusOverNumber (NumStruct)

(* let a = IntArith.plus (1, 2) *)

module type NumbersWithConcealReveal = sig 
    type number 
    exception Too_small
    val succ: number -> number 
    val pred: number -> number 
    val is_zero: number -> bool 
    val plus: (number * number) -> number
    val conceal: int -> number 
    val reveal: number -> int 
end 


module NumberAsIntWithCAndR () : NumbersWithConcealReveal = struct 
    type number = int 

    exception Too_small

    let is_zero n = n = 0

    let pred n = 
        if is_zero n then raise Too_small
        else n - 1

    let succ n = n + 1

    let rec plus (n, m) = 
        if is_zero n then m 
        else succ (plus(pred (n), m))
    let rec plus (n, m) = 
        if is_zero n then m 
        else succ (plus (pred (n), m))

    let conceal n = n

    let reveal n = n
end 

module NumberAsNumWithCAndR () : NumbersWithConcealReveal = struct 
    type number = Zero | One_more_than of number

    exception Too_small 

    let is_zero = function
        | Zero -> true 
        | One_more_than x -> false

    let pred = function
        | Zero -> raise Too_small
        | One_more_than x -> x

    let succ x = One_more_than x

    let rec plus (n, m) = 
        if is_zero n then m 
        else succ (plus (pred (n), m))

    let rec conceal x = 
        if x = 0 then Zero 
        else One_more_than (conceal (x - 1))

    let rec reveal = function
        | Zero -> 0
        | One_more_than x -> 1 + reveal x
end 

module IntStruct2 = NumberAsIntWithCAndR ()
module NumStruct2 = NumberAsNumWithCAndR ()

module IntArith2 = PlusOverNumber (IntStruct2)
module NumArith2 = PlusOverNumber (NumStruct2)

let v = NumStruct2.reveal (NumStruct2.succ (NumStruct2.conceal (2)))

(* wow, magic! *)
let c = NumStruct2.reveal (NumArith2.plus (NumStruct2.conceal (2), NumStruct2.conceal (1)))

module NumberAsInt2 () : N with type number = int = struct 
    type number = int
    exception Too_small

    let is_zero n = n = 0

    let pred n = 
        if is_zero n then raise Too_small
        else n - 1

    let succ n = n + 1

    let rec plus (n, m) = 
        if is_zero n then m 
        else succ (plus(pred (n), m))
end 

module Int2 = NumberAsInt2 ()
module IntArith3 = PlusOverNumber (Int2)

let d = IntArith3.plus (1, 3)

(*
module NumberAsNum2 () : N with type number = num = struct 
    type number = Zero | One_more_than of number
    type num = Zero | One_more_than of num 
    exception Too_small 

    let is_zero = function
        | Zero -> true 
        | One_more_than x -> false

    let pred = function
        | Zero -> raise Too_small
        | One_more_than x -> x

    let succ x = One_more_than x

end

module Num2 = NumberAsNum2 ()
module NumArith3 = PlusOverNumber (Num2)
*)

module type S = sig
    type number1
    type number2
    val similar: (number1 * number2) -> bool 
end 

module Same (A: N) (B: N) : S with type number1 = A.number 
                              and type number2 = B.number = struct 
    type number1 = A.number
    type number2 = B.number

    let rec sim (n, m) =
        if A.is_zero (n) 
        then B.is_zero (m)
        else sim (A.pred (n), B.pred (m))

    let similar (n,  m) =
        try sim (n, m)
        with A.Too_small -> false | B.Too_small -> false     
end

module SimIntXXX = Same (IntStruct2) 
module SimIntNum = Same (IntStruct2) (NumStruct2)

let similar = SimIntNum.similar (IntStruct2.conceal (0), NumStruct2.conceal (0))

module SimNumNum = Same (NumStruct2) (NumStruct2)

let new_plus (x, y) = NumStruct2.reveal (NumStruct2.plus (NumStruct2.conceal (x), NumStruct2.conceal (y)))

module type J = sig 
    val new_plus: (int * int) -> int 
end 

module NewPlusFunctor (AN: NumbersWithConcealReveal) (AP: P with type number = AN.number) : J = struct 
    let new_plus (x, y) = 
        AN.reveal(AP.plus (AN.conceal (x), AN.conceal (y)))
end 

module NPStruct = NewPlusFunctor (NumStruct2) (NumArith2)

module NPStruct = NewPlusFunctor (NumStruct2) (PlusOverNumber (NumStruct2))

module type T = sig 
    type number 
    val times: number * number -> number
end

module TimesOverNumber (AN: N) (AP: P with type number = AN.number) : T with type number = AN.number = struct 
    type number = AN.number 

    let rec times (n, m) = 
        if AN.is_zero n then m
        else times (AN.pred (n), AP.plus (m, m))

    let rec t (n, m ) = 
        if AN.is_zero m then n
        else AP.plus (n, times (n, AN.pred (m)))
end 

