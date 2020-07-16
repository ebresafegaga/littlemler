
exception Too_small

let isZero n = n = 0

let pred n = 
    if isZero n then raise Too_small
    else n - 1

let succ n = n + 1

let rec plus (n, m) = 
    if isZero n then m 
    else succ (plus(pred (n), m))

type num = Zero | One_more_than of num 

let isZeroNum = function
    | Zero -> true 
    | One_more_than x -> false

let predNum = function
    | Zero -> raise Too_small
    | One_more_than x -> x

let succNum x = One_more_than x

let rec plusNum (n, m) = 
    if isZeroNum n then m 
    else succNum (plusNum (predNum (n), m))

let three = One_more_than (One_more_than (One_more_than (Zero)))
let two = One_more_than (One_more_than (Zero))

let five = plusNum (three, two)

