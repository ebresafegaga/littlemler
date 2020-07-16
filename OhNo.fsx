
// bacon or index 
type box = Bacon | Ix of int

let isBacon = function 
    | Bacon -> true
    | Ix x -> false 

exception NoBacon of int

let rec whereIs = function
    | [] -> raise (NoBacon (0))
    | x :: xs -> 
        if isBacon x then 1 
        else 1 + whereIs xs

let l = [Ix 1; Ix 6; Ix 3; Bacon; Ix 12; Ix 2]
let result = try whereIs l with NoBacon n -> n

exception Out_of_range

let rec listItem = function
    | n, [] -> raise Out_of_range
    | n, x :: xs -> 
        if n = 1 then x 
        else listItem (n-1, xs)

let rec find (n, boxes) = 
    try check (n, boxes, listItem (n, boxes)) with Out_of_range -> find (n / 2, boxes)
and check = function
    | n, boxes, Bacon -> n 
    | n, boxes, Ix i -> find (i, boxes)


let rec path (n, boxes) =
    n :: try chk (boxes, listItem (n, boxes)) with Out_of_range -> path (n / 2, boxes)
and chk = function 
    | boxes, Bacon -> []
    | boxes, Ix x -> path (x, boxes)
