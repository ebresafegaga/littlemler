

type pizza = 
    | Crust 
    | Cheese of pizza
    | Onion of pizza 
    | Anchovy of pizza
    | Sausage of pizza

let a = Anchovy (Onion (Anchovy (Anchovy (Cheese Crust))))

let rec removeAnchovy = function 
    | Crust -> Crust
    | Cheese x -> Cheese (removeAnchovy x)
    | Onion x -> Onion (removeAnchovy x)
    | Anchovy x -> removeAnchovy x
    | Sausage x -> Sausage (removeAnchovy x)

let rec topAnchovyWithCheese = function
    | Crust -> Crust
    | Cheese x as p -> topAnchovyWithCheese p 
    | Onion x as p -> topAnchovyWithCheese p
    | Anchovy x as p -> Cheese (topAnchovyWithCheese p)
    | Sausage x as p -> topAnchovyWithCheese p

let substAnchovyByCheese = topAnchovyWithCheese >> removeAnchovy

