
// entree
type meza = 
    | Shrimp 
    | Calamari 
    | Escargots 
    | Hummus 

type main = 
    | Steak 
    | Ravioli
    | Chicken 
    | Eggplant 

type salad = 
    | Green 
    | Cucumber 
    | Greek 

type dessert = 
    | Sundae 
    | Mousse 
    | Torte 

let meal = (Calamari, Ravioli, Greek, Sundae)
let mine = (Hummus, Steak, Green, Torte)
let dan = (Shrimp, Sundae)

let addSteak (x : meza) = (x, Steak)

let eqMain = function 
    | Steak, Steak -> true 
    | Ravioli, Ravioli -> true
    | Chicken, Chicken -> true
    | Eggplant, Eggplant -> true
    | x, y -> false 

// abridged
let hasSteak = function
    | (x:meza), Steak, (y:dessert) -> true 
    | x, y, z -> false 