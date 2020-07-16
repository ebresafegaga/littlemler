
type ShishKebab = 
    | Skewer
    | Onion of ShishKebab
    | Lamb of ShishKebab
    | Tomato of ShishKebab

let a = Onion Skewer
let b = Onion (Lamb (Onion Skewer))

let rec onlyOnions = function 
    | Skewer -> true 
    | Onion x -> onlyOnions x
    | Lamb x -> false 
    | Tomato x -> false 

onlyOnions (Onion (Onion (Skewer)))

let rec isVegetarian = function 
    | Skewer -> true 
    | Onion x -> isVegetarian x
    | Lamb x -> false 
    | Tomato x -> isVegetarian x


type 'a shish = 
    | Bottom of 'a 
    | Onion of 'a shish
    | Lamb of 'a shish
    | Tomato of 'a shish

type rod = Dagger | Fork | Sword 

type plate = GoldPlate | SilverPlate | BrassPlate

let c = Onion (Tomato (Bottom (Dagger)))

let rec isVeggie = function 
    | Bottom x -> true 
    | Onion x -> isVeggie x
    | Lamb x -> false
    | Tomato x -> isVeggie x  

let rec whatBottom = function 
    | Bottom x -> x 
    | Onion x | Lamb x | Tomato x -> whatBottom x
