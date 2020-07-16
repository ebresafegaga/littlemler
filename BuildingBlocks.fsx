let ($) = (<|)

type Seasoning = Salt | Pepper

type Num = Zero | OneMoreThan of Num 

let one = OneMoreThan Zero

type 'a OpenFacedSandwich = Bread of 'a | Slice of 'a OpenFacedSandwich

let b = Bread 19
let b' = Bread true 

let b'' = Bread $ OneMoreThan Zero
let b''' = Bread (Bread $ OneMoreThan Zero)
