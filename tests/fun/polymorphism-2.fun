// val ( closure ( .Map , x -> let ( y = x ) and .Bindings in y | .Cases , .Bindings , .Vals ) )

// testing polymorphism

let f = fun x -> let y = x in y
in (fun x -> f) 7
