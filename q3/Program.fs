

open System

[<EntryPoint>]
let main argv =

    let curry (f:('a*'b)->'c) a b = f (a,b)
    let uncurry (f:('a -> ('b -> 'c))) g a= f (g a)

    let c (f: ('a * 'b) -> 'c) : ('a -> ('b -> 'c)) = fun a -> fun b -> f(a,b)
    let ci (f: 'a -> ('b -> 'c)) : (('a * 'b) -> 'c) = fun (a,b) -> f a b

    let f : ((int * int) -> int) = fun (x:int,y:int) -> x+y
    let f1 :(int->(int -> int)) = fun a b -> a - b

    let test1 = ci(c(f))
    let test2  = c(ci(f1))


    0 // return an integer exit code


