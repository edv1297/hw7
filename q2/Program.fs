// Learn more about F# at http://fsharp.org

open System

type Tree<'a> = 
| Leaf of 'a
| Node of Tree<'a> * Tree<'a>



[<EntryPoint>]


let main argv =

/// treduce is a function that takes in a function and a tree and applies the function to the
/// the subtrees of t. Our function <param name "f"> takes in two values and applies an operation to those two values. </param>
/// Thus f could be something like f x y = x + y where x and y are both integers. Our tree <param name = "t"> matches our Tree type definition </param>
/// where we have a tree with two leaves and we extract the value of the leaves and apply the function with those 
/// values. We should <returns> get a return value of the same type that we input into the function</returns>

    let rec treduce (f:'a->'a->'a) (t:Tree<'a>) = 
        match t with
        | Leaf a -> a
        | Node(a, b) -> f (treduce f a )(treduce f b)

    let f x y =  x+y
    let r = treduce f (Node(Node(Leaf 1, Leaf 2), Leaf 3))
    printfn "Return from treduce %A" r 
    0 // return an integer exit code
