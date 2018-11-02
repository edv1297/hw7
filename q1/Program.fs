///Response to 1c
/// F# uses type inference when we do not explicitly declare the types of variables. In order to 
/// stay as generic as possible F# assumes that f may be a function that takes in one type of variable
/// and outputs a different type of variable. For example, assume we have some function that appends to the word
/// "dogs" and takes in an integer. This function would return a string although it has taken an integer. F# 
/// uses type inferencing in a manner that would not restrict the possibilites giving our function some flexibility.




open System

//Define tree struct with base case and recursive case
type Tree<'a> = 
| Leaf of 'a
| Node of Tree<'a> * Tree<'a>

[<EntryPoint>]
let main argv =

    /// <summary>
    ///  maptree takes in <param name = "f">function that takes in type 'a and returns  output 'a</param> 
    ///  and <param name = "t"> Represents a binary tree as defined above </param>
    /// </summary>

    let rec maptree (f:'a->'a) (t:Tree<'a>)  = 
       match t with   
       | Leaf a -> Leaf (f a) 
       | (Node (a,b)) ->  Node (maptree f a, maptree f b)


    let f x = x+1
    let resultTree = maptree f (Node(Node(Leaf 2, Leaf 3), Leaf 4))
    printfn "Let's add one to all the values in (Node(Node(Leaf 2, Leaf 3), Leaf 4)): \n %A" resultTree 
    0
