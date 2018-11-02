open System
open Parser


type Expr =
 | Variable of char
 | Abstraction of char * Expr
 | Application of Expr * Expr

let expr, exprImpl = recparser()

let variable : Parser<Expr> = pletter |>> (fun x -> Variable x)

let handleLeft = pleft (pletter) (pchar '.') 

let handleMid = pseq handleLeft expr (fun (x,y) -> Abstraction (x,y))

///Remove the parens and make an abstraction out of the insides
let abstraction : Parser<Expr> = pbetween (pstr "(L") (pchar ')') (handleMid)

///Remove the parens and make a new application with the insides
let application: Parser<Expr> = pbetween (pchar '(') (pchar ')') (pseq expr expr (fun (x,y) -> Application (x,y)))

/// Order here goes from base case to more flexible cases of exprs
exprImpl :=  variable <|> abstraction <|> application


/// Parse takes in a <param name = "string">lambda calculus string</param> and <returns> and Expr type</returns> and expression as defined above.
let parse (s: string) : Expr option =
 let x = expr (prepare s)
 match x with
 | Success (x,_) ->Some x
 | Failure -> None


/// A utility function that takes in the output out of parse and represents the abstractions and applications in a <return> string</string>.
let rec prettyprint (e: Expr) : string =
  match e with
  | Variable e -> "Variable("  + e.ToString() + ")"
  | Abstraction(f,e)-> "Abstraction(Variable(" +  f.ToString() + "), " + prettyprint e + ")"
  | Application (e,e2) -> "Application(" + prettyprint e + prettyprint e2 + ")"

[<EntryPoint>]
let main argv =
  let x = argv.[0]
  let asto = parse x
  match asto with
   | Some ast -> printfn "%A" (prettyprint ast)
   | None -> printfn "Invalid program."
  0
  