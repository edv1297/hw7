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

let abstraction : Parser<Expr> = pbetween (pstr "(L") (pchar ')') (handleMid)

//let abstraction : Parser<Expr> = pseq pbetween (pstr "(L") (pchar ')') (handleMid)

let application: Parser<Expr> = pseq expr expr (fun (x,y) -> Application (x,y))

exprImpl := variable <|> abstraction <|> application

let parse (s: string) : Expr option =
 let x = expr (prepare s)
 match x with
 | Success (x,_) -> Some x
 | Failure -> None

(*
let prettyprint (e: Expr) : string =
  match x with
 | Some x -> x.ToString()
 | None -> "Invalid Program."
*)

[<EntryPoint>]
let main argv =
 let x = argv.[0]
 let y = parse x
 printfn "%A" y
 //printfn "%s" (prettyprint (expr (prepare x)))
 0