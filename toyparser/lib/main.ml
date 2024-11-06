open Ast

(* parse : string -> ast *)

let parse (s : string) : ast =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read_token lexbuf in
  ast

type result = int

let string_of_result n = match n with
| Some n -> string_of_int n
| _ -> "NaN"
    
(* eval : ast -> result *)
    
let rec eval = function
  | Const (n) -> Some n
  | Neg (e1) -> 
      (match eval e1 with
       | Some n -> Some (-n)
       | _ -> None)
  | Add (e1, e2) -> 
      (match eval e1, eval e2 with
       | Some n1, Some n2 -> Some (n1 + n2)
       | _ -> None)
  | Sub (e1, e2) -> 
      (match eval e1, eval e2 with
       | Some n1, Some n2 -> Some (n1 - n2)
       | _ -> None)
  | Mul (e1, e2) -> 
      (match eval e1, eval e2 with
       | Some n1, Some n2 -> Some (n1 * n2)
       | _ -> None)
  | Div (e1, e2) -> 
      (match eval e1, eval e2 with
       | Some n1, Some n2 when n2 <> 0 -> Some (n1 / n2)
       | _ -> None)

                    
