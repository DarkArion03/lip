open Ast
open Types

let parse (s : string) : conf =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let trace1 = function
  | _ -> raise NoRuleApplies

let rec trace n c = match n with
    0 -> [c]
  | _ -> 
    try
      let e' = trace1 c
      in c::(trace (n-1) e')
    with 
      UnboundVar _ | NoRuleApplies -> [c]

let rec eval_expr = function
  True -> Bool true
| False -> Bool false
| Var(_) -> Bool true
| Const(_) -> Bool true
| Not(e) -> 
  (match eval_expr e with
    Bool b1 -> Bool (not b1)
  | _ -> failwith "Not argument must be a boolean value")
| And(e1, e2) -> 
  (match (eval_expr e1, eval_expr e2) with
    (Bool b1, Bool b2) -> Bool (b1 && b2)
  | _ -> failwith "And arguments must be boolean values")
| Or(e1, e2) -> 
  (match (eval_expr e1, eval_expr e2) with
    (Bool b1, Bool b2) -> Bool (b1 || b2)
  | _ -> failwith "Or arguments must be boolean values")
| Add(e1, e2) -> 
  (match (eval_expr e1, eval_expr e2) with
    (Nat n1, Nat n2) -> Nat (n1 + n2)
  | _ -> failwith "Add arguments must be natural values")
| Sub(e1, e2) -> 
  (match (eval_expr e1, eval_expr e2) with
    (Nat n1, Nat n2) -> Nat (n1 - n2)
  | _ -> failwith "Sub arguments must be natural values")
| Mul(e1, e2) -> 
  (match (eval_expr e1, eval_expr e2) with
    (Nat n1, Nat n2) -> Nat (n1 * n2)
  | _ -> failwith "Mul arguments must be natural values")
| Eq(e1, e2) -> 
  (match (eval_expr e1, eval_expr e2) with
    (Bool b1, Bool b2) -> Bool (b1 = b2)
  | (Nat n1, Nat n2) -> Bool (n1 = n2)
  | _ -> failwith "Eq arguments must be boolean or natural values")
| Leq(e1, e2) -> 
  (match (eval_expr e1, eval_expr e2) with
    (Nat n1, Nat n2) -> Bool (n1 <= n2)
  | _ -> failwith "Eq arguments must be natural values")