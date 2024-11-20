open Ast

type exprval = Bool of bool | Nat of int

let rec string_of_expr = function
    True -> "True"
  | False -> "False"
  | Zero -> "0" 
  | If(e1,e2,e3) -> "If(" ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ "," ^ (string_of_expr e3) ^ ")"
  | Not(e1) -> "Not(" ^ (string_of_expr e1) ^ ")"
  | And(e1, e2) -> "And(" ^ (string_of_expr e1) ^ (string_of_expr e2) ^ ")"
  | Or(e1, e2) -> "Or(" ^ (string_of_expr e1) ^ (string_of_expr e2) ^ ")"
  | Succ(e1) -> "Succ(" ^ (string_of_expr e1) ^ ")"
  | Pred(e1) -> "Pred(" ^ (string_of_expr e1) ^ ")"
  | IsZero(e1) -> "IsZero(" ^ (string_of_expr e1) ^ ")"

let string_of_val = function
    Bool b -> string_of_bool(b)
  | Nat n -> string_of_int(n)

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

exception NoRuleApplies

let rec trace1 = function
  | If(True, e1, _) -> e1
  | If(False, _, e2) -> e2
  | If(e1, e2, e3) -> If(trace1 e1, e2, e3)
  | Not(True) -> False
  | Not(False) -> True
  | Not(e1) -> Not(trace1 e1)
  | And(True, e2) -> e2
  | And(False, _) -> False
  | And(e1, e2) -> And(trace1 e1, e2)
  | Or(True, _) -> True
  | Or(False, e2) -> e2
  | Or(e1, e2) -> Or(trace1 e1, e2)  
  | Succ(e1) -> Succ(trace1 e1)
  | Pred(Succ(e1)) -> e1
  | Pred(e1) -> Pred(trace1 e1)
  | IsZero(Zero) -> True
  | IsZero(Succ(_)) -> False 
  | IsZero(e1) -> IsZero(trace1 e1)
  | _ -> raise NoRuleApplies

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]

let rec eval = function
  True -> Bool true
| False -> Bool false
| If(e1, e2, e3) -> 
    (match eval e1 with
    | Bool b1 -> if b1 then eval e2 else eval e3
    | _ -> failwith "If condition must be a boolean value")
| Not(e1) -> 
    (match eval e1 with
    | Bool b1 -> Bool (not b1)
    | _ -> failwith "Not argument must be a boolean value")
| And(e1, e2) -> 
    (match (eval e1, eval e2) with
    | (Bool b1, Bool b2) -> Bool (b1 && b2)
    | _ -> failwith "And arguments must be boolean values")
| Or(e1, e2) -> 
    (match (eval e1, eval e2) with
    | (Bool b1, Bool b2) -> Bool (b1 || b2)
    | _ -> failwith "Or arguments must be boolean values")
| Zero -> Nat 0
| Succ(e1) -> 
    (match eval e1 with
    | Nat n1 -> Nat (n1 + 1)
    | _ -> failwith "Succ argument must be a natural value")
| Pred(e1) -> 
    (match eval e1 with
    | Nat n1 -> if n1 > 0 then Nat (n1 - 1) else failwith "Pred argument must be greater than 0"
    | _ -> failwith "Pred argument must be a natural value")
| IsZero(e1) -> 
    (match eval e1 with
    | Nat n1 -> if n1 > 0 then Bool false else Bool true
    | _ -> failwith "IsZero argument must be a natural value")