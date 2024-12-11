open Ast
open Types

(* Stato globale per memorizzare le variabili *)
let state = Hashtbl.create 10 

(* Funzioni per aggiornare e ottenere lo stato delle variabili *)
let update_state var value = 
  Hashtbl.replace state var value

let get_state var = 
  try Hashtbl.find state var 
  with Not_found -> raise (UnboundVar("Unbound variable: " ^ var))

(* Parsing dell'input e generazione del comando *)
let parse (s : string) : conf =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  Cmd(ast, fun _ -> Nat 0)  (* Restituisci una struttura conf che include cmd e uno stato iniziale *)

(* Valutazione delle espressioni *)
let rec eval_expr = function
  | True -> Bool true
  | False -> Bool false
  | Var(s) -> 
    (try 
       get_state s
     with 
       UnboundVar _ -> failwith "Used variables must be initialized")
  | Const(i) -> Nat i
  | Not(e) -> 
    (match eval_expr e with
      | Bool b1 -> Bool (not b1)
      | _ -> failwith "Not argument must be a boolean value")
  | And(e1, e2) -> 
    (match (eval_expr e1, eval_expr e2) with
      | (Bool b1, Bool b2) -> Bool (b1 && b2)
      | _ -> failwith "And arguments must be boolean values")
  | Or(e1, e2) -> 
    (match (eval_expr e1, eval_expr e2) with
      | (Bool b1, Bool b2) -> Bool (b1 || b2)
      | _ -> failwith "Or arguments must be boolean values")
  | Add(e1, e2) -> 
    (match (eval_expr e1, eval_expr e2) with
      | (Nat n1, Nat n2) -> Nat (n1 + n2)
      | _ -> failwith "Add arguments must be natural values")
  | Sub(e1, e2) -> 
    (match (eval_expr e1, eval_expr e2) with
      | (Nat n1, Nat n2) -> Nat (n1 - n2)
      | _ -> failwith "Sub arguments must be natural values")
  | Mul(e1, e2) -> 
    (match (eval_expr e1, eval_expr e2) with
      | (Nat n1, Nat n2) -> Nat (n1 * n2)
      | _ -> failwith "Mul arguments must be natural values")
  | Eq(e1, e2) -> 
    (match (eval_expr e1, eval_expr e2) with
      | (Bool b1, Bool b2) -> Bool (b1 = b2)
      | (Nat n1, Nat n2) -> Bool (n1 = n2)
      | _ -> failwith "Eq arguments must be boolean or natural values")
  | Leq(e1, e2) -> 
    (match (eval_expr e1, eval_expr e2) with
      | (Nat n1, Nat n2) -> Bool (n1 <= n2)
      | _ -> failwith "Leq arguments must be natural values")

(* Tracciamento delle riduzioni di un comando *)
let rec trace1 = function
  | Not(True) -> False
  | Not(False) -> True
  | Not(e1) -> Not(trace1 e1)
  | And(True, e2) -> e2
  | And(False, _) -> False
  | And(e1, e2) -> And(trace1 e1, e2)
  | Or(True, _) -> True
  | Or(False, e2) -> e2
  | Or(e1, e2) -> Or(trace1 e1, e2) 
  | Add(Const n1, Const n2) -> Const (n1 + n2)
  | Add(e1, e2) -> Add(trace1 e1, e2)
  | Sub(Const n1, Const n2) -> Const (n1 - n2)
  | Sub(e1, e2) -> Sub(trace1 e1, e2)
  | Mul(Const n1, Const n2) -> Const (n1 * n2)
  | Mul(e1, e2) -> Mul(trace1 e1, e2)
  | Eq(Const n1, Const n2) -> if n1 = n2 then True else False
  | Eq(e1, e2) -> Eq(trace1 e1, e2)
  | Leq(Const n1, Const n2) -> if n1 <= n2 then True else False
  | Leq(e1, e2) -> Leq(trace1 e1, e2)
  | _ -> raise NoRuleApplies



(* Tracciamento dell'esecuzione per un numero di passi *)
let rec trace n e = 
  if n <= 0 then [e]  
  else 
    try
      let e' = trace1 e
      in e::(trace (n-1) e')
    with NoRuleApplies -> [e]