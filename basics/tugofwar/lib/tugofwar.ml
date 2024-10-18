(* tokens *)
type token = A | B | X

(* val toklist_of_string : string -> token list *)
(* toklist_of_string s transforms the string s into a list of tokens *)
(* Hint: use the function explode in bin/main.ml to convert a string to a char list *)

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;

let rec aux l = match l with 
  | [] -> []
  | x::l' -> match x with
    | 'A' -> A::aux l'
    | '=' -> X::aux l'
    | 'B' -> B::aux l'
    | _ -> failwith "Unexpected Token";;

let toklist_of_string s = 
  let l = explode s in aux l;;

(* val valid : token list -> bool *)
(* valid l is true when l is a list of tokens in the language A* X* B* *)
    
let valid l = 
  List.fold_left (fun acc x -> acc && (x == A || x == X || x == B)) true l;;

(* val win : token list -> token *)
(* win l determines the winner of a tug of war game. X means tie *)

let win l = 
  let countA = List.fold_left (fun acc x -> if x == A then acc+1 else acc) 0 l in
  let countB = List.fold_left (fun acc x -> if x == B then acc+1 else acc) 0 l in
  if countA > countB then A
  else if countA == countB then X
  else B;;

(* val string_of_winner : token -> string *)
let string_of_winner w = match w with
  | A -> "Team A"
  | X -> "Tie"
  | B -> "Team B";;
