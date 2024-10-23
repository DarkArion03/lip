open Token
    
(* tokenize : Lexing.lexbuf -> LexingLib.Token.token list *)

let rec tokenize lexbuf =
  match Lexer.read_token lexbuf with
    EOF -> [EOF]
  | t -> t::(tokenize lexbuf)

(* lexer : string -> LexingLib.Token.token list *)

let lexer (s : string) =
  let lexbuf = Lexing.from_string s in
  tokenize lexbuf

(* string_of_tokenlist : token list -> string *)
    
let string_of_tokenlist tl = 
  List.fold_left (fun s t -> s ^ (string_of_token t ^ (if t=EOF then "" else " "))) "" tl

(* string_of_frequencies : (token * int) list -> string *)
    
let string_of_frequencies fl =
  List.fold_left (fun s (t,n) -> s ^ ((string_of_token t) ^ " -> " ^ string_of_int n ^ "\n")) "" fl

(* frequency : int -> 'a list -> ('a * int) list *)
let frequency n l = 
  let sort_list l = 
    List.sort (fun (_, n) (_, m) -> m-n) l 
  in
  let rec update_freq x f = match f with 
  | [] -> [(x, 1)]
  | (y, m)::f' -> if x = y then (y, m+1)::f' else (y, m)::(update_freq x f')
  in 
  let rec freq_list l f = match l with
  | [] -> f
  | x::l' -> let f' = update_freq x f in freq_list l' f';  
  in 
  let rec cut n l = match n, l with
  |0, _ -> []
  |_, [] -> []
  |_, x::l' -> x::(cut (n-1) l');
  in 
  cut n (sort_list (freq_list l []));;