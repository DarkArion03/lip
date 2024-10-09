let lang1 l = match l with
| [] -> false
| _ -> List.fold_left (fun acc x -> acc && (x == 0 || x == 1)) true l;;

let rec alt_lang1 l = match l with
| [] -> false
| [x] -> (x == 0 || x == 1)
| x::l' -> (x == 0 || x == 1) && lang1 l';;

let rec alt_alt_lang1 l = match l with
| [] -> false
| x::l' -> (x == 0 || x == 1) && (l' == [] || lang1 l');;

let rec lang2 l = match l with 
| [] -> true
| x::l' -> match x with
  | 0 -> List.fold_left (fun acc x -> acc && x == 1) true l'
  | _ -> List.fold_left (fun acc x -> acc && x == 1) true l;;

let rec last l = match l with 
| [] -> failwith ""
| [x] -> x
| _::l' -> last l';;

let rec lang3 l = match l with 
| [] -> false
| [_] -> false
| x::l' -> x == 0 && List.fold_left (fun acc x -> acc && (x == 0 || x == 1)) true l' && last l' == 0;;

let lang4 _ = failwith ""

let lang5 _ = failwith ""
    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
  
