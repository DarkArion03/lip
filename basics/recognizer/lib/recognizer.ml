let lang1 l = match l with
| [] -> false
| _ -> List.fold_left (fun acc x -> acc && (x == '0' || x == '1')) true l;;

let rec alt_lang1 l = match l with
| [] -> false
| [x] -> (x == '0' || x == '1')
| x::l' -> (x == '0' || x == '1') && alt_lang1 l';;

let rec alt_alt_lang1 l = match l with
| [] -> false
| x::l' -> (x == '0' || x == '1') && (l' == [] || alt_alt_lang1 l');;

let lang2 l = match l with 
| [] -> true
| x::l' -> match x with
  | '0' -> List.fold_left (fun acc x -> acc && x == '1') true l'
  | _ -> List.fold_left (fun acc x -> acc && x == '1') true l;;

let rec last l = match l with 
| [] -> failwith ""
| [x] -> x
| _::l' -> last l';;

let lang3 l = match l with 
| [] -> false
| [_] -> false
| x::l' -> x == '0' && List.fold_left (fun acc x -> acc && (x == '0' || x == '1')) true l' && last l' == '0';;

let rec countOnes l = match l with 
| [] -> 0
| x::l' -> match x with
  | '1' -> 1 + countOnes l'
  | _ -> countOnes l';;

let lang4 l = if (countOnes l = 2) 
  then
    List.fold_left (fun acc x -> acc && (x == '0' || x == '1')) true l
  else
    false;;

let rec lang5 l = match l with
  | [] -> false 
  | [_] -> false
  | x::y::l' -> (x = '0' || x = '1') && x = y && (lang5 l' || l' = []);; 
    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
  
