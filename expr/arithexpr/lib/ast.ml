type expr =
    True
  | False
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | If of expr * expr * expr
  | Zero
  | Succ of expr
  | Pred of expr
  | IsZero of expr

let is_value : expr -> bool = function
  | True -> true
  | False -> true
  | _ -> false

let rec is_nv : expr -> bool = function
  | Zero -> true
  | Succ(e1) when is_nv (e1) -> true
  | _ -> false