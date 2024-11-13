open BoolexprLib.Main
open BoolexprLib.Ast

let test_eval expr exp_result =
  (expr |> parse |> eval) = exp_result

(* ### Unit tests for task 4 *)

let%test "test_eval_1" =
  parse "false" |> eval = false

let%test "test_eval_2" =
  parse "true" |> eval = true

let%test "test_eval_3" =
  parse "if true then false else true" |> eval = false

let%test "test_eval_4" =
  parse "if false then false else true" |> eval = true

let%test "test_eval_5" =
  parse "if true then (if true then false else true) else (if true then true else false)" |> eval = false

let%test "test_eval_6" =
  parse "if (if false then false else false) then (if false then true else false) else (if true then false else true)" |> eval = false

let%test "test_eval_7" =
  parse "if (if (if false then false else false) then (if false then true else false) else (if true then false else true)) then (if false then true else false) else (if true then false else true)" |> eval = false

(* ### Unit tests for task 5 *)

let%test "test_trace1_progress_1" =
  let expr = parse "if true then false else true" in
  trace1 expr <> expr

let%test "test_trace1_progress_1" =
  let expr = parse "if false then true else false" in
  trace1 expr <> expr

let%test "test_trace1_progress_1" =
  let expr = parse "if (if true then false else true) then true else false" in
  trace1 expr <> expr

let%test "test_trace1_no_progress_1" =
  let expr = parse "true" in
  try
    let _ = trace1 expr in false
  with NoRuleApplies -> is_value expr

let%test "test_trace1_no_progress_2" =
  let expr = parse "if true then false else true" in
  try
    let _ = trace1 expr in true
  with NoRuleApplies -> is_value expr

let%test "test_trace_less_than_10_steps" =
  let expr = parse "if (if false then false else false) then (if false then true else false) else (if true then false else true)" in
  let trace_steps = trace expr in
  List.length trace_steps <= 10

let%test "test_eval_8" =
  parse "true && true" |> eval = true

let%test "test_eval_9" =
  parse "true && false" |> eval = false

let%test "test_eval_10" =
  parse "false && true" |> eval = false

let%test "test_eval_11" =
  parse "false && false" |> eval = false

let%test "test_eval_12" =
  parse "true || true" |> eval = true

let%test "test_eval_13" =
  parse "true || false" |> eval = true

let%test "test_eval_14" =
  parse "false || true" |> eval = true

let%test "test_eval_15" =
  parse "false || false" |> eval = false
