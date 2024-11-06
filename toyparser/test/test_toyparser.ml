open Toyparser.Main

let%test "test_eval_1" = parse "1 + 2 + 3 + (1 + 2)" |> eval = Some 9

let%test "test_eval_sub_1" = parse "5 - 3 - 1" |> eval = Some 1
let%test "test_eval_sub_2" = parse "10 - 4 - 2" |> eval = Some 4

let%test "test_eval_mul_div_1" = parse "2 * 3 + 4" |> eval = Some 10
let%test "test_eval_mul_div_2" = parse "10 / 2 - 1" |> eval = Some 4
let%test "test_eval_div_by_zero" = parse "10 / 0" |> eval = None

let%test "test_eval_unary_minus_1" = parse "-1 - 2 - -3" |> eval = Some 0
let%test "test_eval_unary_minus_2" = parse "-5 + 5" |> eval = Some 0

let%test "test_eval_hex_1" = parse "0x01 + 2" |> eval = Some 3
let%test "test_eval_hex_2" = parse "0xA + 0xF" |> eval = Some 25

(* YOUR TESTS HERE *)