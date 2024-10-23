open Toylexer.Token
open Toylexer.Main

let%test "test_frequencies_1" =
  lexer "x=y; x=x+1" |> frequency 3 = [(CTOK "x", 3); (ASSIGN, 2); (CTOK "y", 1)]

(* YOUR TESTS HERE *)

(* Test frequenze con un solo token ripetuto *)
let%test "test_frequencies_2" =
  lexer "x x x x" |> frequency 1 = [(CTOK "x", 4)]

(* Test frequenze con token diversi e limite n maggiore del numero di token unici *)
let%test "test_frequencies_3" =
  lexer "x=y; z=x+y" |> frequency 5 = [(CTOK "x", 2); (ASSIGN, 2); (CTOK "y", 2); (SEQ, 1); (CTOK "z", 1)]

(* Test frequenze con limite n minore del numero di token unici *)
let%test "test_frequencies_4" =
  lexer "a=b; a=b+c" |> frequency 2 = [(BTOK "a", 2); (ASSIGN, 2)]

(* Test ATOK - capital letter followed by letters or digits *)
let%test "test_atok" =
  lexer "MyVar123 = 45;" = [ATOK "MyVar123"; ASSIGN; DTOK "45"; SEQ; EOF]

(* Test BTOK - lowercase vowels *)
let%test "test_btok" =
  lexer "a e i o u" = [BTOK "a"; BTOK "e"; BTOK "i"; BTOK "o"; BTOK "u"; EOF]

(* Test CTOK - strings with at most one vowel *)
let%test "test_ctok" =
  lexer "cat dog" = [CTOK "cat"; CTOK "dog"; EOF]

(* Test DTOK - decimal numbers, including negative and floating point *)
let%test "test_dtok" =
  lexer "-3.14 42.0 -0.5" = [DTOK "-3.14"; DTOK "42.0"; DTOK "-0.5"; EOF]

(* Test ETOK - hexadecimal numbers *)
let%test "test_etok" =
  lexer "0x1A3F 0Xff" = [ETOK "0x1A3F"; ETOK "0Xff"; EOF]

(* Test mixed tokens with priorities *)
let%test "test_mixed_tokens_priority" =
  lexer "X123 a i cat 0x1F -7.5 0XAB" =
  [ATOK "X123"; BTOK "a"; BTOK "i"; CTOK "cat"; ETOK "0x1F"; DTOK "-7.5"; ETOK "0XAB"; EOF]

(* Test combination of tokens with complex string input *)
let%test "test_complex_input" =
  lexer "A=B; 0xFF = -3.2;" =
  [ATOK "A"; ASSIGN; ATOK "B"; SEQ; ETOK "0xFF"; ASSIGN; DTOK "-3.2"; SEQ; EOF]