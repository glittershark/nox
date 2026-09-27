open! Core

let par = Parallel.sequential

let test_eval expr =
  let res = Nox.eval par { desc = expr } in
  print_s [%sexp (res : Nox.Value.t)]
;;

let%expect_test "literal integer" =
  test_eval (Lit (Integer 1));
  [%expect {| (Integer 1) |}]
;;

let test_parse_and_eval expr_string =
  let expr = Nox.Parser.parse_string expr_string in
  test_eval expr.desc
;;

let%expect_test "literal integer, parsed" =
  test_parse_and_eval "1";
  [%expect {| (Integer 1) |}]
;;
