open! Core

let test_eval expr =
  let res = Nox.eval Parallel.sequential { desc = expr } in
  print_s [%sexp (res : Nox.Value.t)]
;;

let%expect_test "literal integer" =
  test_eval (Lit (Integer 1));
  [%expect {||}]
;;
