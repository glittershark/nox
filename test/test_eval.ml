open! Core
open! Expect_test_helpers_core

let par = Parallel.sequential

let test_eval ?(force = false) ?(print = false) expr =
  let res = Nox.eval par { desc = expr } in
  let res = if force then Nox.Value.deep_force par res else res in
  if print
  then print_endline (Nox.Value.to_string par res)
  else print_s [%sexp (res : Nox.Value.t)]
;;

let%expect_test "literal integer" =
  test_eval (Lit (Integer 1));
  [%expect {| (Integer 1) |}]
;;

let test_parse_and_eval ?force ?print expr_string =
  let expr = Nox.Parser.parse_string expr_string in
  test_eval ?force ?print expr.desc
;;

let%expect_test "literal integer, parsed" =
  test_parse_and_eval "1";
  [%expect {| (Integer 1) |}]
;;

let%expect_test "literal integer, parsed" =
  test_parse_and_eval "1";
  [%expect {| (Integer 1) |}]
;;

let%expect_test "set with nested attrs" =
  test_parse_and_eval "{x.y = 4;}";
  [%expect {| (Attrs ((attrs ((x (Attrs ((attrs ((y (Thunk <thunk>))))))))))) |}]
;;

let%expect_test "attr selection" =
  test_parse_and_eval ~print:true ~force:true "let m = { x.y = 4; }; in [ m m.x m.x.y ]";
  [%expect
    {| [  { x = { y = 4; }; } { y = 4; } 4 ] |}]
;;
