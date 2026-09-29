open! Core
open Expect_test_helpers_core

let test expr =
  let res = Nox.Parser.parse_string expr in
  print_s [%sexp (res : Nox.Ast.expr)]
;;

let%expect_test "floats" =
  test "1.234";
  [%expect {| ((desc (Lit (Float 1.234)))) |}];
  test "-5.678";
  [%expect {| ((desc (Lit (Float -5.678)))) |}];
  test "-5.678E4";
  [%expect {| ((desc (Lit (Float -56780)))) |}];
  test "-5.678E+4";
  [%expect {| ((desc (Lit (Float -56780)))) |}];
  test "-5.678E-4";
  [%expect {| ((desc (Lit (Float -0.0005678)))) |}]
;;

let%expect_test "strings" =
  let s = {|"\\"|} in
  print_s [%sexp { s : string; length = (String.length s : int) }];
  [%expect {|
    ((s      "\"\\\\\"")
     (length 4))
    |}];
  test s;
  [%expect {| ((desc (Lit (String \)))) |}]
;;

let%expect_test "attr select" =
  test "foo.bar";
  [%expect {|
    ((
      desc (
        Select
        (source ((desc (Ident foo))))
        (path ((Ident_attr bar)))
        (default ()))))
    |}];
  test "foo.bar.baz";
  [%expect {|
    ((
      desc (
        Select
        (source ((desc (Ident foo))))
        (path (
          (Ident_attr bar)
          (Ident_attr baz)))
        (default ()))))
    |}];
  test "foo.bar or 4";
  [%expect {|
    ((
      desc (
        Select
        (source ((desc (Ident foo))))
        (path ((Ident_attr bar)))
        (default (((desc (Lit (Integer 4)))))))))
    |}]
;;

let%expect_test "lists" =
  test "[]";
  [%expect {| ((desc (List ()))) |}];
  test "[ 1 2 3 ]";
  [%expect
    {|
    ((
      desc (
        List (
          ((desc (Lit (Integer 1))))
          ((desc (Lit (Integer 2))))
          ((desc (Lit (Integer 3))))))))
    |}];
  test "[ 1 [2 3] 4 ]";
  [%expect
    {|
    ((
      desc (
        List (
          ((desc (Lit (Integer 1))))
          ((desc (List (((desc (Lit (Integer 2)))) ((desc (Lit (Integer 3))))))))
          ((desc (Lit (Integer 4))))))))
    |}]
;;

let%expect_test "attrsets" =
  test {|{ "" = 1; }|};
  [%expect {| ((desc (Attrset ((((String_attr "")) ((desc (Lit (Integer 1))))))))) |}]
;;
