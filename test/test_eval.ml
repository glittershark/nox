open! Core
module Unix = Core_unix
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
  [%expect {| [  { x = { y = 4; }; } { y = 4; } 4 ] |}]
;;

module%test Lang_tests = struct
  let%expect_test "lang tests" =
    let dir = Unix.opendir "lang" in
    while
      match Unix.readdir_opt dir with
      | None -> false
      | Some file when String.is_suffix file ~suffix:".nix" ->
        let path = "lang" ^/ file in
        let expectation =
          if String.is_substring path ~substring:"skip"
          then `Skip
          else if String.is_substring path ~substring:"okay"
          then `Okay
          else if String.is_substring path ~substring:"fail"
          then `Fail
          else `None
        in
        let result =
          lazy
            (Or_error.try_with (fun () ->
               path |> Nox.Parser.parse_file |> Nox.eval par |> Nox.Value.deep_force par))
        in
        (match expectation with
         | `Skip ->
           print_cr ~cr:CR_soon [%message "Skipped test" (file : string)];
           true
         | `Okay ->
           (match force result with
            | Ok _ -> ()
            | Error err ->
              print_cr [%message "Unexpected error" (path : Filename.t) (err : Error.t)]);
           true
         | `Fail ->
           (match force result with
            | Error _ -> ()
            | Ok value ->
              print_cr
                [%message "Unexpected ok" (path : Filename.t) (value : Nox.Value.t)]);
           true
         | `None -> false)
      | _ -> true
    do
      ()
    done;
    [%expect
      {|
      (* CR require-failed: test/test_eval.ml:81:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected ok"
        (path lang/eval-fail-bad-string-interpolation-1.nix)
        (value (String "${x: x}")))
      (* CR require-failed: test/test_eval.ml:81:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected ok"
        (path lang/eval-fail-bad-string-interpolation-2.nix)
        (value (String ${./fnord})))
      (* CR-soon require-failed: test/test_eval.ml:LINE:COL.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Skipped test" (file eval-fail-infinite-recursion-lambda.skip.nix))
      (* CR require-failed: test/test_eval.ml:81:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected ok"
        (path lang/eval-fail-string-nul-1.nix)
        (value (String "foo\000bar")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-abs-path-default.nix)
        (err (
          "File \"lang/eval-okay-abs-path-default.nix\", line 1, characters 0-1:\n1 | # Test: By default, absolute path literals are allowed\n    ^\nError: [lexer] unexpected character: '#\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-abs-path-warn.nix)
        (err (
          "\nFile \"lang/eval-okay-abs-path-warn.nix\", line 1, characters 0-1:\n1 | /tmp/foo\n    ^\nError: [lexer] unexpected character: '/\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-any-all.nix)
        (err (
          "\nFile \"lang/eval-okay-any-all.nix\", line 1, characters 13-14:\n1 | with builtins;\n                 ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-arithmetic.nix)
        (err (
          "\nFile \"lang/eval-okay-arithmetic.nix\", line 1, characters 12-13:\n1 | with import ./lib.nix;\n                ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-attrnames.nix)
        (err (
          "\nFile \"lang/eval-okay-attrnames.nix\", line 1, characters 12-13:\n1 | with import ./lib.nix;\n                ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-attrs.nix)
        (err (
          "\nFile \"lang/eval-okay-attrs.nix\", line 1, characters 4-5:\n1 | let {\n        ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-attrs2.nix)
        (err (
          "\nFile \"lang/eval-okay-attrs2.nix\", line 1, characters 4-5:\n1 | let {\n        ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-attrs4.nix)
        (err (
          "\nFile \"lang/eval-okay-attrs4.nix\", line 12, characters 6-7:\n12 |   (as ? x)\n           ^\nError: [lexer] unexpected character: '?\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-attrs5.nix)
        (err (
          "\nFile \"lang/eval-okay-attrs5.nix\", line 1, characters 12-13:\n1 | with import ./lib.nix;\n                ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-attrs6.nix)
        (err (
          "\nFile \"lang/eval-okay-attrs6.nix\", line 2, characters 6-9:\n2 |   \"${\"foo\"}\" = \"bar\";\n          ^^^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-autoargs.nix)
        (err (
          "\nFile \"lang/eval-okay-autoargs.nix\", line 8, characters 9-10:\n8 |   xyzzy2 ? xyzzy, # mutually recursive args\n             ^\nError: [lexer] unexpected character: '?\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-backslash-newline-2.nix)
        (err (
          "\nFile \"lang/eval-okay-backslash-newline-2.nix\", line 1, characters 0-1:\n1 | ''a''\\\n    ^\nError: [lexer] unexpected character: ''\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-baseNameOf.nix)
        (err (
          "\nFile \"lang/eval-okay-baseNameOf.nix\", line 1, characters 21-22:\n1 | assert baseNameOf \"\" == \"\";\n                         ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-break.nix)
        (err (
          "\nFile \"lang/eval-okay-break.nix\", line 1, characters 15-16:\n1 | builtins.break (1 + 2)\n                   ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-builtins-add.nix)
        (err (
          "\nFile \"lang/eval-okay-builtins-add.nix\", line 2, characters 16-17:\n2 |   (builtins.add 2 3)\n                    ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-builtins-dirOf.nix)
        (err (
          "\nFile \"lang/eval-okay-builtins-dirOf.nix\", line 17, characters 19-20:\n17 |   pathRoot = dirOf /.;\n                        ^\nError: [lexer] unexpected character: '/\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-builtins.nix)
        (err (
          "\nFile \"lang/eval-okay-builtins.nix\", line 1, characters 16-17:\n1 | assert builtins ? currentSystem;\n                    ^\nError: [lexer] unexpected character: '?\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-callable-attrs.nix)
        (err (
          "\nFile \"lang/eval-okay-callable-attrs.nix\", line 3, characters 4-5:\n3 |     __functor = self: x: self.foo && x;\n        ^\nError: [lexer] unexpected character: '_\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-catattrs.nix)
        (err (
          "\nFile \"lang/eval-okay-catattrs.nix\", line 1, characters 18-21:\n1 | builtins.catAttrs \"a\" [\n                      ^^^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-closure.nix)
        (err (
          "\nFile \"lang/eval-okay-closure.nix\", line 3, characters 36-37:\n3 |   closure = builtins.genericClosure {\n                                        ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-comments.nix)
        (err (
          "\nFile \"lang/eval-okay-comments.nix\", line 1, characters 0-1:\n1 | # A simple comment\n    ^\nError: [lexer] unexpected character: '#\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-concat.nix)
        (err (
          "\nFile \"lang/eval-okay-concat.nix\", line 6, characters 1-2:\n6 | ++ [\n     ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-concatmap.nix)
        (err (
          "\nFile \"lang/eval-okay-concatmap.nix\", line 1, characters 12-13:\n1 | with import ./lib.nix;\n                ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-concatstringssep.nix)
        (err (
          "\nFile \"lang/eval-okay-concatstringssep.nix\", line 1, characters 13-14:\n1 | with builtins;\n                 ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-context-introspection.nix)
        (err (
          "\nFile \"lang/eval-okay-context-introspection.nix\", line 28, characters 38-48:\n28 |   legit-context = builtins.getContext combo-path;\n                                           ^^^^^^^^^^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-context.nix)
        (err (
          "\nFile \"lang/eval-okay-context.nix\", line 2, characters 52-53:\n2 |   s = \"foo ${builtins.substring 33 100 (baseNameOf \"${./eval-okay-context.nix}\")} bar\";\n                                                        ^\nError: [lexer] unexpected character: '$\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-convertHash.nix)
        (err (
          "\nFile \"lang/eval-okay-convertHash.nix\", line 16, characters 25-26:\n16 |   hashesBase16 = import ./eval-okay-hashstring.exp;\n                              ^\nError: [lexer] unexpected character: '/\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-curpos.nix)
        (err (
          "\nFile \"lang/eval-okay-curpos.nix\", line 1, characters 0-1:\n1 | # Bla\n    ^\nError: [lexer] unexpected character: '#\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-deepseq.nix)
        (err (
          "\nFile \"lang/eval-okay-deepseq.nix\", line 1, characters 17-18:\n1 | builtins.deepSeq (\n                     ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-delayed-with-inherit.nix)
        (err (
          "\nFile \"lang/eval-okay-delayed-with-inherit.nix\", line 2, characters 21-22:\n2 |   pkgs_ = with pkgs; {\n                         ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-delayed-with.nix)
        (err (
          "\nFile \"lang/eval-okay-delayed-with.nix\", line 3, characters 21-22:\n3 |   pkgs_ = with pkgs; {\n                         ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-deprecate-cursed-or.nix)
        (err (
          "\nFile \"lang/eval-okay-deprecate-cursed-or.nix\", line 2, characters 2-3:\n2 |   # These are cursed and should warn\n      ^\nError: [lexer] unexpected character: '#\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-derivation-legacy.nix)
        (err (
          "\nFile \"lang/eval-okay-derivation-legacy.nix\", line 1, characters 27-28:\n1 | (builtins.derivationStrict {\n                               ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-dotdotslash-abs-fatal.nix)
        (err (
          "\nFile \"lang/eval-okay-dotdotslash-abs-fatal.nix\", line 1, characters 0-1:\n1 | # Test: ../ relative paths work even when absolute paths are fatal\n    ^\nError: [lexer] unexpected character: '#\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-dotdotslash-path-fatal.nix)
        (err (
          "\nFile \"lang/eval-okay-dotdotslash-path-fatal.nix\", line 1, characters 0-1:\n1 | # Test: ../ paths are not affected by lint-short-path-literals=fatal\n    ^\nError: [lexer] unexpected character: '#\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-dotslash-abs-fatal.nix)
        (err (
          "\nFile \"lang/eval-okay-dotslash-abs-fatal.nix\", line 1, characters 0-1:\n1 | # Test: ./ relative paths work even when absolute paths are fatal\n    ^\nError: [lexer] unexpected character: '#\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-dotslash-path-fatal.nix)
        (err (
          "\nFile \"lang/eval-okay-dotslash-path-fatal.nix\", line 1, characters 0-1:\n1 | # Test: ./ paths are not affected by lint-short-path-literals=fatal\n    ^\nError: [lexer] unexpected character: '#\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-dynamic-attrs-2.nix)
        (err (
          "\nFile \"lang/eval-okay-dynamic-attrs-2.nix\", line 2, characters 8-9:\n2 |   a.\"${\"b\"}\" = true;\n            ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-dynamic-attrs-3.nix)
        (err (
          "\nFile \"lang/eval-okay-dynamic-attrs-3.nix\", line 1, characters 0-1:\n1 | # dynamic attrs are not generally allowed in `let`, and inherit, but they are if they only contain a string\n    ^\nError: [lexer] unexpected character: '#\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-dynamic-attrs-bare.nix)
        (err (
          "\nFile \"lang/eval-okay-dynamic-attrs-bare.nix\", line 7, characters 29-30:\n7 |   hasAttrs = { a.b = null; } ? ${aString}.b;\n                                 ^\nError: [lexer] unexpected character: '?\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-dynamic-attrs.nix)
        (err (
          "\nFile \"lang/eval-okay-dynamic-attrs.nix\", line 7, characters 29-30:\n7 |   hasAttrs = { a.b = null; } ? \"${aString}\".b;\n                                 ^\nError: [lexer] unexpected character: '?\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-elem.nix)
        (err (
          "\nFile \"lang/eval-okay-elem.nix\", line 1, characters 12-13:\n1 | with import ./lib.nix;\n                ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-empty-args.nix)
        (err (
          "\nFile \"lang/eval-okay-empty-args.nix\", line 1, characters 4-5:\n1 | ({ }: { x, y }: \"${x}${y}\") { } {\n        ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-eq-derivations.nix)
        (err (
          "\nFile \"lang/eval-okay-eq-derivations.nix\", line 19, characters 4-5:\n19 |     // {\n         ^\nError: [lexer] unexpected character: '/\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-eq.nix)
        (err (
          "\nFile \"lang/eval-okay-eq.nix\", line 7, characters 2-3:\n7 | ] == [\n      ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-equal-function-attrset-distinct-similar.nix)
        (err (
          "\nFile \"lang/eval-okay-equal-function-attrset-distinct-similar.nix\", line 1, characters 0-1:\n1 | # Distinct but not identical functions in attribute set compare as unequal\n    ^\nError: [lexer] unexpected character: '#\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-equal-function-attrset-identical.nix)
        (err (
          "\nFile \"lang/eval-okay-equal-function-attrset-identical.nix\", line 1, characters 0-1:\n1 | # Function comparison in attribute set uses value identity optimization\n    ^\nError: [lexer] unexpected character: '#\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-equal-function-direct-distinct-similar.nix)
        (err (
          "\nFile \"lang/eval-okay-equal-function-direct-distinct-similar.nix\", line 1, characters 0-1:\n1 | # Direct comparison of distinct but not identical functions returns false\n    ^\nError: [lexer] unexpected character: '#\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-equal-function-direct-identical.nix)
        (err (
          "\nFile \"lang/eval-okay-equal-function-direct-identical.nix\", line 1, characters 0-1:\n1 | # Direct comparison of identical function returns false\n    ^\nError: [lexer] unexpected character: '#\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-equal-function-list-distinct-similar.nix)
        (err (
          "\nFile \"lang/eval-okay-equal-function-list-distinct-similar.nix\", line 1, characters 0-1:\n1 | # Distinct but not identical functions in list compare as unequal\n    ^\nError: [lexer] unexpected character: '#\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-equal-function-list-identical.nix)
        (err (
          "\nFile \"lang/eval-okay-equal-function-list-identical.nix\", line 1, characters 0-1:\n1 | # Function comparison in list uses value identity optimization\n    ^\nError: [lexer] unexpected character: '#\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-filter.nix)
        (err (
          "\nFile \"lang/eval-okay-filter.nix\", line 1, characters 12-13:\n1 | with import ./lib.nix;\n                ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-flake-ref-to-string.nix)
        (err (
          "\nFile \"lang/eval-okay-flake-ref-to-string.nix\", line 1, characters 26-27:\n1 | builtins.flakeRefToString {\n                              ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-flatten.nix)
        (err (
          "\nFile \"lang/eval-okay-flatten.nix\", line 1, characters 12-13:\n1 | with import ./lib.nix;\n                ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-float.nix)
        (err (
          "\nFile \"lang/eval-okay-float.nix\", line 3, characters 16-17:\n3 |   (builtins.add (0.5 + 0.5) (2.0 + 0.5))\n                    ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-floor-ceil.nix)
        (err (
          "\nFile \"lang/eval-okay-floor-ceil.nix\", line 1, characters 12-13:\n1 | with import ./lib.nix;\n                ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-foldlStrict-lazy-elements.nix)
        (err (
          "\nFile \"lang/eval-okay-foldlStrict-lazy-elements.nix\", line 1, characters 0-1:\n1 | # Tests that the rhs argument of op is not forced unconditionally\n    ^\nError: [lexer] unexpected character: '#\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-foldlStrict-lazy-initial-accumulator.nix)
        (err (
          "\nFile \"lang/eval-okay-foldlStrict-lazy-initial-accumulator.nix\", line 1, characters 0-1:\n1 | # Checks that the nul value for the accumulator is not forced unconditionally.\n    ^\nError: [lexer] unexpected character: '#\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-foldlStrict.nix)
        (err (
          "\nFile \"lang/eval-okay-foldlStrict.nix\", line 1, characters 12-13:\n1 | with import ./lib.nix;\n                ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-fromTOML-timestamps.nix)
        (err (
          "\nFile \"lang/eval-okay-fromTOML-timestamps.nix\", line 1, characters 18-19:\n1 | builtins.fromTOML ''\n                      ^\nError: [lexer] unexpected character: ''\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-fromTOML.nix)
        (err (
          "\nFile \"lang/eval-okay-fromTOML.nix\", line 3, characters 21-22:\n3 |   (builtins.fromTOML ''\n                         ^\nError: [lexer] unexpected character: ''\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-fromjson-escapes.nix)
        (err (
          "\nFile \"lang/eval-okay-fromjson-escapes.nix\", line 1, characters 0-1:\n1 | # This string contains all supported escapes in a JSON string, per json.org\n    ^\nError: [lexer] unexpected character: '#\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-fromjson.nix)
        (err (
          "\nFile \"lang/eval-okay-fromjson.nix\", line 1, characters 18-19:\n1 | builtins.fromJSON ''\n                      ^\nError: [lexer] unexpected character: ''\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-functionargs.nix)
        (err (
          "\nFile \"lang/eval-okay-functionargs.nix\", line 4, characters 7-8:\n4 |     { }:\n           ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-getattrpos-functionargs.nix)
        (err (
          "\nFile \"lang/eval-okay-getattrpos-functionargs.nix\", line 2, characters 14-15:\n2 |   fun = { foo }: { };\n                  ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-getattrpos-undefined.nix)
        (err (
          "\nFile \"lang/eval-okay-getattrpos-undefined.nix\", line 1, characters 26-33:\n1 | builtins.unsafeGetAttrPos \"abort\" builtins\n                              ^^^^^^^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-getattrpos.nix)
        (err (
          "\nFile \"lang/eval-okay-getattrpos.nix\", line 5, characters 34-39:\n5 |   pos = builtins.unsafeGetAttrPos \"foo\" as;\n                                      ^^^^^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-getenv.nix)
        (err (
          "\nFile \"lang/eval-okay-getenv.nix\", line 1, characters 16-26:\n1 | builtins.getEnv \"TEST_VAR\" + (if builtins.getEnv \"NO_SUCH_VAR\" == \"\" then \"bar\" else \"bla\")\n                    ^^^^^^^^^^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-groupBy.nix)
        (err (
          "\nFile \"lang/eval-okay-groupBy.nix\", line 1, characters 12-13:\n1 | with import ./lib.nix;\n                ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-hashfile.nix)
        (err (
          "\nFile \"lang/eval-okay-hashfile.nix\", line 3, characters 4-5:\n3 |     ./data\n        ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-hashstring.nix)
        (err (
          "\nFile \"lang/eval-okay-hashstring.nix\", line 8, characters 21-22:\n8 | builtins.concatLists (\n                         ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-home-path-warn.nix)
        (err (
          "\nFile \"lang/eval-okay-home-path-warn.nix\", line 1, characters 16-17:\n1 | builtins.isPath ~/foo\n                    ^\nError: [lexer] unexpected character: '~\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-if.nix)
        (err (
          "\nFile \"lang/eval-okay-if.nix\", line 1, characters 9-10:\n1 | if \"foo\" != \"f\" + \"oo\" then\n             ^\nError: [lexer] unexpected character: '!\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-import.nix)
        (err (
          "\nFile \"lang/eval-okay-import.nix\", line 6, characters 54-55:\n6 |     scopedImport = attrs: fn: scopedImport (overrides // attrs) fn;\n                                                          ^\nError: [lexer] unexpected character: '/\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-ind-string.nix)
        (err (
          "\nFile \"lang/eval-okay-ind-string.nix\", line 3, characters 7-8:\n3 |   s1 = ''\n           ^\nError: [lexer] unexpected character: ''\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-inherit-attr-pos.nix)
        (err (
          "\nFile \"lang/eval-okay-inherit-attr-pos.nix\", line 4, characters 16-17:\n4 |   y = { inherit d x; };\n                    ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-inherit-from.nix)
        (err (
          "\nFile \"lang/eval-okay-inherit-from.nix\", line 3, characters 4-5:\n3 |     (builtins.trace \"used\" {\n        ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-intersectAttrs.nix)
        (err (
          "\nFile \"lang/eval-okay-intersectAttrs.nix\", line 31, characters 12-13:\n31 |     inherit (alphabet)\n                 ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-let.nix)
        (err (
          "\nFile \"lang/eval-okay-let.nix\", line 1, characters 4-5:\n1 | let {\n        ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-list.nix)
        (err (
          "\nFile \"lang/eval-okay-list.nix\", line 1, characters 12-13:\n1 | with import ./lib.nix;\n                ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-listtoattrs.nix)
        (err (
          "\nFile \"lang/eval-okay-listtoattrs.nix\", line 1, characters 0-1:\n1 | # this test shows how to use listToAttrs and that evaluation is still lazy (throw isn't called)\n    ^\nError: [lexer] unexpected character: '#\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-logic.nix)
        (err (
          "\nFile \"lang/eval-okay-logic.nix\", line 1, characters 7-8:\n1 | assert !false && (true || false) -> true;\n           ^\nError: [lexer] unexpected character: '!\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-map.nix)
        (err (
          "\nFile \"lang/eval-okay-map.nix\", line 1, characters 12-13:\n1 | with import ./lib.nix;\n                ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-mapattrs.nix)
        (err (
          "\nFile \"lang/eval-okay-mapattrs.nix\", line 1, characters 12-13:\n1 | with import ./lib.nix;\n                ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-merge-dynamic-attrs.nix)
        (err (
          "\nFile \"lang/eval-okay-merge-dynamic-attrs.nix\", line 6, characters 8-9:\n6 |     \"${\"b\" + \"\"}\" = 2;\n            ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-nested-with.nix)
        (err (
          "\nFile \"lang/eval-okay-nested-with.nix\", line 1, characters 15-16:\n1 | with { x = 1; }; with { x = 2; }; x\n                   ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-new-let.nix)
        (err (
          "\nFile \"lang/eval-okay-new-let.nix\", line 9, characters 16-17:\n9 |       body = 1; # compat test\n                    ^\nError: [lexer] unexpected character: '#\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-null-dynamic-attrs.nix)
        (err (
          "\nFile \"lang/eval-okay-null-dynamic-attrs.nix\", line 1, characters 2-3:\n1 | { ${null} = true; } == { }\n      ^\nError: [lexer] unexpected character: '$\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-overrides.nix)
        (err (
          "\nFile \"lang/eval-okay-overrides.nix\", line 10, characters 2-3:\n10 |   __overrides = overrides;\n       ^\nError: [lexer] unexpected character: '_\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-parse-flake-ref.nix)
        (err (
          "\nFile \"lang/eval-okay-parse-flake-ref.nix\", line 1, characters 23-59:\n1 | builtins.parseFlakeRef \"github:NixOS/nixpkgs/23.05?dir=lib\"\n                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-partition.nix)
        (err (
          "\nFile \"lang/eval-okay-partition.nix\", line 1, characters 12-13:\n1 | with import ./lib.nix;\n                ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-path-string-interpolation.nix)
        (err (
          "\nFile \"lang/eval-okay-path-string-interpolation.nix\", line 5, characters 11-12:\n5 |   simple = ./${foo};\n               ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-path.nix)
        (err (
          "\nFile \"lang/eval-okay-path.nix\", line 2, characters 2-3:\n2 |   # NAR hash of directory with filter\n      ^\nError: [lexer] unexpected character: '#\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-pathexists.nix)
        (err (
          "\nFile \"lang/eval-okay-pathexists.nix\", line 1, characters 20-21:\n1 | builtins.pathExists (./lib.nix)\n                        ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-patterns.nix)
        (err (
          "\nFile \"lang/eval-okay-patterns.nix\", line 4, characters 8-9:\n4 |     args@{\n            ^\nError: [lexer] unexpected character: '@\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-print.nix)
        (err (
          "\nFile \"lang/eval-okay-print.nix\", line 1, characters 13-14:\n1 | with builtins;\n                 ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-readDir-symlinked-directory.nix)
        (err (
          "\nFile \"lang/eval-okay-readDir-symlinked-directory.nix\", line 1, characters 18-19:\n1 | builtins.readDir ./readDir/ldir\n                      ^\nError: [lexer] unexpected character: '/\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-readDir.nix)
        (err (
          "\nFile \"lang/eval-okay-readDir.nix\", line 1, characters 18-19:\n1 | builtins.readDir ./readDir\n                      ^\nError: [lexer] unexpected character: '/\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-readFileType.nix)
        (err (
          "\nFile \"lang/eval-okay-readFileType.nix\", line 2, characters 31-32:\n2 |   bar = builtins.readFileType ./readDir/bar;\n                                   ^\nError: [lexer] unexpected character: '/\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-readfile.nix)
        (err (
          "\nFile \"lang/eval-okay-readfile.nix\", line 1, characters 19-20:\n1 | builtins.readFile ./eval-okay-readfile.nix\n                       ^\nError: [lexer] unexpected character: '/\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-redefine-builtin.nix)
        (err (
          "\nFile \"lang/eval-okay-redefine-builtin.nix\", line 4, characters 18-19:\n4 | (builtins.tryEval <foobaz>).success\n                      ^\nError: [lexer] unexpected character: '<\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-regex-match.nix)
        (err (
          "\nFile \"lang/eval-okay-regex-match.nix\", line 1, characters 13-14:\n1 | with builtins;\n                 ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-regex-match2.nix)
        (err (
          "\nFile \"lang/eval-okay-regex-match2.nix\", line 1, characters 0-1:\n1 | # Derived from nixpkgs f870c6ccc8951fc48aeb293cf3e98ade6ac42668 by instrumenting\n    ^\nError: [lexer] unexpected character: '#\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-regex-split.nix)
        (err (
          "\nFile \"lang/eval-okay-regex-split.nix\", line 1, characters 13-14:\n1 | with builtins;\n                 ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-regression-20220122.nix)
        (err (
          "\nFile \"lang/eval-okay-regression-20220122.nix\", line 1, characters 2-3:\n1 | ((_: _) 1) + ((__: __) 2)\n      ^\nError: [lexer] unexpected character: '_\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-regression-20220125.nix)
        (err (
          "\nFile \"lang/eval-okay-regression-20220125.nix\", line 1, characters 2-3:\n1 | ((__curPosFoo: __curPosFoo) 1) + ((__curPosBar: __curPosBar) 2)\n      ^\nError: [lexer] unexpected character: '_\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-regrettable-rec-attrset-merge.nix)
        (err (
          "\nFile \"lang/eval-okay-regrettable-rec-attrset-merge.nix\", line 1, characters 0-1:\n1 | # This is for backwards compatibility, not because we like it.\n    ^\nError: [lexer] unexpected character: '#\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-remove.nix)
        (err (
          "\nFile \"lang/eval-okay-remove.nix\", line 1, characters 4-5:\n1 | let {\n        ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-repeated-empty-attrs.nix)
        (err (
          "\nFile \"lang/eval-okay-repeated-empty-attrs.nix\", line 1, characters 0-1:\n1 | # Tests that empty attribute sets are not printed as `\194\171repeated\194\187`.\n    ^\nError: [lexer] unexpected character: '#\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-replacestrings.nix)
        (err (
          "\nFile \"lang/eval-okay-replacestrings.nix\", line 1, characters 13-14:\n1 | with builtins;\n                 ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-scope-1.nix)
        (err (
          "\nFile \"lang/eval-okay-scope-1.nix\", line 3, characters 8-9:\n3 |     { x }:\n            ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-scope-2.nix)
        (err (
          "\nFile \"lang/eval-okay-scope-2.nix\", line 4, characters 8-9:\n4 |     { x }:\n            ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-scope-3.nix)
        (err (
          "\nFile \"lang/eval-okay-scope-3.nix\", line 4, characters 8-9:\n4 |     { x }:\n            ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-scope-4.nix)
        (err (
          "\nFile \"lang/eval-okay-scope-4.nix\", line 1, characters 4-5:\n1 | let {\n        ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-scope-6.nix)
        (err (
          "\nFile \"lang/eval-okay-scope-6.nix\", line 1, characters 4-5:\n1 | let {\n        ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-scope-7.nix)
        (err (
          "\nFile \"lang/eval-okay-scope-7.nix\", line 2, characters 10-11:\n2 |   inherit (x) y;\n              ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-search-path.nix)
        (err (
          "\nFile \"lang/eval-okay-search-path.nix\", line 1, characters 12-13:\n1 | with import ./lib.nix;\n                ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-seq.nix)
        (err (
          "\nFile \"lang/eval-okay-seq.nix\", line 1, characters 13-14:\n1 | builtins.seq 1 2\n                 ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-short-path-literal-warn.nix)
        (err (
          "\nFile \"lang/eval-okay-short-path-literal-warn.nix\", line 1, characters 4-5:\n1 | test/subdir\n        ^\nError: [lexer] unexpected character: '/\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-short-path-variation.nix)
        (err (
          "\nFile \"lang/eval-okay-short-path-variation.nix\", line 1, characters 0-1:\n1 | # Test: Different short path literals should produce warnings\n    ^\nError: [lexer] unexpected character: '#\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-sort.nix)
        (err (
          "\nFile \"lang/eval-okay-sort.nix\", line 1, characters 13-14:\n1 | with builtins;\n                 ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-splitversion.nix)
        (err (
          "\nFile \"lang/eval-okay-splitversion.nix\", line 1, characters 22-29:\n1 | builtins.splitVersion \"1.2.3\"\n                          ^^^^^^^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-string.nix)
        (err (
          "\nFile \"lang/eval-okay-string.nix\", line 3, characters 12-13:\n3 | + toString (/a/b + /c/d)\n                ^\nError: [lexer] unexpected character: '/\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-strings-as-attrs-names.nix)
        (err (
          "\nFile \"lang/eval-okay-strings-as-attrs-names.nix\", line 8, characters 24-31:\n8 |   t1 = builtins.getAttr \"key 1\" attr;\n                            ^^^^^^^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-substring-context.nix)
        (err (
          "\nFile \"lang/eval-okay-substring-context.nix\", line 1, characters 13-14:\n1 | with builtins;\n                 ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-substring.nix)
        (err (
          "\nFile \"lang/eval-okay-substring.nix\", line 1, characters 13-14:\n1 | with builtins;\n                 ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-symlink-resolution.nix)
        (err (
          "\nFile \"lang/eval-okay-symlink-resolution.nix\", line 1, characters 25-26:\n1 | import symlink-resolution/foo/overlays/overlay.nix\n                             ^\nError: [lexer] unexpected character: '/\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-tail-call-1.nix)
        (err (
          "\nFile \"lang/eval-okay-tail-call-1.nix\", line 2, characters 14-15:\n2 |   f = n: if n == 100000 then n else f (n + 1);\n                  ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-toJSON-peel.nix)
        (err (
          "\nFile \"lang/eval-okay-toJSON-peel.nix\", line 1, characters 0-1:\n1 | # Characterise `builtins.toJSON`'s peeling of `__toString` and `outPath` attrs\n    ^\nError: [lexer] unexpected character: '#\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-toString-returning-path.nix)
        (err (
          "\nFile \"lang/eval-okay-toString-returning-path.nix\", line 1, characters 11-12:\n1 | toString { __toString = self: ./dir1/a.nix; }\n               ^\nError: [lexer] unexpected character: '_\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-tojson.nix)
        (err (
          "\nFile \"lang/eval-okay-tojson.nix\", line 1, characters 16-17:\n1 | builtins.toJSON {\n                    ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-toxml.nix)
        (err (
          "\nFile \"lang/eval-okay-toxml.nix\", line 1, characters 0-1:\n1 | # Make sure the expected XML output is produced; in particular, make sure it\n    ^\nError: [lexer] unexpected character: '#\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-toxml2.nix)
        (err (
          "\nFile \"lang/eval-okay-toxml2.nix\", line 1, characters 15-16:\n1 | builtins.toXML [\n                   ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-tryeval-failed-thunk-reeval.nix)
        (err (
          "\nFile \"lang/eval-okay-tryeval-failed-thunk-reeval.nix\", line 1, characters 0-1:\n1 | # Since Nix 2.34, errors are memoized\n    ^\nError: [lexer] unexpected character: '#\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-tryeval.nix)
        (err (
          "\nFile \"lang/eval-okay-tryeval.nix\", line 2, characters 23-26:\n2 |   x = builtins.tryEval \"x\";\n                           ^^^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-types.nix)
        (err (
          "\nFile \"lang/eval-okay-types.nix\", line 1, characters 13-14:\n1 | with builtins;\n                 ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-url-literal-default.nix)
        (err (
          "\nFile \"lang/eval-okay-url-literal-default.nix\", line 1, characters 0-1:\n1 | # Test: By default (no flags), unquoted URL literals are accepted\n    ^\nError: [lexer] unexpected character: '#\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-url-literal-quoted-fatal.nix)
        (err (
          "\nFile \"lang/eval-okay-url-literal-quoted-fatal.nix\", line 1, characters 0-1:\n1 | # Test: Quoted URLs are always accepted even with fatal setting\n    ^\nError: [lexer] unexpected character: '#\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-url-literal-warn.nix)
        (err (
          "\nFile \"lang/eval-okay-url-literal-warn.nix\", line 1, characters 5-6:\n1 | http://example.com\n         ^\nError: [lexer] unexpected character: '/\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-versions.nix)
        (err (
          "\nFile \"lang/eval-okay-versions.nix\", line 10, characters 20-21:\n10 |   lt = builtins.sub 0 1;\n                         ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-warn-indentation.nix)
        (err (
          "\nFile \"lang/eval-okay-warn-indentation.nix\", line 1, characters 14-24:\n1 | builtins.warn \"foo\\nbar\" null\n                  ^^^^^^^^^^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-warn-indentation2.nix)
        (err (
          "\nFile \"lang/eval-okay-warn-indentation2.nix\", line 1, characters 14-26:\n1 | builtins.warn \"\\nfoo\\nbar\" null\n                  ^^^^^^^^^^^^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-with.nix)
        (err (
          "\nFile \"lang/eval-okay-with.nix\", line 1, characters 4-5:\n1 | let {\n        ^\nError: [parser] unexpected token\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-xml.nix)
        (err (
          "\nFile \"lang/eval-okay-xml.nix\", line 15, characters 7-8:\n15 |       z,\n            ^\nError: [lexer] unexpected character: ',\n")))
      (* CR require-failed: test/test_eval.ml:75:14.
         Do not 'X' this CR; instead make the required property true,
         which will make the CR disappear.  For more information, see
         [Expect_test_helpers_base.require]. *)
      ("Unexpected error"
        (path lang/eval-okay-zipAttrsWith.nix)
        (err (
          "\nFile \"lang/eval-okay-zipAttrsWith.nix\", line 1, characters 12-13:\n1 | with import ./lib.nix;\n                ^\nError: [parser] unexpected token\n")))
      |}]
  ;;
end
