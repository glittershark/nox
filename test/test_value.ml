open! Core
open Expect_test_helpers_core

(* [Nox.Value.equal] takes its [Parallel.t] locally, so it can't be partially applied.
   Eta-expand it into a module that [require_equal] and friends can consume. *)
module Value = struct
  include Nox.Value

  let equal a b = equal Parallel.sequential a b
end

let%expect_test "weird ident printing" =
  let test s = s |> Nox.Ident.of_string |> Nox.Ident.to_string |> print_endline in
  test "$";
  [%expect {| "\$" |}];
  test "\\";
  [%expect {| "\\" |}]
;;

let%expect_test "Value.to_string round-trips with parse+eval" =
  let%quick_test prop (value : Nox.Value.t) =
    let string = Nox.Value.to_string Parallel.sequential value in
    let parsed =
      Or_error.try_with ~backtrace:true (fun () -> Nox.Parser.parse_string string)
      |> Or_error.tag_s_lazy ~tag:(lazy [%message (string : string)])
      |> ok_exn
    in
    let round_tripped = Nox.eval Parallel.sequential parsed in
    require_equal
      (module Value)
      value
      round_tripped
      ~if_false_then_print_s:
        (lazy
          [%sexp
            { string : string
            ; parsed : Nox.Ast.expr
            ; round_tripped =
                (Nox.Value.deep_force Parallel.sequential round_tripped : Nox.Value.t)
            }])
  in
  [%expect {||}]
;;

module Small_value = struct
  type t = Nox.Value.t [@@deriving sexp_of]

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let open Let_syntax in
    let leaf =
      union
        [ return (Nox.Value.Null : Nox.Value.t)
        ; (let%map b = bool in
           (Nox.Value.Bool b : Nox.Value.t))
        ; (let%map i = int_inclusive 0 2 in
           (Nox.Value.Integer i : Nox.Value.t))
        ; (let%map f = of_list [ 0.; 1. ] in
           (Nox.Value.Float f : Nox.Value.t))
        ; (let%map s = of_list [ "a"; "b" ] in
           (Nox.Value.String s : Nox.Value.t))
        ]
    in
    let names = List.map [ "x"; "y" ] ~f:Nox.Ident.of_string in
    let rec gen ~depth =
      if depth <= 0
      then leaf
      else (
        let sub = gen ~depth:(depth - 1) in
        union
          [ leaf
          ; (let%bind length = int_inclusive 0 2 in
             let%map elements = list_with_length sub ~length in
             (Nox.Value.List (Iarray.of_list elements) : Nox.Value.t))
          ; (let%bind length = int_inclusive 0 2 in
             let%map entries =
               list_with_length
                 ~length
                 (let%map name = of_list names
                  and value = sub in
                  name, value)
             in
             (Nox.Value.Attrs
                { attrs =
                    List.fold entries ~init:Nox.Ident.Map.empty ~f:(fun acc (key, data) ->
                      Map.set acc ~key ~data)
                }
              : Nox.Value.t))
          ; (let%map value = sub in
             Nox.Value.thunk (fun _ -> value))
          ])
    in
    gen ~depth:2
  ;;
end

let%expect_test "[equal] is reflexive" =
  let%quick_test prop (value : Value.t) =
    require
      (Value.equal value value)
      ~if_false_then_print_s:(lazy [%message "not equal to itself" (value : Value.t)])
  in
  [%expect {||}]
;;

let%expect_test "[equal] is symmetric" =
  let%quick_test prop (a : Small_value.t) (b : Small_value.t) =
    require_equal
      (module Bool)
      (Value.equal a b)
      (Value.equal b a)
      ~if_false_then_print_s:(lazy [%message "asymmetric" (a : Value.t) (b : Value.t)])
  in
  [%expect {||}]
;;

let%expect_test "[equal] is symmetric across constructors" =
  let%quick_test prop (a : Value.t) (b : Value.t) =
    require_equal
      (module Bool)
      (Value.equal a b)
      (Value.equal b a)
      ~if_false_then_print_s:(lazy [%message "asymmetric" (a : Value.t) (b : Value.t)])
  in
  [%expect {||}]
;;

let%expect_test "[equal] is transitive" =
  let%quick_test prop (a : Small_value.t) (b : Small_value.t) (c : Small_value.t) =
    if Value.equal a b && Value.equal b c
    then
      require
        (Value.equal a c)
        ~if_false_then_print_s:
          (lazy [%message "not transitive" (a : Value.t) (b : Value.t) (c : Value.t)])
  in
  [%expect {||}]
;;

(* Wrap every node of a value in a thunk. [equal] should not be able to tell the result
   apart from the original. *)
let rec thunkify (value : Nox.Value.t) : Nox.Value.t =
  let forced : Nox.Value.t =
    match value with
    | List l -> List (Iarray.map l ~f:thunkify)
    | Attrs { attrs } -> Attrs { attrs = Map.map attrs ~f:thunkify }
    | (Null | Bool _ | Integer _ | Float _ | String _ | Lambda _ | Thunk _) as value ->
      value
  in
  Nox.Value.thunk (fun _ -> forced)
;;

let%expect_test "[equal] sees through thunks" =
  let%quick_test prop (value : Value.t) =
    require
      (Value.equal value (thunkify value))
      ~if_false_then_print_s:
        (lazy [%message "thunking changed the value" (value : Value.t)])
  in
  [%expect {||}]
;;

let%expect_test "floats compare with IEEE semantics" =
  let nan : Nox.Value.t = Float Float.nan in
  require (not (Value.equal nan nan));
  require
    (not (Value.equal (List (Iarray.of_list [ nan ])) (List (Iarray.of_list [ nan ]))));
  require (Value.equal (Float 0.) (Float (-0.)));
  [%expect {||}]
;;

let%expect_test "lambdas are never equal, not even to themselves" =
  let lambda : Nox.Value.t =
    Lambda
      { args = Iarray.of_list [ Nox.Ident.of_string "x" ]
      ; body = { desc = Ident (Nox.Ident.of_string "x") }
      }
  in
  require (not (Value.equal lambda lambda));
  (* And a lambda nested inside a structure poisons the whole structure. *)
  let list : Nox.Value.t = List (Iarray.of_list [ (Integer 1 : Nox.Value.t); lambda ]) in
  require (not (Value.equal list list));
  [%expect {||}]
;;
