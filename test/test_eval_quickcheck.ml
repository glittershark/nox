open! Core
open Expect_test_helpers_core

(* [Nox.eval] leaves thunks unforced, so force the way the CLI does before looking at the
   result. *)
let eval expr =
  Nox.eval Parallel.sequential expr |> Nox.Value.deep_force Parallel.sequential
;;

let check_evaluates_to expr ~expect =
  match eval expr with
  | Integer actual -> [%test_result: int] actual ~expect
  | other -> raise_s [%message "expected an integer" (other : Nox.Value.t)]
;;

(* Closed arithmetic. The type has no scoping rules, so the generator and shrinker can
   both be derived. *)
module Arith = struct
  type t =
    | Int of int
    | Add of t * t
    | Sub of t * t
  [@@deriving sexp_of, quickcheck]

  let rec to_expr (t : t) : Nox.Ast.expr =
    match t with
    | Int i -> { desc = Lit (Integer i) }
    | Add (lhs, rhs) ->
      { desc = Bin_op { lhs = to_expr lhs; op = Plus; rhs = to_expr rhs } }
    | Sub (lhs, rhs) ->
      { desc = Bin_op { lhs = to_expr lhs; op = Minus; rhs = to_expr rhs } }
  ;;

  let rec eval = function
    | Int i -> i
    | Add (lhs, rhs) -> eval lhs + eval rhs
    | Sub (lhs, rhs) -> eval lhs - eval rhs
  ;;
end

let%expect_test "arithmetic agrees with OCaml" =
  quickcheck_m (module Arith) ~f:(fun t ->
    check_evaluates_to (Arith.to_expr t) ~expect:(Arith.eval t))
;;

(* Arithmetic plus [let]. A [Var] is only legal where a binding encloses it, so the
   generator is written by hand to thread the set of names in scope. *)
module Term = struct
  type t =
    | Int of int
    | Var of string
    | Add of t * t
    | Sub of t * t
    | Let of string * t * t
  [@@deriving sexp_of]

  let rec to_expr (t : t) : Nox.Ast.expr =
    match t with
    | Int i -> { desc = Lit (Integer i) }
    | Var name -> { desc = Ident (Nox.Ident.of_string name) }
    | Add (lhs, rhs) ->
      { desc = Bin_op { lhs = to_expr lhs; op = Plus; rhs = to_expr rhs } }
    | Sub (lhs, rhs) ->
      { desc = Bin_op { lhs = to_expr lhs; op = Minus; rhs = to_expr rhs } }
    | Let (name, bound, body) ->
      { desc =
          Let
            { bindings =
                Iarray.singleton
                  ([: Nox.Ast.Ident_attr (Nox.Ident.of_string name) :], to_expr bound)
            ; body = to_expr body
            }
      }
  ;;

  let eval t =
    let rec eval env = function
      | Int i -> i
      | Var name -> Map.find_exn env name
      | Add (lhs, rhs) -> eval env lhs + eval env rhs
      | Sub (lhs, rhs) -> eval env lhs - eval env rhs
      | Let (name, bound, body) ->
        (* [let] in nox binds its value in the enclosing scope, not recursively. *)
        eval (Map.set env ~key:name ~data:(eval env bound)) body
    in
    eval String.Map.empty t
  ;;

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let open Let_syntax in
    (* A small pool of names, so bindings shadow each other often enough to exercise it. *)
    let names = [ "a"; "b"; "c" ] in
    let rec gen ~in_scope ~depth =
      let leaves =
        let int_leaf =
          let%map i = int_inclusive (-1000) 1000 in
          Int i
        in
        match in_scope with
        | [] -> [ int_leaf ]
        | _ :: _ ->
          [ int_leaf
          ; (let%map name = of_list in_scope in
             Var name)
          ]
      in
      if depth <= 0
      then union leaves
      else (
        let sub = gen ~in_scope ~depth:(depth - 1) in
        union
          (leaves
           @ [ (let%map lhs = sub
                and rhs = sub in
                Add (lhs, rhs))
             ; (let%map lhs = sub
                and rhs = sub in
                Sub (lhs, rhs))
             ; (let%bind name = of_list names in
                let%map bound = sub
                and body = gen ~in_scope:(name :: in_scope) ~depth:(depth - 1) in
                Let (name, bound, body))
             ]))
    in
    gen ~in_scope:[] ~depth:3
  ;;

  (* Shrinking would have to preserve scoping, which the generic shrinkers can't do. *)
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let%expect_test "let-bound arithmetic agrees with OCaml" =
  Base_quickcheck.Test.run_exn (module Term) ~f:(fun t ->
    check_evaluates_to (Term.to_expr t) ~expect:(Term.eval t))
;;
