open! Core
include Types.Ast

let[@inline] rec uncurry_lambda (expr : expr) : expr =
  match expr.desc with
  | Lam { args; body } ->
    (match (uncurry_lambda body).desc with
     | Lam { args = body_args; body } ->
       { desc = Lam { args = Iarray.concat [: args; body_args :]; body } }
     | _ -> expr)
  | _ -> expr
;;
