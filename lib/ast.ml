open! Core

type expr = Types.expr = { desc : expr_desc } [@@deriving sexp_of]

and expr_desc = Types.expr_desc =
  | Lit of Types.value
  | Ident of Ident.t
  | Bin_op of
      { lhs : expr
      ; op : bin_op
      ; rhs : expr
      }
  | If of
      { cond : expr
      ; then_ : expr
      ; else_ : expr
      }
  | Apply of
      { func : expr
      ; args : expr iarray
      }
  | Let of
      { bindings : attrset
      ; body : expr
      }
  | Lam of
      { args : Ident.t iarray
      ; body : expr
      }
  | List of expr iarray
  | Attrset of attrset
[@@deriving sexp_of]

and attrset = (name:Ident.t * value:expr) iarray [@@deriving sexp_of]

and bin_op = Types.bin_op =
  | Plus
  | Minus
[@@deriving sexp_of]

let[@inline] rec uncurry_lambda (expr : expr) : expr =
  match expr.desc with
  | Lam { args; body } ->
    (match (uncurry_lambda body).desc with
     | Lam { args = body_args; body } ->
       { desc = Lam { args = Iarray.concat [: args; body_args :]; body } }
     | _ -> expr)
  | _ -> expr
;;
