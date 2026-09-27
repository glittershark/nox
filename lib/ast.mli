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

val uncurry_lambda : expr -> expr
