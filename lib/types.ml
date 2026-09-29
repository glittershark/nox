open! Core
open! Import

type value =
  | Null
  | Bool of bool
  | Integer of int
  | Float of float
  | String of string (* TODO: context *)
  | Attrs of attrs
  | List of value iarray
  | Lambda of lambda
  | Thunk of value Parallel.Lazy.t
[@@deriving sexp_of]

and attrs = { attrs : value Ident.Map.t } [@@unboxed] [@@deriving sexp_of]

and lambda =
  { args : Ident.t iarray
  ; body : expr
  }
[@@deriving sexp_of]

and expr = { desc : expr_desc }

and expr_desc =
  | Lit of value
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

and attrset = (name:Ident.t * value:expr) iarray

and bin_op =
  | Plus
  | Minus
