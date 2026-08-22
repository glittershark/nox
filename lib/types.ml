open! Core

module rec Value : sig
  type t =
    | Null
    | Bool of bool
    | Integer of int
    | Float of float
    | String of string (* TODO: context *)
    | Attrs of Attrs.t
    | List of t iarray
    | Lambda of Lambda.t
    | Thunk of t Parallel.Lazy.t
end =
  Value

and Attrs : sig
  type t = { attrs : Value.t Ident.Map.t } [@@unboxed]
end =
  Attrs

and Lambda : sig
  type t =
    { args : Ident.t iarray
    ; body : Ast.expr
    }
end =
  Lambda

and Ast : sig
  type expr = { desc : expr_desc }

  and expr_desc =
    | Lit of Value.t
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
end =
  Ast
