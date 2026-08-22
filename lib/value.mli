@@ portable

open! Core

module Lambda : sig
  type t = Types.Lambda.t =
    { args : Ident.t iarray
    ; body : Ast.expr
    }
  [@@deriving sexp_of]
end

module Attrs : sig
  type t = Types.Attrs.t = { attrs : Types.Value.t Ident.Map.t }
  [@@unboxed] [@@deriving sexp_of]

  val empty : t
end

type t = Types.Value.t =
  | Null
  | Bool of bool
  | Integer of int
  | Float of float
  | String of string (* TODO: context *)
  | Attrs of Attrs.t
  | List of t iarray
  | Lambda of Lambda.t
  | Thunk of t Parallel.Lazy.t
[@@deriving sexp_of]

val thunk : (Parallel.t @ local -> t) @ once portable -> t
val plus : Parallel.t @ local -> t -> t -> t
val minus : Parallel.t @ local -> t -> t -> t
val force : Parallel.t @ local -> t -> t
val deep_force : Parallel.t @ local -> t -> t
