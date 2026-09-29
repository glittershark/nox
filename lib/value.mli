@@ portable

open! Core
open! Import

module Lambda : sig
  type t = Types.lambda =
    { args : Ident.t iarray
    ; body : Ast.expr
    }
  [@@deriving sexp_of]
end

module Attrs : sig
  type t = Types.attrs = { attrs : Types.value Ident.Map.t }
  [@@unboxed] [@@deriving sexp_of, quickcheck]

  val empty : t
end

type t = Types.value =
  | Null
  | Bool of bool
  | Integer of int
  | Float of float
  | String of string (* TODO: context *)
  | Attrs of Attrs.t
  | List of t iarray
  | Lambda of Lambda.t
  | Thunk of t Parallel.Lazy.t
[@@deriving sexp_of, quickcheck]

val thunk : (Parallel.t @ local -> t) @ once portable -> t
val plus : Parallel.t @ local -> t -> t -> t
val minus : Parallel.t @ local -> t -> t -> t

(** Force one layer of thunks *)
val force : Parallel.t @ local -> t -> t

(** Force as many layers of thunks as necessary until a normal-form value is reached *)
val recursive_force : Parallel.t @ local -> t -> t

(** Like [recursive_force], but also forces values within attrsets and lists *)
val deep_force : Parallel.t @ local -> t -> t

val to_string : Parallel.t @ local -> t -> string
val equal : Parallel.t @ local -> t -> t -> bool
