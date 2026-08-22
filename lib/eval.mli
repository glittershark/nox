@@ portable

open! Core

module Env : sig
  type t : value mod contended portable

  val empty : t
end

val eval : Parallel.t @ local -> env:Env.t -> Ast.expr -> Value.t
