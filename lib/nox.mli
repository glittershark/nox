@@ portable

open! Core
open! Async
module Ast = Ast
module Value = Value

val eval : Parallel.t @ local -> Ast.expr -> Value.t
val command : Command.t @@ nonportable
