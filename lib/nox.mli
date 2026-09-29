@@ portable

open! Core
open! Async
module Ast = Ast
module Ident = Ident
module Value = Value
module Parser = Parser

val eval : Parallel.t @ local -> Ast.expr -> Value.t
val command : Command.t @@ nonportable
