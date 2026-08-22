open! Core

include module type of struct
  include Types.Ast
end

val uncurry_lambda : expr -> expr
