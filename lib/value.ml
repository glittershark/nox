open! Core

module Parallel_lazy = struct
  type ('a : value mod contended) t = 'a Parallel.Lazy.t

  let sexp_of_t (type a : value mod contended) (sexp_of_a : a -> Sexp.t) t =
    match Parallel.Lazy.peek t with
    | Null -> [%sexp "<thunk>"]
    | This v -> [%sexp (v : a)]
  ;;
end

type t = Types.Value.t =
  | Null
  | Bool of bool
  | Integer of int
  | Float of float
  | String of string (* TODO: context *)
  | Attrs of attrs
  | List of t iarray
  | Lambda of lambda
  | Thunk of t Parallel_lazy.t
[@@deriving sexp_of]

and attrs = Types.Attrs.t = { attrs : t Ident.Map.t } [@@unboxed] [@@deriving sexp_of]

and lambda = Types.Lambda.t =
  { args : Ident.t iarray
  ; body : (Ast.expr[@sexp.opaque])
  }
[@@deriving sexp_of]

module Attrs = struct
  include Types.Attrs

  let sexp_of_t = sexp_of_attrs
  let empty = { attrs = Ident.Map.empty }
end

module Lambda = struct
  include Types.Lambda

  let sexp_of_t = sexp_of_lambda
end

let force par : t -> t = function
  | Thunk lzy -> Parallel.Lazy.force par lzy
  | t -> t
;;

let rec deep_force par : t -> t = function
  | Thunk lzy -> Parallel.Lazy.force par lzy |> deep_force par
  | t -> t
;;

let plus par v1 v2 =
  match #(deep_force par v1, deep_force par v2) with
  | #(Integer i1, Integer i2) -> Integer (i1 + i2)
  | #(String s1, String s2) -> String (s1 ^ s2)
  | _ -> failwith "Cannot + those things I think?"
;;

let minus par v1 v2 =
  match #(deep_force par v1, deep_force par v2) with
  | #(Integer i1, Integer i2) -> Integer (i1 - i2)
  | _ -> failwith "Cannot - those things I think?"
;;

let thunk f : t = Thunk (Parallel.Lazy.from_fun (fun par : t -> f par))
