open! Core
open! Import

type lambda = Types.lambda =
  { args : Ident.t iarray
  ; body : (Ast.expr[@sexp.opaque])
  }
[@@deriving sexp_of]

let quickcheck_observer_lambda = Quickcheck.Observer.singleton ()
let quickcheck_shrinker_lambda = Quickcheck.Shrinker.empty ()

module T = struct
  type t = Types.value =
    | Null
    | Bool of bool
    | Integer of int
    | Float of float
    | String of string (* TODO: context *)
    | Attrs of attrs
    | List of t iarray
    | Lambda of lambda [@quickcheck.do_not_generate]
    | Thunk of t Parallel.Lazy.t
  [@@deriving sexp_of, quickcheck]

  and attrs = Types.attrs = { attrs : t Map.M(Ident).t }
  [@@unboxed] [@@deriving sexp_of, quickcheck ~portable]
end

include T

module Attrs = struct
  type t = Types.attrs = { attrs : T.t Map.M(Ident).t }
  [@@unboxed] [@@deriving sexp_of ~portable, quickcheck ~portable]

  let empty = { attrs = Ident.Map.empty }
end

module Lambda = struct
  type t = Types.lambda =
    { args : Ident.t iarray
    ; body : (Ast.expr[@sexp.opaque])
    }
  [@@deriving sexp_of]

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

let rec to_string =
  let escape_string =
    String.Escaping.escape ~escapeworthy:[ '"'; '$'; '\\' ] ~escape_char:'\\'
    |> (Staged.unstage [@mode portable])
  in
  fun par t ->
    match t with
    | Null -> "null"
    | Bool true -> "true"
    | Bool false -> "false"
    | Integer i -> Int.to_string i
    | Float f -> Float.to_string f
    | String s ->
      let escaped = escape_string s in
      [%string {|"%{escaped}"|}]
    | Attrs a ->
      String.concat
        [ "{"
        ; Map.fold a.attrs ~init:"" ~f:(fun ~key ~data acc ->
            String.concat [ acc; Ident.to_string key; " = "; to_string par data; ";" ])
        ; "}"
        ]
    | List l ->
      String.concat
        [ "["; Iarray.fold l ~init:"" ~f:(fun acc v -> acc ^ " " ^ to_string par v); "]" ]
    | Lambda _ -> "<lambda>"
    | Thunk _ -> to_string par (force par t)
;;

let rec equal par a b =
  match a, b with
  | Thunk t1, _ -> equal par (Parallel.Lazy.force par t1) b
  | _, Thunk t2 -> equal par a (Parallel.Lazy.force par t2)
  | Null, Null -> true
  | Null, _ -> false
  | Bool b1, Bool b2 -> [%equal: bool] b1 b2
  | Bool _, _ -> false
  | Integer i1, Integer i2 -> [%equal: int] i1 i2
  | Integer _, _ -> false
  | Float f1, Float f2 -> [%equal: float] f1 f2
  | Float _, _ -> false
  | String s1, String s2 -> [%equal: string] s1 s2
  | String _, _ -> false
  | Attrs a1, Attrs a2 ->
    Map.length a1.attrs = Map.length a2.attrs
    && (Map.for_alli a1.attrs ~f:(fun ~key ~data ->
          match Map.find a2.attrs key with
          | None -> false
          | Some data2 -> equal par data data2)
    [@nontail])
  | Attrs _, _ -> false
  | List l1, List l2 ->
    Iarray.length l1 = Iarray.length l2
    && (Iarray.for_alli l1 ~f:(fun i v1 -> equal par v1 (Iarray.get l2 i)) [@nontail])
  | List _, _ -> false
  (* Lambdas are never equal, not even to themselves. *)
  | Lambda _, _ -> false
;;
