open! Core

module Parallel = struct
  include Parallel

  module Lazy : sig @@ portable
    include module type of struct
      include Lazy
    end

    [%%rederive: type nonrec ('a : value mod contended) t = 'a t [@@deriving sexp_of]]

    [%%rederive:
      type nonrec ('a : value mod contended portable) t = 'a t
      [@@deriving quickcheck ~portable]]
  end = struct
    include Lazy

    let sexp_of_t (type a : value mod contended) (sexp_of_a : a -> Sexp.t) t =
      match peek t with
      | Null -> [%sexp "<thunk>"]
      | This v -> [%sexp (v : a)]
    ;;

    [%%template
    [@@@mode.default p = (portable, nonportable)]

    let quickcheck_generator (type a : value mod contended portable) a =
      (Quickcheck.Generator.map [@mode p]) ~f:(fun (a : a) -> from_val a) a
    ;;

    let quickcheck_observer (type a : value mod contended portable) a =
      (Quickcheck.Observer.unmap [@mode p])
        ~f:(fun t : a -> force Parallel.sequential t)
        a
    ;;

    let quickcheck_shrinker (type a : value mod contended portable) a =
      (Quickcheck.Shrinker.map [@mode p])
        a
        ~f:(fun (a : a) -> from_val a)
        ~f_inverse:(fun t : a -> force Parallel.sequential t)
    ;;]
  end
end
