open! Core

module Env = struct
  type t : value mod contended portable = Value.t Ident.Map.t

  let empty = Ident.Map.empty
end

let rec eval par ~(env : Env.t) (expr : Ast.expr) =
  match expr.desc with
  | Lit v -> v
  | Ident ident ->
    (match Map.find_or_null env ident with
     | Null -> raise_s [%message "Identifier not bound" (ident : Ident.t)]
     | This v -> v)
  | Bin_op { lhs; op; rhs } ->
    let #(lhs_v, rhs_v) =
      Parallel.fork_join2
        par
        (fun par -> eval par ~env lhs)
        (fun par -> eval par ~env rhs)
    in
    (match op with
     | Plus -> Value.plus par lhs_v rhs_v
     | Minus -> Value.minus par lhs_v rhs_v)
  | If { cond; then_; else_ } ->
    let cond = eval par ~env cond in
    (match Value.deep_force par cond with
     | Bool true -> eval par ~env then_
     | Bool false -> eval par ~env else_
     | _ -> raise_s [%message "Expecting boolean in condition of if"])
  | Lam { args; body } -> Lambda { args; body }
  | Apply { func; args } -> eval_apply par ~env func args
  | Let { bindings; body } ->
    let { Value.Attrs.attrs = bindings } = eval_attrset par ~env bindings in
    let env = Map.merge_by_case env bindings ~first:Keep ~second:Keep ~both:Keep_second in
    eval par ~env body
  | List lst ->
    lst
    |> Parallel.Arrays.Iarray.of_iarray
    |> Parallel.Arrays.Iarray.map par ~f:(fun par expr -> eval par ~env expr)
    |> Parallel.Arrays.Iarray.to_iarray
    |> List
  | Attrset attrs -> eval_attrset par ~env attrs |> Attrs

and eval_apply par ~env func args =
  let #(func_v, args_vs) =
    Parallel.fork_join2
      par
      (fun par -> eval par ~env func)
      (fun par ->
        args
        |> Parallel.Arrays.Iarray.of_iarray
        |> Parallel.Arrays.Iarray.map par ~f:(fun par arg -> eval par ~env arg)
        |> Parallel.Arrays.Iarray.to_iarray)
  in
  let { Value.Lambda.args = arg_names; body } =
    match Value.deep_force par func_v with
    | Lambda l -> l
    | _ -> raise_s [%message "Expected function"]
  in
  let nargs_in_lambda = Iarray.length arg_names in
  let nargs_in_apply = Iarray.length args_vs in
  if nargs_in_apply >= nargs_in_lambda
  then (
    let env =
      let mutable env' = env in
      for i = 0 to nargs_in_lambda - 1 do
        env'
        <- Map.set
             env'
             ~key:(Iarray.unsafe_get arg_names i)
             ~data:(Iarray.unsafe_get args_vs i)
      done;
      env'
    in
    let res = eval par ~env body in
    if nargs_in_apply = nargs_in_lambda
    then res
    else (
      (* over-application *)
      let extra_args =
        Iarray.init (nargs_in_apply - nargs_in_lambda) ~f:(fun i ->
          string_of_int i |> Ident.of_string)
      in
      Lambda
        { args = extra_args
        ; body =
            { desc =
                Apply
                  { func = { desc = Lit res }
                  ; args =
                      Iarray.map extra_args ~f:(fun arg : Ast.expr ->
                        { desc = Ident arg })
                  }
            }
        }))
  else (
    (* under-application *)
    let extra_args = Iarray.subo arg_names ~pos:(nargs_in_lambda - nargs_in_apply) in
    Lambda
      { args = extra_args
      ; body =
          { desc =
              Apply
                { func = { desc = Lit func_v }
                ; args =
                    Iarray.init nargs_in_lambda ~f:(fun i : Ast.expr ->
                      { desc =
                          (if i < nargs_in_apply
                           then (* TODO: unsafe *)
                             Lit (Iarray.get args_vs i)
                           else Ident (Iarray.get extra_args (i - nargs_in_apply)))
                      })
                }
          }
      })

and eval_attrset par ~env attrset =
  { Value.Attrs.attrs =
      Ident.Map.of_iteri_exn ~iteri:(fun ~f ->
        Iarray.iter attrset ~f:(fun (~name, ~value) ->
          f ~key:name ~data:(Value.thunk (fun par -> eval par ~env value)))
        [@nontail])
  }
;;
