open! Core
open! Await
open! Async
module Ast = Ast
module Ident = Ident
module Value = Value
module Parser = Parser

let () = Parser.pp_exceptions ()
let eval par expr = Eval.eval par ~env:Eval.Env.empty expr

let with_parallel ~f =
  Concurrent_in_async.schedule_with_concurrent Terminator.unkillable ~f:(fun conc ->
    let scheduler =
      Parallel_scheduler.scheduler ~on_root:(Concurrent.scheduler conc) ()
    in
    let conc = Concurrent.create (Concurrent.await conc) ~scheduler in
    f conc [@nontail])
;;

module Eval = struct
  module Input = struct
    type t =
      | Expr of string
      | Path of File_path.t

    let param =
      Command.Param.choose_one_non_optional
        ~if_nothing_chosen:Raise
        [ (let%map_open.Command expr =
             flag "expr" (required string) ~doc:"EXPR expr to evaluate"
           in
           Expr expr)
        ; (let%map_open.Command path =
             flag "file" (required File_path.arg_type) ~doc:"PATH file path to evaluate"
           in
           Path path)
        ]
    ;;

    let load = function
      | Expr expr -> return expr
      | Path path -> Filesystem_async.read_file path
    ;;
  end

  let command =
    Command.async
      ~summary:"Evaluate and print the result of a nix expression"
      (let%map_open.Command () = return ()
       and input = Input.param in
       fun () ->
         let%bind input = Input.load input in
         with_parallel ~f:(fun conc ->
           match Parser.parse_string input with
           | exception exn ->
             Core.print_endline (Stdlib.Printexc.to_string exn);
             Error exn
           | expr ->
             Concurrent.spawn_join conc () ~f:(fun _ par _ : (unit, exn) result ->
               let res = eval par expr |> Value.recursive_force par in
               Core.print_endline (Value.to_string par res);
               Ok ()))
         >>= function
         | Ok () -> return ()
         | Error _ -> exit 1)
  ;;
end

let command =
  Command.group ~summary:"nOx - an oxcaml nix evaluator" [ "eval", Eval.command ]
;;
