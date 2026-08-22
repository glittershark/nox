open! Core
open! Await
open! Async
module Ast = Ast
module Value = Value

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

let cmd_eval =
  Command.async
    ~summary:"Evaluate and print the result of a nix expression"
    (let%map_open.Command () = return ()
     and expr = flag "expr" (required string) ~doc:"EXPR expr to evaluate" in
     fun () ->
       with_parallel ~f:(fun conc ->
         match Parser.parse_string expr with
         | exception exn ->
           Core.print_endline (Stdlib.Printexc.to_string exn);
           Error exn
         | expr ->
           Concurrent.spawn_join conc () ~f:(fun _ par _ : (unit, exn) result ->
             let res = eval par expr |> Value.deep_force par in
             Core.print_s [%sexp (res : Value.t)];
             Ok ()))
       >>= function
       | Ok () -> return ()
       | Error _ -> exit 1)
;;

let command = Command.group ~summary:"nOx - an oxcaml nix evaluator" [ "eval", cmd_eval ]
