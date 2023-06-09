open Core
open Async

let gettid () =
  let gettid = ok_exn Core_unix.gettid in
  Core.Printf.printf "top -Hp %s\n%!" (Pid.to_string (Core_unix.getpid ()));
  let%bind () = after (sec 5.) in
  Clock.every (sec 1.) (fun () ->
    don't_wait_for
      (In_thread.run (fun () ->
         Core.Printf.printf "%d\n%!" (Core_unix.Thread_id.to_int (gettid ()));
         (* wait for a little while, to force more than one thread to be used *)
         Core_unix.sleep 5)));
  Deferred.never ()
;;

let getifaddrs () =
  Core_unix.getifaddrs () |> List.iter ~f:(printf !"%{sexp:Unix.Ifaddr.t}\n")
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:"core_unix examples"
       [ ( "gettid"
         , Command.async
             ~summary:"demonstrate thread ID call"
             (Command.Param.return gettid) )
       ; ( "getifaddrs"
         , Command.basic
             ~summary:"demonstrate getifaddrs call"
             (Command.Param.return getifaddrs) )
       ])
;;
