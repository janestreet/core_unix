open! Core

let command =
  Command.basic
    ~summary:""
    (let%map_open.Command _t = anon ("T" %: int)
     and _s = flag "f" (required string) ~doc:"demo" in
     fun () -> print_endline "executing")
;;

let () = Command_unix.run ~add_validate_parsing_flag:true command
