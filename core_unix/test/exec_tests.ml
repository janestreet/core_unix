open! Core
open! Import

let%expect_test "Unix.fork_exec" =
  let pid = Unix.fork_exec ~prog:"program_that_doesnt_exist" ~argv:[] () in
  let res = Unix.waitpid pid |> Unix.Exit_or_signal.or_error in
  print_s [%sexp (res : unit Or_error.t)];
  [%expect {| (Error (Unix.Exit_or_signal (Exit_non_zero 127))) |}]
;;

let%expect_test "fork then exec /bin/true" =
  match Unix.fork () with
  | `In_the_child -> never_returns (Unix.exec ~prog:"/bin/true" ~argv:[] ())
  | `In_the_parent pid ->
    let res = Unix.waitpid pid |> Unix.Exit_or_signal.or_error in
    print_s [%sexp (res : unit Or_error.t)];
    [%expect {| (Ok ()) |}]
;;

let%expect_test "fork then exec /bin/false" =
  match Unix.fork () with
  | `In_the_child -> never_returns (Unix.exec ~prog:"/bin/false" ~argv:[] ())
  | `In_the_parent pid ->
    let res = Unix.waitpid pid |> Unix.Exit_or_signal.or_error in
    print_s [%sexp (res : unit Or_error.t)];
    [%expect {| (Error (Unix.Exit_or_signal (Exit_non_zero 1))) |}]
;;

let%expect_test "fork then exec with args" =
  match Unix.fork () with
  | `In_the_child ->
    never_returns
      (Unix.exec
         ~prog:"/bin/echo"
         ~argv:[ "/bin/echo"; "hello"; "there"; "how"; "are"; "you" ]
         ())
  | `In_the_parent pid ->
    let res = Unix.waitpid pid |> Unix.Exit_or_signal.or_error in
    print_s [%sexp (res : unit Or_error.t)];
    [%expect
      {|
      hello there how are you
      (Ok ())
      |}]
;;

let%expect_test "fork then exec failed" =
  (match Unix.fork () with
   | `In_the_child ->
     (try never_returns (Unix.exec ~prog:"program_that_doesnt_exist" ~argv:[] ()) with
      | exn -> print_s [%sexp (exn : Exn.t)]);
     (* This is a little strange. To get the error out of the child process we have to
        print the exception and then call [_exit] to not call at_exit handlers in that
        process. (they're the same handlers as in the parent, so e.g. expect tests get
        messed up if we allow them to run) *)
     Caml_unix._exit 127
   | `In_the_parent pid ->
     let res = Unix.waitpid pid |> Unix.Exit_or_signal.or_error in
     print_s [%sexp (res : unit Or_error.t)]);
  [%expect
    {|
    (Unix.Unix_error
     "No such file or directory"
     execvp
     "((prog program_that_doesnt_exist) (argv ()))")
    (Error (Unix.Exit_or_signal (Exit_non_zero 127)))
    |}]
;;

let%expect_test "at_exit handlers not executed in child process" =
  let do_exec = ref true in
  Stdlib.at_exit (fun () -> if !do_exec then print_endline "at_exit handler executed");
  let pid = Unix.fork_exec ~prog:"program_that_doesnt_exist" ~argv:[] () in
  let res = Unix.waitpid pid |> Unix.Exit_or_signal.or_error in
  print_s [%sexp (res : unit Or_error.t)];
  [%expect {| (Error (Unix.Exit_or_signal (Exit_non_zero 127))) |}];
  do_exec := false
;;

(* Disable this [at_exit] for other tests *)
