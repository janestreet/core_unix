open! Core
open! Import

let%expect_test "Unix.fork_exec" =
  show_raise (fun () -> Unix.fork_exec ~prog:"program_that_doesnt_exist" ~argv:[] ());
  [%expect {| (raised (Core_unix.fork_exec (exec program_that_doesnt_exist) ENOENT)) |}]
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
  show_raise (fun () -> Unix.fork_exec ~prog:"program_that_doesnt_exist" ~argv:[] ());
  [%expect {| (raised (Core_unix.fork_exec (exec program_that_doesnt_exist) ENOENT)) |}];
  (* Disable this [at_exit] for other tests *)
  do_exec := false
;;

let%expect_test "fork_exec redirection" =
  let filename = Filename_unix.temp_file "test_fork_exec" ".txt" in
  Exn.protect
    ~finally:(fun () -> Sys_unix.remove filename)
    ~f:(fun () ->
      let pipe_rd, pipe_wr = Unix.pipe () in
      let _ : int = Unix.write ~pos:0 ~len:5 pipe_wr ~buf:(Bytes.of_string "hello") in
      Unix.close pipe_wr;
      Unix.fork_exec
        ~use_path:true
        ~prog:"cat"
        ~argv:[ "cat" ]
        ~preexec:
          [ Fd_open { filename; flags = [ O_WRONLY ]; perm = 0o644; fd = Unix.stdout }
          ; Fd_dup2 { src = pipe_rd; dst = Unix.stdin }
          ; Fd_close pipe_rd
          ]
        ()
      |> Unix.waitpid_exn;
      Unix.close pipe_rd;
      Unix.fork_exec
        ~use_path:true
        ~prog:"cat"
        ~argv:[ "cat" ]
        ~preexec:[ Fd_open { filename; flags = [ O_RDONLY ]; perm = 0; fd = Unix.stdin } ]
        ()
      |> Unix.waitpid_exn);
  [%expect {| hello |}]
;;

(* Test that fork_exec is able to move its internal pipe fd out of the way, even if the
   user uses lots of fds *)
let%expect_test "fork_exec preexec FD management" =
  let preexec =
    List.init 100 ~f:(fun i ->
      Unix.Pre_exec_command.Fd_dup2
        { src = Unix.stdin; dst = Unix.File_descr.of_int (3 + i) })
    @ [ Unix.Pre_exec_command.Fd_open
          { fd = Unix.stdout
          ; filename = "/does/not/exist"
          ; flags = [ O_WRONLY ]
          ; perm = 0
          }
      ]
  in
  show_raise (fun () ->
    let res =
      Unix.fork_exec ~prog:"/bin/true" ~argv:[ "true" ] ~preexec ()
      |> Unix.waitpid
      |> Unix.Exit_or_signal.or_error
    in
    print_s [%sexp (res : unit Or_error.t)]);
  [%expect
    {|
    (raised (
      Core_unix.fork_exec
      (Fd_open
        (fd       1)
        (filename /does/not/exist)
        (flags (O_WRONLY))
        (perm 0))
      ENOENT))
    |}]
;;

(* Test various combinations of signal masking and dispositions in fork_exec preexec, by
   checking that writing to a closed pipe dies with SIGPIPE or exits with an error, as
   appropriate *)
let%expect_test "fork_exec preexec signal manipulation" =
  let pipe_rd, pipe_wr = Unix.pipe ~close_on_exec:false () in
  Unix.close pipe_rd;
  let run preexec =
    let preexec : Unix.Pre_exec_command.t list =
      Fd_dup2 { src = pipe_wr; dst = Unix.stdout }
      :: Fd_open
           { filename = "/dev/null"; flags = [ O_WRONLY ]; perm = 0; fd = Unix.stderr }
      :: preexec
    in
    Unix.fork_exec ~preexec ~prog:"/bin/echo" ~argv:[ "echo"; "hello" ] ()
    |> Unix.waitpid
    |> Unix.Exit_or_signal.or_error
  in
  let sigpipe disp = Stdlib.Sys.Safe.signal Stdlib.Sys.sigpipe disp in
  let sigpipe' disp = ignore (sigpipe disp : Stdlib.Sys.signal_behavior) in
  (* Signal_default: causes SIGPIPE *)
  let prev_state = sigpipe Signal_default in
  print_s [%sexp (run [] : unit Or_error.t)];
  [%expect {| (Error (Unix.Exit_or_signal (Signal sigpipe))) |}];
  (* Signal handler: exec will reset this to default, so SIGPIPE *)
  sigpipe' (Signal_handle (fun _ -> failwith "should never run"));
  print_s [%sexp (run [] : unit Or_error.t)];
  [%expect {| (Error (Unix.Exit_or_signal (Signal sigpipe))) |}];
  (* Signal_ignore: no SIGPIPE *)
  sigpipe' Signal_ignore;
  print_s [%sexp (run [] : unit Or_error.t)];
  [%expect {| (Error (Unix.Exit_or_signal (Exit_non_zero 1))) |}];
  (* Signal_ignore, but re-enabled in preexec: causes SIGPIPE *)
  sigpipe' Signal_ignore;
  print_s [%sexp (run [ Signal_setdefault Signal.pipe ] : unit Or_error.t)];
  [%expect {| (Error (Unix.Exit_or_signal (Signal sigpipe))) |}];
  (* Signal_default, but ignored in preexec: no SIGPIPE *)
  sigpipe' Signal_default;
  print_s [%sexp (run [ Signal_setignore Signal.pipe ] : unit Or_error.t)];
  [%expect {| (Error (Unix.Exit_or_signal (Exit_non_zero 1))) |}];
  (* Signal_default, but masked: no SIGPIPE *)
  sigpipe' Signal_default;
  print_s [%sexp (run [ Signal_setmask [ Signal.pipe ] ] : unit Or_error.t)];
  [%expect {| (Error (Unix.Exit_or_signal (Exit_non_zero 1))) |}];
  (* Restore state *)
  sigpipe' prev_state;
  Unix.close pipe_wr;
  ()
;;

let%expect_test "fork_exec preexec PDEATHSIG" =
  let pipe_rd, pipe_wr = Unix.pipe ~close_on_exec:false () in
  if String.equal (Unix.Utsname.sysname (Unix.uname ())) "Linux"
  then (
    match Unix.fork () with
    | `In_the_child ->
      let _pid =
        (* Use SET_PDEATHSIG to kill the sleep when the parent exits *)
        Unix.fork_exec
          ~preexec:[ Signal_setpdeathsig Signal.term ]
          ~use_path:true
          ~prog:"sleep"
          ~argv:[ "sleep"; "1d" ]
          ()
      in
      Unix.exit_immediately 0
    | `In_the_parent pid ->
      Unix.close pipe_wr;
      Unix.waitpid_exn pid;
      (* This read will return only when the last process holding the write end exits *)
      let n = Unix.read ~pos:0 ~len:1 pipe_rd ~buf:(Bytes.make 1 'a') in
      assert (n = 0))
;;

let%expect_test "fork_exec preexec setscheduler" =
  Unix.fork_exec ~prog:"/bin/sh" ~argv:[ "sh"; "-c"; "chrt -p $$ | grep -o 'SCHED.*'" ] ()
  |> Unix.waitpid_exn;
  [%expect {| SCHED_OTHER |}];
  Unix.fork_exec
    ~preexec:[ Sched_setscheduler { policy = Round_robin; priority = 10 } ]
    ~prog:"/bin/sh"
    ~argv:[ "sh"; "-c"; "chrt -p $$ | grep -o 'SCHED.*'" ]
    ()
  |> Unix.waitpid_exn;
  [%expect {| SCHED_RR |}];
  show_raise (fun () ->
    Unix.fork_exec
      ~preexec:[ Sched_setscheduler { policy = Round_robin; priority = -100 } ]
      ~prog:"/bin/true"
      ~argv:[ "true" ]
      ()
    |> Unix.waitpid_exn);
  [%expect
    {|
    (raised (
      Core_unix.fork_exec
      (Sched_setscheduler
        (policy   Round_robin)
        (priority -100))
      EINVAL))
    |}]
;;

let%expect_test "fork_exec preexec niceness" =
  match Unix.RLimit.nice with
  | Error _ -> ()
  | Ok resource ->
    Unix.fork_exec ~use_path:true ~prog:"nice" ~argv:[ "nice" ] () |> Unix.waitpid_exn;
    [%expect {| 0 |}];
    Unix.fork_exec
      ~use_path:true
      ~preexec:[ Sched_nice { niceness = 10; ignore_eperm = false } ]
      ~prog:"nice"
      ~argv:[ "nice" ]
      ()
    |> Unix.waitpid_exn;
    [%expect {| 10 |}];
    let before = Unix.RLimit.get resource in
    Unix.RLimit.set resource { before with cur = Limit (Int64.of_int 0) };
    Unix.fork_exec
      ~use_path:true
      ~preexec:[ Sched_nice { niceness = -20; ignore_eperm = true } ]
      ~prog:"nice"
      ~argv:[ "nice" ]
      ()
    |> Unix.waitpid_exn;
    [%expect {| 0 |}];
    show_raise (fun () ->
      Unix.fork_exec
        ~use_path:true
        ~preexec:[ Sched_nice { niceness = -20; ignore_eperm = false } ]
        ~prog:"nice"
        ~argv:[ "nice" ]
        ()
      |> Unix.waitpid_exn);
    [%expect
      {|
      (raised (
        Core_unix.fork_exec
        (Sched_nice
          (niceness     -20)
          (ignore_eperm false))
        EPERM))
      |}];
    Unix.RLimit.set resource before
;;

let%expect_test "fork_exec preexec setaffinity" =
  Unix.fork_exec
    ~preexec:[ Sched_setaffinity [ 0 ] ]
    ~use_path:true
    ~prog:"nproc"
    ~argv:[ "nproc" ]
    ()
  |> Unix.waitpid_exn;
  [%expect {| 1 |}];
  show_raise (fun () ->
    Unix.fork_exec
      ~preexec:[ Sched_setaffinity [ 0; 100000 ] ]
      ~prog:"/bin/true"
      ~argv:[ "true" ]
      ()
    |> Unix.waitpid_exn);
  [%expect {| (raised (Core_unix.fork_exec (Sched_setaffinity (0 100000)) EINVAL)) |}]
;;

let%expect_test "fork_exec fork failure" =
  if String.equal (Unix.Utsname.sysname (Unix.uname ())) "Linux"
  then (
    match Unix.fork () with
    | `In_the_child ->
      Unix.fork_exec
        ~use_path:true
        ~prog:"prlimit"
        ~argv:[ "prlimit"; "--nproc=0:"; "--pid"; Unix.getpid () |> Pid.to_string ]
        ()
      |> Unix.waitpid_exn;
      show_raise (fun () ->
        Unix.fork_exec ~prog:"/bin/true" ~argv:[ "true" ] () |> Unix.waitpid_exn);
      Unix.exit_immediately 0
    | `In_the_parent pid ->
      Unix.waitpid_exn pid;
      [%expect {| (raised (Core_unix.fork_exec vfork EAGAIN)) |}])
;;
