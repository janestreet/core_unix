open! Core
module Path = Command.Private.Path

let read_stdout_and_stderr (process_info : Core_unix.Process_info.t) =
  (* We need to read each of stdout and stderr in a separate thread to avoid deadlocks
     if the child process decides to wait for a read on one before closing the other.
     Buffering may hide this problem until output is "sufficiently large". *)
  let start_reading descr info =
    let output = ref None in
    let thread =
      Core_thread.create
        ~on_uncaught_exn:`Print_to_stderr
        (fun () ->
          let result =
            Result.try_with (fun () ->
              descr |> Core_unix.in_channel_of_descr |> In_channel.input_all)
          in
          output := Some result)
        ()
    in
    Staged.stage (fun () ->
      Core_thread.join thread;
      Core_unix.close descr;
      match !output with
      | None -> raise_s [%message "BUG failed to read" (info : Info.t)]
      | Some (Ok output) -> output
      | Some (Error exn) -> raise exn)
  in
  (* We might hang forever trying to join the reading threads if the child process keeps
     the file descriptor open. Not handling this because I think we've never seen it
     in the wild despite running vulnerable code for years. *)
  (* We have to start both threads before joining any of them. *)
  let finish_stdout = start_reading process_info.stdout (Info.of_string "stdout") in
  let finish_stderr = start_reading process_info.stderr (Info.of_string "stderr") in
  Staged.unstage finish_stdout (), Staged.unstage finish_stderr ()
;;

module For_unix = Command.Private.For_unix (struct
    module Pid = Pid

    module Unix = struct
      let getpid = Core_unix.getpid
      let putenv = (Core_unix.putenv [@ocaml.alert "-unsafe_multidomain"])
      let unsafe_getenv = Sys_unix.unsafe_getenv
      let unsetenv = (Core_unix.unsetenv [@ocaml.alert "-unsafe_multidomain"])
      let exec = Core_unix.exec

      module Run_output = struct
        type t =
          { stdout : string
          ; stderr : string
          }
      end

      let run ~prog ~args ~env () =
        let process_info = Core_unix.create_process_env ~prog ~args ~env () in
        Core_unix.close process_info.stdin;
        let stdout, stderr = read_stdout_and_stderr process_info in
        ignore
          (Core_unix.wait (`Pid process_info.pid) : Pid.t * Core_unix.Exit_or_signal.t);
        { Run_output.stdout; stderr }
      ;;
    end

    module Version_util = Version_util_compat
  end)

let run = For_unix.run
let shape = For_unix.shape

module Deprecated = struct
  let run = For_unix.deprecated_run
end

module Shape = struct
  let help_text = For_unix.help_for_shape
end
