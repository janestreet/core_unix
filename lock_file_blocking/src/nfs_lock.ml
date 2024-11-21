open! Core
module Unix = Core_unix
open Shared

let boot_time () =
  read_file_and_convert "/proc/stat" ~of_string:(fun s ->
    let lines = String.split_lines s in
    match List.filter_map lines ~f:(String.chop_prefix ~prefix:"btime ") with
    | [ btime ] ->
      Int.of_string btime |> Time_ns.Span.of_int_sec |> Time_ns.of_span_since_epoch
    | _ -> failwith "can't find btime in /proc/stat")
;;

let process_start_time pid =
  (* Find the start time for a process, without requiring the [Procfs] library
       -- start time is represented in USER_HZ units in /proc/<pid>/stat (confusingly
       referred to as 'jiffies' in the man page); USER_HZ is almost certainly 100, for
       mostly historical reasons, but just to be sure we'll ask sysconf.
  *)
  let%bind.Option boot_time = boot_time () in
  let boot_time = Time_ns.to_time_float_round_nearest boot_time in
  let of_string stat =
    let jiffies =
      let fields =
        String.rsplit2_exn stat ~on:')' |> snd |> String.strip |> String.split ~on:' '
      in
      List.nth_exn fields 19 |> Float.of_string
    in
    let hz = Unix.sysconf Unix.CLK_TCK |> Option.value_exn |> Int64.to_float in
    jiffies /. hz |> Time_float.Span.of_sec |> Time_float.add boot_time
  in
  read_file_and_convert (sprintf !"/proc/%{Pid}/stat" pid) ~of_string
;;

module Info = struct
  module T = struct
    type t =
      { host : string
      ; pid : Pid.Stable.V1.t
      ; message : string
      ; start_time : Time_float.Stable.With_utc_sexp.V2.t option [@sexp.option]
      }
    [@@deriving compare, sexp, fields ~getters]
  end

  include T
  include Sexpable.To_stringable (T)

  let create ~message =
    let pid = Unix.getpid () in
    { host = Unix.gethostname (); pid; message; start_time = process_start_time pid }
  ;;

  let of_file = read_file_and_convert ~of_string
end

let lock_path path = path ^ ".nfs_lock"

let lock_path_permissive path =
  if String.is_suffix ~suffix:".nfs_lock" path then path else lock_path path
;;

let get_hostname_and_pid path =
  let lock_path = lock_path_permissive path in
  Option.map (Info.of_file lock_path) ~f:(fun info -> Info.host info, Info.pid info)
;;

let get_message path =
  let lock_path = lock_path_permissive path in
  Option.map (Info.of_file lock_path) ~f:Info.message
;;

let unlink_if_exists path =
  try Unix.unlink path with
  | Unix.Unix_error ((ENOTDIR | ENOENT), _, _) -> ()
;;

(* Check if the process is running: sends signal 0 to pid, which should work if
   the process is running and is owned by the user running this code. If the
   process is not owned by the user running this code we should fail to unlock
   either earlier (unable to read the file) or later (unable to remove the
   file). *)
let pid_start_matches_lock ~info =
  match Option.both (Info.start_time info) (process_start_time (Info.pid info)) with
  | None -> true (* don't have both start times: fall back to old behaviour *)
  | Some (lock_start, pid_start) ->
    (* our method of calculating start time is open to some inaccuracy, so let's
       be generous and allow for up to 1s of difference (this would only allow
       for a collision if pids get reused within 1s, which seems unlikely) *)
    let epsilon = Time_float.Span.of_sec 1. in
    Time_float.Span.( < ) (Time_float.abs_diff lock_start pid_start) epsilon
;;

let read_lock_file ~lock_path =
  match Sys_unix.file_exists ~follow_symlinks:false lock_path with
  | `Unknown -> `Error
  | `No -> `Not_locked
  | `Yes ->
    (match Info.of_file lock_path with
     | None ->
       (* Used to report "unknown lock file format", but that can be confusing. *)
       `Error
     | Some info -> `Locked info)
;;

let with_openfile_exn file ~f =
  (* opening the file for writing to make sure [flock] works regardless of
     how the NFS client is configured (simulation of flock via lockf requires
     write mode) *)
  let fd = Unix.openfile ~mode:[ Unix.O_RDWR ] file in
  Exn.protect ~finally:(fun () -> Unix.close fd) ~f:(fun () -> f fd)
;;

let with_flock_exn fd ~f =
  if not (Unix.flock fd Unix.Flock_command.lock_exclusive)
  then raise_s [%sexp "flock failed (held by another process)"];
  Exn.protect
    ~finally:(fun () ->
      let _ : bool = Unix.flock fd Unix.Flock_command.unlock in
      ())
    ~f
;;

let unlink_lock_files path =
  let lock_path = lock_path path in
  (* We need to be able to recover from the situation where [path] does not exist
     for whatever reason, but [lock_path] is present. We use [unlink_if_exists]
     to be able to cope with this situation and properly clean up stale locks. *)
  unlink_if_exists path;
  Unix.unlink lock_path
;;

let do_if_locked_exn path ~error ~f =
  let lock_path = lock_path path in
  match read_lock_file ~lock_path with
  | `Error -> error (sprintf "failed to parse info from lock file %s" lock_path)
  | `Not_locked -> ()
  | `Locked info -> f ~info
;;

(* Remove the lock if stale, fail if the lock is held by another process. *)
let prepare_to_lock_exn path =
  (* Make sure error messages contain a reference to "lock.nfs_lock", which is the
     actually important file. *)
  let lock_path = lock_path path in
  let error s = failwithf "lock file %S: %s" lock_path s () in
  do_if_locked_exn path ~error ~f:(fun ~info ->
    let my_hostname = Unix.gethostname () in
    let locking_hostname = Info.host info in
    let locking_pid = Info.pid info in
    if String.( <> ) my_hostname locking_hostname
    then
      error
        (sprintf
           "lock already held on %s, unlock attempted from %s"
           locking_hostname
           my_hostname)
    else (
      let locking_pid_exists () =
        Signal_unix.can_send_to locking_pid && pid_start_matches_lock ~info
      in
      if locking_pid_exists ()
      then
        error
          (sprintf
             "locking process (pid %i) still running on %s"
             (Pid.to_int locking_pid)
             locking_hostname)
      else (
        let lock_file_still_there () =
          [%compare.equal: Info.t option] (Info.of_file lock_path) (Some info)
        in
        match lock_file_still_there () with
        | false ->
          error
            (sprintf
               "lock was held by pid %i (but not anymore, please retry)"
               (Pid.to_int locking_pid))
        | true ->
          (* We know the lock is held by a process that no longer exists.  We want to
             clean it up, but we need to protect against many processes "cleaning up" the
             same lock in parallel (thus removing each other's locks).

             Since we know all the cleanup is done on the same box, we can use flock to
             make sure a given file is cleaned up at most once.
             (empirically, flock on NFS ensures mutual exclusion locally)

             We try to detect if some other process also tried to clean this up at the
             same time: If openfile or flock fails, or [lock_file_still_there ()] returns
             false, indicating an ABA problem. The safest thing to do in that case is to
             raise and rely on the outer retry loop (in [blocking_create] or in the
             caller) to handle the new state. *)
          with_openfile_exn lock_path ~f:(fun fd ->
            with_flock_exn fd ~f:(fun () ->
              if lock_file_still_there ()
              then (
                try unlink_lock_files path with
                | e -> error (Exn.to_string e))
              else
                error
                  (sprintf
                     "lost the race trying to clean up after pid %i"
                     (Pid.to_int locking_pid)))))))
;;

let is_my_own_info info =
  String.equal (Unix.gethostname ()) (Info.host info)
  && Pid.equal (Unix.getpid ()) (Info.pid info)
  && pid_start_matches_lock ~info
;;

let unlock_self_exn path =
  let error s =
    failwithf "Lock_file.Nfs.unlock_self_exn: unable to unlock %s: %s" path s ()
  in
  (* We can't just assume that we're holding the lock here, we check it first
     by reading the lock file. It helps to guard against the user error of double-unlock,
     but more importantly our own [at_exit] handler can already do double-unlock,
     since it runs unconditionally. *)
  do_if_locked_exn path ~error ~f:(fun ~info ->
    if not (is_my_own_info info)
    then
      error
        (sprintf
           "lock owned (stolen?) by a different process (pid %i running on %s)"
           (Pid.to_int (Info.pid info))
           (Info.host info))
    else (
      try unlink_lock_files path with
      | e -> error (Exn.to_string e)))
;;

(* See mli for more information on the algorithm we use for locking over NFS.  Ensure
     that you understand it before you make any changes here. *)
let create_exn ?(message = "") path =
  try
    prepare_to_lock_exn path;
    let fd = Unix.openfile path ~mode:[ Unix.O_WRONLY; Unix.O_CREAT ] in
    let cleanup = ref (fun () -> Unix.close fd) in
    protect
      ~finally:(fun () -> !cleanup ())
      ~f:(fun () ->
        Unix.link ~target:path ~link_name:(lock_path path) ();
        Unix.ftruncate fd ~len:0L;
        let info = Info.create ~message in
        (* if this fprintf fails, empty lock file would be left behind, and
             subsequent calls to [Lock_file.Nfs.create_exn] would be unable to
             figure out that it is stale/corrupt and remove it. So we need to
             remove it ourselves *)
        try
          let out_channel = Unix.out_channel_of_descr fd in
          (cleanup := fun () -> Stdlib.close_out_noerr out_channel);
          fprintf out_channel "%s\n%!" (Sexp.to_string_hum (Info.sexp_of_t info))
        with
        | Sys_error _ as err ->
          Unix.unlink path;
          Unix.unlink (lock_path path);
          raise err);
    at_exit (fun () ->
      try unlock_self_exn path with
      | _ -> ())
  with
  | e ->
    failwithf
      "Lock_file.Nfs.create_exn: unable to lock '%s' - %s"
      path
      (Exn.to_string e)
      ()
;;

let create ?message path = Or_error.try_with (fun () -> create_exn ?message path)
let lock_attempt_suffix = ".lock-attempt"

let attempt_path ~path ~info =
  let md5_hash = Md5.to_hex (Md5.digest_string (Info.to_string info)) in
  [%string "%{path}.%{md5_hash}%{lock_attempt_suffix}"]
;;

let is_possibly_attempt_file ~of_:path ~attempt_file_basename =
  let path_basename = Filename.basename path in
  let extracted_md5 =
    let%bind.Option name =
      String.chop_suffix attempt_file_basename ~suffix:lock_attempt_suffix
    in
    let%bind.Option hash = String.chop_prefix ~prefix:(path_basename ^ ".") name in
    Option.try_with (fun () -> Md5.of_hex_exn hash)
  in
  Option.is_some extracted_md5
;;

let%expect_test "attempt parse back" =
  let info = Info.create ~message:"" in
  let path = "/path/to/my-lock" in
  let path_basename = Filename.basename path in
  let attempt_file = attempt_path ~path ~info in
  let attempt_file_basename = Filename.basename attempt_file in
  [%test_eq: bool]
    (is_possibly_attempt_file ~of_:path_basename ~attempt_file_basename)
    true;
  [%test_eq: bool]
    (is_possibly_attempt_file ~of_:"other-lock" ~attempt_file_basename)
    false;
  [%test_eq: bool]
    (is_possibly_attempt_file
       ~of_:path_basename
       ~attempt_file_basename:(path_basename ^ ".notmd5" ^ lock_attempt_suffix))
    false;
  [%test_eq: bool]
    (is_possibly_attempt_file
       ~of_:path_basename
       ~attempt_file_basename:
         (String.substr_replace_all
            attempt_file_basename
            ~pattern:lock_attempt_suffix
            ~with_:".not-lock-attempt"))
    false
;;

let unlink_old_attempts path =
  let dir, path_basename = Filename.split path in
  let relevant_attempts =
    match Sys_unix.readdir dir with
    | exception _ -> [||]
    | files ->
      Array.filter files ~f:(fun attempt_file_basename ->
        is_possibly_attempt_file ~of_:path_basename ~attempt_file_basename)
  in
  (* attempts are supposed to be extremely short-lived (created, then immediately
     linked and removed), so cleaning up attempts that are more than 5 minutes old seems
     fine *)
  let now = Time_float.now () in
  Array.filter_map relevant_attempts ~f:(fun file ->
    let file = dir ^/ file in
    match Core_unix.stat file with
    | exception _ ->
      (* maybe the file was removed? don't bother *)
      None
    | stat ->
      let mtime = Time_float.of_span_since_epoch (Time_float.Span.of_sec stat.st_mtime) in
      Some (Time_float.diff now mtime, file))
  |> Array.to_list
  |> List.sort ~compare:[%compare: Time_float.Span.t * string]
  |> (fun l ->
       (* keep 3 last attempts unconditionally in case it helps investigate something *)
       List.drop l 3)
  |> List.filter_map ~f:(fun (age, file) ->
    if Time_float.Span.( > ) age (Time_float.Span.of_min 5.) then Some file else None)
  |> List.iter ~f:(fun file -> unlink_if_exists file)
;;

let maybe_unlink_old_attempts path =
  (* Cleaning up old attempts should almost never be needed, since the
     attempt files are removed immediately by the process that creates
     them. So we do this probabilistically to avoid always paying the readdir
     overhead, which can be large if the directory contains lots of files.
  *)
  if am_running_test || Float.( < ) (Random.float 1.0) 0.05 then unlink_old_attempts path
;;

let create_v2_exn ?(message = "") path =
  try
    maybe_unlink_old_attempts path;
    let info = Info.create ~message in
    let attempt_path = attempt_path ~path ~info in
    prepare_to_lock_exn path;
    let lock_dir = Filename.dirname path in
    if not (Sys_unix.is_directory_exn lock_dir)
    then
      (* if the directory doesn't exist, this makes the error message not mention the
         non-deterministic attempt filename *)
      failwithf "%s either does not exist or is not a directory" lock_dir ();
    protect
      ~finally:(fun () -> unlink_if_exists attempt_path)
      ~f:(fun () ->
        Out_channel.with_file ~fail_if_exists:true attempt_path ~f:(fun ch ->
          Out_channel.output_string ch (Info.to_string info));
        (* [link] will fail if the target path already exists, so if it succeeds then we
           know that we must have succeeded in taking the lock. *)
        Unix.link ~target:attempt_path ~link_name:(lock_path path) ());
    (try
       (* This file is useless, but we're still creating it for backwards compatibility.
          Some tests check that it exists, and it's possible that some people have scripts
          that read it, so it's easier to create it than to worry about the consequences
          of dropping it.

          Note that we intentionally do not use [Unix.link] here because that would make
          it possible for [create_v1] to overwrite files created by [create_v2]. *)
       Out_channel.write_all path ~data:(Info.to_string info)
     with
     | _ -> ());
    at_exit (fun () ->
      try unlock_self_exn path with
      | _ -> ())
  with
  | e ->
    failwithf
      "Lock_file.Nfs.create_exn: unable to lock '%s' - %s"
      path
      (Exn.to_string e)
      ()
;;

let create_v2 ?message path = Or_error.try_with (fun () -> create_v2_exn ?message path)

(* default timeout is to wait indefinitely *)
let blocking_create ?timeout ?message path =
  repeat_with_timeout ?timeout (fun path -> create_exn ?message path) path
;;

let blocking_create_v2 ?timeout ?message path =
  repeat_with_timeout ?timeout (fun path -> create_v2_exn ?message path) path
;;

let critical_section ?message path ~timeout ~f =
  blocking_create ~timeout ?message path;
  Exn.protect ~f ~finally:(fun () -> unlock_self_exn path)
;;

let unlock_exn path = unlock_self_exn path
let unlock path = Or_error.try_with (fun () -> unlock_exn path)
