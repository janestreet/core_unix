open Core
module Unix = Core_unix

(* Currently this test doesn't always pass for Nfs lock: it detects both a safety bug
   where we're cleaning up someone else's lock and a liveness bug where we're trying to
   unlock an empty [lockfile]. *)

(* this is so that after safety violation everything stops *)
let exit_if_safety_violation_observed () =
  match Unix.access "die" [ `Read ] with
  | Error _exn -> ()
  | Ok () -> exit 1
;;

let maybe_die_to_leave_behind_stale_lockfile () =
  let stale_lockfile_probability = 0.1 in
  if Float.(Random.float 1. < stale_lockfile_probability)
  then Signal_unix.send_exn Signal.kill (`Pid (Unix.getpid ()))
;;

let delay t =
  let t = Random.float_range 0.0 t in
  let (_ : float) = Unix.nanosleep t in
  ()
;;

let nfs_critical_section path ~f =
  let rec obtain () =
    exit_if_safety_violation_observed ();
    match Lock_file_blocking.Nfs.create path with
    | Error _e ->
      delay 0.0003;
      obtain ()
    | Ok () -> ()
  in
  obtain ();
  let res = f () in
  Lock_file_blocking.Nfs.unlock_exn path;
  res
;;

let nfs_v2_critical_section path ~f =
  let rec obtain () =
    exit_if_safety_violation_observed ();
    match Lock_file_blocking.Nfs.create_v2 path with
    | Error _e ->
      delay 0.0003;
      obtain ()
    | Ok () -> ()
  in
  obtain ();
  let res = f () in
  Lock_file_blocking.Nfs.unlock_exn path;
  res
;;

let critical_section (type a) ~lock ~unlock ~(f : unit -> a) : a =
  let rec obtain () =
    exit_if_safety_violation_observed ();
    match lock () with
    | `Somebody_else_took_it ->
      delay 0.0003;
      obtain ()
    | `We_took_it lock -> lock
  in
  let lock = obtain () in
  protect ~f ~finally:(fun () -> unlock lock)
;;

(* not quite a critical section because it only ends when the process dies, but close
   enough *)
let local_critical_section path ~f =
  let rec obtain () =
    exit_if_safety_violation_observed ();
    match Lock_file_blocking.create path with
    | false ->
      delay 0.0003;
      obtain ()
    | true -> ()
  in
  obtain ();
  f ()
;;

let mkdir_critical_section (type a) path ~(f : unit -> a) : a =
  critical_section
    ~lock:(fun () -> Lock_file_blocking.Mkdir.lock_exn ~lock_path:path)
    ~unlock:Lock_file_blocking.Mkdir.unlock_exn
    ~f
;;

let symlink_critical_section path ~f =
  critical_section
    ~lock:(fun () ->
      let metadata = sprintf "pid %s" (Unix.getpid () |> Pid.to_string) in
      match Lock_file_blocking.Symlink.lock_exn ~lock_path:path ~metadata with
      | `Somebody_else_took_it _metadata -> `Somebody_else_took_it
      | `We_took_it lock -> `We_took_it lock)
    ~unlock:Lock_file_blocking.Symlink.unlock_exn
    ~f
;;

let mkdir_or_symlink_critical_section (type a) path ~(f : unit -> a) : a =
  let critical_section =
    match Random.bool () with
    | false -> mkdir_critical_section
    | true -> symlink_critical_section
  in
  critical_section path ~f
;;

let nfs_v1_or_v2_critical_section (type a) path ~(f : unit -> a) : a =
  let critical_section =
    match Random.bool () with
    | false -> nfs_critical_section
    | true -> nfs_v2_critical_section
  in
  critical_section path ~f
;;

let flock_critical_section path ~f =
  critical_section
    ~lock:(fun () -> Lock_file_blocking.Flock.lock_exn () ~lock_path:path)
    ~unlock:Lock_file_blocking.Flock.unlock_exn
    ~f
;;

let critical_section ~which_lock path ~f =
  match which_lock with
  | `Nfs -> nfs_critical_section path ~f
  | `Nfs_v2 -> nfs_v2_critical_section path ~f
  | `Nfs_v1_or_v2 -> nfs_v1_or_v2_critical_section path ~f
  | `Local -> local_critical_section path ~f
  | `Mkdir_or_symlink -> mkdir_or_symlink_critical_section path ~f
  | `Symlink -> symlink_critical_section path ~f
  | `Flock -> flock_critical_section path ~f
;;

let save file contents =
  let fd = Unix.openfile file ~mode:[ O_WRONLY; O_CREAT ] in
  let out_channel = Unix.out_channel_of_descr fd in
  fprintf out_channel "%s\n%!" contents
;;

let go ~which_lock ~test_stale_locks path =
  match Unix.fork () with
  | `In_the_child ->
    Random.self_init ();
    let () =
      critical_section ~which_lock path ~f:(fun () ->
        exit_if_safety_violation_observed ();
        let pid = Pid.to_string (Unix.getpid ()) in
        save pid pid;
        (match Unix.mkdir "zoo" with
         | exception exn ->
           (try Unix.mkdir "die" with
            | _ -> ());
           raise_s [%sexp "Safety violation!", (exn : exn)]
         | _ -> ());
        delay 0.002;
        exit_if_safety_violation_observed ();
        Unix.rmdir "zoo";
        Unix.unlink pid;
        if test_stale_locks then maybe_die_to_leave_behind_stale_lockfile ())
    in
    exit 0
  | `In_the_parent pid -> pid
;;

let () =
  Command_unix.run
    (Command.basic
       ~summary:"This puts a lock file at [path] under heavy contention"
       (let%map_open.Command which_lock =
          flag
            ~doc:"Nfs|Nfs_v2|Local|Mkdir|Flock which lock protocol to use"
            "which"
            (required
               (sexp_conv
                  [%of_sexp:
                    [ `Nfs
                    | `Nfs_v2
                    | `Nfs_v1_or_v2
                    | `Local
                    | `Mkdir_or_symlink
                    | `Symlink
                    | `Flock
                    ]]))
        and path =
          flag
            ~doc:"FILE the path of the file to lock"
            "path"
            (optional_with_default "test-lock-file/lockfile" string)
        and count =
          flag
            ~doc:"NUM number of concurrent processes trying to take the for lock"
            "n"
            (optional_with_default 200 int)
        and test_stale_locks =
          flag
            ~doc:
              "BOOL test what happens when stale lock files are introduced (processes \
               killed with SIGKILL mid-critical-section). Defaults to false because some \
               lock types deadlock in this scenario."
            "stale-locks"
            (optional_with_default false bool)
        in
        fun () ->
          let dirname, basename = Filename.split path in
          Unix.mkdir_p dirname;
          Unix.chdir dirname;
          let ps =
            List.init count ~f:(fun _i -> go ~which_lock ~test_stale_locks basename)
          in
          List.iter ps ~f:(fun _pid ->
            let _pid, status = Unix.wait `Any in
            match status with
            | Error (`Signal s) when Signal.equal Signal.kill s && test_stale_locks -> ()
            | e -> Unix.Exit_or_signal.or_error e |> Or_error.ok_exn)))
;;
