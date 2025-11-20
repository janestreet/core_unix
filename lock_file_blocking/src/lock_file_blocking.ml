open! Core
module Unix = Core_unix

[%%import "config.h"]

open Shared

(* We have reason to believe that lockf doesn't work properly on CIFS mounts. The idea
   behind requiring both lockf and flock is to prevent programs taking locks on network
   filesystems where they may not be sound.

   However, this assumes that [lockf] and [flock] take independent locks, which is true on
   local Linux filesystems, but is false on many OSes (for example, Mac OS X), so we use
   just [flock] on non-linux OSes and give up the fail-on-CIFS-and-NFS property.

   We prefer [flock] and not [lockf] because [lockf] has bad semantics if used multiple
   times within the same process: for example [lockf a; lockf b; close a] succeeds (bad!)
   and leaves the file unlocked (bad!) if [a] and [b] are unrelated file descriptors for
   the same file. *)

let flock fd ~exclusive =
  let flock_command =
    match exclusive with
    | true -> Unix.Flock_command.lock_exclusive
    | false -> Unix.Flock_command.lock_shared
  in
  Unix.flock fd flock_command
;;

let lockf ?(mode = Unix.F_TLOCK) fd =
  try
    Unix.lockf fd ~mode ~len:Int64.zero;
    true
  with
  | _ -> false
;;

[%%ifdef JSC_LINUX_EXT]

let lock fd =
  (* [lockf] doesn't throw any exceptions, so if an exception is raised from this
     function, it must have come from [flock]. *)
  let flocked = flock fd ~exclusive:true in
  let lockfed = lockf fd in
  flocked && lockfed
;;

[%%else]

let lock = flock ~exclusive:true

[%%endif]

let is_locked path =
  try
    let fd = Unix.openfile path ~mode:[ Unix.O_RDONLY; Unix.O_CLOEXEC ] ~perm:0o664 in
    let flocked = flock fd ~exclusive:true in
    let lockfed = lockf fd ~mode:Unix.F_TEST in
    Unix.close fd;
    (* releases any locks from [flock] and/or [lockf] *)
    if flocked && lockfed then false else true
  with
  | Unix.Unix_error (ENOENT, _, _) -> false
  | e -> raise e
;;

let create
  ?(message = Pid.to_string (Unix.getpid ()))
  ?(close_on_exec = true)
  ?(unlink_on_exit = false)
  path
  =
  let message = sprintf "%s\n" message in
  (* We use [~perm:0o664] rather than our usual default perms, [0o666], because lock files
     shouldn't rely on the umask to disallow tampering by other. *)
  let fd =
    Unix.openfile
      path
      ~mode:([ Unix.O_WRONLY; O_CREAT ] @ if close_on_exec then [ O_CLOEXEC ] else [])
      ~perm:0o664
  in
  try
    if lock fd
    then (
      if unlink_on_exit
      then
        at_exit (fun () ->
          Unix.close fd;
          (* Do not unlink if (e.g. a forked child, or our parent) still holds this lock *)
          if not (is_locked path)
          then (
            try Unix.unlink path with
            | _ -> ()));
      Unix.ftruncate fd ~len:Int64.zero;
      ignore (Unix.write_substring fd ~buf:message ~pos:0 ~len:(String.length message));
      (* we truncated the file, so we need the region lock back.  We don't really
         understand why/if this call is needed, but experimental evidence indicates that
         we need to do it. *)
      ignore (lockf fd);
      true)
    else (
      Unix.close fd;
      (* releases any locks from [flock] and/or [lockf] *)
      false)
  with
  | e ->
    Unix.close fd;
    (* releases any locks from [flock] and/or [lockf] *)
    raise e
;;

let create_exn ?message ?close_on_exec ?unlink_on_exit path =
  if not (create ?message ?close_on_exec ?unlink_on_exit path)
  then
    failwithf
      "Lock_file.create_exn '%s' was unable to acquire the lock. The process that \
       acquired the lock is likely still running"
      path
      ()
;;

(* default timeout is to wait indefinitely *)
let blocking_create
  ?max_retry_delay
  ?random
  ?timeout
  ?message
  ?close_on_exec
  ?unlink_on_exit
  path
  =
  repeat_with_timeout
    ?max_retry_delay
    ?random
    ?timeout
    (fun path ->
      match create_exn ?message ?close_on_exec ?unlink_on_exit path with
      | exception exn -> Error (`Retriable exn)
      | res -> Ok res)
    path
;;

let get_pid path =
  let of_string string = Int.of_string string |> Pid.of_int in
  read_file_and_convert ~of_string path
;;

module Nfs = Nfs_lock

(* The reason this function is used is to make sure the file the path is pointing to
   remains stable across [chdir]. In fact we'd prefer for it to remain stable over other
   things, such as [rename] of a parent directory. That could be achieved if we [open] the
   [dir] and use the resulting file descriptor with linkat, unlinkat, etc system calls,
   but that's less portable and most programs that use locks will break anyway if their
   directory is renamed. *)
let canonicalize_dirname path =
  let dir, name = Filename.dirname path, Filename.basename path in
  let dir = Filename_unix.realpath dir in
  dir ^/ name
;;

module Mkdir = struct
  type t = Locked of { lock_path : string }

  let lock_exn ~lock_path =
    let lock_path = canonicalize_dirname lock_path in
    match Unix.mkdir lock_path with
    | exception Core_unix.Unix_error (EEXIST, _, _) -> `Somebody_else_took_it
    | () -> `We_took_it (Locked { lock_path })
  ;;

  let unlock_exn (Locked { lock_path }) = Unix.rmdir lock_path
end

module Symlink = struct
  type t = Locked of { lock_path : string }

  let lock_exn ~lock_path ~metadata =
    let lock_path = canonicalize_dirname lock_path in
    match Unix.symlink ~link_name:lock_path ~target:metadata with
    | exception Core_unix.Unix_error (EEXIST, _, _) ->
      `Somebody_else_took_it (Or_error.try_with (fun () -> Unix.readlink lock_path))
    | () -> `We_took_it (Locked { lock_path })
  ;;

  let unlock_exn (Locked { lock_path }) = Unix.unlink lock_path
end

module Flock = struct
  type t =
    { fd : Caml_unix.file_descr
    ; mutable unlocked : bool
    }

  let lock_exn ?lock_owner_uid ?(exclusive = true) ?(close_on_exec = true) () ~lock_path =
    let fd =
      Core_unix.openfile
        lock_path
        ~mode:([ Unix.O_WRONLY; O_CREAT ] @ if close_on_exec then [ O_CLOEXEC ] else [])
        ~perm:0o664
    in
    Option.iter lock_owner_uid ~f:(fun uid -> Core_unix.fchown fd ~uid ~gid:(-1));
    match flock ~exclusive fd with
    | false ->
      Core_unix.close fd;
      `Somebody_else_took_it
    | true -> `We_took_it { fd; unlocked = false }
    | exception exn ->
      Core_unix.close fd;
      raise exn
  ;;

  let unlock_exn t =
    if t.unlocked then raise_s [%sexp "Lock_file_blocking.Flock.unlock_exn called twice"];
    t.unlocked <- true;
    Core_unix.close t.fd
  ;;
end
