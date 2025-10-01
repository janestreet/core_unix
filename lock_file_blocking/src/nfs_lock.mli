open! Core

(** An implementation-neutral NFS lock file scheme that relies on the atomicity of link
    over NFS. Rather than relying on a working traditional advisory lock system over NFS,
    we create a hard link between the file given to the [create] call and a new file
    <filename>.nfs_lock. This link call is atomic (in that it succeeds or fails) across
    all systems that have the same filesystem mounted. The link file must be cleaned up on
    program exit (normally accomplished by an [at_exit] handler, but see caveats below).

    There are a few caveats compared to local file locks:

    - These calls require the locker to have write access to the directory containing the
      file being locked.

    - Unlike a normal flock call the lock may not be removed when the calling program
      exits (in particular if it is killed with SIGKILL).

    - NFS lock files are non-standard and difficult to reason about. This implementation
      strives to strike a balance between safety and utility in the common case:
      - one program per machine
      - one shared user running the program

    Use cases outside of this may push on/break assumptions used for easy lock
    cleanup/taking and may lead to double-taking the lock. If you have such an odd use
    case you should test it carefully/consider a different locking mechanism.

    Specific known bugs:

    - Safety bug: if a stale lock file is present, two instances on the same machine
      racing to clean up that lock can both "succeed" so the lock ends up taken twice.

    - Liveness bug (fixed with create_v2): a process can write its hostname*pid
      information to the void upon taking the lock, so you may end up with a broken
      (empty) lock file, which needs manual clean-up afterwards. (it seems that for this
      to happen another process needs to take and release the lock in quick succession) *)

(** [create ?message path] tries to create and lock the file at [path] by creating a hard
    link to [path].nfs_lock. The contents of [path] will be replaced with a sexp
    containing the caller's hostname and pid, and the optional [message].

    Efforts will be made to release this lock when the calling program exits. But there is
    no guarantee that this will occur under some types of program crash. If the program
    crashes without removing the lock file an attempt will be made to clean up on restart
    by checking the hostname and pid stored in the lockfile. *)
val create : ?message:string -> string -> unit Or_error.t

(** [create_v2_exn path] uses a slightly different locking scheme: instead of using the
    original [path], we have every locking attempt create a new file
    ....path.nfs_lock.attempts/<hash> to do the locking.

    The scheme is expected to be backwards-compatible with [create_exn], while avoiding
    some concurrency bugs the original scheme suffers from. *)
val create_v2 : ?message:string -> ?exn:[ `Mach | `Hum ] -> string -> unit Or_error.t

(** [create_exn ?message path] is like [create], but throws an exception when it fails to
    obtain the lock. *)
val create_exn : ?message:string -> string -> unit

(** [create_v2_exn ?message path] is like [create_v2], but throws an exception when it
    fails to obtain the lock. *)
val create_v2_exn : ?message:string -> ?exn:[ `Mach | `Hum ] -> string -> unit

(** [blocking_create ?timeout ?message path] is like [create], but sleeps for a short
    while between lock attempts and does not return until it succeeds, [timeout] expires,
    or a permission error can be caught. Timeout defaults to wait indefinitely. *)
val blocking_create : ?timeout:Time_float.Span.t -> ?message:string -> string -> unit

(** [blocking_create_v2 ?timeout ?message path] has the same semantics as
    [blocking_create] except the locking scheme of [create_v2] is used to attempt
    acquiring the lock.

    [exn] defaults to [`Hum] and controls what the preferred error reporting format is: if
    it's [`Mach], then unrecoverable errors (currently just permission errors) will raise
    [Unix_error].

    If it's [`Hum], then all errors will come wrapped in an error message that mentions
    the lock file. *)
val blocking_create_v2
  :  ?timeout:Time_float.Span.t
  -> ?message:string
  -> ?exn:[ `Mach | `Hum ]
  -> string
  -> unit

(** [critical_section ?message ~timeout path ~f] wraps function [f] (including exceptions
    escaping it) by first locking (using {!blocking_create}) and then unlocking the given
    lock file. *)
val critical_section
  :  ?message:string
  -> string
  -> timeout:Time_float.Span.t
  -> f:(unit -> 'a)
  -> 'a

(** [get_hostname_and_pid path] reads the lock file at [path] and returns the hostname and
    pid in the file. Returns [None] if the file cannot be read. *)
val get_hostname_and_pid : string -> (string * Pid.t) option

(** [get_message path] reads the lock file at [path] and returns the message in the file.
    Returns [None] if the file cannot be read. *)
val get_message : string -> string option

(** [unlock_exn path] unlocks [path] if [path] was locked from the same host and the pid
    in the file is either the current pid or not the pid of a running process.

    It will raise if for some reason the lock at the given path cannot be unlocked, for
    example if the lock is taken by somebody else that is still alive on the same box, or
    taken by a process on a different host, or if there are Unix permissions issues, etc.

    This function should be used only by programs that need to release their lock before
    exiting. If releasing the lock can or should wait till the end of the running process,
    do not call this function -- this library already takes care of releasing at exit all
    the locks taken. *)
val unlock_exn : string -> unit

val unlock : string -> unit Or_error.t
