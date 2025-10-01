(** Mutual exclusion between processes using flock and lockf. A file is considered locked
    only if both of these mechanisms work.

    These locks are advisory, meaning that they will not work with systems that don't also
    try to acquire the matching locks. Although lockf can work across systems (and, in our
    environment, does work across Linux systems), it is not guaranteed to do so across all
    implementations. *)

open! Core

(** [create ?close_on_exec ?message path] tries to create a file at [path] containing the
    text [message], which defaults to the pid of the locking process. It returns true on
    success, false on failure.

    Note: there is no way to release the lock or the fd created inside! It will only be
    released when the process dies. If [close_on_exec] is [false], then the lock will not
    be released until children created via fork and exec also terminate. If not specified,
    [close_on_exec=true].

    Note that by default, the lock file is not cleaned up for you when the process exits.
    If you pass [unlink_on_exit:true], an [at_exit] handler will be set up to remove the
    lock file on program termination.

    The lock file is created with mode 664, so will not be world-writable even with
    umask 0. *)
val create
  :  ?message:string
  -> ?close_on_exec:bool (** defaults to true *)
  -> ?unlink_on_exit:bool (** defaults to false *)
  -> string
  -> bool

(** [create_exn ?message path] is like [create] except that it throws an exception on
    failure instead of returning a boolean value. *)
val create_exn
  :  ?message:string
  -> ?close_on_exec:bool (** defaults to true *)
  -> ?unlink_on_exit:bool (** defaults to false *)
  -> string
  -> unit

(** [blocking_create t] tries to create the lock. If another process holds the lock this
    function will retry periodically until it is released, [timeout] expires, or a
    permission error can be caught. The delay between retries is chosen uniformly at
    random between 0 and [max_retry_delay]. *)
val blocking_create
  :  ?max_retry_delay:Time_float.Span.t (** defaults to [min(300ms, timeout / 3)] *)
  -> ?random:Random.State.t Lazy.t (** defaults to a system-dependent low-entropy seed *)
  -> ?timeout:Time_float.Span.t (** defaults to wait indefinitely *)
  -> ?message:string
  -> ?close_on_exec:bool (** defaults to true *)
  -> ?unlink_on_exit:bool (** defaults to false *)
  -> string
  -> unit

(** [is_locked path] returns [true] when the file at [path] exists and is locked, [false]
    otherwise. Requires write permission for the lock file. *)
val is_locked : string -> bool

(** [get_pid path] reads the lock file at [path] and returns the pid in the file. Returns
    [None] if the file cannot be read, or if the file contains a message that is not an
    int. *)
val get_pid : string -> Pid.t option

module Nfs = Nfs_lock

(** This is the dumbest lock imaginable: we [mkdir] to lock and [rmdir] to unlock. This
    gives you pretty good mutual exclusion, but it makes you vulnerable to stale locks. *)
module Mkdir : sig
  type t

  (** Raises an exception if the [mkdir] system call fails for any reason other than
      [EEXIST]. *)
  val lock_exn : lock_path:string -> [ `We_took_it of t | `Somebody_else_took_it ]

  (** Raises an exception if the [rmdir] system call fails. *)
  val unlock_exn : t -> unit
end

(** This is a bit better than [Mkdir] and is very likely to be compatible: it lets you
    atomically write the owner of the lock into the symlink, it's used both by emacs and
    hg, and it's supposed to work on nfs. *)
module Symlink : sig
  type t

  (** [metadata] should include some information to help the user identify the lock
      holder. Usually it's the pid of the holder, but if you use this across a fork or
      take the lock multiple times in the same program, then some extra information could
      be useful. This string will be saved as the target of a (usually dangling) symbolic
      link at path [lock_path].

      [`Somebody_else_took_it] returns the metadata of the process who took it or an error
      if that can't be determined (for example: they released the lock by the time we
      tried to inspect it)

      Raises an exception if taking the lock fails for any reason other than somebody else
      holding the lock. *)
  val lock_exn
    :  lock_path:string
    -> metadata:string
    -> [ `We_took_it of t | `Somebody_else_took_it of string Or_error.t ]

  val unlock_exn : t -> unit
end

(** This just uses [flock]. The main reason this module exists is that [create] won't let
    you release locks, so we need a new interface.

    Another difference is that implementation is simpler because it omits some of the
    features, such as

    1. Unlinking on unlock. That feature is unsafe. The unsafety comes from the fact that
       [open] and [flock] are separated in time. Consider the following scenario:
       - two processes [a] and [b] open the file
       - [a] locks, unlinks and unlocks it (the fd stays open in [b])
       - [b] locks, using the fd obtained earlier, and stays in critical section
       - process [c] finds that there is no file, creates a new one, locks it and enters
         critical section You end up with [b] and [c] in the critical section together!

    2. Writing pid or message in the file. The file is shared between multiple processes
       so this feature seems hard to think about, and it already led to weird code. Let's
       just remove it. You can still find who holds the file open by inspecting output of
       [lslocks] or [lsof]. *)
module Flock : sig
  type t

  (** Raises an exception if taking the lock fails for any reason other than somebody else
      holding the lock. Optionally sets the lock owner to [lock_owner_uid]. *)
  val lock_exn
    :  ?lock_owner_uid:int
    -> ?exclusive:bool (* default: [true] *)
    -> ?close_on_exec:bool (* default: [true] *)
    -> unit
    -> lock_path:string
    -> [ `We_took_it of t | `Somebody_else_took_it ]

  (** Raises an exception if this lock was already unlocked earlier. *)
  val unlock_exn : t -> unit
end
