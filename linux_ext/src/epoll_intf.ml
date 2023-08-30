open Base
open Core
open Core_unix

(** epoll(): a Linux I/O multiplexer of the same family as select() or poll().  Its main
    differences are support for Edge- or Level-triggered notifications (we're using
    Level-triggered to emulate "select") and much better scaling with the number of file
    descriptors.

    See the man pages for a full description of the epoll facility. *)

module type S = sig
  module Flags : sig
    (** An [Epoll.Flags.t] is an immutable set of flags for which one can register
        interest in a file descriptor.  It is implemented as a bitmask, and so all
        operations (+, -, etc.) are constant time with no allocation.

        [sexp_of_t] produces a human-readable list of bits, e.g., "(in out)". *)
    type t [@@deriving sexp_of]

    include Flags.S with type t := t

    (** The names of the flags match the man pages.  E.g. [in_] = "EPOLLIN", [out] =
        "EPOLLOUT", etc. *)

    (** Associated fd is readable                      *)
    val none : t

    (** Associated fd is readable                      *)
    val in_ : t

    (** Associated fd is writable                      *)
    val out : t

    (*_ val rdhup   : t (\* Event flag For detecting tcp half-close        *\) *)

    (** Urgent data available                          *)
    val pri : t

    (** Error condition (always on, no need to set it) *)
    val err : t

    (** Hang up happened (always on)                   *)
    val hup : t

    (** Edge-Triggered behavior (see man page)         *)
    val et : t

    (** One-shot behavior for the associated fd        *)
    val oneshot : t
  end

  (** An [Epoll.t] maintains a map from [File_descr.t] to [Flags.t], where the domain is
      the set of file descriptors that one is interested in, and the flags associated
      with each file descriptor specify the types of events one is interested in being
      notified about for that file descriptor.  Our implementation maintains a
      user-level table equivalent to the kernel epoll set, so that [sexp_of_t] produces
      useful human-readable information, and so that we can present our standard table
      interface.

      The implementation assumes that one never closes a file descriptor that is the
      domain of an [Epoll.t], since doing so might remove the [fd] from the kernel epoll
      set without the implementation's knowledge.

      An [Epoll.t] also has a buffer that is used to store the set of ready [fd]s
      returned by calling [wait]. *)
  type t [@@deriving sexp_of]

  val invariant : t -> unit

  (** [create ~num_file_descrs] creates a new epoll set able to watch file descriptors
      in \[0, [num_file_descrs]).  Additionally, the set allocates space for reading the
      "ready" events when [wait] returns, allowing for up to [max_ready_events] to be
      returned in a single call to [wait]. *)
  val create : (num_file_descrs:int -> max_ready_events:int -> t) Or_error.t

  val close : t -> unit

  (** Map operations *)

  (** [find] raises in the case that [t] is closed. *)
  val find : t -> File_descr.t -> Flags.t option

  val find_exn : t -> File_descr.t -> Flags.t
  val set : t -> File_descr.t -> Flags.t -> unit
  val remove : t -> File_descr.t -> unit
  val iter : t -> f:(File_descr.t -> Flags.t -> unit) -> unit
  val fold : t -> init:'a -> f:(File_descr.t -> Flags.t -> 'a -> 'a) -> 'a

  (** [wait t ~timeout] blocks until at least one file descriptor in [t] is ready for
      one of the events it is being watched for, or [timeout] passes.  [wait] side
      effects [t] by storing the ready set in it.  One can subsequently access the ready
      set by calling [iter_ready] or [fold_ready].

      With [wait ~timeout:(`After span)], [span <= 0] is treated as [0].  If [span > 0],
      then [span] is rounded to the nearest millisecond, with a minimum value of one
      millisecond.

      Note that this method should not be considered thread-safe.  There is mutable
      state in [t] that will be changed by invocations to [wait] that cannot be
      prevented by mutexes around [wait]. *)
  val wait
    :  t
    -> timeout:[ `Never | `Immediately | `After of Time_ns.Span.t ]
    -> [ `Ok | `Timeout ]

  (** [wait_timeout_after t span = wait t ~timeout:(`After span)].  [wait_timeout_after]
      is a performance hack to avoid allocating [`After span]. *)
  val wait_timeout_after : t -> Time_ns.Span.t -> [ `Ok | `Timeout ]

  (** [iter_ready] and [fold_ready] iterate over the ready set computed by the last
      call to [wait]. *)
  val iter_ready : t -> f:(File_descr.t -> Flags.t -> unit) -> unit

  val fold_ready : t -> init:'a -> f:('a -> File_descr.t -> Flags.t -> 'a) -> 'a

  module Expert : sig
    (** [clear_ready t] sets the number of ready events in [t] to [0]. This
        should be called after all the events in [t] have been processed,
        following a call to {!wait}. *)
    val clear_ready : t -> unit
  end

  (*_
    (* pwait -> with the specified sigmask, analogous to pselect *)
    (* val pwait   : t -> timeout:Span.t -> int list -> [ `Ok of Ready_fds.t | `Timeout ] *)
  *)
end
