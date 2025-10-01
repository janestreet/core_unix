open! Core
module Unix := Core_unix

type ok_or_eof =
  | Ok
  | Eof
[@@deriving compare ~localize, sexp_of]

(** [Iobuf] has analogs of various [Bigstring] functions. These analogs advance by the
    amount written/read. *)
val input : ([> write ], Iobuf.seek) Iobuf.t -> In_channel.t -> ok_or_eof

val read : ([> write ], Iobuf.seek) Iobuf.t -> Unix.File_descr.t -> ok_or_eof
val really_read : ([> write ], Iobuf.seek) Iobuf.t -> Unix.File_descr.t -> ok_or_eof

val really_pread
  :  ([> write ], Iobuf.seek) Iobuf.t
  -> Unix.File_descr.t
  -> offset:int
  -> ok_or_eof

val read_assume_fd_is_nonblocking
  :  ([> write ], Iobuf.seek) Iobuf.t
  -> Unix.File_descr.t
  -> Unix.Syscall_result.Unit.t

val pread_assume_fd_is_nonblocking
  :  ([> write ], Iobuf.seek) Iobuf.t
  -> Unix.File_descr.t
  -> offset:int
  -> unit

val recvfrom_assume_fd_is_nonblocking
  :  ([> write ], Iobuf.seek) Iobuf.t
  -> Unix.File_descr.t
  -> Unix.sockaddr

(** [recvmmsg]'s context comprises data needed by the system call. Setup can be expensive,
    particularly for many buffers.

    NOTE: Unlike most system calls involving iobufs, the lo offset is not respected.
    Instead, the iobuf is implicity [reset] (i.e., [lo <- lo_min] and [hi <- hi_max])
    prior to reading and a [flip_lo] applied afterward. This is to prevent the
    memory-unsafe case where an iobuf's lo pointer is advanced and [recvmmsg] attempts to
    copy into memory exceeding the underlying [bigstring]'s capacity. If any of the
    returned iobufs have had their underlying bigstring or limits changed (e.g., through a
    call to [set_bounds_and_buffer] or [narrow_lo]), the call will fail with [EINVAL]. *)
module Recvmmsg_context : sig
  type t

  (** Do not change these [Iobuf]'s [buf]s or limits before calling
      [recvmmsg_assume_fd_is_nonblocking]. *)
  val create : (read_write, Iobuf.seek) Iobuf.t array -> t
end

(** [recvmmsg_assume_fd_is_nonblocking fd context] returns the number of [context] iobufs
    read into (or [errno]). [fd] must not block. [THREAD_IO_CUTOFF] is ignored.

    [EINVAL] is returned if an [Iobuf] passed to [Recvmmsg_context.create] has its [buf]
    or limits changed. *)
val recvmmsg_assume_fd_is_nonblocking
  : (Unix.File_descr.t -> Recvmmsg_context.t -> Unix.Syscall_result.Int.t) Or_error.t

val send_nonblocking_no_sigpipe
  :  unit
  -> (([> read ], Iobuf.seek) Iobuf.t -> Unix.File_descr.t -> Unix.Syscall_result.Unit.t)
       Or_error.t

val sendto_nonblocking_no_sigpipe
  :  unit
  -> (([> read ], Iobuf.seek) Iobuf.t
      -> Unix.File_descr.t
      -> Unix.sockaddr
      -> Unix.Syscall_result.Unit.t)
       Or_error.t

(** Write from the iobuf to the specified channel without changing the iobuf window.
    Returns the number of bytes written. *)
module Peek : sig
  val output : ([> read ], _) Iobuf.t -> Out_channel.t -> int
  val write : ([> read ], _) Iobuf.t -> Unix.File_descr.t -> int
  val really_write : ([> read ], _) Iobuf.t -> Unix.File_descr.t -> unit
  val write_assume_fd_is_nonblocking : ([> read ], _) Iobuf.t -> Unix.File_descr.t -> int
end

(** As [Peek], but advances the window by the number of bytes written. *)
val output : ([> read ], Iobuf.seek) Iobuf.t -> Out_channel.t -> unit

val write : ([> read ], Iobuf.seek) Iobuf.t -> Unix.File_descr.t -> unit
val really_write : ([> read ], Iobuf.seek) Iobuf.t -> Unix.File_descr.t -> unit

val write_assume_fd_is_nonblocking
  :  ([> read ], Iobuf.seek) Iobuf.t
  -> Unix.File_descr.t
  -> unit

val pwrite_assume_fd_is_nonblocking
  :  ([> read ], Iobuf.seek) Iobuf.t
  -> Unix.File_descr.t
  -> offset:int
  -> unit

(** {2 Expert} *)

(** The [Expert] module is for building efficient out-of-module [Iobuf] abstractions. *)
module Expert : sig
  (** [fillf_float t ~c_format float] attempts to fill a string representation of a float
      into an iobuf at the current position. The representation is specified by standard C
      [printf] formatting codes.

      The highest available byte of the window is unusable and will be set to 0 in the
      case that a properly formatted string would otherwise fully fill the window.

      If there is enough room in (window - 1) to format the float as specified then [`Ok]
      is returned and the window is advanced past the written bytes.

      If there is not enough room in (window - 1) to format as specified then [`Truncated]
      is returned.

      If C [snprintf] indicates a format error then [`Format_error] is returned.

      Operation is unsafe if a format code not intended for a double precision float is
      used (e.g., %s) or if more than one format specifier is provided, etc. *)
  val fillf_float
    :  (read_write, Iobuf.seek) Iobuf.t
    -> c_format:string
    -> float
    -> [ `Ok | `Truncated | `Format_error ]

  val to_iovec_shared : ?pos:int -> ?len:int -> (_, _) Iobuf.t -> Bigstring.t Unix.IOVec.t
end
