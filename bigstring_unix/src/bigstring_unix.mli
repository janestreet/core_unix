@@ portable

(** String type based on [Bigarray], for use in I/O and C-bindings, extending
    {{!Core.Bigstring} [Core.Bigstring]}. *)

open! Core
module Unix := Core_unix

include module type of struct
  include Core.Bigstring
end

(** Type of I/O errors.

    In [IOError (n, exn)], [n] is the number of bytes successfully read/written before the
    error and [exn] is the exception that occurred (e.g., [Unix_error], [End_of_file]) *)
exception IOError of int * exn

(** {2 Input functions} *)

(** [read ?min_len fd ?pos ?len bstr] reads at least [min_len] (must be [>= 0]) and at
    most [len] (must be [>= min_len]) bytes from file descriptor [fd], and writes them to
    bigstring [bstr] starting at position [pos]. Returns the number of bytes actually
    read.

    [read] returns zero only if [len = 0]. If [len > 0] and there's nothing left to read,
    [read] raises to indicate EOF even if [min_len = 0].

    NOTE: Even if [len] is zero, there may still be errors when reading from the
    descriptor!

    Raises [Invalid_argument] if the designated ranges are out of bounds. Raises [IOError]
    in the case of input errors, or on EOF if the minimum length could not be read. *)
val read
  :  ?min_len:int (** default = 0 *)
  -> Unix.File_descr.t
  -> ?pos:int (** default = 0 *)
  -> ?len:int (** default = [length bstr - pos] *)
  -> t @ local
  -> int

(** [really_read fd ?pos ?len bstr] reads [len] bytes from file descriptor [fd], and
    writes them to bigstring [bstr] starting at position [pos].

    Raises [Invalid_argument] if the designated range is out of bounds. Raises [IOError]
    in the case of input errors, or on EOF. *)
val really_read
  :  Unix.File_descr.t
  -> ?pos:int (** default = 0 *)
  -> ?len:int (** default = [length bstr - pos] *)
  -> t @ local
  -> unit

(** [really_recv sock ?pos ?len bstr] receives [len] bytes from socket [sock], and writes
    them to bigstring [bstr] starting at position [pos]. If [len] is zero, the function
    returns immediately without performing the underlying system call.

    Raises [Invalid_argument] if the designated range is out of bounds. Raises [IOError]
    in the case of input errors, or on EOF. *)
val really_recv
  :  Unix.File_descr.t
  -> ?pos:int (** default = 0 *)
  -> ?len:int (** default = [length bstr - pos] *)
  -> t @ local
  -> unit

(** [recv_peek_assume_fd_is_nonblocking sock ?pos ~len bstr] peeks [len] bytes from socket
    [sock], and writes them to bigstring [bstr] starting at position [pos]. If [len] is
    zero, the function returns immediately without performing the underlying system call.

    Raises [Invalid_argument] if the designated range is out of bounds. Raises
    [Unix_error] in the case of input errors *)
val recv_peek_assume_fd_is_nonblocking
  :  Unix.File_descr.t
  -> ?pos:int (** default = 0 *)
  -> len:int
  -> t @ local
  -> int

(** [recvfrom_assume_fd_is_nonblocking sock ?pos ?len bstr] reads up to [len] bytes into
    bigstring [bstr] starting at position [pos] from socket [sock] without yielding to
    other OCaml-threads.

    Returns the number of bytes actually read and the socket address of the client.

    Raises [Unix_error] in the case of input errors. Raises [Invalid_argument] if the
    designated range is out of bounds. *)
val recvfrom_assume_fd_is_nonblocking
  :  Unix.File_descr.t
  -> ?pos:int (** default = 0 *)
  -> ?len:int (** default = [length bstr - pos] *)
  -> t @ local
  -> int * Unix.sockaddr

(** [read_assume_fd_is_nonblocking fd ?pos ?len bstr] reads up to [len] bytes into
    bigstring [bstr] starting at position [pos] from file descriptor [fd] without yielding
    to other OCaml-threads. Returns the number of bytes actually read.

    Raises [Invalid_argument] if the designated range is out of bounds. *)
val read_assume_fd_is_nonblocking
  :  Unix.File_descr.t
  -> ?pos:int (** default = 0 *)
  -> ?len:int (** default = [length bstr - pos] *)
  -> t @ local
  -> Unix.Syscall_result.Int.t

(** Like [read] but uses [pread] to read from the given offset in the file. *)
val pread
  :  ?min_len:int
  -> Unix.File_descr.t
  -> offset:int
  -> ?pos:int (** default = 0 *)
  -> ?len:int (** default = [length bstr - pos] *)
  -> t @ local
  -> int

(** Like [really_read] but uses [pread] to read from the given offset in the file. *)
val really_pread
  :  Unix.File_descr.t
  -> offset:int
  -> ?pos:int (** default = 0 *)
  -> ?len:int (** default = [length bstr - pos] *)
  -> t @ local
  -> unit

(** [pread_assume_fd_is_nonblocking fd ~offset ?pos ?len bstr] reads up to [len] bytes
    from file descriptor [fd] at offset [offset], and writes them to bigstring [bstr]
    starting at position [pos]. The [fd] must be capable of seeking, and the current file
    offset used for a regular [read()] is unchanged. Please see [man pread] for more
    information. Returns the number of bytes actually read.

    Raises [Invalid_argument] if the designated range is out of bounds. Raises
    [Unix_error] in the case of input errors. *)
val pread_assume_fd_is_nonblocking
  :  Unix.File_descr.t
  -> offset:int
  -> ?pos:int (** default = 0 *)
  -> ?len:int (** default = [length bstr - pos] *)
  -> t @ local
  -> int

(** [input ?min_len ic ?pos ?len bstr] tries to read [len] bytes (guarantees to read at
    least [min_len] bytes, which must be [>= 0] and [<= len]), if possible, before
    returning, from input channel [ic], and writes them to bigstring [bstr] starting at
    position [pos]. Returns the number of bytes actually read.

    NOTE: Even if [len] is zero, there may still be errors when reading from the
    descriptor, which will be done if the internal buffer is empty!

    NOTE: If at least [len] characters are available in the input channel buffer and if
    [len] is not zero, data will only be fetched from the channel buffer. Otherwise data
    will be read until at least [min_len] characters are available.

    Raises [Invalid_argument] if the designated range is out of bounds. Raises [IOError]
    in the case of input errors, or on premature EOF. *)
val input
  :  ?min_len:int (** default = 0 *)
  -> In_channel.t
  -> ?pos:int (** default = 0 *)
  -> ?len:int (** default = [length bstr - pos] *)
  -> t @ local
  -> int

(** [really_input ic ?pos ?len bstr] reads exactly [len] bytes from input channel [ic],
    and writes them to bigstring [bstr] starting at position [pos].

    Raises [Invalid_argument] if the designated range is out of bounds. Raises [IOError]
    in the case of input errors, or on premature EOF. *)
val really_input
  :  In_channel.t
  -> ?pos:int (** default = 0 *)
  -> ?len:int (** default = [length bstr - pos] *)
  -> t @ local
  -> unit

(** {2 Output functions} *)

(** [really_write fd ?pos ?len bstr] writes [len] bytes in bigstring [bstr] starting at
    position [pos] to file descriptor [fd].

    Raises [Invalid_argument] if the designated range is out of bounds. Raises [IOError]
    in the case of output errors. *)
val really_write
  :  Unix.File_descr.t
  -> ?pos:int (** default = 0 *)
  -> ?len:int (** default = [length bstr - pos] *)
  -> t @ local shared
  -> unit

(** [really_send_no_sigpipe sock ?pos ?len bstr] sends [len] bytes in bigstring [bstr]
    starting at position [pos] to socket [sock] without blocking and ignoring [SIGPIPE].

    Raises [Invalid_argument] if the designated range is out of bounds. Raises [IOError]
    in the case of output errors.

    [really_send_no_sigpipe] is not implemented on some platforms, in which case it
    returns an [Error] value indicating that it is unimplemented. *)
val really_send_no_sigpipe
  : (Unix.File_descr.t
     -> ?pos:int (** default = 0 *)
     -> ?len:int (** default = [length bstr - pos] *)
     -> t @ local shared
     -> unit)
      Or_error.t

(** [send_nonblocking_no_sigpipe sock ?pos ?len bstr] tries to send [len] bytes in
    bigstring [bstr] starting at position [pos] to socket [sock]. Returns [bytes_written].

    Raises [Invalid_argument] if the designated range is out of bounds. *)
val send_nonblocking_no_sigpipe
  : (Unix.File_descr.t
     -> ?pos:int (** default = 0 *)
     -> ?len:int (** default = [length bstr - pos] *)
     -> t @ local shared
     -> Unix.Syscall_result.Int.t)
      Or_error.t

(** [sendto_nonblocking_no_sigpipe sock ?pos ?len bstr sockaddr] tries to send [len] bytes
    in bigstring [bstr] starting at position [pos] to socket [sock] using address [addr].
    Returns [bytes_written].

    Raises [Invalid_argument] if the designated range is out of bounds. *)
val sendto_nonblocking_no_sigpipe
  : (Unix.File_descr.t
     -> ?pos:int (** default = 0 *)
     -> ?len:int (** default = [length bstr - pos] *)
     -> t @ local shared
     -> Unix.sockaddr
     -> Unix.Syscall_result.Int.t)
      Or_error.t

(** [write fd ?pos ?len bstr] writes [len] bytes in bigstring [bstr] starting at position
    [pos] to file descriptor [fd]. Returns the number of bytes actually written.

    Raises [Invalid_argument] if the designated range is out of bounds. Raises
    [Unix_error] in the case of output errors. *)
val write
  :  Unix.File_descr.t
  -> ?pos:int (** default = 0 *)
  -> ?len:int (** default = [length bstr - pos] *)
  -> t @ local shared
  -> int

(** [pwrite_assume_fd_is_nonblocking fd ~offset ?pos ?len bstr] writes up to [len] bytes
    of bigstring [bstr] starting at position [pos] to file descriptor [fd] at position
    [offset]. The [fd] must be capable of seeking, and the current file offset used for
    non-positional [read()]/[write()] calls is unchanged. Returns the number of bytes
    written.

    Raises [Invalid_argument] if the designated range is out of bounds. Raises
    [Unix_error] in the case of output errors. *)
val pwrite_assume_fd_is_nonblocking
  :  Unix.File_descr.t
  -> offset:int
  -> ?pos:int (** default = 0 *)
  -> ?len:int (** default = [length bstr - pos] *)
  -> t @ local shared
  -> int

(** [write_assume_fd_is_nonblocking fd ?pos ?len bstr] writes [len] bytes in bigstring
    [bstr] starting at position [pos] to file descriptor [fd] without yielding to other
    OCaml-threads. Returns the number of bytes actually written.

    Raises [Invalid_argument] if the designated range is out of bounds. Raises
    [Unix_error] in the case of output errors. *)
val write_assume_fd_is_nonblocking
  :  Unix.File_descr.t
  -> ?pos:int (** default = 0 *)
  -> ?len:int (** default = [length bstr - pos] *)
  -> t @ local shared
  -> int

(** [writev fd ?count iovecs] writes [count] [iovecs] of bigstrings to file descriptor
    [fd]. Returns the number of bytes written.

    Raises [Invalid_argument] if [count] is out of range. Raises [Unix_error] in the case
    of output errors. *)
val writev
  :  Unix.File_descr.t
  -> ?count:int (** default = [Array.length iovecs] *)
  -> t Unix.IOVec.t array @ shared
  -> int

(** [writev_assume_fd_is_nonblocking fd ?count iovecs] writes [count] [iovecs] of
    bigstrings to file descriptor [fd] without yielding to other OCaml-threads. Returns
    the number of bytes actually written.

    Raises [Invalid_argument] if the designated range is out of bounds. Raises
    [Unix_error] in the case of output errors. *)
val writev_assume_fd_is_nonblocking
  :  Unix.File_descr.t
  -> ?count:int (** default = [Array.length iovecs] *)
  -> t Unix.IOVec.t array @ shared
  -> int

(** [recvmmsg_assume_fd_is_nonblocking fd iovecs ~count ~lens] receives up to [count]
    messages into [iovecs] from file descriptor [fd] without yielding to other OCaml
    threads. If [~count] is supplied, it must be that [0 <= count <= Array.length iovecs].
    If [~srcs] is supplied, saves the source addresses for corresponding received messages
    there. If supplied, [Array.length srcs] must be [>= count]. Saves the lengths of the
    received messages in [lens]. It is required that [Array.length lens >= count].

    If an IOVec isn't long enough for its corresponding message, excess bytes may be
    discarded, depending on the type of socket the message is received from. While the
    [recvmmsg] system call itself does return details of such truncation, etc., those
    details are not (yet) passed through this interface.

    See ["recvmmsg(2)"] re. the underlying system call.

    Returns the number of messages actually read, or a negative number to indicate
    [EWOULDBLOCK] or [EAGAIN]. This is a compromise to mitigate the exception overhead for
    what ends up being a very common result with our use of [recvmmsg].

    Raises [Invalid_argument] if the designated range is out of bounds. Raises
    [Unix_error] in the case of output errors. *)
val recvmmsg_assume_fd_is_nonblocking
  : (Unix.File_descr.t
     -> ?count:int (** default = [Array.length iovecs] *)
     -> ?srcs:Unix.sockaddr array
     -> t Unix.IOVec.t array
     -> lens:int array
     -> int)
      Or_error.t

val unsafe_recvmmsg_assume_fd_is_nonblocking
  : (Unix.File_descr.t
     -> t Unix.IOVec.t array
     -> int
     -> Unix.sockaddr array option
     -> int array
     -> int)
      Or_error.t

(** [sendmsg_nonblocking_no_sigpipe sock ?count iovecs] sends [count] [iovecs] of
    bigstrings to socket [sock]. Returns [Some bytes_written], or [None] if the operation
    would have blocked. This system call will not cause signal [SIGPIPE] if an attempt is
    made to write to a socket that was closed by the other side.

    Raises [Invalid_argument] if [count] is out of range. Raises [Unix_error] in the case
    of output errors. *)
val sendmsg_nonblocking_no_sigpipe
  : (Unix.File_descr.t
     -> ?count:int (** default = [Array.length iovecs] *)
     -> t Unix.IOVec.t array @ shared
     -> int option)
      Or_error.t

(** [output ?min_len oc ?pos ?len bstr] tries to output [len] bytes (guarantees to write
    at least [min_len] bytes, which must be [>= 0]), if possible, before returning, from
    bigstring [bstr] starting at position [pos] to output channel [oc]. Returns the number
    of bytes actually written.

    NOTE: You may need to flush [oc] to make sure that the data is actually sent.

    NOTE: If [len] characters fit into the channel buffer completely, they will be
    buffered. Otherwise writes will be attempted until at least [min_len] characters have
    been sent.

    Raises [Invalid_argument] if the designated range is out of bounds.

    Raises [IOError] in the case of output errors. The [IOError] argument counting the
    number of successful bytes includes those that have been transferred to the channel
    buffer before the error. *)
val output
  :  ?min_len:int (** default = 0 *)
  -> Out_channel.t
  -> ?pos:int (** default = 0 *)
  -> ?len:int (** default = [length bstr - pos] *)
  -> t @ local shared
  -> int

(** [really_output oc ?pos ?len bstr] outputs exactly [len] bytes from bigstring [bstr]
    starting at position [pos] to output channel [oc].

    Raises [Invalid_argument] if the designated range is out of bounds.

    Raises [IOError] in the case of output errors. The [IOError] argument counting the
    number of successful bytes includes those that have been transferred to the channel
    buffer before the error. *)
val really_output
  :  Out_channel.t
  -> ?pos:int (** default = 0 *)
  -> ?len:int (** default = [length bstr - pos] *)
  -> t @ local shared
  -> unit

(** {2 Unsafe functions} *)

(** [unsafe_read_assume_fd_is_nonblocking fd ~pos ~len bstr] is similar to
    {!Bigstring.read_assume_fd_is_nonblocking}, but does not perform any bounds checks.
    Will crash on bounds errors! *)
external unsafe_read_assume_fd_is_nonblocking
  :  Unix.File_descr.t
  -> pos:int
  -> len:int
  -> t @ local
  -> Unix.Syscall_result.Int.t
  = "bigstring_read_assume_fd_is_nonblocking_stub"

(** [unsafe_write fd ~pos ~len bstr] is similar to {!Bigstring.write}, but does not
    perform any bounds checks. Will crash on bounds errors! *)
external unsafe_write
  :  Unix.File_descr.t
  -> pos:int
  -> len:int
  -> t @ local shared
  -> int
  = "bigstring_write_stub"

(** [unsafe_write_assume_fd_is_nonblocking fd ~pos ~len bstr] is similar to
    {!Bigstring.write_assume_fd_is_nonblocking}, but does not perform any bounds checks.
    Will crash on bounds errors! *)
external unsafe_write_assume_fd_is_nonblocking
  :  Unix.File_descr.t
  -> pos:int
  -> len:int
  -> t @ local shared
  -> int
  = "bigstring_write_assume_fd_is_nonblocking_stub"

(** [unsafe_read ~min_len fd ~pos ~len bstr] is similar to {!Bigstring.read}, but does not
    perform any bounds checks. Will crash on bounds errors! *)
external unsafe_read
  :  min_len:int
  -> Unix.File_descr.t
  -> pos:int
  -> len:int
  -> t @ local
  -> int
  = "bigstring_read_stub"

(** [unsafe_really_recv sock ~pos ~len bstr] is similar to {!Bigstring.really_recv}, but
    does not perform any bounds checks. Will crash on bounds errors! *)
external unsafe_really_recv
  :  Unix.File_descr.t
  -> pos:int
  -> len:int
  -> t @ local
  -> unit
  = "bigstring_really_recv_stub"

(** [unsafe_really_write fd ~pos ~len bstr] is similar to {!Bigstring.write}, but does not
    perform any bounds checks. Will crash on bounds errors! *)
external unsafe_really_write
  :  Unix.File_descr.t
  -> pos:int
  -> len:int
  -> t @ local shared
  -> unit
  = "bigstring_really_write_stub"

(** [unsafe_really_send_no_sigpipe sock ~pos ~len bstr] is similar to {!Bigstring.send},
    but does not perform any bounds checks. Will crash on bounds errors! *)
val unsafe_really_send_no_sigpipe
  : (Unix.File_descr.t -> pos:int -> len:int -> t @ local shared -> unit) Or_error.t

(** [unsafe_send_nonblocking_no_sigpipe sock ~pos ~len bstr] is similar to
    {!Bigstring.send_nonblocking_no_sigpipe}, but does not perform any bounds checks. Will
    crash on bounds errors! *)
val unsafe_send_nonblocking_no_sigpipe
  : (Unix.File_descr.t
     -> pos:int
     -> len:int
     -> t @ local shared
     -> Unix.Syscall_result.Int.t)
      Or_error.t

(** [unsafe_writev fd iovecs count] is similar to {!Bigstring.writev}, but does not
    perform any bounds checks. Will crash on bounds errors! *)
external unsafe_writev
  :  Unix.File_descr.t
  -> t Unix.IOVec.t array @ shared
  -> int
  -> int
  = "bigstring_writev_stub"

(** [unsafe_sendmsg_nonblocking_no_sigpipe fd iovecs count] is similar to
    {!Bigstring.sendmsg_nonblocking_no_sigpipe}, but does not perform any bounds checks.
    Will crash on bounds errors! *)
val unsafe_sendmsg_nonblocking_no_sigpipe
  : (Unix.File_descr.t -> t Unix.IOVec.t array @ shared -> int -> int option) Or_error.t

(** [unsafe_input ~min_len ic ~pos ~len bstr] is similar to {!Bigstring.input}, but does
    not perform any bounds checks. Will crash on bounds errors! *)
external unsafe_input
  :  min_len:int
  -> In_channel.t
  -> pos:int
  -> len:int
  -> t @ local
  -> int
  = "bigstring_input_stub"

(** [unsafe_output ~min_len oc ~pos ~len bstr] is similar to {!Bigstring.output}, but does
    not perform any bounds checks. Will crash on bounds errors! *)
external unsafe_output
  :  min_len:int
  -> Out_channel.t
  -> pos:int
  -> len:int
  -> t @ local shared
  -> int
  = "bigstring_output_stub"

(** {2 Memory mapping} *)

(** [map_file shared fd n] memory-maps [n] characters of the data associated with
    descriptor [fd] to a bigstring. Iff [shared] is [true], all changes to the bigstring
    will be reflected in the file.

    Users must keep in mind that operations on the resulting bigstring may result in disk
    operations which block the runtime. This is true for pure OCaml operations (such as
    [t.{1} <- 1]), and for calls to [blit]. While some I/O operations may release the
    OCaml lock, users should not expect this to be done for all operations on a bigstring
    returned from [map_file]. *)
val map_file : shared:bool -> Unix.File_descr.t -> int -> t
