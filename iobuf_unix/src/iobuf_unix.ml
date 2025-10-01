[%%import "config.h"]

open! Core
module Unix = Core_unix
module File_descr = Unix.File_descr
module Syscall_result = Unix.Syscall_result

type ok_or_eof =
  | Ok
  | Eof
[@@deriving compare ~localize, sexp_of]

let input t ch =
  match
    Bigstring_unix.input
      ch
      (Iobuf.Expert.buf t)
      ~pos:(Iobuf.Expert.lo t)
      ~len:(Iobuf.length t)
  with
  | n ->
    Iobuf.unsafe_advance t n;
    Ok
  | exception Bigstring_unix.IOError (n, End_of_file) ->
    Iobuf.unsafe_advance t n;
    Eof
;;

let read t fd =
  match
    Bigstring_unix.read
      fd
      (Iobuf.Expert.buf t)
      ~pos:(Iobuf.Expert.lo t)
      ~len:(Iobuf.length t)
  with
  | n ->
    Iobuf.unsafe_advance t n;
    Ok
  | exception Bigstring_unix.IOError (n, End_of_file) ->
    Iobuf.unsafe_advance t n;
    Eof
;;

let really_read t fd =
  let len = Iobuf.length t in
  match
    Bigstring_unix.really_read fd (Iobuf.Expert.buf t) ~pos:(Iobuf.Expert.lo t) ~len
  with
  | () ->
    Iobuf.unsafe_advance t len;
    Ok
  | exception Bigstring_unix.IOError (n, End_of_file) ->
    Iobuf.unsafe_advance t n;
    Eof
;;

let really_pread t fd ~offset =
  let len = Iobuf.length t in
  match
    Bigstring_unix.really_pread
      fd
      (Iobuf.Expert.buf t)
      ~offset
      ~pos:(Iobuf.Expert.lo t)
      ~len
  with
  | () ->
    Iobuf.unsafe_advance t len;
    Ok
  | exception Bigstring_unix.IOError (n, End_of_file) ->
    Iobuf.unsafe_advance t n;
    Eof
;;

let read_assume_fd_is_nonblocking t fd =
  let nread =
    Bigstring_unix.read_assume_fd_is_nonblocking
      fd
      (Iobuf.Expert.buf t)
      ~pos:(Iobuf.Expert.lo t)
      ~len:(Iobuf.length t)
  in
  if Syscall_result.Int.is_ok nread
  then Iobuf.unsafe_advance t (Syscall_result.Int.ok_exn nread);
  Syscall_result.ignore_ok_value nread
;;

let pread_assume_fd_is_nonblocking t fd ~offset =
  let nread =
    Bigstring_unix.pread_assume_fd_is_nonblocking
      fd
      ~offset
      (Iobuf.Expert.buf t)
      ~pos:(Iobuf.Expert.lo t)
      ~len:(Iobuf.length t)
  in
  Iobuf.unsafe_advance t nread
;;

let recvfrom_assume_fd_is_nonblocking t fd =
  let nread, sockaddr =
    Bigstring_unix.recvfrom_assume_fd_is_nonblocking
      fd
      (Iobuf.Expert.buf t)
      ~pos:(Iobuf.Expert.lo t)
      ~len:(Iobuf.length t)
  in
  Iobuf.unsafe_advance t nread;
  sockaddr
;;

[%%ifdef JSC_RECVMMSG]

(* Allocate and pre-populate the [struct mmsghdr]s and associated [struct iovec]s. Reusing
   this context reduces the cost of calls to [recvmmsg] considerably if the iobuf array is
   large. *)
module Recvmmsg_context = struct
  type ctx

  external unsafe_ctx
    :  ([> write ], Iobuf.seek) Iobuf.t array
    -> ctx
    = "iobuf_recvmmsg_ctx"

  let ctx ts =
    if Array.for_all ts ~f:(fun t -> Iobuf.length t = Iobuf.capacity t)
    then unsafe_ctx ts
    else
      raise_s
        [%sexp
          "Recvmmsg_context.create: all buffers must be reset"
          , (ts : (_, _) Iobuf.With_shallow_sexp.t array)]
  ;;

  (* we retain a reference to the underlying bigstrings, in the event that callers
     mistakenly use set_bounds_and_buffer. Since we've cached the underlying memory
     referenced by the bigstring, we want to prevent it from being garbage collected and
     released. *)
  type t =
    { iobufs : (read_write, Iobuf.seek) Iobuf.t array
    ; bstrs : Bigstring.t array
    ; ctx : ctx
    }

  let create iobufs =
    { iobufs
    ; bstrs = Array.map iobufs ~f:(fun buf -> Iobuf.Expert.buf buf)
    ; ctx = ctx iobufs
    }
  ;;
end

external unsafe_recvmmsg_assume_fd_is_nonblocking
  :  File_descr.t
  -> (read_write, Iobuf.seek) Iobuf.t array
  -> Recvmmsg_context.ctx
  -> Unix.Syscall_result.Int.t
  = "iobuf_recvmmsg_assume_fd_is_nonblocking_stub"
[@@noalloc]

let recvmmsg_assume_fd_is_nonblocking fd { Recvmmsg_context.iobufs; ctx; _ } =
  unsafe_recvmmsg_assume_fd_is_nonblocking fd iobufs ctx
;;

let recvmmsg_assume_fd_is_nonblocking =
  (* We link with [--wrap recvmmsg].  If we have compiled on a machine with recvmmsg
     (e.g., CentOS 6) but then run on a machine without (e.g., CentOS 5), our wrapped
     [recvmmsg] always returns -1 and sets errno to ENOSYS. *)
  match
    Unix.Syscall_result.Int.to_result
      (let fd = File_descr.of_int (-1) in
       recvmmsg_assume_fd_is_nonblocking fd (Recvmmsg_context.create [||]))
  with
  | Error ENOSYS -> Or_error.unimplemented "Iobuf.recvmmsg_assume_fd_is_nonblocking"
  | _ -> Ok recvmmsg_assume_fd_is_nonblocking
;;

[%%else]

(* not JSC_RECVMMSG *)

module Recvmmsg_context = struct
  type t = unit

  let create = ignore
end

let recvmmsg_assume_fd_is_nonblocking =
  Or_error.unimplemented "Iobuf.recvmmsg_assume_fd_is_nonblocking"
;;

[%%endif]

(* JSC_RECVMMSG *)

let unsafe_sent t result =
  if Syscall_result.Int.is_ok result
  then (
    Iobuf.unsafe_advance t (Syscall_result.Int.ok_exn result);
    Syscall_result.unit)
  else Syscall_result.Int.reinterpret_error_exn result
;;

(* Don't use [Or_error.map].  The natural usage results in a partially applied function,
   which is slower to call. *)
let send_nonblocking_no_sigpipe () =
  match Bigstring_unix.send_nonblocking_no_sigpipe with
  | Error _ as e -> e
  | Ok send ->
    Ok
      (fun t fd ->
        unsafe_sent
          t
          (send fd (Iobuf.Expert.buf t) ~pos:(Iobuf.Expert.lo t) ~len:(Iobuf.length t)))
;;

let sendto_nonblocking_no_sigpipe () =
  match Bigstring_unix.sendto_nonblocking_no_sigpipe with
  | Error _ as e -> e
  | Ok sendto ->
    Ok
      (fun t fd addr ->
        unsafe_sent
          t
          (sendto
             fd
             (Iobuf.Expert.buf t)
             ~pos:(Iobuf.Expert.lo t)
             ~len:(Iobuf.length t)
             addr))
;;

module Peek = struct
  let output t ch =
    Bigstring_unix.output
      ch
      (Iobuf.Expert.buf t)
      ~pos:(Iobuf.Expert.lo t)
      ~len:(Iobuf.length t)
  ;;

  let write t fd =
    Bigstring_unix.write
      fd
      (Iobuf.Expert.buf t)
      ~pos:(Iobuf.Expert.lo t)
      ~len:(Iobuf.length t)
  ;;

  let really_write t fd =
    Bigstring_unix.really_write
      fd
      (Iobuf.Expert.buf t)
      ~pos:(Iobuf.Expert.lo t)
      ~len:(Iobuf.length t)
  ;;

  let write_assume_fd_is_nonblocking t fd =
    (* This is safe because of the invariant of [t] that the window is within the buffer
       (unless the user has violated the invariant with an unsafe operation). *)
    Bigstring_unix.unsafe_write_assume_fd_is_nonblocking
      fd
      (Iobuf.Expert.buf t)
      ~pos:(Iobuf.Expert.lo t)
      ~len:(Iobuf.length t)
  ;;
end

let output t ch =
  let nwritten = Peek.output t ch in
  Iobuf.unsafe_advance t nwritten
;;

let write t fd =
  let nwritten = Peek.write t fd in
  Iobuf.unsafe_advance t nwritten
;;

let really_write t fd =
  Peek.really_write t fd;
  Iobuf.unsafe_advance t (Iobuf.length t)
;;

let write_assume_fd_is_nonblocking t fd =
  let nwritten = Peek.write_assume_fd_is_nonblocking t fd in
  Iobuf.unsafe_advance t nwritten
;;

let pwrite_assume_fd_is_nonblocking t fd ~offset =
  let nwritten =
    Bigstring_unix.pwrite_assume_fd_is_nonblocking
      fd
      ~offset
      (Iobuf.Expert.buf t)
      ~pos:(Iobuf.Expert.lo t)
      ~len:(Iobuf.length t)
  in
  Iobuf.unsafe_advance t nwritten
;;

module Expert = struct
  external unsafe_pokef_float
    :  (read_write, _) Iobuf.t
    -> c_format:string
    -> max_length:int
    -> (float[@unboxed])
    -> int
    = "iobuf_unsafe_pokef_double_bytecode" "iobuf_unsafe_pokef_double"
  [@@noalloc]

  let fillf_float t ~c_format value =
    let limit = Iobuf.length t in
    let result = unsafe_pokef_float t ~c_format ~max_length:(Iobuf.length t) value in
    if result >= limit
    then `Truncated
    else if result < 0
    then `Format_error
    else (
      Iobuf.unsafe_advance t result;
      `Ok)
  ;;

  let to_iovec_shared ?pos ?len t =
    let pos, len =
      Ordered_collection_common.get_pos_len_exn
        ()
        ?pos
        ?len
        ~total_length:(Iobuf.length t)
    in
    Unix.IOVec.of_bigstring (Iobuf.Expert.buf t) ~pos:(Iobuf.Expert.lo t + pos) ~len
  ;;
end
