[%%import "config.h"]

open! Core
open! Iobuf
module Unix = Core_unix
module File_descr = Unix.File_descr
module Syscall_result = Unix.Syscall_result

type ok_or_eof =
  | Ok
  | Eof
[@@deriving compare, sexp_of]

let input t ch =
  match Bigstring_unix.input ch (Expert.buf t) ~pos:(Expert.lo t) ~len:(length t) with
  | n ->
    unsafe_advance t n;
    Ok
  | exception Bigstring_unix.IOError (n, End_of_file) ->
    unsafe_advance t n;
    Eof
;;

let read t fd =
  match Bigstring_unix.read fd (Expert.buf t) ~pos:(Expert.lo t) ~len:(length t) with
  | n ->
    unsafe_advance t n;
    Ok
  | exception Bigstring_unix.IOError (n, End_of_file) ->
    unsafe_advance t n;
    Eof
;;

let read_assume_fd_is_nonblocking t fd =
  let nread =
    Bigstring_unix.read_assume_fd_is_nonblocking
      fd
      (Expert.buf t)
      ~pos:(Expert.lo t)
      ~len:(length t)
  in
  if Syscall_result.Int.is_ok nread
  then unsafe_advance t (Syscall_result.Int.ok_exn nread);
  Syscall_result.ignore_ok_value nread
;;

let pread_assume_fd_is_nonblocking t fd ~offset =
  let nread =
    Bigstring_unix.pread_assume_fd_is_nonblocking
      fd
      ~offset
      (Expert.buf t)
      ~pos:(Expert.lo t)
      ~len:(length t)
  in
  unsafe_advance t nread
;;

let recvfrom_assume_fd_is_nonblocking t fd =
  let nread, sockaddr =
    Bigstring_unix.recvfrom_assume_fd_is_nonblocking
      fd
      (Expert.buf t)
      ~pos:(Expert.lo t)
      ~len:(length t)
  in
  unsafe_advance t nread;
  sockaddr
;;

[%%ifdef JSC_RECVMMSG]

(* Allocate and pre-populate the [struct mmsghdr]s and associated [struct iovec]s. Reusing
   this context reduces the cost of calls to [recvmmsg] considerably if the iobuf array is
   large. *)
module Recvmmsg_context = struct
  type ctx

  external unsafe_ctx : ([> write ], seek) t array -> ctx = "iobuf_recvmmsg_ctx"

  let ctx ts =
    if Array.for_all ts ~f:(fun t -> length t = capacity t)
    then unsafe_ctx ts
    else
      raise_s
        [%sexp
          "Recvmmsg_context.create: all buffers must be reset"
          , (ts : (_, _) t_with_shallow_sexp array)]
  ;;

  (* we retain a reference to the underlying bigstrings, in the event that callers
     mistakenly use set_bounds_and_buffer. Since we've cached the underlying memory
     referenced by the bigstring, we want to prevent it from being garbage collected and
     released. *)
  type nonrec t =
    { iobufs : (read_write, seek) t array
    ; bstrs : Bigstring.t array
    ; ctx : ctx
    }

  let create iobufs =
    { iobufs; bstrs = Array.map iobufs ~f:(fun buf -> Expert.buf buf); ctx = ctx iobufs }
  ;;
end

external unsafe_recvmmsg_assume_fd_is_nonblocking
  :  File_descr.t
  -> (read_write, seek) t array
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
    unsafe_advance t (Syscall_result.Int.ok_exn result);
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
        unsafe_sent t (send fd (Expert.buf t) ~pos:(Expert.lo t) ~len:(length t)))
;;

let sendto_nonblocking_no_sigpipe () =
  match Bigstring_unix.sendto_nonblocking_no_sigpipe with
  | Error _ as e -> e
  | Ok sendto ->
    Ok
      (fun t fd addr ->
        unsafe_sent t (sendto fd (Expert.buf t) ~pos:(Expert.lo t) ~len:(length t) addr))
;;

module Peek = struct
  let output t ch =
    Bigstring_unix.output ch (Expert.buf t) ~pos:(Expert.lo t) ~len:(length t)
  ;;

  let write t fd =
    Bigstring_unix.write fd (Expert.buf t) ~pos:(Expert.lo t) ~len:(length t)
  ;;

  let write_assume_fd_is_nonblocking t fd =
    (* This is safe because of the invariant of [t] that the window is within the buffer
       (unless the user has violated the invariant with an unsafe operation). *)
    Bigstring_unix.unsafe_write_assume_fd_is_nonblocking
      fd
      (Expert.buf t)
      ~pos:(Expert.lo t)
      ~len:(length t)
  ;;
end

let output t ch =
  let nwritten = Peek.output t ch in
  unsafe_advance t nwritten
;;

let write t fd =
  let nwritten = Peek.write t fd in
  unsafe_advance t nwritten
;;

let write_assume_fd_is_nonblocking t fd =
  let nwritten = Peek.write_assume_fd_is_nonblocking t fd in
  unsafe_advance t nwritten
;;

let pwrite_assume_fd_is_nonblocking t fd ~offset =
  let nwritten =
    Bigstring_unix.pwrite_assume_fd_is_nonblocking
      fd
      ~offset
      (Expert.buf t)
      ~pos:(Expert.lo t)
      ~len:(length t)
  in
  unsafe_advance t nwritten
;;

module Expert = struct
  external unsafe_pokef_float
    :  (read_write, _) t
    -> c_format:string
    -> max_length:int
    -> (float[@unboxed])
    -> int
    = "iobuf_unsafe_pokef_double_bytecode" "iobuf_unsafe_pokef_double"
    [@@noalloc]

  let fillf_float t ~c_format value =
    let limit = length t in
    let result = unsafe_pokef_float t ~c_format ~max_length:(length t) value in
    if result >= limit
    then `Truncated
    else if result < 0
    then `Format_error
    else (
      unsafe_advance t result;
      `Ok)
  ;;

  let to_iovec_shared ?pos ?len t =
    let pos, len =
      Ordered_collection_common.get_pos_len_exn () ?pos ?len ~total_length:(length t)
    in
    Unix.IOVec.of_bigstring (Expert.buf t) ~pos:(Expert.lo t + pos) ~len
  ;;
end

module In_channel_optimized = struct
  let next_newline buf =
    let len = Iobuf.length buf in
    Iobuf.Unsafe.Peek.index_or_neg ~pos:0 buf ~len '\n'
  ;;

  let present_line ~fix_win_eol ~acc ~f buf ~len =
    let len_of_line =
      if fix_win_eol
         && len > 0
         && Char.equal '\r' (Iobuf.Unsafe.Peek.char ~pos:(len - 1) buf)
      then len - 1
      else len
    in
    Iobuf.unsafe_resize buf ~len:len_of_line;
    f acc buf
  ;;

  let rec fold_full_lines_in_buf ~fix_win_eol ~acc ~f buf =
    let len = next_newline buf in
    if len >= 0
    then (
      let hi = Iobuf.Expert.hi buf in
      let lo = Iobuf.Expert.lo buf in
      let next_line_starts_at = lo + len + 1 in
      (* [present_line] modifies [hi] and [lo], so we must cache and restore them. *)
      let acc = present_line ~acc ~f ~fix_win_eol buf ~len in
      Iobuf.Expert.set_lo buf next_line_starts_at;
      Iobuf.Expert.set_hi buf hi;
      fold_full_lines_in_buf ~fix_win_eol ~acc ~f buf)
    else acc
  ;;

  let fold_lines_raw ?(fix_win_eol = true) ?(buf = Iobuf.create ~len:1024) ch ~init ~f =
    Iobuf.reset buf;
    let acc = ref init in
    while
      let result = input buf ch in
      Iobuf.flip_lo buf;
      acc := fold_full_lines_in_buf ~fix_win_eol ~acc:!acc ~f buf;
      let length = Iobuf.length buf in
      let capacity = Iobuf.capacity buf in
      if length = capacity
      then (
        let new_capacity = max 1024 (capacity * 2) in
        let str = Bigstring.create new_capacity in
        Iobuf.Consume.To_bigstring.blito ~src:(Iobuf.read_only buf) ~dst:str ();
        Iobuf.Expert.reinitialize_of_bigstring ~pos:0 ~len:new_capacity buf str;
        Iobuf.resize buf ~len:length);
      Iobuf.compact buf;
      match result with
      | Ok -> true
      | Eof -> false
    do
      ()
    done;
    if Iobuf.length buf < Iobuf.capacity buf
    then (
      Iobuf.flip_lo buf;
      acc := present_line ~fix_win_eol ~acc:!acc ~f buf ~len:(Iobuf.length buf));
    !acc
  ;;

  let fold_lines ?fix_win_eol ?buf ch ~init ~f =
    fold_lines_raw ?fix_win_eol ?buf ch ~init ~f:(fun acc buf ->
      Iobuf.Unsafe.Peek.stringo buf ~pos:0 |> f acc)
  ;;

  let iter_lines ?fix_win_eol ?buf ch ~f =
    fold_lines ?fix_win_eol ?buf ch ~init:() ~f:(fun () s -> f s)
  ;;

  let input_lines ?fix_win_eol ?buf ch =
    (* Vec is not usable as a dependency. *)
    let v = Queue.create () in
    iter_lines ?fix_win_eol ?buf ch ~f:(fun str -> Queue.enqueue v str);
    Queue.to_array v
  ;;
end
