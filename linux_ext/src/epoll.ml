open! Base
open! Core
include Epoll_intf

module Epoll_flags (Flag_values : sig
  (* We use [Int63] rather than [Int] because these flags use 32 bits. *)
  val in_ : Int63.t
  val out : Int63.t

  (* val rdhup   : Int63.t *)
  val pri : Int63.t
  val err : Int63.t
  val hup : Int63.t
  val et : Int63.t
  val oneshot : Int63.t
end) =
struct
  let none = Int63.zero

  include Flag_values

  include Flags.Make (struct
    let allow_intersecting = false
    let should_print_error = true
    let remove_zero_flags = false

    let known =
      [ in_, "in"
      ; out, "out"
      ; (* rdhup, "rdhup"; *)
        pri, "pri"
      ; err, "err"
      ; hup, "hup"
      ; et, "et"
      ; oneshot, "oneshot"
      ]
    ;;
  end)
end

module Null_impl : S = struct
  module Flags = Epoll_flags (struct
    let in_ = Int63.of_int (1 lsl 0)
    let out = Int63.of_int (1 lsl 1)

    (* let rdhup   = Int63.of_int (1 lsl 2) *)
    let pri = Int63.of_int (1 lsl 3)
    let err = Int63.of_int (1 lsl 4)
    let hup = Int63.of_int (1 lsl 5)
    let et = Int63.of_int (1 lsl 6)
    let oneshot = Int63.of_int (1 lsl 7)
  end)

  type t = [ `Epoll_is_not_implemented ] [@@deriving sexp_of]

  let create = Or_error.unimplemented "Linux_ext.Epoll.create"
  let close _ = assert false
  let invariant _ = assert false
  let find _ _ = assert false
  let find_exn _ _ = assert false
  let set _ _ _ = assert false
  let remove _ _ = assert false
  let iter _ ~f:_ = assert false
  let fold _ ~init:_ ~f:_ = assert false
  let wait _ ~timeout:_ = assert false
  let wait_timeout_after _ _ = assert false
  let iter_ready _ ~f:_ = assert false
  let fold_ready _ ~init:_ ~f:_ = assert false

  module Expert = struct
    let clear_ready _ = assert false
  end

  (* let pwait _ ~timeout:_ _      = assert false *)
end

module _ = Null_impl

[%%import "config.h"]
[%%ifdef JSC_LINUX_EXT]

module Impl = struct
  open Core_unix
  module Unix = Core_unix

  external flag_epollin : unit -> Int63.t = "core_linux_epoll_EPOLLIN_flag"
  external flag_epollout : unit -> Int63.t = "core_linux_epoll_EPOLLOUT_flag"

  (* external flag_epollrdhup   : unit -> Int63.t  = "core_linux_epoll_EPOLLRDHUP_flag" *)
  external flag_epollpri : unit -> Int63.t = "core_linux_epoll_EPOLLPRI_flag"
  external flag_epollerr : unit -> Int63.t = "core_linux_epoll_EPOLLERR_flag"
  external flag_epollhup : unit -> Int63.t = "core_linux_epoll_EPOLLHUP_flag"
  external flag_epollet : unit -> Int63.t = "core_linux_epoll_EPOLLET_flag"
  external flag_epolloneshot : unit -> Int63.t = "core_linux_epoll_EPOLLONESHOT_flag"

  module Flags = Epoll_flags (struct
    let in_ = flag_epollin ()
    let out = flag_epollout ()

    (* let rdhup   = flag_epollrdhup () *)
    let pri = flag_epollpri ()
    let err = flag_epollerr ()
    let hup = flag_epollhup ()
    let et = flag_epollet ()
    let oneshot = flag_epolloneshot ()
  end)

  external epoll_create : unit -> File_descr.t = "core_linux_epoll_create"

  (* Some justification for the below interface: Unlike select() and poll(), epoll() fills
     in an array of ready events, analogous to a read() call where you pass in a buffer to
     be filled.

     Since this is at the core of the I/O loop, we'd like to avoid reallocating that
     buffer on every call to poll.  We're allocating the array on the ocaml side (as a
     Bigstring), then iterating through it in-place, reducing allocation, copies, and any
     intermediate lists.  For very high message rates and many fds this could be a very
     beneficial. *)
  type ready_events = Bigstring.t

  external epoll_sizeof_epoll_event : unit -> int = "core_linux_epoll_sizeof_epoll_event"
    [@@noalloc]

  external epoll_offsetof_readyfd : unit -> int = "core_linux_epoll_offsetof_readyfd"
    [@@noalloc]

  external epoll_offsetof_readyflags
    :  unit
    -> int
    = "core_linux_epoll_offsetof_readyflags"
    [@@noalloc]

  let sizeof_epoll_event = epoll_sizeof_epoll_event ()
  let offsetof_readyfd = epoll_offsetof_readyfd ()
  let offsetof_readyflags = epoll_offsetof_readyflags ()

  external epoll_ctl_add
    :  File_descr.t
    -> File_descr.t
    -> Flags.t
    -> unit
    = "core_linux_epoll_ctl_add"

  external epoll_ctl_mod
    :  File_descr.t
    -> File_descr.t
    -> Flags.t
    -> unit
    = "core_linux_epoll_ctl_mod"

  external epoll_ctl_del
    :  File_descr.t
    -> File_descr.t
    -> unit
    = "core_linux_epoll_ctl_del"

  module Table = Bounded_int_table

  module T = struct
    type 'a t =
      { epollfd : File_descr.t
      ; (* [flags_by_fd] has one entry for each file-descr in the epoll set, and stores
           the epoll flags that the kernel's epoll set currently has for that
           file-descr.  Keeping our own representation of the kernel data structure is
           useful for debugging, since the information appears in a human-readable way
           in [sexp_of_t]'s output.  It also allows us to hide the distinction between
           [epoll_ctl_add] and [epoll_ctl_mod], since we know which to use based on
           whether the file descriptor is already being watched. *)
        flags_by_fd : (File_descr.t, Flags.t) Table.t
      ; max_ready_events : int
      ; (* [num_ready_events] holds the number of ready events in [ready_events], as
           determined by the last call to [wait]. *)
        mutable num_ready_events : int
      ; ready_events : 'a
      }
    [@@deriving fields ~iterators:iter, sexp_of]
  end

  open T

  let epoll_readyfd t i =
    Bigstring.unsafe_get_int32_le t ~pos:((i * sizeof_epoll_event) + offsetof_readyfd)
    |> File_descr.of_int
  ;;

  let epoll_readyflags t i =
    Bigstring.unsafe_get_int32_le t ~pos:((i * sizeof_epoll_event) + offsetof_readyflags)
    |> Flags.of_int
  ;;

  type in_use = ready_events T.t

  module Pretty = struct
    type ready_event =
      { file_descr : File_descr.t
      ; flags : Flags.t
      }
    [@@deriving sexp_of]

    type ready_events = ready_event array [@@deriving sexp_of]
    type t = ready_events T.t [@@deriving sexp_of]
  end

  let to_pretty t =
    { t with
      ready_events =
        Array.init t.num_ready_events ~f:(fun i ->
          { Pretty.file_descr = epoll_readyfd t.ready_events i
          ; flags = epoll_readyflags t.ready_events i
          })
    }
  ;;

  let sexp_of_in_use t = Pretty.sexp_of_t (to_pretty t)

  type t = [ `Closed | `In_use of in_use ] ref [@@deriving sexp_of]

  let close t =
    match !t with
    | `Closed -> ()
    | `In_use { epollfd; _ } ->
      t := `Closed;
      Unix.close epollfd
  ;;

  let invariant t : unit =
    match !t with
    | `Closed -> ()
    | `In_use t ->
      (try
         let check f field = f (Field.get field t) in
         Fields.iter
           ~epollfd:ignore
           ~flags_by_fd:(check (Table.invariant ignore ignore))
           ~max_ready_events:
             (check (fun max_ready_events -> assert (max_ready_events > 0)))
           ~num_ready_events:(check (fun num_ready -> assert (num_ready >= 0)))
           ~ready_events:ignore
       with
       | exn ->
         failwiths
           ~here:[%here]
           "Epoll.invariant failed"
           (exn, t)
           [%sexp_of: exn * in_use])
  ;;

  let create ~num_file_descrs ~max_ready_events =
    if max_ready_events < 0
    then
      failwiths
        ~here:[%here]
        "Epoll.create got nonpositive max_ready_events"
        max_ready_events
        [%sexp_of: int];
    ref
      (`In_use
        { epollfd = epoll_create ()
        ; flags_by_fd =
            Table.create
              ~num_keys:num_file_descrs
              ~key_to_int:File_descr.to_int
              ~sexp_of_key:File_descr.sexp_of_t
              ()
        ; max_ready_events
        ; num_ready_events = 0
        ; ready_events = Bigstring.create (sizeof_epoll_event * max_ready_events)
        })
  ;;

  let in_use_exn t =
    match !t with
    | `Closed -> failwith "attempt to use closed epoll set"
    | `In_use r -> r
  ;;

  let find t file_descr =
    let t = in_use_exn t in
    Table.find t.flags_by_fd file_descr
  ;;

  let find_exn t file_descr =
    let t = in_use_exn t in
    Table.find_exn t.flags_by_fd file_descr
  ;;

  let iter t ~f =
    let t = in_use_exn t in
    Table.iteri t.flags_by_fd ~f:(fun ~key:file_descr ~data:flags -> f file_descr flags)
  ;;

  let fold t ~init ~f =
    let t = in_use_exn t in
    Table.fold t.flags_by_fd ~init ~f:(fun ~key ~data -> f key data)
  ;;

  let set t fd flags =
    let t = in_use_exn t in
    let already_present = Table.mem t.flags_by_fd fd in
    (* Both [epoll_ctl_add] and [epoll_ctl_mod] may raise if the file descriptor does not
       support polling. Perform these operations first and let them raise before modifying
       the table to reflect the change in epoll state. *)
    let () =
      if already_present
      then epoll_ctl_mod t.epollfd fd flags
      else epoll_ctl_add t.epollfd fd flags
    in
    Table.set t.flags_by_fd ~key:fd ~data:flags
  ;;

  let remove t fd =
    let t = in_use_exn t in
    if Table.mem t.flags_by_fd fd
    then (
      Table.remove t.flags_by_fd fd;
      epoll_ctl_del t.epollfd fd)
  ;;

  external epoll_wait
    :  File_descr.t
    -> ready_events
    -> int
    -> int
    = "core_linux_epoll_wait"

  let wait_internal t ~timeout_ms =
    let t = in_use_exn t in
    (* We clear [num_ready_events] because [epoll_wait] will invalidate [ready_events],
       and we don't want another thread to observe [t] and see junk. *)
    t.num_ready_events <- 0;
    t.num_ready_events <- epoll_wait t.epollfd t.ready_events timeout_ms;
    if t.num_ready_events = 0 then `Timeout else `Ok
  ;;

  let wait_timeout_after t span =
    let timeout_ms =
      if Time_ns.Span.( <= ) span Time_ns.Span.zero
      then 0
      else (
        (* For positive timeouts, we use a minimum timeout of one millisecond, to ensure
           that we are guaranteed that the timeout has passed when we wake up.  If we
           allowed a positive sub-millisecond timeout, we would round down and end up
           using a timeout of zero, causing [wait_internal] to return immediately.  Such
           behaviour has been seen to cause Async to spin, repeatedly requesting slightly
           smaller timeouts. *)
        let span = Time_ns.Span.max span Time_ns.Span.millisecond in
        Int63.to_int_exn
          Time_ns.Span.(
            div
              (span + of_int63_ns (Int63.of_int 500_000))
              (of_int63_ns (Int63.of_int 1_000_000))))
    in
    assert (timeout_ms >= 0);
    wait_internal t ~timeout_ms
  ;;

  let wait t ~timeout =
    (* From the epoll man page:

       | Specifying a timeout of -1 makes epoll_wait() wait indefinitely, while
       | specifying a timeout equal to zero makes epoll_wait() to return immediately
       | even if no events are available (return code equal to zero). *)
    match timeout with
    | `Never -> wait_internal t ~timeout_ms:(-1)
    | `Immediately -> wait_internal t ~timeout_ms:0
    | `After span -> wait_timeout_after t span
  ;;

  let fold_ready t ~init ~f =
    let t = in_use_exn t in
    let ac = ref init in
    for i = 0 to t.num_ready_events - 1 do
      ac := f !ac (epoll_readyfd t.ready_events i) (epoll_readyflags t.ready_events i)
    done;
    !ac
  ;;

  let iter_ready t ~f =
    let t = in_use_exn t in
    for i = 0 to t.num_ready_events - 1 do
      f (epoll_readyfd t.ready_events i) (epoll_readyflags t.ready_events i)
    done
  ;;

  module Expert = struct
    let clear_ready t =
      let t = in_use_exn t in
      t.num_ready_events <- 0
    ;;
  end

  (* external epoll_pwait
   *   : File_descr.t -> Events_buffer.raw -> int -> int list -> int
   *   = "core_linux_epoll_pwait"
   *
   * let pwait t ~timeout sigs =
   *   let millis = Float.iround_exn ~dir:`Zero ( Span.to_ms timeout ) in
   *   let num_ready = epoll_pwait t.epollfd t.events millis sigs in
   *   if num_ready = 0 then `Timeout
   *   else `Ok { Ready_fds.num_ready ; events = t.events }
   * ;; *)

  let create = Ok create
end

[%%else]

module Impl = Null_impl

[%%endif]
