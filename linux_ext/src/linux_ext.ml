open! Core
module Unix = Core_unix
module Thread = Core_thread
module Time_ns = Time_ns_unix
module File_descr = Unix.File_descr
module Syscall_result = Unix.Syscall_result

module Sysinfo0 = struct
  type t =
    { uptime : Time_float.Span.t
    ; load1 : int
    ; load5 : int
    ; load15 : int
    ; total_ram : int
    ; free_ram : int
    ; shared_ram : int
    ; buffer_ram : int
    ; total_swap : int
    ; free_swap : int
    ; procs : int
    ; totalhigh : int
    ; freehigh : int
    ; mem_unit : int
    }
  [@@deriving bin_io, sexp]
end

(* If you update any of the [tcp_${x}_option] types, you also must update
   [linux_tcpopt_${x}], in the C stubs (and do make sure you get the order correct!). *)
type tcp_bool_option =
  | TCP_CORK
  | TCP_QUICKACK
[@@deriving sexp, bin_io]

type tcp_string_option = TCP_CONGESTION [@@deriving sexp, bin_io]

module Bound_to_interface = struct
  type t =
    | Any
    | Only of string
  [@@deriving sexp_of]
end

module Priority : sig
  type t [@@deriving sexp]

  val equal : t -> t -> bool
  val of_int : int -> t
  val to_int : t -> int
  val incr : t -> t
  val decr : t -> t
end = struct
  type t = int [@@deriving sexp]

  let of_int t = t
  let to_int t = t
  let incr t = t - 1
  let decr t = t + 1
  let equal (t : t) t' = t = t'
end

module Peer_credentials = struct
  (* C code depends on the layout of the type *)
  type t =
    { pid : Pid.t
    ; uid : int
    ; gid : int
    }
  [@@deriving sexp_of]
end

(* This expands a kernel command-line cpu-list string, which is a comma-separated list
   with elements:

   {|
   N        single value
   N-M      closed range
   N-M:A/S  groups of (A)mount in closed range with (S)tride
   |}

   See: https://www.kernel.org/doc/html/v4.14/admin-guide/kernel-parameters.html
*)
let cpu_list_of_string_exn str =
  let parse_int_pair ~sep str =
    try
      (* NOTE: since we're dealing with CPUs, don't need to handle negatives. *)
      String.lsplit2_exn str ~on:sep |> Tuple2.map ~f:int_of_string
    with
    | _ ->
      raise_s
        [%message
          "cpu_list_of_string_exn: expected separated integer pair"
            (sep : char)
            (str : string)]
  in
  let parse_range_pair str =
    let first, last = parse_int_pair ~sep:'-' str in
    if first > last
    then
      raise_s
        [%message
          "cpu_list_of_string_exn: range start is after end" (first : int) (last : int)]
    else first, last
  in
  (* Empty cpu-list is represented as an empty string. *)
  let parts = if String.(str = "") then [] else String.split ~on:',' str in
  List.fold parts ~init:[] ~f:(fun acc part ->
    (* first, see if we've got a ':' for a grouped range. *)
    match String.lsplit2 part ~on:':', String.lsplit2 part ~on:'-' with
    | None, None ->
      (* Single value. *)
      let cpu =
        try int_of_string part with
        | _ ->
          raise_s [%message "cpu_list_of_string_exn: expected integer" (part : string)]
      in
      acc @ [ cpu ]
    | None, Some _range ->
      (* Simple range. *)
      let first, last = parse_range_pair part in
      let rlist = List.init (last - first + 1) ~f:(Int.( + ) first) in
      acc @ rlist
    | Some (range, amt_stride), _ ->
      let first, last = parse_range_pair range in
      let amt, stride = parse_int_pair ~sep:'/' amt_stride in
      if amt <= 0 || stride <= 0
      then
        (* A kernel won't treat these kindly, they're wrong and we'll
           raise in this code. *)
        raise_s
          [%message
            "cpu_list_of_string_exn: invalid grouped range stride or amount"
              (amt : int)
              (stride : int)]
      else if amt >= stride
      then (
        (* odd, but valid: whole closed range. *)
        let rlist = List.init (last - first + 1) ~f:(Int.( + ) first) in
        acc @ rlist)
      else (
        (* This is probably simpler with procedural code, but
           we'll do it functional-style :o).  *)
        let n_sublists = Float.round_up ((last - first + 1) // stride) |> Float.to_int in
        let starts = List.init n_sublists ~f:(fun li -> first + (li * stride)) in
        let rlist =
          List.concat_map starts ~f:(fun start ->
            let group_end = Int.min (start + (amt - 1)) last in
            List.init (group_end - start + 1) ~f:(Int.( + ) start))
        in
        acc @ rlist))
  |> List.dedup_and_sort ~compare:Int.compare
;;

let cpu_list_of_file_exn file =
  match In_channel.with_file file ~f:In_channel.input_lines |> List.hd with
  | None -> []
  | Some cpu_list -> cpu_list_of_string_exn cpu_list
;;

let isolated_cpus =
  Memo.unit (fun () -> cpu_list_of_file_exn "/sys/devices/system/cpu/isolated")
;;

let online_cpus =
  Memo.unit (fun () -> cpu_list_of_file_exn "/sys/devices/system/cpu/online")
;;

let cpus_local_to_nic ~ifname =
  cpu_list_of_file_exn (sprintf "/sys/class/net/%s/device/local_cpulist" ifname)
;;

(* These module contains definitions that get used when the necessary features are not
   enabled. We put these somewhere where they'll always be compiled, to prevent them from
   getting out of sync with the real implementations. *)
module Null_toplevel = struct
  module Sysinfo = struct
    include Sysinfo0

    let sysinfo = Or_error.unimplemented "Linux_ext.Sysinfo.sysinfo"
  end

  let u = Or_error.unimplemented
  let cores = u "Linux_ext.cores"
  let cpu_list_of_string_exn = cpu_list_of_string_exn
  let isolated_cpus = u "Linux_ext.isolated_cores"
  let online_cpus = u "Linux_ext.online_cores"
  let cpus_local_to_nic = u "Linux_ext.cpus_local_to_nic"
  let file_descr_realpath = u "Linux_ext.file_descr_realpath"
  let get_ipv4_address_for_interface = u "Linux_ext.get_ipv4_address_for_interface"
  let get_mac_address = u "Linux_ext.get_mac_address"
  let bind_to_interface = u "Linux_ext.bind_to_interface"
  let get_bind_to_interface = u "Linux_ext.get_bind_to_interface"
  let get_terminal_size = u "Linux_ext.get_terminal_size"
  let gettcpopt_bool = u "Linux_ext.gettcpopt_bool"
  let gettcpopt_string = u "Linux_ext.gettcpopt_string"
  let setpriority = u "Linux_ext.setpriority"
  let getpriority = u "Linux_ext.getpriority"
  let in_channel_realpath = u "Linux_ext.in_channel_realpath"
  let out_channel_realpath = u "Linux_ext.out_channel_realpath"
  let pr_get_name = u "Linux_ext.pr_get_name"
  let pr_get_pdeathsig = u "Linux_ext.pr_get_pdeathsig"
  let pr_set_name_first16 = u "Linux_ext.pr_set_name_first16"
  let pr_set_pdeathsig = u "Linux_ext.pr_set_pdeathsig"
  let sched_setaffinity = u "Linux_ext.sched_setaffinity"
  let sched_getaffinity = u "Linux_ext.sched_getaffinity"
  let sched_setaffinity_this_thread = u "Linux_ext.sched_setaffinity_this_thread"
  let send_no_sigpipe = u "Linux_ext.send_no_sigpipe"
  let send_nonblocking_no_sigpipe = u "Linux_ext.send_nonblocking_no_sigpipe"
  let sendfile = u "Linux_ext.sendfile"
  let sendmsg_nonblocking_no_sigpipe = u "Linux_ext.sendmsg_nonblocking_no_sigpipe"
  let settcpopt_bool = u "Linux_ext.settcpopt_bool"
  let settcpopt_string = u "Linux_ext.settcpopt_string"
  let peer_credentials = u "Linux_ext.peer_credentials"
  let setfsuid = u "Linux_ext.setfsuid"
  let setfsgid = u "Linux_ext.setfsgid"
  let getfsuid = u "Linux_ext.getfsuid"
  let getfsgid = u "Linux_ext.getfsgid"

  module Epoll = Epoll.Impl
end

module Null : Linux_ext_intf.S = struct
  type nonrec tcp_bool_option = tcp_bool_option =
    | TCP_CORK
    | TCP_QUICKACK
  [@@deriving sexp, bin_io]

  type nonrec tcp_string_option = tcp_string_option = TCP_CONGESTION
  [@@deriving sexp, bin_io]

  module Bound_to_interface = struct
    type t = Bound_to_interface.t =
      | Any
      | Only of string
    [@@deriving sexp_of]
  end

  module Peer_credentials = Peer_credentials
  module Priority = Priority

  module Clock = struct
    type t

    let get = Or_error.unimplemented "Linux_ext.Clock.get"
    let get_time = Or_error.unimplemented "Linux_ext.Clock.get_time"
    let set_time = Or_error.unimplemented "Linux_ext.Clock.set_time"
    let get_resolution = Or_error.unimplemented "Linux_ext.Clock.get_resolution"
    let get_process_clock = Or_error.unimplemented "Linux_ext.Clock.get_process_clock"
    let get_thread_clock = Or_error.unimplemented "Linux_ext.Clock.get_thread_clock"
  end

  module Eventfd = struct
    type t = File_descr.t [@@deriving compare ~localize, sexp_of]

    module Flags = struct
      (* These (and flags below) are in octal to match the system header file
         <bits/eventfd.h> *)
      let nonblock = Int63.of_int 0o4000
      let cloexec = Int63.of_int 0o2000000
      let semaphore = Int63.of_int 0o1

      include Flags.Make (struct
          let allow_intersecting = true
          let should_print_error = true
          let remove_zero_flags = false
          let known = [ nonblock, "nonblock"; cloexec, "cloexec"; semaphore, "semaphore" ]
        end)
    end

    let create = Or_error.unimplemented "Linux_ext.Eventfd.create"
    let read _ = assert false
    let write _ = assert false
    let to_file_descr t = t
  end

  module Fallocate = struct
    module Flags = struct
      (* As per include/uapi/linux/memfd.h *)

      let i63 = Int63.of_int
      let keep_size = i63 0x01
      let punch_hole = i63 0x02
      let collapse_range = i63 0x08
      let zero_range = i63 0x10
      let insert_range = i63 0x20
      let unshare_range = i63 0x40

      include Flags.Make (struct
          let allow_intersecting = false
          let should_print_error = true
          let remove_zero_flags = false

          let known =
            [ keep_size, "keep_size"
            ; punch_hole, "punch_hole"
            ; collapse_range, "collapse_range"
            ; zero_range, "zero_range"
            ; insert_range, "insert_range"
            ; unshare_range, "unshare_range"
            ]
          ;;
        end)
    end

    let fallocate = Or_error.unimplemented "Linux_ext.Fallocate.fallocate"
  end

  module Timerfd = struct
    module Clock = struct
      type t = unit [@@deriving bin_io, compare ~localize, sexp]

      let realtime = ()
      let monotonic = ()
    end

    module Flags = struct
      let nonblock = Int63.of_int 0o4000
      let cloexec = Int63.of_int 0o2000000

      include Flags.Make (struct
          let allow_intersecting = false
          let should_print_error = true
          let remove_zero_flags = false
          let known = List.rev [ nonblock, "nonblock"; cloexec, "cloexec" ]
        end)
    end

    type t = File_descr.t [@@deriving compare ~localize, sexp_of]

    let to_file_descr t = t

    type repeat =
      { fire_after : Time_ns.Span.t
      ; interval : Time_ns.Span.t
      }

    let create = Or_error.unimplemented "Linux_ext.Timerfd.create"
    let set_at _ _ = assert false
    let set_after _ _ = assert false
    let set_repeating ?after:_ _ _ = assert false
    let set_repeating_at _ _ _ = assert false
    let clear _ = assert false
    let get _ = assert false

    module Private = struct
      let unsafe_timerfd_settime _ = assert false
    end
  end

  module Memfd = struct
    module Flags = struct
      (* As per include/uapi/linux/memfd.h *)

      let i63 = Int63.of_int
      let cloexec = i63 0x0001
      let allow_sealing = i63 0x0002
      let hugetlb = i63 0x0004
      let noexec_seal = i63 0x0008
      let exec = i63 0x0010
      let hugetlb_flag_encode_shift = 26
      let huge_2mb = i63 (21 lsl hugetlb_flag_encode_shift)
      let huge_1gb = i63 (30 lsl hugetlb_flag_encode_shift)

      include Flags.Make (struct
          let allow_intersecting = true (* huge_* flags intersect *)
          let should_print_error = true
          let remove_zero_flags = false

          let known =
            [ cloexec, "cloexec"
            ; allow_sealing, "allow_sealing"
            ; hugetlb, "hugetlb"
            ; noexec_seal, "noexec_seal"
            ; exec, "exec"
            ; huge_2mb, "huge_2mb"
            ; huge_1gb, "huge_1gb"
            ]
          ;;
        end)
    end

    type t = File_descr.t [@@deriving sexp_of]

    let to_file_descr = Fn.id
    let create = Or_error.unimplemented "Linux_ext.Memfd.create"
  end

  module Extended_file_attributes = struct
    module Get_attr_result = struct
      type t =
        | Ok of string
        | ENOATTR
        | ERANGE
        | ENOTSUP
      [@@deriving sexp_of]
    end

    let getxattr = Or_error.unimplemented "Linux_ext.Extended_file_attributes.getxattr"

    module Set_attr_result = struct
      type t =
        | Ok
        | EEXIST
        | ENOATTR
        | ENOTSUP
      [@@deriving sexp_of]
    end

    let setxattr = Or_error.unimplemented "Linux_ext.Extended_file_attributes.setxattr"
  end

  include Null_toplevel
end

module _ = Null
(* We leave a dummy reference to Null since it may trigger warning 60 (unused-module)
   depending on the conditional compilation below. *)

[%%import "config.h"]
[%%if defined JSC_POSIX_TIMERS && defined JSC_LINUX_EXT]

module Clock = struct
  type t

  (* These functions should be in Unix, but due to the dependency on Time,
     this is not possible (cyclic dependency). *)
  external get_time : t -> float = "core_unix_clock_gettime"

  let get_time t = Time_float.Span.of_sec (get_time t)

  external set_time : t -> float -> unit = "core_unix_clock_settime"

  let set_time t s = set_time t (Time_float.Span.to_sec s)

  external get_resolution : t -> float = "core_unix_clock_getres"

  let get_resolution t = Time_float.Span.of_sec (get_resolution t)

  external get_process_clock : unit -> t = "core_unix_clock_process_cputime_id_stub"
  external get_thread_clock : unit -> t = "core_unix_clock_thread_cputime_id_stub"

  [%%ifdef JSC_THREAD_CPUTIME]

  external get : Thread.t -> t = "core_unix_pthread_getcpuclockid"

  let get = Ok get

  [%%else]

  let get = Or_error.unimplemented "Linux_ext.Clock.get"

  [%%endif]

  let get_time = Ok get_time
  let set_time = Ok set_time
  let get_resolution = Ok get_resolution
  let get_process_clock = Ok get_process_clock
  let get_thread_clock = Ok get_thread_clock
end

[%%else]

module Clock = Null.Clock

[%%endif]
[%%if defined JSC_FALLOCATE && defined JSC_LINUX_EXT]

module Fallocate = struct
  module Flags = struct
    external keep_size : unit -> Int63.t = "core_linux_fallocate_FALLOC_FL_KEEP_SIZE"
    external punch_hole : unit -> Int63.t = "core_linux_fallocate_FALLOC_FL_PUNCH_HOLE"

    external collapse_range
      :  unit
      -> Int63.t
      = "core_linux_fallocate_FALLOC_FL_COLLAPSE_RANGE"

    external zero_range : unit -> Int63.t = "core_linux_fallocate_FALLOC_FL_ZERO_RANGE"

    external insert_range
      :  unit
      -> Int63.t
      = "core_linux_fallocate_FALLOC_FL_INSERT_RANGE"

    external unshare_range
      :  unit
      -> Int63.t
      = "core_linux_fallocate_FALLOC_FL_UNSHARE_RANGE"

    let keep_size = keep_size ()
    let punch_hole = punch_hole ()
    let collapse_range = collapse_range ()
    let zero_range = zero_range ()
    let insert_range = insert_range ()
    let unshare_range = unshare_range ()

    include Flags.Make (struct
        let allow_intersecting = false
        let should_print_error = true
        let remove_zero_flags = false

        let known =
          [ keep_size, "keep_size"
          ; punch_hole, "punch_hole"
          ; collapse_range, "collapse_range"
          ; zero_range, "zero_range"
          ; insert_range, "insert_range"
          ; unshare_range, "unshare_range"
          ]
        ;;
      end)
  end

  external fallocate
    :  (int[@untagged])
    -> mode:(int[@untagged])
    -> offset:(int[@untagged])
    -> size:(int[@untagged])
    -> unit
    = "caml_no_bytecode_impl" "core_linux_fallocate"
  [@@noalloc]

  let[@inline] fallocate fd ~mode ~offset ~size =
    fallocate (File_descr.to_int fd) ~mode:(Flags.to_int_exn mode) ~offset ~size
  ;;

  let fallocate = Or_error.return fallocate
end

[%%else]

module Fallocate = Null.Fallocate

[%%endif]
[%%if defined JSC_TIMERFD && defined JSC_LINUX_EXT]

module Timerfd = struct
  module Clock : sig
    type t [@@deriving bin_io, compare ~localize, sexp]

    val realtime : t
    val monotonic : t
  end = struct
    type t = Int63.t [@@deriving bin_io, compare ~localize, sexp]

    external realtime : unit -> Int63.t = "core_linux_timerfd_CLOCK_REALTIME"

    let realtime = realtime ()

    external monotonic : unit -> Int63.t = "core_linux_timerfd_CLOCK_MONOTONIC"

    let monotonic = monotonic ()
  end

  module Flags = struct
    external nonblock : unit -> Int63.t = "core_linux_timerfd_TFD_NONBLOCK"

    let nonblock = nonblock ()

    external cloexec : unit -> Int63.t = "core_linux_timerfd_TFD_CLOEXEC"

    let cloexec = cloexec ()

    include Flags.Make (struct
        let allow_intersecting = false
        let should_print_error = true
        let remove_zero_flags = false
        let known = List.rev [ nonblock, "nonblock"; cloexec, "cloexec" ]
      end)
  end

  type t = File_descr.t [@@deriving compare ~localize, sexp_of]

  let to_file_descr t = t

  external timerfd_create : Clock.t -> Flags.t -> int = "core_linux_timerfd_create"

  (* At Jane Street, we link with [--wrap timerfd_create] so that we can use
     our own wrapper around [timerfd_create].  This allows us to compile an executable on
     a machine that has timerfd (e.g. CentOS 6) but then run the executable on a machine
     that does not (e.g. CentOS 5), but that has our wrapper library.  We set up our
     wrapper so that when running on a machine that doesn't have it, [timerfd_create]
     raises ENOSYS. *)
  let create =
    let create ?(flags = Flags.empty) clock =
      File_descr.of_int (timerfd_create clock flags)
    in
    match Result.try_with (fun () -> create Clock.realtime) with
    | Ok t ->
      Unix.close t;
      Ok create
    | Error (Unix.Unix_error (ENOSYS, _, _)) ->
      Or_error.unimplemented "Linux_ext.Timerfd.create"
    | Error _ ->
      (* [timerfd_create] is implemented but fails with the arguments we used above.
         [create] might still be usable with different arguments, so we expose it
         here. *)
      Ok create
  ;;

  external unsafe_timerfd_settime
    :  t
    -> bool
    -> initial:Int63.t
    -> interval:Int63.t
    -> Syscall_result.Unit.t
    = "core_linux_timerfd_settime"
  [@@noalloc]

  let timerfd_settime t ~absolute ~initial ~interval =
    (* We could accept [interval < 0] or [initial < 0 when absolute], but then the
       conversions to timespecs in the C code become tedious and [timerfd_setttime] fails
       when it gets anything negative anyway. *)
    if Int63.O.(initial < zero || interval < zero)
    then
      raise_s
        [%sexp
          "timerfd_settime got invalid parameters (initial < 0 or interval < 0)."
          , { timerfd = (t : t); initial : Int63.t; interval : Int63.t }];
    unsafe_timerfd_settime t absolute ~initial ~interval
    |> Syscall_result.Unit.ok_or_unix_error_exn ~syscall_name:"timerfd_settime"
  ;;

  let initial_of_span span =
    Time_ns.Span.to_int63_ns
      (if Time_ns.Span.( <= ) span Time_ns.Span.zero
       then Time_ns.Span.nanosecond
       else span)
  ;;

  let set_at t at =
    if Time_ns.( <= ) at Time_ns.epoch
    then failwiths "Timerfd.set_at got time before epoch" at [%sexp_of: Time_ns.t];
    timerfd_settime
      t
      ~absolute:true
      ~initial:(Time_ns.to_int63_ns_since_epoch at)
      ~interval:Int63.zero
  ;;

  let set_after t span =
    timerfd_settime t ~absolute:false ~initial:(initial_of_span span) ~interval:Int63.zero
  ;;

  let set_repeating ?after t interval =
    if Time_ns.Span.( <= ) interval Time_ns.Span.zero
    then
      failwiths
        "Timerfd.set_repeating got invalid interval"
        interval
        [%sexp_of: Time_ns.Span.t];
    let interval = Time_ns.Span.to_int63_ns interval in
    timerfd_settime
      t
      ~absolute:false
      ~initial:(Option.value_map after ~f:initial_of_span ~default:interval)
      ~interval
  ;;

  let set_repeating_at t at (interval : Time_ns.Span.t) =
    if Time_ns.( <= ) at Time_ns.epoch
    then
      failwiths "Timerfd.set_repeating_at got time before epoch" at [%sexp_of: Time_ns.t];
    if Time_ns.Span.( <= ) interval Time_ns.Span.zero
    then
      failwiths
        "Timerfd.set_repeating_at got invalid interval"
        interval
        [%sexp_of: Time_ns.Span.t];
    let interval = Time_ns.Span.to_int63_ns interval in
    timerfd_settime
      t
      ~absolute:true
      ~initial:(Time_ns.to_int63_ns_since_epoch at)
      ~interval
  ;;

  let clear t = timerfd_settime t ~absolute:false ~initial:Int63.zero ~interval:Int63.zero

  type repeat =
    { fire_after : Time_ns.Span.t
    ; interval : Time_ns.Span.t
    }

  external timerfd_gettime : t -> repeat = "core_linux_timerfd_gettime"

  let get t =
    let spec = timerfd_gettime t in
    if Time_ns.Span.equal spec.interval Time_ns.Span.zero
    then
      if Time_ns.Span.equal spec.fire_after Time_ns.Span.zero
      then `Not_armed
      else `Fire_after spec.fire_after
    else `Repeat spec
  ;;

  module Private = struct
    let unsafe_timerfd_settime = unsafe_timerfd_settime
  end
end

[%%else]

module Timerfd = Null.Timerfd

[%%endif]
[%%if defined JSC_MEMFD && defined JSC_LINUX_EXT]

module Memfd = struct
  module Flags = struct
    external cloexec : unit -> Int63.t = "core_linux_memfd_MFD_CLOEXEC"
    external allow_sealing : unit -> Int63.t = "core_linux_memfd_MFD_ALLOW_SEALING"
    external hugetlb : unit -> Int63.t = "core_linux_memfd_MFD_HUGETLB"
    external noexec_seal : unit -> Int63.t = "core_linux_memfd_MFD_NOEXEC_SEAL"
    external exec : unit -> Int63.t = "core_linux_memfd_MFD_EXEC"
    external huge_2mb : unit -> Int63.t = "core_linux_memfd_MFD_HUGE_2MB"
    external huge_1gb : unit -> Int63.t = "core_linux_memfd_MFD_HUGE_1GB"

    let cloexec = cloexec ()
    let allow_sealing = allow_sealing ()
    let hugetlb = hugetlb ()
    let noexec_seal = noexec_seal ()
    let exec = exec ()
    let huge_2mb = huge_2mb ()
    let huge_1gb = huge_1gb ()

    include Flags.Make (struct
        let allow_intersecting = true (* huge_* flags intersect *)
        let should_print_error = true
        let remove_zero_flags = false

        let known =
          [ cloexec, "cloexec"
          ; allow_sealing, "allow_sealing"
          ; hugetlb, "hugetlb"
          ; noexec_seal, "noexec_seal"
          ; exec, "exec"
          ; huge_2mb, "huge_2mb"
          ; huge_1gb, "huge_1gb"
          ]
        ;;
      end)
  end

  type t = File_descr.t [@@deriving compare ~localize, sexp_of]

  external create
    :  flags:Flags.t
    -> initial_size:int
    -> name:string
    -> t
    = "core_linux_memfd_create"

  let create =
    let create ?(flags = Flags.empty) ?(initial_size = 0) name =
      create ~flags ~initial_size ~name
    in
    Or_error.return create
  ;;

  let to_file_descr t = t
end

[%%else]

module Memfd = Null.Memfd

[%%endif]
[%%ifdef JSC_LINUX_EXT]

type file_descr = Unix.File_descr.t

module Eventfd = struct
  module Flags = struct
    external cloexec : unit -> Int63.t = "core_linux_eventfd_EFD_CLOEXEC"
    external nonblock : unit -> Int63.t = "core_linux_eventfd_EFD_NONBLOCK"
    external semaphore : unit -> Int63.t = "core_linux_eventfd_EFD_SEMAPHORE"

    let cloexec = cloexec ()
    let nonblock = nonblock ()
    let semaphore = semaphore ()
    let known = [ cloexec, "cloexec"; nonblock, "nonblock"; semaphore, "semaphore" ]

    include Flags.Make (struct
        let allow_intersecting = true
        let should_print_error = true
        let known = known
        let remove_zero_flags = false
      end)
  end

  type t = File_descr.t [@@deriving compare ~localize, sexp_of]

  external create : Int32.t -> Flags.t -> t = "core_linux_eventfd"
  external read : t -> Int64.t = "core_linux_eventfd_read"
  external write : t -> Int64.t -> unit = "core_linux_eventfd_write"

  let create =
    let create ?(flags = Flags.empty) init = create init flags in
    Or_error.return create
  ;;

  let to_file_descr t = t
end

external sendfile
  :  sock:file_descr
  -> fd:file_descr
  -> pos:int
  -> len:int
  -> int
  = "core_linux_sendfile_stub"

let sendfile ?(pos = 0) ?len ~fd sock =
  let len =
    match len with
    | Some len -> len
    | None -> Int64.to_int_exn (Int64.( - ) (Unix.fstat fd).st_size (Int64.of_int pos))
  in
  sendfile ~sock ~fd ~pos ~len
;;

(* Raw result of sysinfo syscall *)
module Raw_sysinfo = struct
  type t =
    { uptime : int
    ; load1 : int
    ; load5 : int
    ; load15 : int
    ; total_ram : int
    ; free_ram : int
    ; shared_ram : int
    ; buffer_ram : int
    ; total_swap : int
    ; free_swap : int
    ; procs : int
    ; totalhigh : int
    ; freehigh : int
    ; mem_unit : int
    }
end

module Sysinfo = struct
  include Sysinfo0

  external raw_sysinfo : unit -> Raw_sysinfo.t = "core_linux_sysinfo"

  let sysinfo =
    Ok
      (fun () ->
        let raw = raw_sysinfo () in
        { uptime = Time_float.Span.of_int_sec raw.Raw_sysinfo.uptime
        ; load1 = raw.Raw_sysinfo.load1
        ; load5 = raw.Raw_sysinfo.load5
        ; load15 = raw.Raw_sysinfo.load15
        ; total_ram = raw.Raw_sysinfo.total_ram
        ; free_ram = raw.Raw_sysinfo.free_ram
        ; shared_ram = raw.Raw_sysinfo.shared_ram
        ; buffer_ram = raw.Raw_sysinfo.buffer_ram
        ; total_swap = raw.Raw_sysinfo.total_swap
        ; free_swap = raw.Raw_sysinfo.free_swap
        ; procs = raw.Raw_sysinfo.procs
        ; totalhigh = raw.Raw_sysinfo.totalhigh
        ; freehigh = raw.Raw_sysinfo.freehigh
        ; mem_unit = raw.Raw_sysinfo.mem_unit
        })
  ;;
end

external gettcpopt_bool
  :  file_descr
  -> tcp_bool_option
  -> bool
  = "core_linux_gettcpopt_bool_stub"

external settcpopt_bool
  :  file_descr
  -> tcp_bool_option
  -> bool
  -> unit
  = "core_linux_settcpopt_bool_stub"

external gettcpopt_string
  :  file_descr
  -> tcp_string_option
  -> string
  = "core_linux_gettcpopt_string_stub"

external settcpopt_string
  :  file_descr
  -> tcp_string_option
  -> string
  -> unit
  = "core_linux_settcpopt_string_stub"

external peer_credentials
  :  file_descr
  -> Peer_credentials.t
  = "core_linux_peer_credentials"

external unsafe_send_nonblocking_no_sigpipe
  :  file_descr
  -> pos:int
  -> len:int
  -> Bytes.t
  -> int
  = "core_linux_send_nonblocking_no_sigpipe_stub"

let unsafe_send_nonblocking_no_sigpipe fd ~pos ~len buf =
  let res = unsafe_send_nonblocking_no_sigpipe fd ~pos ~len buf in
  if res = -1 then None else Some res
;;

external unsafe_send_no_sigpipe
  :  file_descr
  -> pos:int
  -> len:int
  -> Bytes.t
  -> int
  = "core_linux_send_no_sigpipe_stub"

let check_send_args ?pos ?len buf =
  let str_len = Bytes.length buf in
  let pos =
    match pos with
    | None -> 0
    | Some pos ->
      if pos < 0 then invalid_arg "send_nonblocking_no_sigpipe: pos < 0";
      if pos > str_len then invalid_arg "send_nonblocking_no_sigpipe: pos > str_len";
      pos
  in
  let len =
    match len with
    | None -> str_len - pos
    | Some len ->
      if len < 0 then invalid_arg "send_nonblocking_no_sigpipe: pos < 0";
      if pos + len > str_len
      then invalid_arg "send_nonblocking_no_sigpipe: pos + len > str_len";
      len
  in
  pos, len
;;

let send_nonblocking_no_sigpipe sock ?pos ?len buf =
  let pos, len = check_send_args ?pos ?len buf in
  unsafe_send_nonblocking_no_sigpipe sock ~pos ~len buf
;;

let send_no_sigpipe sock ?pos ?len buf =
  let pos, len = check_send_args ?pos ?len buf in
  unsafe_send_no_sigpipe sock ~pos ~len buf
;;

external unsafe_sendmsg_nonblocking_no_sigpipe
  :  file_descr
  -> string Unix.IOVec.t array
  -> int
  -> int
  = "core_linux_sendmsg_nonblocking_no_sigpipe_stub"

let unsafe_sendmsg_nonblocking_no_sigpipe fd iovecs count =
  let res = unsafe_sendmsg_nonblocking_no_sigpipe fd iovecs count in
  if res = -1 then None else Some res
;;

let sendmsg_nonblocking_no_sigpipe sock ?count iovecs =
  let count =
    match count with
    | None -> Array.length iovecs
    | Some count ->
      if count < 0 then invalid_arg "sendmsg_nonblocking_no_sigpipe: count < 0";
      let n_iovecs = Array.length iovecs in
      if count > n_iovecs
      then invalid_arg "sendmsg_nonblocking_no_sigpipe: count > n_iovecs";
      count
  in
  unsafe_sendmsg_nonblocking_no_sigpipe sock iovecs count
;;

external pr_set_pdeathsig : Signal.t -> unit = "core_linux_pr_set_pdeathsig_stub"
external pr_get_pdeathsig : unit -> Signal.t = "core_linux_pr_get_pdeathsig_stub"
external pr_set_name_first16 : string -> unit = "core_linux_pr_set_name"
external pr_get_name : unit -> string = "core_linux_pr_get_name"

let file_descr_realpath fd =
  Filename_unix.realpath ("/proc/self/fd/" ^ File_descr.to_string fd)
;;

let out_channel_realpath oc = file_descr_realpath (Unix.descr_of_out_channel oc)
let in_channel_realpath ic = file_descr_realpath (Unix.descr_of_in_channel ic)

external raw_sched_setaffinity
  :  pid:int
  -> cpuset:int list
  -> unit
  = "core_linux_sched_setaffinity"

let pid_to_int_or_zero = function
  | None -> 0
  | Some pid -> Pid.to_int pid
;;

let sched_setaffinity ?pid ~cpuset () =
  raw_sched_setaffinity ~pid:(pid_to_int_or_zero pid) ~cpuset
;;

external raw_sched_getaffinity : pid:int -> int list = "core_linux_sched_getaffinity"

let sched_getaffinity ?pid () = raw_sched_getaffinity ~pid:(pid_to_int_or_zero pid)

(* defined in core_unix_stubs.c *)
external gettid : unit -> int = "core_unix_gettid"

let sched_setaffinity_this_thread ~cpuset =
  sched_setaffinity ~pid:(Pid.of_int (gettid ())) ~cpuset ()
;;

(* defined in linux_ext_stubs.c *)
external raw_setpriority : pid:int -> Priority.t -> unit = "core_linux_setpriority"
external raw_getpriority : pid:int -> Priority.t = "core_linux_getpriority"

let setpriority ?pid priority = raw_setpriority ~pid:(pid_to_int_or_zero pid) priority
let getpriority ?pid () = raw_getpriority ~pid:(pid_to_int_or_zero pid)

let cores =
  Memo.unit (fun () ->
    match Option.bind (Core_unix.sysconf NPROCESSORS_ONLN) ~f:Int64.to_int with
    | None ->
      (* Fall back to our own implementation on the off-chance that the C library for some
         reason doesn't support this conf.
         We use this as a fallback instead of the only implementation because glibc tries
         hard to be robust, for example it's resilient to /sys or even /proc not
         being mounted. *)
      List.length (online_cpus ())
    | Some n -> n)
;;

external get_terminal_size : File_descr.t -> int * int = "core_linux_get_terminal_size"

let get_terminal_size = function
  | `Fd fd -> get_terminal_size fd
  | `Controlling ->
    protectx
      (Unix.openfile "/dev/tty" ~mode:[ O_RDWR ] ~perm:0)
      ~finally:Unix.close
      ~f:get_terminal_size
;;

external get_ipv4_address_for_interface
  :  string
  -> string
  = "core_linux_get_ipv4_address_for_interface"

external get_mac_address : ifname:string -> string = "core_linux_get_mac_address"

(* The C-stub is a simple pass-through of the linux SO_BINDTODEVICE semantics, wherein an
   empty string removes any binding *)
external bind_to_interface'
  :  File_descr.t
  -> string
  -> unit
  = "core_linux_bind_to_interface"

let bind_to_interface fd ifname =
  let name =
    match ifname with
    | Bound_to_interface.Only name -> name
    | Bound_to_interface.Any -> ""
  in
  bind_to_interface' fd name
;;

external get_bind_to_interface'
  :  File_descr.t
  -> string
  = "core_linux_get_bind_to_interface"

let get_bind_to_interface fd =
  match get_bind_to_interface' fd with
  | "" -> Bound_to_interface.Any
  | name -> Bound_to_interface.Only name
;;

external setfsuid : uid:int -> int = "core_linux_setfsuid"
external setfsgid : gid:int -> int = "core_linux_setfsgid"

let getfsuid () = setfsuid ~uid:(-1)
let getfsgid () = setfsgid ~gid:(-1)

module Epoll = Epoll.Impl

let cores = Ok cores
let isolated_cpus = Ok isolated_cpus
let online_cpus = Ok online_cpus
let cpus_local_to_nic = Ok cpus_local_to_nic
let file_descr_realpath = Ok file_descr_realpath
let get_ipv4_address_for_interface = Ok get_ipv4_address_for_interface
let get_mac_address = Ok get_mac_address
let bind_to_interface = Ok bind_to_interface
let get_bind_to_interface = Ok get_bind_to_interface
let get_terminal_size = Ok get_terminal_size
let gettcpopt_bool = Ok gettcpopt_bool
let gettcpopt_string = Ok gettcpopt_string
let setpriority = Ok setpriority
let getpriority = Ok getpriority
let in_channel_realpath = Ok in_channel_realpath
let out_channel_realpath = Ok out_channel_realpath
let pr_get_name = Ok pr_get_name
let pr_get_pdeathsig = Ok pr_get_pdeathsig
let pr_set_name_first16 = Ok pr_set_name_first16
let pr_set_pdeathsig = Ok pr_set_pdeathsig
let sched_setaffinity = Ok sched_setaffinity
let sched_getaffinity = Ok sched_getaffinity
let sched_setaffinity_this_thread = Ok sched_setaffinity_this_thread
let send_no_sigpipe = Ok send_no_sigpipe
let send_nonblocking_no_sigpipe = Ok send_nonblocking_no_sigpipe
let sendfile = Ok sendfile
let sendmsg_nonblocking_no_sigpipe = Ok sendmsg_nonblocking_no_sigpipe
let settcpopt_bool = Ok settcpopt_bool
let settcpopt_string = Ok settcpopt_string
let peer_credentials = Ok peer_credentials
let setfsuid = Ok setfsuid
let setfsgid = Ok setfsgid
let getfsuid = Ok getfsuid
let getfsgid = Ok getfsgid

module Extended_file_attributes = struct
  module Flags = struct
    external only_create : unit -> Int63.t = "core_linux_xattr_XATTR_CREATE_flag"
    external only_replace : unit -> Int63.t = "core_linux_xattr_XATTR_REPLACE_flag"

    let set = Int63.zero
  end

  module Get_attr_result = struct
    type t =
      | Ok of string
      | ENOATTR
      | ERANGE
      | ENOTSUP
    [@@deriving sexp_of]
  end

  module Set_attr_result = struct
    type t =
      | Ok
      | EEXIST
      | ENOATTR
      | ENOTSUP
    [@@deriving sexp_of]
  end

  external getxattr
    :  bool
    -> string
    -> string
    -> Get_attr_result.t
    = "core_linux_getxattr"

  external setxattr
    :  bool
    -> string
    -> string
    -> string
    -> Int63.t
    -> Set_attr_result.t
    = "core_linux_setxattr"

  let getxattr ~follow_symlinks ~path ~name = getxattr follow_symlinks path name

  let setxattr ?(how = `Set) ~follow_symlinks ~path ~name ~value () =
    let flags =
      match how with
      | `Set -> Flags.set
      | `Create -> Flags.only_create ()
      | `Replace -> Flags.only_replace ()
    in
    setxattr follow_symlinks path name value flags
  ;;

  let getxattr = Ok getxattr
  let setxattr = Ok setxattr
end

[%%else]

(* Uncomment this if you need to suppress "unused <blah>" warnings.
   Keeping commented out because this section is not checked by the CI, so is likely
   to go stale / cause maintenance pains.  *)
(* module _ = Thread
module _ = Syscall_result

let _ = isolated_cpus
let _ = online_cpus
   let _ = cpus_local_to_nic *)

include Null_toplevel
module Eventfd = Null.Eventfd
module Extended_file_attributes = Null.Extended_file_attributes

[%%endif]
