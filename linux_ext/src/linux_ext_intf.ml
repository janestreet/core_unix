open! Core
open Core_unix
module Thread = Core_thread

module type S = sig @@ portable
  (** {2 sysinfo} *)

  module Sysinfo : sig
    (** Result of sysinfo syscall (man 2 sysinfo). *)
    type t =
      { uptime : Time_float.Span.t (** Time since boot *)
      ; load1 : int (** Load average over the last minute *)
      ; load5 : int (** Load average over the last 5 minutes *)
      ; load15 : int (** Load average over the last 15 minutes *)
      ; total_ram : int (** Total usable main memory *)
      ; free_ram : int (** Available memory size *)
      ; shared_ram : int (** Amount of shared memory *)
      ; buffer_ram : int (** Memory used by buffers *)
      ; total_swap : int (** Total swap page size *)
      ; free_swap : int (** Available swap space *)
      ; procs : int (** Number of current processes *)
      ; totalhigh : int (** Total high memory size *)
      ; freehigh : int (** Available high memory size *)
      ; mem_unit : int (** Memory unit size in bytes *)
      }
    [@@deriving sexp, bin_io]

    val sysinfo : (unit -> t) Or_error.t
  end

  (** {2 Filesystem functions} *)

  (** [sendfile ?pos ?len ~fd sock] sends mmap-able data from file descriptor [fd] to
      socket [sock] using offset [pos] and length [len]. Returns the number of characters
      actually written.

      NOTE: If the returned value is unequal to what was requested (= the initial size of
      the data by default), the system call may have been interrupted by a signal, the
      source file may have been truncated during operation, or a timeout may have occurred
      on the socket during sending. It is currently impossible to find out which of these
      events actually happened. Calling {!sendfile} several times on the same descriptor
      that only partially accepted data due to a timeout will eventually lead to the Unix
      error [EAGAIN].

      Raises [Unix_error] on Unix-errors. *)
  val sendfile
    : (?pos:int (** Defaults to 0. *)
       -> ?len:int
            (** Defaults to length of data (file) associated with descriptor [fd]. *)
       -> fd:File_descr.t
       -> File_descr.t
       -> int)
        Or_error.t

  (** [copy_file_range ~fd_in ?off_in ~fd_out ?off_out ~len ()] copies up to [len] bytes
      from file descriptor [fd_in] to file descriptor [fd_out], handling the data transfer
      within the kernel. When [off_in] is [None], data is read starting at the file offset
      of [fd_in], which is advanced by the number of bytes copied. When [off_in] is
      [Some n], data is read starting at offset [n] without changing the file offset of
      [fd_in]. [off_out] behaves analogously for [fd_out]. Returns the number of bytes
      copied, which may be less than [len].

      If [~min_len] is supplied, retries on partial copies and [EINTR] until at least
      [min_len] bytes have been copied (or no more progress can be made). [min_len]
      defaults to 0.

      Raises [Unix_error] on failure. *)
  val copy_file_range
    : (fd_in:File_descr.t
       -> ?off_in:int
       -> fd_out:File_descr.t
       -> ?off_out:int
       -> len:int
       -> ?min_len:int
       -> unit
       -> int)
        Or_error.t

  (** [really_copy_file_range ~fd_in ?off_in ~fd_out ?off_out ~len ()] is like
      {!copy_file_range} but retries on partial copies and [EINTR] until exactly [len]
      bytes have been copied or all the bytes from [fd_in] if it has <[len] bytes before
      eof. Raises [Unix_error] on failure. *)
  val really_copy_file_range
    : (fd_in:File_descr.t
       -> ?off_in:int
       -> fd_out:File_descr.t
       -> ?off_out:int
       -> len:int
       -> unit
       -> unit)
        Or_error.t

  (** Type for status of SO_BINDTODEVICE socket option. The socket may either restrict the
      traffic to a given (by name, e.g. "eth0") interface, or do no restriction at all. *)
  module Bound_to_interface : sig
    type t =
      | Any
      | Only of string
    [@@deriving sexp_of]
  end

  (** {2 Non-portable TCP functionality} *)

  type tcp_bool_option =
    | TCP_CORK
    (** (Since Linux 2.2) If set, don’t send out partial frames. All queued partial frames
        are sent when the option is cleared again. This is useful for prepending headers
        before calling [sendfile(2)], or for throughput optimization. As currently
        implemented, there is a 200ms ceiling on the time for which output is corked by
        TCP_CORK. If this ceiling is reached, queued data is automatically transmitted.

        This option should not be used in code intended to be portable. *)
    | TCP_QUICKACK
    (** (Since Linux 2.4.4) Quick ack solves an unfortunate interaction between the
        delayed acks and the Nagle algorithm (TCP_NODELAY). On fast LANs, the Linux TCP
        stack quickly reaches a CWND (congestion window) of 1 (Linux interprets this as "1
        unacknowledged packet", BSD/Windows and others consider it "1 unacknowledged
        segment of data").

        If Linux determines a connection to be bidirectional, it will delay sending acks,
        hoping to bundle them with other outgoing data. This can lead to serious
        connection stalls on, say, a TCP market data connection with one second
        heartbeats. TCP_QUICKACK can be used to prevent entering this delayed ack state.

        This option should not be used in code intended to be portable. *)
  [@@deriving sexp, bin_io]

  type tcp_int_option =
    | TCP_KEEPIDLE
    (** (since Linux 2.4) The time (in seconds) the connection needs to remain idle before
        TCP starts sending keepalive probes, if the socket option SO_KEEPALIVE has been
        set on this socket. This option should not be used in code intended to be
        portable. *)
    | TCP_KEEPINTVL
    (** (since Linux 2.4) The time (in seconds) between individual keepalive probes. This
        option should not be used in code intended to be portable. *)
    | TCP_KEEPCNT
    (** (since Linux 2.4) The maximum number of keepalive probes TCP should send before
        dropping the connection. This option should not be used in code intended to be
        portable. *)

  type tcp_string_option =
    | TCP_CONGESTION
    (** (Since Linux 2.6.13) Get or set the congestion-control algorithm for this socket.

        The algorithm "reno" is always permitted; other algorithms may be available,
        depending on kernel configuration and loaded modules (see
        /proc/sys/net/ipv4/tcp_allowed_congestion_control; add more using modprobe).

        "man 7 tcp" states that getsockopt(... TCP_CONGESTION ...) can return the empty
        string to indicate "uses the default congestion algorithm", but this does not seem
        to be necessarily true; sometimes in that situation it will just return the name
        of the default congestion algorithm. *)
  [@@deriving sexp, bin_io]

  (** [gettcpopt_bool sock opt] Returns the current value of the boolean TCP socket option
      [opt] for socket [sock]. *)
  val gettcpopt_bool : (File_descr.t -> tcp_bool_option -> bool) Or_error.t

  (** [settcpopt_bool sock opt v] sets the current value of the boolean TCP socket option
      [opt] for socket [sock] to value [v]. *)
  val settcpopt_bool : (File_descr.t -> tcp_bool_option -> bool -> unit) Or_error.t

  (** [gettcpopt_int sock opt] Returns the current value of the int TCP socket option
      [opt] for socket [sock]. *)
  val gettcpopt_int : (File_descr.t -> tcp_int_option -> int) Or_error.t

  (** [settcpopt_int sock opt v] sets the current value of the int TCP socket option [opt]
      for socket [sock] to value [v]. *)
  val settcpopt_int : (File_descr.t -> tcp_int_option -> int -> unit) Or_error.t

  (** [gettcpopt_string sock opt] Returns the current value of the string TCP socket
      option [opt] for socket [sock]. *)
  val gettcpopt_string : (File_descr.t -> tcp_string_option -> string) Or_error.t

  (** [settcpopt_string sock opt v] sets the current value of the string TCP socket option
      [opt] for socket [sock] to value [v]. *)
  val settcpopt_string : (File_descr.t -> tcp_string_option -> string -> unit) Or_error.t

  type ip_int_option =
    | IP_TOS
    (** (since Linux 1.0) Set or receive the Type-Of-Service (TOS) field that is sent with
        every IP packet originating from this socket. It is used to prioritize packets on
        the network. TOS is a byte. There are some standard TOS flags defined:
        IPTOS_LOWDELAY to minimize delays for interactive traffic, IPTOS_THROUGHPUT to
        optimize throughput, IPTOS_RELIABILITY to optimize for reliability, IPTOS_MINCOST
        should be used for "filler data" where slow transmission doesn't matter. At most
        one of these TOS values can be specified. Other bits are invalid and shall be
        cleared. Linux sends IPTOS_LOWDELAY datagrams first by default, but the exact
        behavior depends on the configured queueing discipline. Some high priority levels
        may require superuser privileges (the CAP_NET_ADMIN capability). The priority can
        also be set in a protocol independent way by the (SOL_SOCKET, SO_PRIORITY) socket
        option (see socket(7)). *)

  (** [setipopt_int sock opt v] sets the current value of the integer IP socket option
      [opt] for sock [sock]. *)
  val setipopt_int : (File_descr.t -> ip_int_option -> int -> unit) Or_error.t

  (** [send_nonblocking_no_sigpipe sock ?pos ?len buf] tries to do a nonblocking send on
      socket [sock] given buffer [buf], offset [pos] and length [len]. Prevents [SIGPIPE],
      i.e., raises a Unix-error in that case immediately. Returns [Some bytes_written] or
      [None] if the operation would have blocked.

      Raises [Invalid_argument] if the designated buffer range is invalid. Raises
      [Unix_error] on Unix-errors. *)
  val send_nonblocking_no_sigpipe
    : (File_descr.t
       -> ?pos:int (** default = 0 *)
       -> ?len:int (** default = [Bytes.length buf - pos] *)
       -> Bytes.t
       -> int option)
        Or_error.t

  (** [send_no_sigpipe sock ?pos ?len buf] tries to do a blocking send on socket [sock]
      given buffer [buf], offset [pos] and length [len]. Prevents [SIGPIPE], i.e., raises
      a Unix-error in that case immediately. Returns the number of bytes written.

      Raises [Invalid_argument] if the designated buffer range is invalid. Raises
      [Unix_error] on Unix-errors. *)
  val send_no_sigpipe
    : (File_descr.t
       -> ?pos:int (** default = 0 *)
       -> ?len:int (** default = [Bytes.length buf - pos] *)
       -> Bytes.t
       -> int)
        Or_error.t

  (** [sendmsg_nonblocking_no_sigpipe sock ?count iovecs] tries to do a nonblocking send
      on socket [sock] using [count] I/O-vectors [iovecs]. Prevents [SIGPIPE], i.e.,
      raises a Unix-error in that case immediately. Returns [Some bytes_written] or [None]
      if the operation would have blocked.

      Raises [Invalid_argument] if the designated ranges are invalid. Raises [Unix_error]
      on Unix-errors. *)
  val sendmsg_nonblocking_no_sigpipe
    : (File_descr.t -> ?count:int -> string IOVec.t array -> int option) Or_error.t

  (** {2 Non-portable socket functionality} *)

  module Peer_credentials : sig
    type t =
      { pid : Pid.t
      ; uid : int
      ; gid : int
      }
    [@@deriving sexp_of]
  end

  (** [peer_credential fd] takes a file descriptor of a unix socket. It returns the pid
      and real ids of the process on the other side, as described in [man 7 socket] entry
      for SO_PEERCRED. This is useful in particular in the presence of pid namespace, as
      the returned pid will be a pid in the current namespace, not the namespace of the
      other process.

      Raises [Unix_error] if something goes wrong (file descriptor doesn't satisfy the
      conditions above, no process on the other side of the socket, etc.). *)
  val peer_credentials : (File_descr.t -> Peer_credentials.t) Or_error.t

  (** {2 Clock functions} *)

  module Clock : sig
    type t

    (** All these functions can raise [Unix_error]. *)

    (** Returns the CPU-clock associated with the thread. *)
    val get : (Thread.t -> t) Or_error.t

    val get_time : (t -> Time_float.Span.t) Or_error.t
    val set_time : (t -> Time_float.Span.t -> unit) Or_error.t
    val get_resolution : (t -> Time_float.Span.t) Or_error.t

    (** The clock measuring the CPU time of a process. *)
    val get_process_clock : (unit -> t) Or_error.t

    (** The clock measuring the CPU time of the current thread. *)
    val get_thread_clock : (unit -> t) Or_error.t
  end

  (** {2 Eventfd functions} *)

  module Eventfd : sig
    module Flags : sig
      type t : immutable_data [@@deriving sexp_of]

      include Flags.S with type t := t

      (** [EFD_CLOEXEC] *)
      val cloexec : t

      (** [EFD_NONBLOCK] *)
      val nonblock : t

      (** [EFD_SEMAPHORE] *)
      val semaphore : t
    end

    type t = private File_descr.t [@@deriving compare ~localize, sexp_of]

    (** [create ?flags init] creates a new event file descriptor with [init] as the
        counter's initial value. With Linux 2.6.26 or earlier, [flags] must be [empty]. *)
    val create : (?flags:Flags.t -> Int32.t -> t) Or_error.t

    (** [read t] will block until [t]'s counter is nonzero, after which its behavior
        depends on whether [t] was created with the {!Flags.semaphore} flag set. If it was
        set, then [read t] will return [1] and decrement [t]'s counter. If it was not set,
        then [read t] will return the value of [t]'s counter and set the counter to [0].
        The returned value should be interpreted as an unsigned 64-bit integer.

        In the case that [t] was created with the {!Flags.nonblock} flag set, this
        function will raise a Unix error with the error code [EAGAIN] or [EWOULDBLOCK],
        instead of blocking. *)
    val read : t -> Int64.t

    (** [write t v] will block until [t]'s counter is less than the max value of a
        [uint64_t], after which it will increment [t]'s counter by [v], which will be
        interpreted as an unsigned 64-bit integer.

        In the case that [t] was created with the {!Flags.nonblock} flag set, this
        function will raise a Unix error with the error code [EAGAIN] or [EWOULDBLOCK],
        instead of blocking. *)
    val write : t -> Int64.t -> unit

    val to_file_descr : t -> File_descr.t
  end

  (** {2 Fallocate functions} *)

  module Fallocate : sig
    module Flags : sig
      type t : immutable_data [@@deriving sexp_of]

      include Flags.S with type t := t

      (** FALLOC_FL_KEEP_SIZE *)
      val keep_size : t

      (** FALLOC_FL_PUNCH_HOLE *)
      val punch_hole : t

      (** FALLOC_FL_COLLAPSE_RANGE *)
      val collapse_range : t

      (** FALLOC_FL_ZERO_RANGE *)
      val zero_range : t

      (** FALLOC_FL_INSERT_RANGE *)
      val insert_range : t

      (** FALLOC_FL_UNSHARE_RANGE *)
      val unshare_range : t
    end

    (** From [man 2 fallocate]:

        fallocate() allows the caller to directly manipulate the allocated disk space for
        the file referred to by [fd] for the byte range starting at [offset] and
        continuing for [size] bytes.

        The [mode] argument determines the operation to be performed on the given range. *)
    val fallocate
      : (File_descr.t -> mode:Flags.t -> offset:int -> size:int -> unit) Or_error.t
  end

  (** {2 Timerfd functions} *)

  module Timerfd : sig
    (** Clock used to mark the progress of a timer. *)
    module Clock : sig
      type t [@@deriving bin_io, compare ~localize, sexp]

      (** Settable system-wide clock. *)
      val realtime : t

      (** Nonsettable clock. It is not affected by manual changes to the system time. *)
      val monotonic : t
    end

    module Flags : sig
      type t : immutable_data [@@deriving sexp_of]

      include Flags.S with type t := t

      (** [TFD_NONBLOCK] *)
      val nonblock : t

      (** [TFD_CLOEXEC] *)
      val cloexec : t
    end

    type t = private File_descr.t [@@deriving compare ~localize, sexp_of]

    val to_file_descr : t -> File_descr.t

    (** [create ?flags clock] creates a new timer file descriptor. With Linux 2.6.26 or
        earlier, [flags] must be empty. *)
    val create : (?flags:Flags.t -> Clock.t -> t) Or_error.t

    (** [set_at t at] and [set_after t span] set [t] to fire once, at [at] or after
        [span]. [set_after] treats [span <= 0] as [span = 1ns]; unlike the underlying
        system call, [timerfd_settime], it does not clear the timer if [span = 0]. To
        clear a timerfd, use [Timerfd.clear].

        [set_repeating ?after t interval] sets [t] to fire every [interval] starting after
        [after] (default is [interval]), raising if [interval <= 0].

        [set_repeating_at t start interval] sets [t] to fire every [interval] starting at
        [start] and raising if [interval <= 0]. A [start] time in the past will cause the
        timer to start immediately. *)
    val set_at : t -> Time_ns.t -> unit

    val set_after : t -> Time_ns.Span.t -> unit
    val set_repeating : ?after:Time_ns.Span.t -> t -> Time_ns.Span.t -> unit
    val set_repeating_at : t -> Time_ns.t -> Time_ns.Span.t -> unit

    (** [clear t] causes [t] to not fire anymore. *)
    val clear : t -> unit

    type repeat =
      { fire_after : Time_ns.Span.t
      ; interval : Time_ns.Span.t
      }

    (** [get t] returns the current state of the timer [t]. *)
    val get : t -> [ `Not_armed | `Fire_after of Time_ns.Span.t | `Repeat of repeat ]

    (**/**)

    (*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

        https://opensource.janestreet.com/standards/#private-submodules *)
    module Private : sig
      val unsafe_timerfd_settime
        :  File_descr.t
        -> bool
        -> initial:Int63.t
        -> interval:Int63.t
        -> Syscall_result.Unit.t
    end
  end

  (** {2 Memfd functions} *)

  module Memfd : sig
    module Flags : sig
      type t : immutable_data [@@deriving sexp_of]

      include Flags.S with type t := t

      (** MFD_CLOEXEC *)
      val cloexec : t

      (** MFD_ALLOW_SEALING *)
      val allow_sealing : t

      (** MFD_HUGETLB *)
      val hugetlb : t

      (** MFD_NOEXEC_SEAL *)
      val noexec_seal : t

      (** MFD_EXEC *)
      val exec : t

      (** MFD_HUGE_2MB *)
      val huge_2mb : t

      (** MFD_HUGE_1GB *)
      val huge_1gb : t
    end

    type t = private File_descr.t [@@deriving sexp_of]

    val to_file_descr : t -> File_descr.t

    (** From memfd_create():

        [create] creates an anonymous file and returns a file descriptor that refers to
        it. The file behaves like a regular file, and so can be modified, truncated,
        memory-mapped, and so on. However, unlike a regular file, it lives in RAM and has
        a volatile backing storage.

        Once all references to the file are dropped, it is automatically released.
        Anonymous memory is used for all backing pages of the file. Therefore, files
        created by [create] have the same semantics as other anonymous memory allocations
        such as those allocated using [mmap] with the [MAP_ANONYMOUS] flag. *)
    val create : (?flags:Flags.t -> ?initial_size:int -> string -> t) Or_error.t
  end

  (** {2 Parent death notifications} *)

  (** [pr_set_pdeathsig s] sets the signal [s] to be sent to the executing process when
      its parent dies. NOTE: the parent may have died before or while executing this
      system call. To make sure that you do not miss this event, you should call
      {!getppid} to get the parent process id after this system call. If the parent has
      died, the returned parent PID will be 1, i.e., the init process will have adopted
      the child. You should then either send the signal to yourself using [Unix.kill], or
      execute an appropriate handler. *)
  val pr_set_pdeathsig : (Signal.t -> unit) Or_error.t

  (** [pr_get_pdeathsig ()] gets the signal that will be sent to the currently executing
      process when its parent dies. *)
  val pr_get_pdeathsig : (unit -> Signal.t) Or_error.t

  (** {2 Task name} *)

  (** [pr_set_name_first16 name] sets the name of the executing thread to [name]. Only the
      first 16 bytes in [name] will be used; the rest is ignored. *)
  val pr_set_name_first16 : (string -> unit) Or_error.t

  (** [pr_get_name ()] gets the name of the executing thread. The name is at most 16 bytes
      long. *)
  val pr_get_name : (unit -> string) Or_error.t

  (** {2 Pathname resolution} *)

  (** [file_descr_realpath fd] returns the canonicalized absolute pathname of the file
      associated with file descriptor [fd].

      Raises [Unix_error] on errors. *)
  val file_descr_realpath : (File_descr.t -> string) Or_error.t

  (** [out_channel_realpath oc] returns the canonicalized absolute pathname of the file
      associated with output channel [oc].

      Raises [Unix_error] on errors. *)
  val out_channel_realpath : (Out_channel.t -> string) Or_error.t

  (** [in_channel_realpath ic] returns the canonicalized absolute pathname of the file
      associated with input channel [ic].

      Raises [Unix_error] on errors. *)
  val in_channel_realpath : (In_channel.t -> string) Or_error.t

  (** {2 Affinity} *)

  (** Setting the CPU affinity causes a thread to only run on the cores chosen. You can
      find out how many cores a system has in /proc/cpuinfo. This can be useful in two
      ways: first, it limits a process to a core so that it won't interfere with processes
      on other cores. Second, you save time by not moving the process back and forth
      between CPUs, which sometimes invalidates their cache.

      See [man sched_setaffinity] for details.

      Note, in particular, that affinity is a "per-thread attribute that can be adjusted
      independently for each of the threads in a thread group", and so omitting [~pid] (or
      specifying [?pid] as [None]) "will set the attribute for the calling thread", and
      "passing the value returned from a call to getpid will set the attribute for the
      main thread of the thread group". (A thread group is what you might think of as a
      process.) *)
  val sched_setaffinity : (?pid:Pid.t -> cpuset:int list -> unit -> unit) Or_error.t

  val sched_getaffinity : (?pid:Pid.t -> unit -> int list) Or_error.t

  (** [sched_setaffinity_this_thread] is equivalent to [sched_setaffinity ?pid:None],
      though happens to be implemented by using [gettid] rather than passing [pid=0] to
      [sched_setaffinity]. It exists for historical reasons, and may be removed in the
      future. *)
  val sched_setaffinity_this_thread : (cpuset:int list -> unit) Or_error.t

  (** [cores ()] returns the number of cores on the machine. This may be different than
      the number of cores available to the calling process. *)
  val cores : (unit -> int) Or_error.t

  (** Parse a kernel %*pbl-format CPU list (e.g. [1,2,3,10-15]) into an [int list]. *)
  val cpu_list_of_string_exn : string -> int list

  (** [isolated_cpus ()] returns the list of cores marked as isolated. *)
  val isolated_cpus : (unit -> int list) Or_error.t

  (** [online_cpus ()] returns the list of cores online for scheduling. *)
  val online_cpus : (unit -> int list) Or_error.t

  (** [non_isolated_cpus ()] returns the list of online cores not marked as isolated. This
      is exactly the set difference between [online_cpus] and [isolated_cpus]. *)
  val non_isolated_cpus : (unit -> int list) Or_error.t

  (** [allowed_cpus ?include_offline ?pid ()] returns the list of cores allowed for
      scheduling for this particular PID. If no PID is specified, this checks the current
      PID. By default, this function automatically filters the returned list of cores to
      only include online cores. Set [include_offline] to true to also return CPU cores
      that the kernel is reporting as allowed, but are currently offline. *)
  val allowed_cpus : (?include_offline:bool -> ?pid:Pid.t -> unit -> int list) Or_error.t

  (** [cpus_local_to_nic ~ifname] returns the list of cores NUMA-local to [ifname]. *)
  val cpus_local_to_nic : (ifname:string -> int list) Or_error.t

  (** [get_terminal_size term] returns [(rows, cols)], the number of rows and columns of
      the controlling terminal (raises if no controlling terminal), or of the specified
      file descriptor (useful when writing to stdout, because stdout doesn't have to be
      the controlling terminal). *)
  val get_terminal_size : ([ `Controlling | `Fd of File_descr.t ] -> int * int) Or_error.t

  (** [Priority.t] is what is usually referred to as the "nice" value of a process. It is
      also known as the "dynamic" priority. It is used with normal (as opposed to
      real-time) processes that have static priority zero. See [Unix.Scheduler.set] for
      setting the static priority. *)
  module Priority : sig
    type t : immediate [@@deriving sexp]

    val equal : t -> t -> bool
    val of_int : int -> t
    val to_int : t -> int
    val incr : t -> t
    val decr : t -> t
  end

  (** The meaning of [pid]s for [get/setpriority] is a bit weird. According to the POSIX
      standard the priority is per process (pid), however in Linux the priority is per
      thread (tid/LWP): [man 2 setpriority] says, in the "BUGS" section, that "According
      to POSIX, the nice value is a per-process setting. However, under the current
      Linux/NPTL implementation of POSIX threads, the nice value is a per-thread attribute
      ... portable applications should avoid relying on the Linux behavior".

      As a result, if you omit [?pid] or pass [?pid:None], only the current thread will be
      affected; passing [~pid:(getpid ())] will only affect the main thread of the current
      process. *)

  (** Set the thread's priority in the Linux scheduler. Omitting the [pid] argument means
      that (only) the calling thread will be affected. *)
  val setpriority : (?pid:Pid.t -> Priority.t -> unit) Or_error.t

  (** Get the thread's priority in the Linux scheduler. Omitting the [pid] argument means
      that the calling thread's priority will be retrieved. *)
  val getpriority : (?pid:Pid.t -> unit -> Priority.t) Or_error.t

  (** [get_ipv4_address_for_interface "eth0"] returns the IP address assigned to eth0, or
      throws an exception if no IP address is configured. *)
  val get_ipv4_address_for_interface : (string -> string) Or_error.t

  (** [get_mac_address] returns the mac address of [ifname] in the canonical form, e.g.
      "aa:bb:cc:12:34:56". *)
  val get_mac_address : (ifname:string -> string) Or_error.t

  (** [bind_to_interface fd (Only "eth0")] restricts packets from being received/sent on
      the given file descriptor [fd] on any interface other than "eth0". Use
      [bind_to_interface fd Any] to allow traffic on any interface. The bindings are not
      cumulative; you may only select one interface, or [Any].

      Not to be confused with a traditional BSD sockets API [bind()] call, this
      Linux-specific socket option ([SO_BINDTODEVICE]) is used for applications on
      multi-homed machines with specific security concerns. For similar functionality when
      using multicast, see {!Core_unix.mcast_set_ifname}. *)
  val bind_to_interface : (File_descr.t -> Bound_to_interface.t -> unit) Or_error.t

  (** [get_bind_to_interface fd] returns the current interface the socket is bound to. It
      uses getsockopt() with Linux-specific [SO_BINDTODEVICE] option. Empty string means
      it is not bound to any specific interface. See [man 7 socket] for more information. *)

  val get_bind_to_interface : (File_descr.t -> Bound_to_interface.t) Or_error.t

  (* {2 Identity} *)

  (** Set the user identity used for filesystem checks for this thread. Returns the
      previous user identity.

      No error indications are returned even if the call fails. To check if it was
      successful, you can call [getfsuid] to check if the fsuid actually updated. *)
  val setfsuid : (uid:int -> int) Or_error.t

  (** Set the group identity used for filesystem checks for this thread. Returns the
      previous group identity.

      No error indications are returned even if the call fails. To check if it was
      successful, you can call [getfsgid] to check if the fsgid actually updated. *)
  val setfsgid : (gid:int -> int) Or_error.t

  (** Get the user identity used for filesystem checks. This is a wrapper around
      [setfsuid ~fsuid:(-1)] *)
  val getfsuid : (unit -> int) Or_error.t

  (** Get the group identity used for filesystem checks. This is a wrapper around
      [setfsgid ~fsgid:(-1)] *)
  val getfsgid : (unit -> int) Or_error.t

  (** For keeping your memory in RAM, i.e. preventing it from being swapped out. *)
  module Mman : sig
    module Mcl_flags : sig
      type t =
        | Current
        (** Lock all pages which are currently mapped into the address space of the
            process *)
        | Future
        (** Lock all pages which will become mapped into the address space of the process
            in the future *)
        | Onfault (** Lock pages only when they are faulted in (since Linux 4.4) *)
      [@@deriving sexp]
    end

    (** [mlockall flags] locks all pages mapped into the address space of the calling
        process. This includes the pages of the code, data and stack segment, as well as
        shared libraries, user space kernel data, shared memory, and memory-mapped files.
        All mapped pages are guaranteed to be resident in RAM when the call returns
        successfully; the pages are guaranteed to stay in RAM until later unlocked.

        Raises [Unix_error] on error. See [man 2 mlockall] for details. *)
    val mlockall : (Mcl_flags.t list -> unit) Or_error.t

    (** [munlockall ()] unlocks all pages mapped into the address space of the calling
        process.

        Raises [Unix_error] on error. See [man 2 munlockall] for details. *)
    val munlockall : (unit -> unit) Or_error.t
  end

  module Epoll : Epoll.S

  module Extended_file_attributes : sig
    (** Extended attributes are name:value pairs associated with inodes (files,
        directories, symlinks, etc). They are extensions to the normal attributes which
        are associated with all inodes in the system (i.e. the 'man 2 stat' data). A
        complete overview of extended attributes concepts can be found in 'man 5 attr'.

        [getxattr] retrieves the value of the extended attribute identified by name and
        associated with the given path in the filesystem.

        The [name] includes a namespace prefix - there may be several, disjoint namespaces
        associated with an individual inode. The [value] is a chunk of arbitrary textual
        or binary data.

        If the attribute exists, it is returned as [Ok string]. Several common errors are
        returned as possible constructors, namely:
        - [ENOATTR]: The named attribute does not exist, or the process has no access to
          this attribute.
        - [ERANGE]: The size of the value buffer is too small to hold the result.
        - [ENOTSUP]: Extended attributes are not supported by the filesystem, or are
          disabled.

        Many other errors are possible, and will raise an exception. See the man pages for
        full details. *)

    module Get_attr_result : sig
      type t =
        | Ok of string
        | ENOATTR
        | ERANGE
        | ENOTSUP
      [@@deriving sexp_of]
    end

    val getxattr
      : (follow_symlinks:bool -> path:string -> name:string -> Get_attr_result.t)
          Or_error.t

    (** [setxattr] sets the value of the extended attribute identified by name and
        associated with the given path in the filesystem.

        [how] defaults to [`Set], in which case the extended attribute will be created if
        need be, or will simply replace the value if the attribute exists. If [how] is
        [`Create], then [setxattr] returns [EEXIST] if the named attribute exists already.
        If [how] is [`Replace], then [setxattr] returns [ENOATTR] if the named attribute
        does not already exist.

        [ENOTSUP] means extended attributes are not supported by the filesystem, or are
        disabled. Many other errors are possible, and will raise an exception. See the man
        pages for full details. *)

    module Set_attr_result : sig
      type t =
        | Ok
        | EEXIST
        | ENOATTR
        | ENOTSUP
      [@@deriving sexp_of]
    end

    val setxattr
      : (?how:[ `Set | `Create | `Replace ]
         -> follow_symlinks:bool
         -> path:string
         -> name:string
         -> value:string
         -> unit
         -> Set_attr_result.t)
          Or_error.t

    (** [listxattr] retrieves the list of extended attribute names associated with the
        given path in the filesystem.

        If the call succeeds, the attribute names are returned as [Ok (string list)].
        Several common errors are returned as possible constructors, namely:
        - [ERANGE]: The size of the internal buffer is too small to hold the result.
        - [ENOTSUP]: Extended attributes are not supported by the filesystem, or are
          disabled.
        - [E2BIG]: The size of the result is larger than 64K, which means it is impossible
          to retrieve the list of attributes due to a VFS limitation.

        Many other errors are possible, and will raise an exception. See the man pages for
        full details. *)

    module List_attr_result : sig
      type t =
        | Ok of string list
        | ERANGE
        | ENOTSUP
        | E2BIG
      [@@deriving sexp_of]
    end

    val listxattr : (follow_symlinks:bool -> path:string -> List_attr_result.t) Or_error.t

    (** [removexattr] removes the extended attribute identified by name and associated
        with the given path in the filesystem.

        If the call succeeds, [Ok] is returned. Otherwise, several common errors are
        returned as possible constructors, namely:
        - [ENOATTR]: The named attribute does not exist, or the process has no access to
          this attribute.
        - [ENOTSUP]: Extended attributes are not supported by the filesystem, or are
          disabled.

        Many other errors are possible, and will raise an exception. See the man pages for
        full details. *)

    module Remove_attr_result : sig
      type t =
        | Ok
        | ENOATTR
        | ENOTSUP
      [@@deriving sexp_of]
    end

    val removexattr
      : (follow_symlinks:bool -> path:string -> name:string -> Remove_attr_result.t)
          Or_error.t
  end
end
