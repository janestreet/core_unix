open Core
open Poly
module Unix = Core_unix
open Unix
open Linux_ext
open Expect_test_helpers_core
module Thread = Core_thread

let%test_module "[Timerfd]" =
  (module struct
    open Timerfd

    let%test_unit "unsafe_timerfd_settime returning errno" =
      let result =
        Private.unsafe_timerfd_settime
          (File_descr.of_int (-1))
          false
          ~initial:Int63.zero
          ~interval:Int63.zero
      in
      if Syscall_result.Unit.is_ok result
      then
        failwiths
          ~here:[%here]
          "unsafe_timerfd_settime unexpectedly succeeded"
          result
          [%sexp_of: Syscall_result.Unit.t];
      [%test_result: Unix.Error.t] (Syscall_result.Unit.error_exn result) ~expect:EBADF
    ;;

    let%test_unit _ =
      match create with
      | Error _ -> ()
      | Ok create ->
        let t = create Clock.realtime in
        assert (get t = `Not_armed);
        set_after t Time_ns.Span.minute;
        assert (
          match get t with
          | `Fire_after span -> Time_ns.Span.( <= ) span Time_ns.Span.minute
          | _ -> false);
        let span = Time_ns.Span.scale Time_ns.Span.minute 2. in
        set_repeating t ~after:Time_ns.Span.minute span;
        assert (
          match get t with
          | `Repeat { fire_after; interval } ->
            Time_ns.Span.( <= ) fire_after Time_ns.Span.minute
            && Time_ns.Span.equal interval span
          | _ -> false)
    ;;
  end)
;;

let%test_module "[Memfd]" =
  (module struct
    open Memfd

    let%test_unit "[create] returns file descriptors that look correct in \
                   [/proc/self/fd/...] and are the right size"
      =
      match create with
      | Error _ -> ()
      | Ok create ->
        let name = "foo" in
        let initial_size = 4096 * 2 in
        let fd =
          create ~flags:Flags.(cloexec + allow_sealing) ~initial_size name
          |> to_file_descr
        in
        let basename =
          Unix.readlink [%string "/proc/self/fd/%{fd#File_descr}"] |> Filename.basename
        in
        assert (String.(basename = [%string "memfd:%{name} (deleted)"]));
        assert (Int64.((Unix.fstat fd).st_size = of_int initial_size));
        Unix.close fd
    ;;
  end)
;;

let%test_unit _ =
  match cores with
  | Error _ -> ()
  | Ok cores ->
    assert (cores () > 0);
    assert (cores () < 100000)
;;

(* 99,999 cores ought to be enough for anybody *)

let%test "lo interface addr is 127.0.0.1" =
  (* This could be a false positive if the test box is misconfigured. *)
  match get_ipv4_address_for_interface with
  | Error _ -> true
  | Ok f -> f "lo" = "127.0.0.1"
;;

(* Epoll unit test included here for some example usage. Creates 2 sockets,
   adds them to an epoll set, sends data to one of them and calls Epoll.wait.
   The test passes if the resulting Ready_fds set has 1 ready fd, matching
   the one we sent to, with read, !write, and !error. *)
let%test_module _ =
  (module struct
    module Flags = Epoll.Flags

    let udp_listener ~port =
      let sock = Unix.socket ~domain:Unix.PF_INET ~kind:Unix.SOCK_DGRAM ~protocol:0 () in
      let iaddr = Unix.ADDR_INET (Unix.Inet_addr.localhost, port) in
      Unix.setsockopt sock Unix.SO_REUSEADDR true;
      Unix.bind sock ~addr:iaddr;
      sock
    ;;

    let send_substring s buf ~port =
      let addr = Unix.ADDR_INET (Unix.Inet_addr.localhost, port) in
      let len = String.length buf in
      Unix.sendto_substring s ~buf ~pos:0 ~len ~mode:[] ~addr
    ;;

    let with_epoll ~f =
      protectx
        ~finally:Epoll.close
        ~f
        ((Or_error.ok_exn Epoll.create) ~num_file_descrs:1024 ~max_ready_events:256)
    ;;

    let%test_unit "epoll errors" =
      with_epoll ~f:(fun t ->
        let tmp = "temporary-file-for-testing-epoll" in
        let fd = Unix.openfile tmp ~mode:[ Unix.O_CREAT; Unix.O_WRONLY ] in
        (* Epoll does not support ordinary files, and so should fail if you ask it to watch
           one. *)
        assert (Result.is_error (Result.try_with (fun () -> Epoll.set t fd Flags.none)));
        Unix.close fd;
        Unix.unlink tmp)
    ;;

    let%test_unit "epoll test" =
      with_epoll ~f:(fun epset ->
        let span = Time_ns.Span.of_sec 0.1 in
        let sock1 = udp_listener ~port:7070 in
        let sock2 = udp_listener ~port:7071 in
        Epoll.set epset sock1 Flags.in_;
        Epoll.set epset sock2 Flags.in_;
        let _sent = send_substring sock2 "TEST" ~port:7070 in
        match Epoll.wait_timeout_after epset span with
        | `Timeout -> assert false
        | `Ok ->
          let ready =
            Epoll.fold_ready epset ~init:[] ~f:(fun ac fd flags ->
              if flags = Flags.in_ then fd :: ac else ac)
          in
          (* Explanation of the test:
             1) I create two udp sockets, sock1 listening on 7070 and sock2, on 7071
             2) These two sockets are both added to epoll for read notification
             3) I send a packet, _using_ sock2 to sock1 (who is listening on 7070)
             4) epoll_wait should return, with [ sock1 ] ready to be read.
          *)
          (match ready with
           | [ sock ] when sock = sock1 -> ()
           | [ _ ] -> failwith "wrong socket is ready"
           | xs -> failwithf "%d sockets are ready" (List.length xs) ()))
    ;;

    let%test_unit "Timerfd.set_after small span test" =
      match Timerfd.create with
      | Error _ -> ()
      | Ok timerfd_create ->
        with_epoll ~f:(fun epoll ->
          let timerfd = timerfd_create Timerfd.Clock.realtime in
          Epoll.set epoll (timerfd :> File_descr.t) Epoll.Flags.in_;
          List.iter [ 0; 1 ] ~f:(fun span_ns ->
            Timerfd.set_after timerfd (Time_ns.Span.of_int63_ns (Int63.of_int span_ns));
            match Epoll.wait epoll ~timeout:`Never with
            | `Timeout -> assert false
            | `Ok -> ());
          Unix.close (timerfd :> Unix.File_descr.t))
    ;;

    let%test_unit "epoll detects an error on the write side of a pipe when the read side \
                   of the pipe closes\n\
                   after a partial read"
      =
      let saw_sigpipe = ref false in
      let new_sigpipe_handler = `Handle (fun _ -> saw_sigpipe := true) in
      let old_sigpipe_handler = Signal.Expert.signal Signal.pipe new_sigpipe_handler in
      Exn.protect
        ~finally:(fun () -> Signal.Expert.set Signal.pipe old_sigpipe_handler)
        ~f:(fun () ->
          let r, w = Unix.pipe () in
          let w_len = 1_000_000 in
          let r_len = 1_000 in
          let read =
            Thread.create
              ~on_uncaught_exn:`Print_to_stderr
              (fun () ->
                let nr =
                  Bigstring_unix.read r (Bigstring.create r_len) ~pos:0 ~len:r_len
                in
                assert (nr > 0 && nr <= r_len);
                Unix.close r)
              ()
          in
          let nw =
            Bigstring_unix.writev w [| Unix.IOVec.of_bigstring (Bigstring.create w_len) |]
          in
          assert (nw > 0 && nw < w_len);
          Thread.join read;
          with_epoll ~f:(fun epoll ->
            Epoll.set epoll w Epoll.Flags.out;
            match Epoll.wait_timeout_after epoll Time_ns.Span.second with
            | `Timeout -> assert false
            | `Ok ->
              assert !saw_sigpipe;
              let saw_fd = ref false in
              Epoll.iter_ready epoll ~f:(fun fd flags ->
                assert (Unix.File_descr.equal fd w);
                assert (Epoll.Flags.equal flags Epoll.Flags.err);
                saw_fd := true);
              assert !saw_fd))
    ;;

    let%test_unit "epoll removes a file descriptor from tracking even if it gets EBADFD" =
      with_epoll ~f:(fun epoll ->
        let r, w = Unix.pipe () in
        Epoll.set epoll w Epoll.Flags.out;
        Unix.close r;
        Unix.close w;
        (* It is bad to close a FD before removing it from epoll, and it raises *)
        (match Epoll.remove epoll w with
         | () -> assert false
         | exception Unix.Unix_error (EBADF, _, _) -> ());
        (* Even though the above failed, the FD should be removed from the tracking set.
           When the FD numbers are reused below, they should work fine. *)
        let r, w = Unix.pipe () in
        Epoll.set epoll w Epoll.Flags.out;
        Epoll.remove epoll w;
        Unix.close r;
        Unix.close w)
    ;;
  end)
;;

module Flags = Epoll.Flags

let with_epoll ~f =
  protectx
    ~finally:Epoll.close
    ~f
    ((Or_error.ok_exn Epoll.create) ~num_file_descrs:1024 ~max_ready_events:256)
;;

let make_socket () = Unix.socket ~domain:Unix.PF_INET ~kind:Unix.SOCK_DGRAM ~protocol:0 ()

let%expect_test "[Epoll.set] has allocation limits" =
  with_epoll ~f:(fun epset ->
    let sock1 = make_socket () in
    require_allocation_does_not_exceed (Minor_words 6) [%here] (fun () ->
      Epoll.set epset sock1 Flags.in_));
  [%expect {| |}]
;;

let%expect_test "[Epoll.find] does not allocate when not present" =
  with_epoll ~f:(fun epset ->
    let sock1 = make_socket () in
    require_no_allocation [%here] (fun () -> ignore (Epoll.find epset sock1 : _ option)));
  [%expect {| |}]
;;

let%expect_test "[Epoll.find] has allocation limits when present" =
  with_epoll ~f:(fun epset ->
    let sock1 = make_socket () in
    Epoll.set epset sock1 Flags.in_;
    require_allocation_does_not_exceed (Minor_words 2) [%here] (fun () ->
      ignore (Epoll.find epset sock1 : _ option)));
  [%expect {| |}]
;;

let%expect_test "[Epoll.remove] does not allocate" =
  with_epoll ~f:(fun epset ->
    let sock1 = make_socket () in
    require_no_allocation [%here] (fun () -> ignore (Epoll.remove epset sock1 : unit));
    Epoll.set epset sock1 Flags.in_;
    require_no_allocation [%here] (fun () -> ignore (Epoll.remove epset sock1 : unit)));
  [%expect {| |}]
;;

let%expect_test "[Epoll.Expert.clear_ready]" =
  with_epoll ~f:(fun t ->
    let print_num_ready () =
      let num_ready = ref 0 in
      Epoll.iter_ready t ~f:(fun _ _ -> incr num_ready);
      print_s [%message (!num_ready : int)]
    in
    let file_descr = make_socket () in
    Epoll.set t file_descr Flags.out;
    match Epoll.wait t ~timeout:`Immediately with
    | `Timeout -> assert false
    | `Ok ->
      print_num_ready ();
      Epoll.Expert.clear_ready t;
      print_num_ready ();
      [%expect {|
        (!num_ready 1)
        (!num_ready 0)
        |}])
;;

(* Eventfd *)

let create = Or_error.ok_exn Eventfd.create

let%test_unit "Eventfd.read returns the initial value on a non-semaphore fd" =
  let fd = create 1l in
  [%test_result: Int64.t] ~expect:1L (Eventfd.read fd);
  let fd = create 10l in
  [%test_result: Int64.t] ~expect:10L (Eventfd.read fd)
;;

let%test_unit "Eventfd.read returns [1] on a semaphore fd" =
  let fd = create ~flags:Eventfd.Flags.semaphore 1l in
  [%test_result: Int64.t] ~expect:1L (Eventfd.read fd);
  let fd = create ~flags:Eventfd.Flags.semaphore 10l in
  [%test_result: Int64.t] ~expect:1L (Eventfd.read fd)
;;

let nonblock_read fd =
  try Some (Eventfd.read fd) with
  | _ -> None
;;

let%test_unit "Eventfd.write updates the counter, and Eventfd.read clears it on a \
               non-semaphore fd"
  =
  let fd = create ~flags:Eventfd.Flags.nonblock 1l in
  Eventfd.write fd 10L;
  [%test_result: Int64.t] ~expect:11L (Eventfd.read fd);
  [%test_result: Int64.t option] ~expect:None (nonblock_read fd)
;;

let%test_unit "Eventfd.read will not block until the counter is decremented to zero on a \
               semaphore fd"
  =
  let fd = create ~flags:Eventfd.Flags.(nonblock + semaphore) 10l in
  let count = ref 10L in
  while Int64.(!count > 0L) do
    [%test_result: Int64.t option] ~expect:(Some 1L) (nonblock_read fd);
    Int64.decr count
  done;
  [%test_result: Int64.t] ~expect:0L !count
;;

(* CPU Affinity *)
let%expect_test "set and get affinity" =
  match sched_getaffinity, sched_setaffinity with
  | Ok sched_getaffinity, Ok sched_setaffinity ->
    let print_affinity () = printf !"%{sexp:int list}" (sched_getaffinity ()) in
    let starting_cpus = sched_getaffinity () in
    sched_setaffinity ~cpuset:[ 0 ] ();
    print_affinity ();
    [%expect {| (0) |}];
    sched_setaffinity ~cpuset:[ 1; 2 ] ();
    print_affinity ();
    [%expect {| (1 2) |}];
    sched_setaffinity ~cpuset:starting_cpus ();
    printf "%b" (starting_cpus = sched_getaffinity ());
    [%expect {| true |}]
  | _ -> ()
;;

(* Priority *)
let%test_module "getpriority and setpriority" =
  (module struct
    let getpriority = ok_exn getpriority
    let setpriority = ok_exn setpriority
    let gettid = ok_exn Core_unix.gettid

    let print_priority ?pid () =
      printf !"%{sexp:Linux_ext.Priority.t}\n" (getpriority ?pid ())
    ;;

    let%expect_test "this thread" =
      let starting_priority = getpriority () in
      (* All of these actually refer to the current thread. *)
      let tids = [ None; Some (gettid () |> Thread_id.to_int |> Pid.of_int) ] in
      List.iteri (List.cartesian_product tids tids) ~f:(fun index (set_pid, get_pid) ->
        let priority = Linux_ext.Priority.of_int (index + 1) in
        setpriority ?pid:set_pid priority;
        print_priority ?pid:get_pid ());
      [%expect {|
        1
        2
        3
        4
        |}];
      setpriority starting_priority;
      print_s
        [%sexp ([%equal: Linux_ext.Priority.t] starting_priority (getpriority ()) : bool)];
      [%expect {| true |}]
    ;;

    let%expect_test "other thread" =
      let child_tid = Set_once.create () in
      let ready_for_thread_to_get_priority = Set_once.create () in
      let priority_retrieved_in_thread = Set_once.create () in
      let ready_for_thread_to_exit = Set_once.create () in
      (* [mutex] guards the vars above; [condition] is signalled when any change.

         As there are only two threads involved, [signal] will suffice, but we may as well
         broadcast since we do not care about performance and don't want to ambush someone
         that modifies this test to add three threads.

         The [`Locked] argument indicates that the caller knows they must have the mutex
         locked. *)
      let mutex = Caml_threads.Mutex.create () in
      let condition = Caml_threads.Condition.create () in
      let rec wait_for_set_once set_once `Locked =
        match Set_once.get set_once with
        | None ->
          Caml_threads.Condition.wait condition mutex;
          wait_for_set_once set_once `Locked
        | Some v -> v
      in
      let set_set_once set_once value `Locked =
        Set_once.set_exn set_once [%here] value;
        Caml_threads.Condition.broadcast condition
      in
      let thread_body () =
        Caml_threads.Mutex.lock mutex;
        setpriority (Priority.of_int 4);
        let tid = gettid () |> Thread_id.to_int |> Pid.of_int in
        set_set_once child_tid tid `Locked;
        wait_for_set_once ready_for_thread_to_get_priority `Locked;
        let priority = getpriority () in
        set_set_once priority_retrieved_in_thread priority `Locked;
        wait_for_set_once ready_for_thread_to_exit `Locked;
        Caml_threads.Mutex.unlock mutex
      in
      Caml_threads.Mutex.lock mutex;
      let starting_priority = getpriority () in
      setpriority (Priority.of_int 6);
      print_priority ();
      [%expect {| 6 |}];
      (* Create the thread. *)
      let thread = Caml_threads.Thread.create thread_body () in
      let child_tid = wait_for_set_once child_tid `Locked in
      (* When the child starts, it sets its priority to 4. We can read it by its TID: *)
      print_priority ~pid:child_tid ();
      [%expect {| 4 |}];
      (* Our priority is unchanged: *)
      print_priority ();
      [%expect {| 6 |}];
      (* We can modify the child's priority from here: *)
      setpriority ~pid:child_tid (Priority.of_int 5);
      print_priority ~pid:child_tid ();
      [%expect {| 5 |}];
      (* Our priority is unchanged: *)
      print_priority ();
      [%expect {| 6 |}];
      (* Ask the child to read its own priority: *)
      set_set_once ready_for_thread_to_get_priority () `Locked;
      let priority_retrieved_in_thread =
        wait_for_set_once priority_retrieved_in_thread `Locked
      in
      (* It reads the correct priority: *)
      print_s [%sexp (priority_retrieved_in_thread : Priority.t)];
      [%expect {| 5 |}];
      (* Clean up. *)
      set_set_once ready_for_thread_to_exit () `Locked;
      Caml_threads.Mutex.unlock mutex;
      Caml_threads.Thread.join thread;
      setpriority starting_priority
    ;;
  end)
;;

let%test_unit "get_terminal_size" =
  match get_terminal_size with
  | Error _ -> ()
  | Ok f ->
    let with_tmp_fd f =
      protectx
        (Filename_unix.temp_file "get_terminal_size" "")
        ~finally:Unix.unlink
        ~f:(fun fname ->
        protectx
          (Unix.openfile fname ~mode:[ Unix.O_RDONLY ] ~perm:0)
          ~finally:Unix.close
          ~f)
    in
    (match with_tmp_fd (fun fd -> f (`Fd fd)) with
     | exception Unix.Unix_error (ENOTTY, _, _) -> ()
     | res ->
       raise_s
         [%sexp "get_terminal_size should have failed but returned", (res : int * int)])
;;

(* Tested by hand:
   eprintf !"get_terminal_size: %{sexp: int * int}\n%!" (f `Controlling);
*)

(* Extended file attributes *)
let%test_module "getxattr and setxattr" =
  (module struct
    let expect_error f =
      match f () with
      | exception exn -> print_s [%sexp (exn : Exn.t)]
      | _ -> raise_s [%message "expected error but returned"]
    ;;

    let with_tmpfile f =
      let tmpfile = "temporary-file-for-testing-xattr" in
      let fd = Unix.openfile tmpfile ~mode:[ Unix.O_CREAT; Unix.O_WRONLY ] in
      Unix.close fd;
      (try f tmpfile with
       | (_ : exn) -> ());
      Unix.unlink tmpfile
    ;;

    let get_and_print ~follow_symlinks ~path ~name =
      let value =
        (Extended_file_attributes.getxattr |> ok_exn) ~follow_symlinks ~path ~name
      in
      print_s [%sexp (value : Extended_file_attributes.Get_attr_result.t)]
    ;;

    let set_and_print ?how ~follow_symlinks ~path ~name ~value () =
      let result =
        (Extended_file_attributes.setxattr |> ok_exn)
          ?how
          ~follow_symlinks
          ~path
          ~name
          ~value
          ()
      in
      print_s [%sexp (result : Extended_file_attributes.Set_attr_result.t)]
    ;;

    let%expect_test "simple test" =
      with_tmpfile (fun path ->
        let name = "user.foo" in
        get_and_print ~follow_symlinks:true ~path ~name;
        [%expect {| ENOATTR |}];
        set_and_print ~follow_symlinks:true ~path ~name ~value:"bar" ();
        [%expect {| Ok |}];
        get_and_print ~follow_symlinks:true ~path ~name;
        [%expect {| (Ok bar) |}])
    ;;

    let%expect_test "symlink test" =
      with_tmpfile (fun path ->
        let symlink_to_path = path ^ ".symlink" in
        Unix.symlink ~target:path ~link_name:symlink_to_path;
        let name = "user.foo" in
        get_and_print ~follow_symlinks:false ~path:symlink_to_path ~name;
        [%expect {| ENOATTR |}];
        get_and_print ~follow_symlinks:true ~path:symlink_to_path ~name;
        [%expect {| ENOATTR |}];
        expect_error (fun () ->
          set_and_print ~follow_symlinks:false ~path:symlink_to_path ~name ~value:"baz" ());
        [%expect
          {|
          (Unix.Unix_error
           "Operation not permitted"
           lsetxattr
           temporary-file-for-testing-xattr.symlink)
          |}];
        set_and_print ~follow_symlinks:true ~path:symlink_to_path ~name ~value:"bar" ();
        [%expect {| Ok |}];
        get_and_print ~follow_symlinks:false ~path:symlink_to_path ~name;
        [%expect {| ENOATTR |}];
        get_and_print ~follow_symlinks:true ~path:symlink_to_path ~name;
        [%expect {| (Ok bar) |}];
        Unix.unlink symlink_to_path)
    ;;

    let%expect_test "roundtrip a binary blob that contains null characters to confirm \
                     that they are preserved"
      =
      with_tmpfile (fun path ->
        let name = "user.foo" in
        get_and_print ~follow_symlinks:false ~path ~name;
        [%expect {| ENOATTR |}];
        set_and_print ~follow_symlinks:false ~path ~name ~value:"foo\000\nbar\000" ();
        [%expect {| Ok |}];
        get_and_print ~follow_symlinks:false ~path ~name;
        [%expect {| (Ok "foo\000\nbar\000") |}])
    ;;

    let%expect_test "test setxattr [`Create] semantics" =
      with_tmpfile (fun path ->
        let name = "user.foo" in
        let set_and_create () =
          set_and_print ~how:`Create ~follow_symlinks:true ~path ~name ~value:"blah" ()
        in
        set_and_create ();
        [%expect {| Ok |}];
        get_and_print ~follow_symlinks:true ~path ~name;
        [%expect {| (Ok blah) |}];
        set_and_create ();
        [%expect {| EEXIST |}])
    ;;

    let%expect_test "test setxattr [`Replace] semantics" =
      with_tmpfile (fun path ->
        let name = "user.foo" in
        let set_with_replace () =
          set_and_print ~how:`Replace ~follow_symlinks:true ~path ~name ~value:"xyz" ()
        in
        set_with_replace ();
        [%expect {| ENOATTR |}];
        set_and_print ~how:`Create ~follow_symlinks:true ~path ~name ~value:"bar" ();
        [%expect {| Ok |}];
        get_and_print ~follow_symlinks:true ~path ~name;
        [%expect {| (Ok bar) |}];
        set_with_replace ();
        [%expect {| Ok |}];
        get_and_print ~follow_symlinks:true ~path ~name;
        [%expect {| (Ok xyz) |}])
    ;;

    let%expect_test "test getxattr and setxattr on a non-existent file" =
      let path = "some-file-that-doesnt-exist" in
      let name = "user.foo" in
      expect_error (fun () ->
        set_and_print ~follow_symlinks:true ~path ~name ~value:"xyz" ());
      [%expect
        {|
        (Unix.Unix_error
         "No such file or directory"
         setxattr
         some-file-that-doesnt-exist)
        |}];
      expect_error (fun () -> get_and_print ~follow_symlinks:true ~path ~name);
      [%expect
        {|
        (Unix.Unix_error
         "No such file or directory"
         getxattr
         some-file-that-doesnt-exist)
        |}]
    ;;
  end)
;;

let with_listening_server_unix_socket fname ~f =
  let server_sock = Unix.socket ~domain:PF_UNIX ~kind:SOCK_STREAM ~protocol:0 () in
  Unix.bind server_sock ~addr:(ADDR_UNIX fname);
  let thread =
    Thread.create
      ~on_uncaught_exn:`Print_to_stderr
      (fun () -> Unix.listen server_sock ~backlog:10)
      ()
  in
  f fname;
  Thread.join thread
;;

let%test_unit "peer_credentials" =
  match Linux_ext.peer_credentials with
  | Error _ -> ()
  | Ok peer_credentials ->
    protectx
      (Filename_unix.temp_file "linux_ext" "")
      ~finally:Unix.unlink
      ~f:(fun fname ->
      (let fd = Unix.openfile fname ~mode:[ O_RDONLY ] in
       try
         ignore (peer_credentials fd : Peer_credentials.t);
         failwith "peer credential on non socket should have raised"
       with
       | Unix.Unix_error (ENOTSOCK, _, _) -> ());
      with_listening_server_unix_socket (fname ^ ".peercredsocket") ~f:(fun fname ->
        let client_sock = Unix.socket ~domain:PF_UNIX ~kind:SOCK_STREAM ~protocol:0 () in
        let rec connect count =
          try Unix.connect client_sock ~addr:(ADDR_UNIX fname) with
          | Unix_error (ECONNREFUSED, _, _) when count < 100 ->
            (* the server might not have listened yet *)
            ignore (Unix.nanosleep 0.1 : float);
            connect (count + 1)
        in
        connect 0;
        let p = peer_credentials client_sock in
        [%test_eq: Pid.t] p.pid (Unix.getpid ());
        [%test_eq: int] p.uid (Unix.getuid ());
        [%test_eq: int] p.gid (Unix.getgid ())))
;;

let%expect_test "cpu_list_of_string_exn" =
  let cpu_lines =
    [ ""
    ; "0"
    ; "0,2,10"
    ; "0,2-5"
    ; "5-2"
    ; "0,2-5:6/2"
    ; "0-15:1/2,9-15:1/2"
    ; "3-"
    ; "0-3:10/20"
    ; "0-3:0/20"
    ; "0-3:-2/0"
    ; "0-3:0/0"
    ]
  in
  List.iter cpu_lines ~f:(fun cpu_list ->
    try
      let cpulist = Linux_ext.cpu_list_of_string_exn cpu_list in
      let strlist = List.map cpulist ~f:string_of_int |> String.concat ~sep:"," in
      print_endline [%string "CPUs: %{strlist}"]
    with
    | e -> print_endline [%string "Error: %{e#Exn}"]);
  [%expect
    {|
    CPUs:
    CPUs: 0
    CPUs: 0,2,10
    CPUs: 0,2,3,4,5
    Error: ("cpu_list_of_string_exn: range start is after end" (first 5) (last 2))
    CPUs: 0,2,3,4,5
    CPUs: 0,2,4,6,8,9,10,11,12,13,14,15
    Error: ("cpu_list_of_string_exn: expected separated integer pair" (sep -) (str 3-))
    CPUs: 0,1,2,3
    Error: ("cpu_list_of_string_exn: invalid grouped range stride or amount" (amt 0)
      (stride 20))
    Error: ("cpu_list_of_string_exn: invalid grouped range stride or amount" (amt -2)
      (stride 0))
    Error: ("cpu_list_of_string_exn: invalid grouped range stride or amount" (amt 0)
      (stride 0))
    |}]
;;

let%expect_test "TCP_CONGESTION" =
  let gettcpopt_string = Or_error.ok_exn Linux_ext.gettcpopt_string in
  let settcpopt_string = Or_error.ok_exn Linux_ext.settcpopt_string in
  let sock = Unix.socket ~domain:PF_INET ~kind:SOCK_STREAM ~protocol:0 () in
  (* man 7 tcp says that "reno" is always permitted. *)
  settcpopt_string sock TCP_CONGESTION "reno";
  let str = gettcpopt_string sock TCP_CONGESTION in
  print_s [%sexp (str : string)];
  [%expect {| reno |}];
  (* basically everyone uses cubic as the default these days, so for the purposes of
     this test I'm willing to assume that it's available for this test. *)
  settcpopt_string sock TCP_CONGESTION "cubic";
  let str = gettcpopt_string sock TCP_CONGESTION in
  print_s [%sexp (str : string)];
  [%expect {| cubic |}];
  (* Passing a garbage algo should raise: *)
  List.iter [ ""; "\000"; "notaccalgo"; "longer-than-16-chars" ] ~f:(fun garbage ->
    Expect_test_helpers_base.show_raise (fun () ->
      settcpopt_string sock TCP_CONGESTION garbage));
  [%expect
    {|
    (raised (Unix.Unix_error "Invalid argument" setsockopt ""))
    (raised (Unix.Unix_error "Invalid argument" setsockopt ""))
    (raised (Unix.Unix_error "No such file or directory" setsockopt ""))
    (raised (Unix.Unix_error "No such file or directory" setsockopt ""))
    |}];
  (* We need to close the socket to clean up the test... *)
  Unix.close sock;
  (* But it also gives us an opportunity to demonstrate correct behaviour (gracefully
     raising) on an illegal file descriptor: *)
  Expect_test_helpers_base.show_raise (fun () -> gettcpopt_string sock TCP_CONGESTION);
  Expect_test_helpers_base.show_raise (fun () ->
    settcpopt_string sock TCP_CONGESTION "reno");
  [%expect
    {|
    (raised (Unix.Unix_error "Bad file descriptor" getsockopt ""))
    (raised (Unix.Unix_error "Bad file descriptor" setsockopt ""))
    |}]
;;
