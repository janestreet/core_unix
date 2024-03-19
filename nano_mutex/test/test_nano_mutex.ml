open! Core
open! Nano_mutex
module Unix = Core_unix
module Thread = Core_thread

let%test_unit _ =
  let l = create () in
  lock_exn l;
  unlock_exn l;
  match try_lock l with
  | Ok `Not_acquired | Error _ -> assert false
  | Ok `Acquired -> unlock_exn l
;;

let%test_unit _ =
  List.iter
    ([ 2, 100, 0.; 10, 100, 0.; 10, 100, 0.001 ]
     @
     if Sys.word_size_in_bits = 32
     then [] (* not enough address space when the stack limit is high *)
     else [ 100, 10, 0.001 ])
    ~f:(fun (num_threads, num_iterations, pause_for) ->
      try
        let l = create () in
        let am_holding_lock = ref false in
        let one_thread () =
          Thread.create
            ~on_uncaught_exn:`Print_to_stderr
            (fun () ->
              for _ = 1 to num_iterations do
                lock_exn l;
                if !am_holding_lock then failwith "lock multiply acquired";
                am_holding_lock := true;
                ignore (Unix.nanosleep pause_for : float);
                am_holding_lock := false;
                unlock_exn l
              done)
            ()
        in
        let threads = List.init num_threads ~f:(fun _ -> one_thread ()) in
        List.iter threads ~f:Thread.join
      with
      | exn ->
        failwiths
          ~here:[%here]
          "test failed"
          (num_threads, num_iterations, pause_for, exn)
          [%sexp_of: int * int * float * exn])
;;

let%test_unit "does not allocate while lock/unlocking while another thread holds the lock"
  =
  let t = create () in
  let thread_started = Thread_safe_ivar.create () in
  let terminate_thread = Thread_safe_ivar.create () in
  let thread =
    Thread.create
      ~on_uncaught_exn:`Kill_whole_process
      (fun () ->
        lock_exn t;
        Thread_safe_ivar.fill thread_started ();
        Core_unix.sleep 10;
        unlock_exn t;
        Thread_safe_ivar.read terminate_thread)
      ()
  in
  Thread_safe_ivar.read thread_started;
  Gc.For_testing.assert_no_allocation [%here] (fun () ->
    lock_exn t;
    unlock_exn t);
  Thread_safe_ivar.fill terminate_thread ();
  Thread.join thread
;;
