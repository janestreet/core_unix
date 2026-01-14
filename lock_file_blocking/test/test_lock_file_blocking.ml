open! Core
open! Lock_file_blocking
module Unix = Core_unix

module%test _ = struct
  let lock_file = Filename_unix.temp_file "lock_file" "unit_test"
  let () = Unix.unlink lock_file
  let%test _ = create lock_file
  let%test _ = not (create lock_file)
  let%test _ = is_locked lock_file
  let nolock_file = Filename_unix.temp_file "nolock_file" "unit_test"

  let () =
    Unix.unlink nolock_file;
    (* Create an empty file. *)
    Unix.close (Unix.openfile nolock_file ~mode:[ Unix.O_CREAT; Unix.O_WRONLY ])
  ;;

  let%test _ =
    (* Check that file exists. *)
    try
      ignore (Unix.stat nolock_file);
      true
    with
    | Unix.Unix_error (ENOENT, _, _) -> false
  ;;

  let%test _ = not (is_locked nolock_file)
end

module%test [@name "[Nfs]"] _ = struct
  open! Nfs

  let create_bool ?message path =
    match create ?message path with
    | Ok () -> true
    | Error _ -> false
  ;;

  let path = Filename_unix.temp_file "lock_file" "unit_test"
  let () = Unix.unlink path
  let%test _ = create_bool path
  let%test _ = not (create_bool path)
  let () = unlock_exn path
  let%test _ = create_bool path
  let () = unlock_exn path

  open Expect_test_helpers_core

  let%expect_test "get lock info" =
    let dir = Filename_unix.temp_dir "lock_file" "unit_test" in
    let path = dir ^/ "lock" in
    assert (create_bool ~message:"hello world" path);
    let pid = get_hostname_and_pid path |> Option.value_exn |> snd in
    require_equal (module Pid) pid (Unix.getpid ());
    let msg = get_message path |> Option.value_exn in
    require_equal (module String) msg "hello world";
    unlock_exn path;
    [%expect ""]
  ;;
end

module%test [@name "[Nfs-v2]"] _ = struct
  open! Nfs

  let%expect_test "locking and unlocking works" =
    Filesystem_core.with_temp_dir (fun tmp_dir ->
      let path = File_path.Absolute.to_string tmp_dir ^/ "lock" in
      (* Taking the initial lock works. *)
      create_v2 path |> ok_exn;
      unlock_exn path;
      (* Taking the lock again after the initial attempt was unlocked works. *)
      create_v2 path |> ok_exn;
      unlock_exn path)
  ;;

  let%expect_test "clean up in case old attempts gets left on disk" =
    let test_lock_unlock ?(additional_files = []) attempt_states =
      Filesystem_core.with_temp_dir (fun tmp_dir ->
        let tmp_dir = File_path.Absolute.to_string tmp_dir in
        let path = tmp_dir ^/ "lock" in
        let create_attempt_file i state =
          let attempt_file_path =
            let suffix =
              match state with
              | `Fresh -> String.make 24 'f'
              | `Old -> String.make 24 '0'
            in
            path ^ sprintf ".%08d%s.lock-attempt" i suffix
          in
          Out_channel.write_all attempt_file_path ~data:"doesn't matter";
          match state with
          | `Fresh -> ()
          | `Old ->
            Unix.utimes
              attempt_file_path
              (* apparently 0 is a special value that corresponds to "now", thus +1 here *)
              ~modif:(Float.of_int (i + 1))
              ~access:(Float.of_int (i + 1))
        in
        List.iteri attempt_states ~f:create_attempt_file;
        List.iter additional_files ~f:(fun additional_file ->
          let additional_file_path = tmp_dir ^/ additional_file in
          Unix.openfile ~mode:[ O_WRONLY; O_CREAT ] additional_file_path |> Unix.close;
          (* Always make additional files old to make sure that we don't accidentally
             clean them up. *)
          Unix.utimes additional_file_path ~modif:1. ~access:1.);
        let ls () =
          List.iter
            (Sys_unix.ls_dir tmp_dir |> List.sort ~compare:String.compare)
            ~f:print_endline
        in
        print_endline "Before lock and unlock:";
        ls ();
        create_v2 path |> ok_exn;
        unlock_exn path;
        print_endline "";
        print_endline "After lock and unlock:";
        ls ())
    in
    (* All fresh attempt lock files are kept. *)
    test_lock_unlock (List.init 5 ~f:(fun i -> if i < 4 then `Fresh else `Old));
    [%expect
      {|
      Before lock and unlock:
      lock.00000000ffffffffffffffffffffffff.lock-attempt
      lock.00000001ffffffffffffffffffffffff.lock-attempt
      lock.00000002ffffffffffffffffffffffff.lock-attempt
      lock.00000003ffffffffffffffffffffffff.lock-attempt
      lock.00000004000000000000000000000000.lock-attempt

      After lock and unlock:
      lock.00000000ffffffffffffffffffffffff.lock-attempt
      lock.00000001ffffffffffffffffffffffff.lock-attempt
      lock.00000002ffffffffffffffffffffffff.lock-attempt
      lock.00000003ffffffffffffffffffffffff.lock-attempt
      |}];
    (* Old attempt lock files are cleared, but we keep the last 3 around. Unrelated files
       are not affected. *)
    test_lock_unlock ~additional_files:[ "unrelated" ] (List.init 5 ~f:(Fn.const `Old));
    [%expect
      {|
      Before lock and unlock:
      lock.00000000000000000000000000000000.lock-attempt
      lock.00000001000000000000000000000000.lock-attempt
      lock.00000002000000000000000000000000.lock-attempt
      lock.00000003000000000000000000000000.lock-attempt
      lock.00000004000000000000000000000000.lock-attempt
      unrelated

      After lock and unlock:
      lock.00000002000000000000000000000000.lock-attempt
      lock.00000003000000000000000000000000.lock-attempt
      lock.00000004000000000000000000000000.lock-attempt
      unrelated
      |}]
  ;;

  open Expect_test_helpers_core

  let%expect_test "get lock info" =
    Filesystem_core.with_temp_dir (fun dir ->
      let path = File_path.Absolute.to_string dir ^/ "lock" in
      create_v2 ~message:"hello world" path |> ok_exn;
      let pid = get_hostname_and_pid path |> Option.value_exn |> snd in
      require_equal (module Pid) pid (Unix.getpid ());
      let msg = get_message path |> Option.value_exn in
      require_equal (module String) msg "hello world";
      unlock_exn path;
      [%expect ""])
  ;;

  let%expect_test "early return on permission error" =
    Filesystem_core.within_temp_dir (fun () ->
      (* chdir to make the error messages more predictable (although we still need to
         erase the file paths from them...) *)
      Unix.chmod "." ~perm:0o555;
      let path = "./lock" in
      let hex32 = lazy (Re.compile (Re.seq [ Re.repn Re.xdigit 32 (Some 32) ])) in
      let rewrite_hex32 string =
        Re.replace_string (force hex32) ~by:"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" string
      in
      let sexp_rewrite_atoms ~f =
        let rec go (sexp : Sexp.t) : Sexp.t =
          match sexp with
          | Atom s -> Atom (f s)
          | List l -> List (List.map ~f:go l)
        in
        go
      in
      let tst f =
        let res = Result.try_with f in
        print_s
          ([%sexp (res : (unit, exn) Result.t)] |> sexp_rewrite_atoms ~f:rewrite_hex32)
      in
      let () = tst (fun () -> blocking_create_v2 ~exn:`Mach path) in
      [%expect
        {|
        (Error (
          Unix.Unix_error
          "Permission denied"
          open
          "((filename ./lock.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.lock-attempt) (mode (O_WRONLY O_CREAT O_TRUNC O_EXCL)) (perm 0o666))"))
        |}])
  ;;
end

open Expect_test_helpers_core

let%expect_test "Symlink lock protocol" =
  let lock_path = Filename_unix.temp_file "lock_file" "unit_test" in
  let () = Unix.unlink lock_path in
  let lock_expect_success ~metadata =
    match Symlink.lock_exn ~lock_path ~metadata with
    | `We_took_it lock -> lock
    | `Somebody_else_took_it _ ->
      failwith "BUG: failed to take the lock when we should have succeeded"
  in
  let lock_expect_failure ~metadata =
    match Symlink.lock_exn ~lock_path ~metadata with
    | `We_took_it _lock -> failwith "BUG: took the lock when we shouldn't have"
    | `Somebody_else_took_it metadata ->
      print_s [%sexp "Somebody else took it", { metadata : string Or_error.t }]
  in
  let lock = lock_expect_success ~metadata:"hello world" in
  lock_expect_failure ~metadata:"goodbye world";
  [%expect {| ("Somebody else took it" ((metadata (Ok "hello world")))) |}];
  let () = Symlink.unlock_exn lock in
  let lock = lock_expect_success ~metadata:"hi" in
  lock_expect_failure ~metadata:"hi";
  [%expect {| ("Somebody else took it" ((metadata (Ok hi)))) |}];
  Symlink.unlock_exn lock
;;
