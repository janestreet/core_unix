open! Core
open Poly
open! Import
open! Unix.Syscall_result

let%test_unit "to_result doesn't allocate" =
  for _ = 1 to 1000 do
    for
      int = -(Int.Private.length_preallocated_errnos - 1)
      to Int.Private.length_preallocated_ms - 1
    do
      let t = Int.Private.of_int int in
      let result =
        Expect_test_helpers_core.require_no_allocation (fun () -> Int.to_result t)
      in
      [%test_result: (int, Unix.Error.t) Result.t]
        result
        ~expect:
          (if is_ok t then Ok int else Error (Unix.Error.of_system_int ~errno:(-int)))
    done
  done
;;

let%test_unit _ =
  for i = 1 to 10000 do
    let err = Unix.Error.of_system_int ~errno:i in
    assert (err = Unit.error_exn (Unit.create_error err))
  done
;;
