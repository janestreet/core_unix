open! Core
module Unix = Core_unix

let sanity_check_rusage (rusage : Unix.Resource_usage.t) =
  let assert_between sexp_of_a le lo v hi =
    if le lo v && le v hi
    then ()
    else
      raise_s [%sexp "value not in the expected range", ~~(lo : a), ~~(v : a), ~~(hi : a)]
  in
  assert_between [%sexp_of: Float.t] Float.( <= ) 0. rusage.utime 10.;
  assert_between [%sexp_of: Float.t] Float.( <= ) 0. rusage.stime 10.;
  assert_between [%sexp_of: Int64.t] Int64.( <= ) 0L rusage.maxrss 2_000_000L
;;

let%expect_test "Resource_usage.get" =
  let rusage = Unix.Resource_usage.get `Self in
  sanity_check_rusage rusage
;;

let%expect_test "wait_with_resource_usage" =
  (let%tydi { pid; _ } = Unix.create_process ~prog:"true" ~args:[] in
   let (pid', exit_or_signal), rusage = Unix.wait_with_resource_usage (`Pid pid) in
   [%test_eq: Pid.t] pid pid';
   [%test_eq: Unix.Exit_or_signal.t] exit_or_signal (Ok ());
   sanity_check_rusage rusage);
  (let%tydi { pid; _ } = Unix.create_process ~prog:"false" ~args:[] in
   let (pid', exit_or_signal), rusage = Unix.wait_with_resource_usage (`Pid pid) in
   [%test_eq: Pid.t] pid pid';
   [%test_eq: Unix.Exit_or_signal.t] exit_or_signal (Error (`Exit_non_zero 1));
   sanity_check_rusage rusage);
  ()
;;
